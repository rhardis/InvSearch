remove(list = ls())
dev.off()
tstart = Sys.time()

library('quantmod')
library(lubridate)
set.seed(1)

getSymbols("^GSPC",from="1900-01-01")
GSPC = to.weekly(GSPC, indexAt="startof")
gspcRets = diff( log( Ad( GSPC ) ) )
returns = as.numeric(gspcRets)
returns[1] = 0

gspcRets.ex = as.data.frame(cbind(gspcRets,seq(1,length(gspcRets))))
#cutoff_idx = gspcRets.ex["1980-06-27",2]
cutoff_idx = gspcRets.ex["2020-06-08",2]
periods_to_model = nrow(gspcRets) - cutoff_idx # Start date for backtest3 
gpx = gspcRets.ex[(nrow(gspcRets.ex)-periods_to_model+1):nrow(gspcRets.ex),]
returns = returns[(length(returns) - periods_to_model + 1):length(returns)]
gspc.df = as.data.frame(GSPC)
prices = gspc.df[(nrow(gspc.df)-periods_to_model+1):nrow(gspc.df),"GSPC.Adjusted"]

library(doParallel)
#registerDoParallel(cores=detectCores())
library(doSNOW)
cl = makeCluster(detectCores())
registerDoSNOW(cl)
pb = txtProgressBar(max = periods_to_model, style=3)
progress = function(n) setTxtProgressBar(pb, n)
opts = list(progress = progress)

state_predictions = foreach(i = 1:periods_to_model, .combine=c, .options.snow = opts) %dopar% {
  library(depmixS4)
  library(plyr)
  truncated_returns = returns[1:(cutoff_idx+i)]
  hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 2, data=data.frame(returns=truncated_returns))
  hmmfit <- fit(hmm, verbose = F)
  post_probs <- posterior(hmmfit)
  post_probs$returns = truncated_returns
  
  s1 = sd(post_probs[post_probs$state==1,]$returns)
  s2 = sd(post_probs[post_probs$state==2,]$returns)
  s3 = sd(post_probs[post_probs$state==3,]$returns)
  s4 = sd(post_probs[post_probs$state==4,]$returns)
  
  v = c(s1,s2,s3,s4)
  vnums = c(1,2,3,4)
  # v = c(s1, s2)
  # vnums = c(1,2)
  vdf = data.frame(cbind(v,vnums))
  vdf = vdf[order(vdf$v),]
  
  lvol = vdf$vnums[1]
  mvol = vdf$vnums[2]
  hvol = vdf$vnums[3]
  correction = vdf$vnums[4]
  
  cstate = post_probs$state[nrow(post_probs)]
  
  if (cstate==lvol){
   pred = "Level1"
  }
  else if (cstate==mvol){
    pred = "Level2"
  }
  else if (cstate==correction){
    pred = "Level4"
  }
  else{
    pred = "Level3"
  }
  # to.state_predictions = pred
  # to.state_predictions
  return(post_probs)
}
close(pb)
stopCluster(cl)

preds_df = cbind(gpx,state_predictions,returns,prices)
names(preds_df)[names(preds_df) == "prices"] = "Close.Price"
names(preds_df)[names(preds_df) == "state_predictions"] = "State_Name"
preds_df["date"] = ymd(rownames(as.data.frame(gspcRets[(nrow(gspcRets)-periods_to_model+1):nrow(gspcRets),])))
write.csv(preds_df, sprintf("C:\\Users\\richa\\Documents\\GitHub\\InvSearch\\data\\state_predictions_weekly_test.csv", periods_to_model))

library(patchwork)
library(ggplot2)

densall = ggplot(data=preds_df, aes(x=returns))+
  geom_density(aes(color=State_Name))+
  ggtitle("Density of Log\nReturns by Regime")+xlim(-.1,.1)

close.p =  ggplot(preds_df, aes(x=date, y=Close.Price, color=State_Name)) +
  geom_line()+
  ylab("Closing Price ($)")+
  scale_x_date(date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Closing Prices")

p = ggplot(preds_df, aes(x=date, y=returns, color=State_Name)) +
  geom_point()+
  ylab("log returns")+
  scale_x_date(date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Regime Predictions")

densall + (p / close.p)


tend = Sys.time()
print(tend-tstart)

