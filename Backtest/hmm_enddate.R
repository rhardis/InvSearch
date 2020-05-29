remove(list = ls())

library('quantmod')
library(lubridate)
set.seed(1)

getSymbols("^GSPC",from="1900-01-01")
gspcRets = diff( log( Ad( GSPC ) ) )
returns = as.numeric(gspcRets)
returns[1] = 0

gspcRets.ex = as.data.frame(cbind(gspcRets,seq(1,length(gspcRets))))
cutoff_idx = gspcRets.ex["1960-12-22",2]
#cutoff_idx = gspcRets.ex["2020-04-06",2]
days_to_model = nrow(gspcRets) - cutoff_idx # Start date for backtest3 
gpx = gspcRets.ex[(nrow(gspcRets.ex)-days_to_model+1):nrow(gspcRets.ex),]

library(doParallel)
registerDoParallel(cores=detectCores())

state_predictions = foreach(i = 1:days_to_model, .combine=c) %dopar% {
  library(depmixS4)
  library(plyr)
  truncated_returns = returns[1:(cutoff_idx+i)]
  hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 4, data=data.frame(returns=truncated_returns))
  hmmfit <- fit(hmm, verbose = F)
  post_probs <- posterior(hmmfit)
  post_probs$returns = truncated_returns
  
  s1 = sd(post_probs[post_probs$state==1,]$returns)
  s2 = sd(post_probs[post_probs$state==2,]$returns)
  s3 = sd(post_probs[post_probs$state==3,]$returns)
  s4 = sd(post_probs[post_probs$state==4,]$returns)
  
  v = c(s1,s2,s3,s4)
  vnums = c(1,2,3,4)
  vdf = data.frame(cbind(v,vnums))
  vdf = vdf[order(vdf$v),]
  
  lvol = vdf$vnums[1]
  mvol = vdf$vnums[2]
  hvol = vdf$vnums[3]
  correction = vdf$vnums[4]
  
  cstate = post_probs$state[nrow(post_probs)]
  
  #write.csv(sprintf("postprobs_%i", i))
  
  if (cstate==correction){
   pred = "Level4"
  }
  else if (cstate==mvol){
    pred = "Level2"
  }
  else if (cstate==lvol){
    pred = "Level1"
  }
  else{
    pred = "Level3"
  }
  to.state_predictions = pred
  to.state_predictions
}

preds_df = cbind(gpx,state_predictions)
write.csv(preds_df, sprintf("C:\\Users\\richa\\Documents\\GitHub\\InvSearch\\data\\state_predictions_1960.csv", days_to_model))
