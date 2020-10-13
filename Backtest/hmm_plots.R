remove(list = ls())
dev.off()

# Obtain S&P500 data from 2004 onwards and
# create the returns stream from this

library('depmixS4')
library('quantmod')
library('gridExtra')
library('patchwork')
library(ggplot2)
library(lubridate)
library(plyr)
set.seed(1)
getSymbols( "^GSPC", from="1927-12-30")
GSPC = to.weekly(GSPC, indexAt="startof")
gspcRets = diff( log( Cl( GSPC ) ) )
returns = as.numeric(gspcRets)
returns[1] = 0

plot(gspcRets)

ggplot(gspcRets, aes(GSPC.Close))+ geom_density()+
  ggtitle("Distribution of Daily Log Returns\n1927-Present") +
  xlab("Return") + ylab("Density")+
  xlim(-.1,.1)

mean(returns)

density(returns)

# Fit a Hidden Markov Model with three states 
# to the S&P500 returns stream
hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 3, data=data.frame(returns=returns))
hmmfit <- fit(hmm, verbose = FALSE)
hmmpars = getpars(hmmfit)
hmmpars
post_probs <- posterior(hmmfit)

post_probs$returns = returns

s1 = sd(post_probs[post_probs$state==1,]$returns)
s2 = sd(post_probs[post_probs$state==2,]$returns)
s3 = sd(post_probs[post_probs$state==3,]$returns)
#s4 = sd(post_probs[post_probs$state==4,]$returns)
s1
s2
s3
# s4

v = c(s1,s2,s3)
vnums = c(1,2,3)
vdf = data.frame(cbind(v,vnums))
vdf = vdf[order(vdf$v),]

lvol = vdf$vnums[1]
mvol = vdf$vnums[2]
hvol = vdf$vnums[3]

label = function(row){
  cstate = row[1]
  
  if (cstate==mvol){
    pred = "MedVol"
  }
  else if (cstate==lvol){
    pred = "LowVol"
  }
  else{
    pred = "HighVol"
  }
  return(pred)
}

post_probs["State_Name"] = apply(post_probs,1,label)

# Plot the returns stream and the posterior
# probabilities of the separate regimes
layout(1:2)
plot(returns, type='l', main='Regime Detection', xlab='', ylab='Returns')
abline(0,0, col="red")
matplot(post_probs[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='bottomleft', c('Regime #1','Regime #2', 'Regime #3', 'Regime #4'), fill=1:3, bty='n')

gspcRets = as.data.frame(gspcRets)
post_probs$state = as.factor(post_probs$state)
post_probs$date = ymd(rownames(gspcRets))
post_probs["Close.Price"] = GSPC$GSPC.Close

dev.off()

densall = ggplot(data=post_probs, aes(x=returns))+
  geom_density(aes(color=State_Name))+
  ggtitle("Density of Log\nReturns by Regime")+xlim(-.1,.1)
ppr = post_probs[post_probs$date >= "2010-11-01",]
close.p =  ggplot(ppr, aes(x=date, y=Close.Price, color=State_Name)) +
  geom_line()+
  ylab("Closing Price ($)")+
  scale_x_date(date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Closing Prices\nNovember 2019 - April 2020")

p <- ggplot(ppr, aes(x=date, y=returns, color=State_Name)) +
  geom_point()+
  ylab("log returns")+
  scale_x_date(date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Regime Predictions\nNovember 2019 - April 2020")

#grid.arrange(densall, p, ncol=2)

densall + (p / close.p)

# The color labels below may be inaccurate.  The backtesting R script dynamically lables them correctly
p2 <- ggplot(post_probs[,], aes(x=date)) +
  geom_line(aes(y=S1, colour="Transition"), size=1.2)+
  geom_line(aes(y=S2, colour="LowVol"), size=1.2)+
  geom_line(aes(y=S3, colour="HighVol"), size=1.2)+
  ylab("Probability of State")+
  scale_x_date(date_labels = "%d-%m-%Y")
p2

p3 <- ggplot(post_probs, aes(x=date)) +
  geom_line(aes(y=S1, colour="s1"))+
  geom_line(aes(y=S2, colour="s2"))+
  geom_line(aes(y=S3, colour="s3"))+
  ylab("Probability of State")+
  scale_x_date(date_labels = "%d-%m-%Y")
p3

highvol.periods = c()
medvol.periods = c()
lowvol.periods = c()
previous_state = ""
count = 1
for (i in 1:nrow(post_probs)){
  current_state = post_probs$State_Name[i]
  
  if (current_state == previous_state){
    count = count+1
  }
  else if (current_state != previous_state){
    print(post_probs$date[i])
    if(previous_state == "HighVol"){
      highvol.periods = c(highvol.periods, count)
    }
    else if (previous_state == "MedVol"){
      medvol.periods = c(medvol.periods, count)
    }
    else if (previous_state == "LowVol"){
      lowvol.periods = c(lowvol.periods, count)
    }
    count = 1
  }
  if ((i == nrow(post_probs)) & (count > 1)){
    if(current_state == "HighVol"){
      highvol.periods = c(highvol.periods, count)
    }
    else if (current_state == "MedVol"){
      medvol.periods = c(medvol.periods, count)
    }
    else if (current_state == "LowVol"){
      lowvol.periods = c(lowvol.periods, count)
    }
  }
  previous_state = current_state
}

hvdf = as.data.frame(highvol.periods)
mvdf = as.data.frame(medvol.periods)
lvdf = as.data.frame(lowvol.periods)

hvpeak = density(hvdf$highvol.periods)$x[which.max(density(hvdf$highvol.periods)$y)]
mvpeak = density(mvdf$medvol.periods)$x[which.max(density(mvdf$medvol.periods)$y)]
lvpeak = density(lvdf$lowvol.periods)$x[which.max(density(lvdf$lowvol.periods)$y)]

hvstats = c(mean(highvol.periods),min(highvol.periods), max(highvol.periods), hvpeak,length(highvol.periods))
mvstats = c(mean(medvol.periods),min(medvol.periods), max(medvol.periods),mvpeak,length(medvol.periods))
lvstats = c(mean(lowvol.periods),min(lowvol.periods), max(lowvol.periods),lvpeak,length(lowvol.periods))
regime_stats = as.data.frame(rbind(lvstats, mvstats, hvstats), row.names = c("Low Volatility","Medium Volatility","High Volatility"))
colnames(regime_stats) = c("Mean Length","Minimum Length","Maximum Length","Peak of Density Length","Number of Periods")

densh = ggplot(hvdf, aes(highvol.periods))+ geom_density()+ geom_vline(xintercept = hvpeak)+ggtitle("Length of High\nVolatility Periods") + xlab("Days") + ylab("Density")
densm = ggplot(mvdf, aes(medvol.periods))+ geom_density()+ geom_vline(xintercept = mvpeak)+ggtitle("Length of Medium\nVolatility Periods") + xlab("Days") + ylab("Density")
densl = ggplot(lvdf, aes(lowvol.periods))+ geom_density()+ geom_vline(xintercept = lvpeak)+ggtitle("Length of Low\nVolatility Periods") + xlab("Days") + ylab("Density")
grid.arrange(densl, densm, densh, ncol=3)

volsize = 0.033

num = nrow(post_probs[abs(post_probs$returns) >= volsize,])

cor = nrow(post_probs[(abs(post_probs$returns) >= volsize) & (post_probs$State_Name == "HighVol"),])

cor
num
cor/num
