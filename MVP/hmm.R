library('depmixS4')
library('quantmod')
library('gridExtra')
library('patchwork')
library(ggplot2)
library(lubridate)
library(plyr)
set.seed(1)

getSymbols("^GSPC",from="1900-01-01")
gspcRets = diff( log( Ad( GSPC ) ) )
returns = as.numeric(gspcRets)
returns[1] = 0

# Fit a Hidden Markov Model with three states 
# to the S&P500 returns stream
numstates = 4
hmm = depmix(returns ~ 1, family = gaussian(), nstates = numstates, data=data.frame(returns=returns))
hmmfit = fit(hmm, verbose = FALSE)
post_probs = posterior(hmmfit)

post_probs$returns = returns

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
cvol = vdf$vnums[4]

label = function(row){
  cstate = row[1]
  
  if (cstate==mvol){
    pred = "Level1"
  }
  else if (cstate==lvol){
    pred = "Level2"
  }
  else if (cstate==hvol){
    pred = "Level3"
  }
  else{
    pred = "Level4"
  }
  return(pred)
}

post_probs["State_Name"] = apply(post_probs,1,label)

write.csv(post_probs,"C:\\Users\\richa\\Documents\\GitHub\\InvSearch\\data\\pprobsGSPC.csv")
