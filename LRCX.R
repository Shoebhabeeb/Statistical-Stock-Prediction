library(HMM)
library(xts)
library(zoo)
library(quantmod)
library(nnet)
library(Rsolnp)
library(nlme)
library(TTR)
library(depmixS4) 
library(ggplot2) 
library(HMM)

#Plot the data
getSymbols("LRCX", from = "1900-01-01",to="2019-11-30", src = 'yahoo', periodicity = 'monthly')
chartSeries(LRCX)
#Bollinger Band chart, % Bollinger change, Volume Traded and Moving Average Convergence Diverence
LRCX%>%chartSeries(TA='addBBands();addVo();addMACD()')
#Window for initial Distribution
subset<-window(LRCX,start = as.Date("1900-01-01"),end=as.Date("2014-11-30"))
train<-cbind(subset$LRCX.Close-subset$LRCX.Open)
trainmean<- mean(c(train))
trainsd<-sd(train)
train = ifelse(train > (trainmean + 0.5*trainsd), "H",(ifelse(train < (trainmean - 0.5*trainsd),"L","M")))
#train<-ifelse (train > 0,"I","D")
I_D = c(train)
i = table(I_D)
i
initial_distribution = c(i[[1]][1]/(i[[1]][1]+i[[2]][1]+i[[3]][1]),i[[3]][1]/(i[[1]][1]+i[[2]][1]+i[[3]][1]),i[[2]][1]/(i[[1]][1]+i[[2]][1]+i[[3]][1]))
initial_distribution
#Creating a hidden and Visible dataframe for our time window
data_LRCX<- read.csv('~/Desktop/CMS/project/Submission/LRCX.csv')
dataset_LRCX<-cbind(data_LRCX$Close-data_LRCX$Open)
trainmean<- mean(dataset_LRCX)
trainsd<-sd(dataset_LRCX)
train = ifelse(dataset_LRCX > (trainmean + 0.5*trainsd), "H",(ifelse(dataset_LRCX < (trainmean - 0.5*trainsd),"L","M")))
train
dataframehidden_visible<- ifelse (dataset_LRCX > 0,"I","D")
dataset_LRCX<-as.data.frame(dataset_LRCX)
dataset_LRCX$V2<-ifelse (dataset_LRCX$V1 > 0,1,2)
dataset_LRCX$V1 = train
Data_Final = dataset_LRCX[c("V1","V2")]
names(Data_Final)[2] = "Visible"
names(Data_Final)[1] = "Hidden"
Data_Final
#Creating TPM and EPM
performance = c(Data_Final$Hidden)
result = c(Data_Final$Visible)
loop <- length(performance) - 1
hh <- 0
hl <- 0
lh <- 0
ll <- 0
lm <- 0 
ml <- 0
hm <- 0
mh <- 0
mm <- 0
for (i in 1:loop) {
  temp = paste(performance[i], performance[i+1], sep = "")
  if(temp=="HH"){
    hh <- hh + 1
  } else if(temp=="HL"){
    hl <- hl + 1
  } else if(temp=="LH"){
    lh <- lh + 1
  } else if(temp=="HM"){
    hm <- hm + 1
  } else if(temp=="MH"){
    mh <- mh + 1
  } else if(temp=="MM"){
    mm <- mm + 1
  } else if(temp=="ML"){
    ml <- ml + 1
  } else if(temp=="LM"){
    lm <- lm + 1
  }else{
    ll <- ll + 1
  }
}
merge <- c()
for (i in 1:length(performance)) {
  temp = paste(performance[i], result[i], sep = "")
  merge[i] = temp
}
x <- table(merge)

h2h <- round(hh/(hh+hl+hm),5)
h2m <- round(hm/(hh+hl+hm),5)
h2l <- 1 - h2h - h2m
l2l <- round(ll/(ll+lh+lm),4)
l2m <- round(lm/(ll+lh+lm),4)
l2h <- 1 - l2l - l2m
m2m <- round(mm/(ml+mh+mm),4)
m2l <- round(ml/(ml+mh+mm),4)
m2h <- 1 - m2l - m2m
A = matrix(c(h2h,h2m,h2l,m2h,m2m,m2l,l2h,l2m,l2l),3,3,byrow = T)
B = matrix(c(x[[1]][1],0, x[[3]][1], x[[2]][1],0, x[[4]][1]), 3,2, byrow = T)
B = B/rowSums(B)
B <- round(B, 4)
A
B
#Functional Algorithms


forward = function(v, a, b, initial_distribution){
  
  T = length(v)
  M = nrow(a)
  alpha = matrix(0, T, M)
  
  alpha[1, ] = initial_distribution*b[, v[1]]
  
  for(t in 2:T){
    tmp = alpha[t-1, ] %*% a
    alpha[t, ] = tmp * b[, v[t]]
  }
  return(alpha)
}

backward = function(v, a, b){
  T = length(v)
  M = nrow(a)
  beta = matrix(1, T, M)
  
  for(t in (T-1):1){
    tmp = as.matrix(beta[t+1, ] * b[, v[t+1]])
    beta[t, ] = t(a %*% tmp)
  }
  return(beta)
}


BaumWelch = function(v, a, b, initial_distribution, n.iter = 100){
  
  for(i in 1:n.iter){
    T = length(v)
    M = nrow(a)
    K=ncol(b)
    alpha = forward(v, a, b, initial_distribution)
    beta = backward(v, a, b)
    xi = array(0, dim=c(M, M, T-1))
    
    for(t in 1:T-1){
      denominator = ((alpha[t,] %*% a) * b[,v[t+1]]) %*% matrix(beta[t+1,]) 
      for(s in 1:M){
        numerator = alpha[t,s] * a[s,] * b[,v[t+1]] * beta[t+1,]
        xi[s,,t]=numerator/as.vector(denominator)
      }
    }
    
    
    xi.all.t = rowSums(xi, dims = 2)
    a = xi.all.t/rowSums(xi.all.t)
    
    gamma = apply(xi, c(1, 3), sum)  
    gamma = cbind(gamma, colSums(xi[, , T-1]))
    for(l in 1:K){
      b[, l] = rowSums(gamma[, which(v==l)])
    }
    b = b/rowSums(b)
    
  }
  return(list(a = a, b = b, initial_distribution = initial_distribution))
}


Viterbi=function(v,a,b,initial_distribution) {
  
  T = length(v)
  M = nrow(a)
  prev = matrix(0, T-1, M)
  omega = matrix(0, M, T)
  
  omega[, 1] = log(initial_distribution * b[, v[1]])
  for(t in 2:T){
    for(s in 1:M) {
      probs = omega[, t - 1] + log(a[, s]) + log(b[s, v[t]])
      prev[t - 1, s] = which.max(probs)
      omega[s, t] = max(probs)
    }
  }
  
  S = rep(0, T)
  last_state=which.max(omega[,ncol(omega)])
  S[1]=last_state
  
  j=2
  for(i in (T-1):1){
    S[j]=prev[i,last_state] 
    last_state=prev[i,last_state] 
    j=j+1
  }
  
  S[which(S==1)]='H'
  S[which(S==2)]='M'
  S[which(S==3)]='L'
  
  S=rev(S)
  
  return(S)
  
}

#Calculating Best Model Parameters to maximize P(O|lamda)
myout = BaumWelch(Data_Final$Visible, A, B, initial_distribution, n.iter = 500)

#Calculating P(O/lambda)
#Forward
Calc_O=forward(Data_Final$Visible,myout$a,myout$b,initial_distribution)
P_O=Calc_O[60,1]+Calc_O[60,2]+Calc_O[60,3]
P_O
#Backward
Calc_OB=backward(Data_Final$Visible,myout$a,myout$b)
P_OB=(B[1,1]*Calc_OB[1,1]*initial_distribution[1])+(B[2,1]*Calc_OB[1,2]*initial_distribution[2])+(B[3,1]*Calc_OB[1,3]*initial_distribution[3])
P_OB

#Calculating Most Likely sequence
myout.Init_hidden=Viterbi(Data_Final$Visible,A,B,initial_distribution)#Sequence from initial variable
myout.hidden=Viterbi(Data_Final$Visible,myout$a,myout$b,initial_distribution)#Sequence from Updated variables
sum(myout.hidden == c(Data_Final$Hidden))/60
sum(myout.Init_hidden == c(Data_Final$Hidden))/60

#Simulating using Monte Carlo for 500 Simulations
library(tidyr)
library(tibble)
library(stringr)
library(ggplot2)
N<-500
LRCX_log_returns<-LRCX%>%Ad()%>%dailyReturn(type='log')
mu = mean(LRCX_log_returns)
sig = sd(LRCX_log_returns)
mc_matrix<-matrix(nrow=12*5,ncol=N)
mc_matrix[1,1]<-as.numeric(LRCX$LRCX.Adjusted[length(LRCX$LRCX.Adjusted),])
for(j in 1:ncol(mc_matrix)){
  mc_matrix[1,j]<-as.numeric(LRCX$LRCX.Adjusted[length(LRCX$LRCX.Adjusted),])
  for(i in 2:nrow(mc_matrix)){
    mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(rnorm(1,mu,sig))
  }
}
name<-str_c("Sim ",seq(1,500))
name<-c("Month",name)
final_mat<-cbind(1:(12*5),mc_matrix)
final_mat<-as.tibble(final_mat)
colnames(final_mat)<-name
dim(final_mat) #1008 501
final_mat%>%gather("Simulation","Price",2:501)%>%ggplot(aes(x=Month,y=Price,Group=Simulation))+geom_line(alpha=0.2)+labs(title="LAM Research Stock (ASML): 500 Monte Carlo Simulations for 5 Years")+theme_bw()
final_mat[48,-1]%>%as.numeric()%>%quantile(probs=c(5, 50,95,99.5, NA)/100,na.rm=TRUE)

#Regime Detection
LRCXRets = diff( log( Cl( LRCX ) ) )
returns = as.numeric(LRCXRets)
plot(LRCXRets)
hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 3, data=data.frame(returns=returns))
hmmfit <- fit(hmm, verbose = FALSE)
post_probs <- posterior(hmmfit)
plot(returns, type='l', main='Regime Detection', xlab='', ylab='Returns')
matplot(post_probs[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='bottomleft', c('Regime #1','Regime #2','Regime #2'), fill=1:3, bty='n')
