th<-c(0,0,0,0)
df<-read.csv(“placebo_cognitivetraining.csv”)
> plot(df$raven, df$bomat, main =" Scatter plot between raven score and bo
mat score")
> plot(df$nfc, df$bomat, main =" Scatter plot between nfc score and bomat
score")
> plot(df$nfc, df$raven, main =" Scatter plot between nfc score and raven
score")
> plot(df$age, df$bomat, main =" Scatter plot between bomat score and age"
)
> hist(df$age)
> df_pre=df[which(df$time=="Pre-Training"),c(1:10)]
> df_post=df[which(df$time=="Post-Training"),c(1:10)]
#BOMAT scores of pre and post training
> c=df[which(df$time=="Pre-Training"),c(6)]
> d=df[which(df$time=="Post-Training"),c(6)]
> control=df[which(df$cond=="Control"),c(1:10)]
cpreb=control[which(control$time=="Pre-Training"),c(6)]
cpostb=control[which(control$time=="Post-Training"),c(6)]
plot(cpreb,type='l',col="red")
lines(cpostb,col="blue")
> xbar1 = mean(cpreb)
> xbar2 = mean(cpostb)
> sd1 = sd(cpreb)
> sd2 = sd(cpostb)
> t = (xbar1-xbar2)/sqrt(sd1^2/10+sd2^2/8)
> 1-pt(t,7)
ctrl<-df[which(df$cond=="Control"),c(3,6)]
ctrl<-ctrl$bomat
> plot(c, type="l",col="red")
> lines(d, col="blue")
>
trt<- df[which(df$cond=="Placebo"),c(3,6)]
trt<-trt$bomat
> xbar1 = mean(trt)
> xbar2 = mean(ctrl)
> sd1 = sd(trt)
> sd2 = sd(ctrl)
> t = (xbar1-xbar2)/sqrt(sd1^2/10+sd2^2/8)
> 1-pt(t,7)
> lik = function(th){
+ mu1=th[1]; sig1=th[2]; mu2=th[3]; sig2=th[4]
+ prod(dnorm(trt,mean=mu1,sd=sig1))*prod(dnorm(ctrl,mean=mu2,sd=sig2))
+ }
> #Prior
> prior = function(th){
+ mu1=th[1]; sig1=th[2]; mu2=th[3]; sig2=th[4]
+ if(sig1 <= 0 | sig2 <= 0) return(0)
+ dnorm(mu1,10,10)*dnorm(mu2,10,10)*dexp(sig1,rate=.1)*dexp(sig2,rate=.1)
+ }
#Posterior
> post = function(th){prior(th) * lik(th)}
> #Starting values
> mu1 = 15; sig1 = 5; mu2 = 15; sig2 = 5
> th0=c(mu1,sig1,mu2,sig2)
> # Here is what does the MCMC (Metropolis method):
> nit=10000
> results = matrix(0, nrow=nit, ncol=4)
> th = th0
> results[1,] = th0
> for(it in 2:nit){
+ cand = th + rnorm(4,sd=.5)
+ ratio = post(cand)/post(th)
+ if(runif(1)<ratio) th=cand
+ results[it,] = th
+
> mu1s = results[,1]
> sig1s = results[,2]
> mu2s = results[,3]
> sig2s = results[,4]
> plot(mu1s)
> plot(sig1s)
> plot(mu1s-mu2s)
> hist(mu1s-mu2s)
> mean(mu1s-mu2s > 0) # <-- our original question
> resultsPlot = function(results){
+ old.par = par(no.readonly = TRUE)
+ on.exit(par(old.par))
+ nvar = dim(results)[2]
+ par(mfrow = c(nvar,1))
+ for(i in 1:nvar)plot(results[,i], ylab = paste("variable",i))
+ }
> resultsPlot(results)
> resultsPlot(results[1:2000,])
> res = results[-c(1:500),]
> mu1s = res[,1]
> sig1s = res[,2]
> mu2s = res[,3]
> sig2s = res[,4]
> hist(mu1s-mu2s)
> mean(mu1s-mu2s > 0)
> #Plot
> par(mfrow = c(4,1))
> plot(res[,1])
> plot(res[,2])
> plot(res[,3])
> plot(res[,4])
> #What is our estimated posterior probability U1-U2>0
> sum((res[,1]-res[,3] > 0))/dim(res)[1]
