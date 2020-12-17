if (method == "he")  {
  tau2 = max(0, (var(yi) - mean(sigma2)))
  se.tau2<-NA
}
if (method == "sj")  { # 
  if (is.na(sum(((yi-mean(yi))^2)/nstud))) 
    stop("Not defined sum(yi-mean(yi)^2/nstud for SJ estimator.")
  if (sum(((yi-mean(yi))^2)/nstud) == 0) {mv = 0.01} else {mv = sum(((yi-mean(yi))^2)/nstud)}
  ri = sigma2/mv
  vi = ri + 1
  Y_bar_v = sum(yi/vi)/sum((1/vi))
  tau2 = sum((1/vi)*(yi - Y_bar_v)^2)/(nstud-1)
  se.tau2<-NA
}
if (method == "pm")  {
  wi0 = 1/sigma2
  yiw0 = sum(wi0 * yi) / sum(wi0)
  Q = sum(wi0 * (yi - yiw0)^2)
  tau2pr = 0
  if (Q - (nstud-1) <= 0) {tau2 <- 0} else
    repeat{      
      wi = 1 / (tau2pr + sigma2)
      yiw = sum(wi * yi) / sum(wi)
      Ftau2 = sum(wi * (yi - yiw)^2) - (nstud-1)
      if (Ftau2 < 0) {tau2 = 0; break}
      if (abs(Ftau2) < 0.0001) {tau2 = tau2pr; break}
      if (Ftau2 > 0) {
        deltatau2 = Ftau2 / (sum(wi^2 * (yi-yiw)^2))
        tau2pr = tau2pr + deltatau2
      }
    }
  se.tau2<-NA
} 
if (method == "ipm")  {
  tau2pr = 0
  wi0 = 1/sigma2
  yiw0 = sum(wi0 * yi) / sum(wi0)
  Q = sum(wi0 * (yi - yiw0)^2)
  sigma22<-(1/(ni1+1))*( exp((-ci/ni2) - (sum(yi)/length(yi)) + (tau2pr/2)) + 2 + exp((ci/ni2) + (sum(yi)/length(yi)) + (tau2pr/2)) )+
    (1/(ni2+1))*( exp(-ci/ni2) + 2 + exp(ci/ni2))
  if (Q - (nstud-1) <= 0) {tau2 <- 0} else
    repeat{      
      
      wi = 1 / (tau2pr + sigma22)
      yiw = sum(wi * yi) / sum(wi)
      Ftau2 = sum(wi * (yi - yiw)^2) - (nstud-1)
      if (Ftau2 < 0) {tau2 = 0; break}
      if (abs(Ftau2) < 0.0001) {tau2 = tau2pr; break}
      if (Ftau2 > 0) {
        deltatau2 = Ftau2 / (sum(wi^2 * (yi-yiw)^2))
        tau2pr = tau2pr + deltatau2
      }
      
    }
  se.tau2<-NA
}
if (method == "RBP") {
  wi<-1/sigma2
  yhat = sum(yi/wi)/sum((1/wi))
  tau2 = sum((yi-yhat)^2) / (nstud + 1)
  se.tau2<-NA
}
if (method == "reml")  {
  tau2.re = hetEstimate(data1=data1,ContCor="Constant05",nstud,method="dl",options=options)[2]
  repeat {
    wi <- 1 / (tau2.re + sigma2)
    teta_w <- sum(wi*yi)/sum(wi)
    tau2 <- (sum(wi^2 * ((yi-teta_w)^2 - sigma2)) / sum(wi^2)) + (1/sum(wi))
    if (tau2<0) {tau2=0}
    if (tau2 - tau2.re == 0) {break}
    tau2.conv = (abs(tau2-tau2.re))/ (1+tau2.re)
    if (tau2.conv <= 0.0001) {break}
    if (tau2.conv > 0){tau2.re = tau2}
  }
  se.tau2 <- NA# sqrt(2/Q^2)
}


if (method == "betabinom"){
  data11<-reshapedata(data1,method="regression")
  tempdat<-hglm(fixed = event/n ~ treat, random = ~1 | study,family = binomial(link = logit),
                rand.family = Beta(link = logit), data = data11, weights=n)  
  mu<-tempdat$fixef[[2]]
  semu<-summary(tempdat)$FixCoefMat[2,2]
  tau2<-tempdat$phi[1]
  z.value = (mu) /semu
  p.valueZ = 2*pnorm(-abs(z.value))
  output<-c(mu,tau2,NA,NA,p.valueZ)
  se.tau2<-NA
} # Checked
if(method=="dlmHK"){
  if (sum(sigma2)<0) stop("DL estimator cannot be used with non-positive sampling variances.")
  wi0 = 1/sigma2
  yw0 = sum(wi0 * yi) / sum(wi0)
  Q = sum(wi0 * (yi - yw0)^2)
  c1 = (sum(wi0) - (sum(wi0^2) / sum(wi0)))
  tau2 = max(0, (Q - (nstud-1))/c1) 
  se.tau2<-NA
  # se.tau2 <- sqrt(1/c1^2 * (2 * (nstud - 1) + 4 * max(tau2, 0) * c1 + 2 * max(tau2, 0)^2 * Q^2))
  wi = 1/(sigma2+tau2) 
  semu = sqrt(1/sum(wi))
  mu = sum(wi*yi)/sum(wi)
  q = (1/(nstud-1)) *sum(wi*((yi-mu)^2))+0.0001
  #q = max(1,q)
  semuAdj = ((sqrt(q)*semu)*3/4)+((semu)/4) # Then multiplied by a t distribution with (k-1,1-a/2)
  z.value = (mu) /semu
  z.valueadj= (mu) /semuAdj
  p.valueZ = 2*pnorm(-abs(z.value))
  p.valuetadj = 2*pt(-abs(z.valueadj),nstud-1)
  output <- c(mu,tau2,semuAdj,semuAdj,p.valuetadj)
}
if(method=="dlHK"){
  if (sum(sigma2)<0) stop("DL estimator cannot be used with non-positive sampling variances.")
  wi0 = 1/sigma2
  yw0 = sum(wi0 * yi) / sum(wi0)
  Q = sum(wi0 * (yi - yw0)^2)
  c1 = (sum(wi0) - (sum(wi0^2) / sum(wi0)))
  tau2 = max(0, (Q - (nstud-1))/c1) 
  se.tau2<-NA
  wi = 1/(sigma2+tau2) 
  semu = sqrt(1/sum(wi))
  mu = sum(wi*yi)/sum(wi)
  q = (1/(nstud-1)) *sum(wi*((yi-mu)^2))+0.0001
  semuAdj = sqrt(q)*semu # Then multiplied by a t distribution with (k-1,1-a/2)
  z.value = (mu) /semu 
  z.valueadj= (mu) /semuAdj
  p.valueZ = 2*pnorm(-abs(z.value))
  p.valuetadj = 2*pt(-abs(z.valueadj),nstud-1)
  output <- c(mu,tau2,semuAdj,semuAdj,p.valuetadj)
}  



if (method == "Bayes") {
  Dat=list(rA = data1$ci,nA = data1$ni2,
           rB = data1$ai,nB = data1$ni1,
           Nstud = length(data1[,3]))
  cat("model
{
      for( i in 1 : Nstud ) { # Likelihood
      rA[i] ~ dbin(pA[i], nA[i]) # rA follows a binomial
      rB[i] ~ dbin(pB[i], nB[i])
      logit(pA[i]) <- mu[i]
      logit(pB[i]) <- mu[i] + delta[i]
      mu[i] ~ dnorm(0.0,1.0E-1)
      delta[i] ~ dnorm(d, prec)
      }
      d ~ dnorm(0.0,1.0E-1) # Vague prior on logOR
      OR <- exp(d) # Odds ratio
      lv ~ dunif(-10,10)
      log(tau.sq)<-lv
      prec<-1/(tau.sq)
      tau<-sqrt(tau.sq)
}", file="UBLogisticRELoguni.txt")
  InitialValues<-function(){list("d" = 0,"tau"=1,"mu" = rep(0,length(Dat[[1]])),
                                 "delta" = rep(0,length(Dat[[1]])))}
  Parameters  <- c("d","OR","tau")
  BMAURE      <-jags(data=Dat, 
                     inits=InitialValues, 
                     parameters.to.save=Parameters, n.iter=100000,n.burnin=floor(100000/3), n.thin=4,n.chain=1,
                     model.file="UBLogisticRELoguni.txt")
  tempdat<-BMAURE
  tau2<-tempdat[[2]][10]$summary[4,][[1]]
  mu<-tempdat[[2]][10]$summary[1,][[1]]
  sd.tau2<-se.tau2<-tempdat[[2]][10]$summary[4,][[2]]
  output<-c(log(mu),tau2,NA,NA,NA)
  
} # Checked