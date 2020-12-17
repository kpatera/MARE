
hetEstimate = function(method, data1, threshold=0.01, ContCor){

if(!(method=="Peto" | method=="Mantel" | method=="exact" | method=="Bayes2" | method=="Bayes" | method=="betabinom")) {
  if(ContCor=="Constant01") data1<-ContCorr(data1=data1,ContC="Constant",cc=0.1)
  if(ContCor=="Constant05") data1<-ContCorr(data1=data1,ContC="Constant",cc=0.5)
  if(ContCor=="TAC01") data1<-ContCorr(data1=data1,ContC="TAC",cc=0.1)
  if(ContCor=="TAC05") data1<-ContCorr(data1=data1,ContC="TAC",cc=0.5) 
  if(ContCor=="Empirical05") data1<-ContCorr(data1=data1,ContC="Empirical05") 
}
ai=data1$ai;bi=data1$bi;ci=data1$ci;di=data1$di; ni1=data1$ni1; ni2=data1$ni2
yi = log((ai*di)/(ci*bi))
sigma2 = 1/ai+1/bi+1/ci+1/di
nstud=dim(data1)[1] 

if (method == "dl")  { # Checked
  if (sum(sigma2)<0) stop("DL estimator cannot be used with non-positive sampling variances.")
  wi0 = 1/sigma2
  yw0 = sum(wi0 * yi) / sum(wi0)
  Q = sum(wi0 * (yi - yw0)^2)
  c1 = (sum(wi0) - (sum(wi0^2) / sum(wi0)))
  tau2 = max(0, (Q - (nstud-1))/c1) 
  se.tau2<-NA
}
if(method == "Mantel"){
  tempdat<-metabin(event.e=ai,n.e=ai+bi,event.c=ci,n.c=ci+di,
                   sm="OR",method="MH",MH.exact=TRUE,backtransf=FALSE,data=data1)
  output<-c(tempdat$TE.random,0,tempdat$seTE.random,0,tempdat$pval.random)  
}
if (method == "Peto") {
  N = (ai+bi+ci+di)
  oi = ai
  ei = ((ai+ci)*(ai+bi)) / (N)
  vi = ((ai+bi)*(ci+di)*(ai+ci)*(bi+di)) / (N^2*(N-1))
  mu = (sum(oi) - sum(ei))/sum(vi)
  semu = sqrt(1/sum(vi))
  z.value = (sum(oi) - sum(ei)) / sqrt(sum(vi))
  p.valueZ = 2*pnorm(-abs(z.value))
  output <- c(mu,0,semu,0,p.valueZ)
  
}
if (method == "exact") {
  tempdat <- rma.glmm(measure="OR", ai=ai, n1i=ni1, ci=ci, n2i=ni2, data=data1, model="CM.EL")
  tau2<-tempdat$tau2
  se.tau2<-tempdat$se.tau2
  
} # Checked - Problems
if (method == "GLMMLaplace"){ 
  data11<-reshapedata(data1,method="Expregression")
  tempdat<- glmer(Outcome ~ Exposure + (1|Study), data=data11, family=binomial(link = "logit"),nAGQ=1)
  tau2<-VarCorr(tempdat)
  tempdat<-summary(tempdat)
  dataout<-c(tempdat$coefficients[2,1],tempdat$coefficients[2,2],
             tempdat$coefficients[2,1]-qnorm(0.025)*sqrt(tempdat$coefficients[2,2]),
             tempdat$coefficients[2,1]+qnorm(0.025)*sqrt(tempdat$coefficients[2,2]),
             tempdat$coefficients[2,3],sqrt(tau2[[1]][[1]]))
  mu<-tempdat$coefficients[2,1]
  p.valuez<-tempdat$coefficients[2,4]
  output<-c(mu,dataout[6],NA,NA,p.valuez)
}
if (method == "BayesHN"){
  Dat=list(rA = data1[,5],nA = data1[,8],
           rB = data1[,3],nB = data1[,7],
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
      OR <- exp(d)
      d ~ dnorm(0.0,1.0E-1)
      tau~dnorm(0.0,0.1)I(0,)
      tau.sq<-tau*tau
      prec<-1/(tau.sq)
}", file="UBLogisticREhnorm.txt")
  InitialValues<-function(){list("d" = 0,"tau"=1,"mu" = rep(0,length(Dat[[1]])),
                                 "delta" = rep(0,length(Dat[[1]])))}
  Parameters  <- c("d","OR","tau")
  BMAURE      <-jags(data=Dat, 
                     inits=InitialValues, 
                     parameters.to.save=Parameters, n.iter=100000,n.burnin=floor(100000/3), n.thin=4, n.chain=3,
                     model.file="UBLogisticREhnorm.txt")
  tempdat<-BMAURE
  tau2<-tempdat[[2]][10]$summary[4,][[1]]
  mu<-tempdat[[2]][10]$summary[1,][[1]]
  output<-c(log(mu),tau2,NA,NA,NA)
  sd.tau2<-se.tau2<-tempdat[[2]][10]$summary[4,][[2]]
}
if (method == "BayesLogU"){
  Dat=list(rA = data1[,5],nA = data1[,8],
           rB = data1[,3],nB = data1[,7],
           Nstud = length(data1[,3]))
  cat("model
      {
        for( i in 1 : Nstud ) {
          rA[i] ~ dbin(pA[i], nA[i])
          rB[i] ~ dbin(pB[i], nB[i])
          mu[i] ~ dnorm(lmean,pControl)
          logit(pB[i]) <- mu[i] + 0.5*delta[i]
          logit(pA[i]) <- mu[i] - 0.5*delta[i]
          delta[i] ~ dnorm(d, prec)
        }
        OR <- log(d)
        d ~ dnorm(0.0,pControl)
        lv ~ dunif(logalpha,logbeta)
        log(tau.sq)<-lv
        prec<-1/(tau.sq)
        tau<-sqrt(tau.sq)
        d.new ~dnorm(d,prec)
      }", file="UBLogisticREhnorm.txt")
  InitialValues<-function(){list("d" = 0,"lv"=0.5,"mu" = rep(0,length(Dat[[1]])),
                                 "delta" = rep(0,length(Dat[[1]])))}
  Parameters  <- c("d","OR","tau")
  BMAURE      <-jags(data=Dat, 
                     inits=InitialValues, 
                     parameters.to.save=Parameters, n.iter=100000,n.burnin=floor(100000/3), n.thin=4, n.chain=3,
                     model.file="UBLogisticREhnorm.txt")
  tempdat<-BMAURE
  tau2<-tempdat[[2]][10]$summary[4,][[1]]
  mu<-tempdat[[2]][10]$summary[1,][[1]]
  output<-c(log(mu),tau2,NA,NA,NA)
  sd.tau2<-se.tau2<-tempdat[[2]][10]$summary[4,][[2]]
}

if(!(method=="dlHK"| method=="dlmHK" | method=="Peto" | method=="Mantel" | method=="Mantel2"  | method=="exact" | method=="BayesHN" | method=="BayesLogU" | method=="betabinom" | method=="GLMMLaplace")){ 
  wi = 1/(sigma2+tau2) 
  semu = sqrt(1/sum(wi))
  mu = sum(wi*yi)/sum(wi)
  q = (1/(nstud-1)) *sum(wi*((yi-mu)^2))+0.0001
  semuAdj = sqrt(q)*semu # Then multiplied by a t distribution with (k-1,1-a/2)
  z.value = (mu) /semu 
  p.valueZ = 2*pnorm(-abs(z.value))
  output <- c(mu,tau2,semu,semuAdj,p.valueZ)
}
return(output)
}