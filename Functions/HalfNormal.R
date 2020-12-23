PLOs=escalc(measure="PLO", xi=ci, ni=(ci+di), data=example_input, replace=FALSE)
logits_mean<-1.75
logits_mean<-mean(PLOs$yi)
blockerDat=list(rA = example_input[,5],nA = example_input[,8],
                rB = example_input[,3],nB = example_input[,7],
                Nstud = length(example_input[,3]),
                lmean=logits_mean)
blockerDat$prec.theta<-as.numeric(input$prec.theta)
blockerDat$prec.tau<-as.numeric(input$prec.tau)



cat("model
{
      for( i in 1 : Nstud ) { # Likelihood
      rA[i] ~ dbin(pA[i], nA[i]) # rA follows a binomial
      rB[i] ~ dbin(pB[i], nB[i])
      logit(pA[i]) <- mu[i]
      logit(pB[i]) <- mu[i] + delta[i]
      mu[i] ~ dnorm(lmean,prec.theta)
      delta[i] ~ dnorm(d, prec)
      }
      OR <- exp(d)
      d ~ dnorm(0.0,prec.theta)
      tau~dnorm(0.0,prec.tau)I(0,)
      tau.sq<-tau*tau
      prec<-1/(tau.sq)
      d.new ~dnorm(d,prec)
}", file="betaMetaHN.txt")



blocker2Params <- c("d","d.new","tau")
nniter=as.numeric(input$niter);nnthin=as.numeric(input$nthin)
jagsoutput3<-jags.model(data=blockerDat,inits=NULL, n.chains=1,
                        file=paste("betaMetaHN.txt"),quiet=TRUE)

#   update(jagsoutput0, n.iter=nniter, n.thin=nnthin, n.burnin=floor(nniter/6), progress.bar="none")

samples <<- coda.samples(jagsoutput3,  n.iter=nniter, n.thin=nnthin, n.burnin=floor(nniter/10),
                         variable.names=blocker2Params)

