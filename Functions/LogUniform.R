      PLOs=escalc(measure="PLO", xi=ci, ni=(ci+di), data=example_input, replace=FALSE)
      logits_mean<-1.75
      logits_mean<-mean(PLOs$yi)
      blockerDat=list(rA = example_input[,5],nA = example_input[,8],
                      rB = example_input[,3],nB = example_input[,7],
                      Nstud = length(example_input[,3]),
                      lmean=logits_mean)
      blockerDat$prec.theta<-as.numeric(input$prec.theta)
      blockerDat$logalpha<-as.numeric(input$logalpha)
      blockerDat$logbeta<-as.numeric(input$logbeta)
      
      
      cat("model
{
     for( i in 1 : Nstud ) {
      rA[i] ~ dbin(pA[i], nA[i])
      rB[i] ~ dbin(pB[i], nB[i])
      mu[i] ~ dnorm(lmean,prec.theta)
      logit(pB[i]) <- mu[i] + 0.5*delta[i]
      logit(pA[i]) <- mu[i] - 0.5*delta[i]
      delta[i] ~ dnorm(d, prec)
      }
      OR <- log(d)
      d ~ dnorm(0.0,prec.theta)
      lv ~ dunif(logalpha,logbeta)
      log(tau.sq)<-lv
      prec<-1/(tau.sq)
      tau<-sqrt(tau.sq)
      d.new ~dnorm(d,prec)
}", file="betaMetaLGU.txt")
      
      
      
      blocker2Params <- c("d","d.new","tau")
      nniter=as.numeric(input$niter);nnthin=as.numeric(input$nthin)
      jagsoutput3<-jags.model(data=blockerDat,inits=NULL, n.chains=1,
                              file=paste("betaMetaLGU.txt"),quiet=TRUE)
      
      #   update(jagsoutput0, n.iter=nniter, n.thin=nnthin, n.burnin=floor(nniter/6), progress.bar="none")
      
      params  <- c("precS","betaS","slog","sSSquared")
      
      
      samples <<- coda.samples(jagsoutput3,  n.iter=nniter, n.thin=nnthin, n.burnin=floor(nniter/10),
                              variable.names=blocker2Params)
      
      