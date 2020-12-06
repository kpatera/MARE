#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(R2jags)
library(metafor)
library(R2jags)

example1 <- structure(list(name = c("Azulay", "Berg", "Lger"), year = c(1994, 1995, 2001), 
                         ai = c(0, 3, 4), bi = c(5, 3, 3), ci = c(0, 0, 2), di = c(5, 6, 5), 
                         ni1 = c(5, 6, 7), ni2 = c(5, 6, 7)), class = "data.frame", row.names = c(NA, -3L)) 

example2 <- structure(list(name = c("vanderMeche", "Bril", "PSGBS", "Nomura"), 
                         year = c(1992L, 1996L, 1997L, 2001L), ai = c(0L, 0L, 3L, 1L), 
                         ni1 = c(74L, 26L, 130L, 23L), ci = c(12L, 0L, 18L, 1L), 
                         ni2 = c(73L, 24L, 121L, 24L), bi = c(74L, 26L, 127L, 22L), 
                         di = c(61L, 24L,103L, 23L)), row.names = c(NA, -4L), class = "data.frame")

example3 <- structure(list(name = c("Cullinan1", "Levi", "Cullinan2", "Colucci","Popov", "Ohtsu", "Bouche", "Lutz", "Koizumi"), 
                         year = c(1985, 1986, 1994, 1995, 2002, 2003, 2004, 2007, 2008), ai = c(1, 2, 3, 0, 1, 5, 1, 1, 0), 
                         bi = c(50, 92, 180, 35, 29, 164, 88, 107, 148), ci = c(1, 3, 0, 1, 0, 1, 1, 0, 0), 
                         di = c(50, 90, 69, 35, 30, 103, 44, 37, 150), ni1 = c(51, 94, 183, 35, 30, 169, 89, 108, 148), 
                         ni2 = c(51, 93, 69, 36, 30, 104, 45, 37, 150)), class = "data.frame", row.names = c(NA, -9L))

example4 <- structure(list(name = c("Levy", "Trefz"), year = c(2007, 2009), 
                          ai = c(2, 4), bi = c(39, 29), ci = c(4, 0), di = c(43, 12), 
                          ni1 = c(41, 33), ni2 = c(47, 12)), class = "data.frame", row.names = c(NA, -2L))

example5 <- structure(list(name = c("vanderMeche", "Bril", "PSGBS", "Diener", "Nomura", "Wang", "ElBayoumi"), 
                           year = c(1992L, 1996L, 1997L, 2001L, 2001L, 2001L, 2011L), 
                           ai = c(1L, 0L, 6L, 0L, 0L, 0L, 0L), ni1 = c(74L, 26L, 130L, 23L, 23L, 20L, 20L), 
                           ci = c(3L, 0L, 5L, 1L, 1L, 0L, 0L), ni2 = c(73L, 24L, 121L, 26L, 24L, 18L, 21L), 
                           bi = c(73L, 26L, 124L, 23L, 23L, 20L, 20L), di = c(70L, 24L, 116L, 25L, 23L, 18L, 21L)), 
                      row.names = c(NA, -7L), class = "data.frame")

examples<-list(ex1=example1,ex2=example2,ex3=example3,ex4=example4,ex5=example5)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$distPlot <- renderPlot({
        
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
    })
    
    
    output$PosteriorPlot <- renderPlot({
      example2 <- structure(list(name = c("vanderMeche", "Bril", "PSGBS", "Nomura"), 
                                 year = c(1992L, 1996L, 1997L, 2001L), ai = c(0L, 0L, 3L, 1L), 
                                 ni1 = c(74L, 26L, 130L, 23L), ci = c(12L, 0L, 18L, 1L), 
                                 ni2 = c(73L, 24L, 121L, 24L), bi = c(74L, 26L, 127L, 22L), 
                                 di = c(61L, 24L,103L, 23L)), row.names = c(NA, -4L), class = "data.frame")
      
      data11<- example2
      temp=escalc(measure="OR", ai=ai, bi=bi, ci=ci, di=di, data=data11, replace=FALSE)
      Nstud=dim(data11)[1]
      s0.sqrt<<-(Nstud/sum(1/(temp$vi)^2))
      #O.Effect<-rma(yi=temp$yi,vi=temp$vi)[[1]][1]
      #E.Effect<- -2.48+1.34*log((0.0001+abs(O.Effect))/(0.0001+0.5))
      #logits_max<-logits_max<-0
      PLOs=escalc(measure="PLO", xi=ci, ni=(ci+di), data=data11, replace=FALSE)
      logits_mean<-1.75
      logits_mean<-mean(PLOs$yi)
      blockerDat=list(rA = data11[,5],nA = data11[,8],
                      rB = data11[,3],nB = data11[,7],
                      Nstud = length(data11[,3]),
                      lmean=logits_mean)
      blockerDat$pControl<-0.01
      blockerDat$logalpha<-as.numeric(input$logalpha)
      blockerDat$logbeta<-as.numeric(input$logbeta)
      nniter=100000;nnthin=1
      
      
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
}", file="betaMetaLGU.txt")
      
      
      
      blocker2Params <- c("d","d.new","tau")
      nniter=100000; nnthin=1
      jagsoutput3<-jags.model(data=blockerDat,inits=NULL, n.chains=1,
                              file=paste("betaMetaLGU.txt"),quiet=TRUE)
      
      #   update(jagsoutput0, n.iter=nniter, n.thin=nnthin, n.burnin=floor(nniter/6), progress.bar="none")
      
      params  <- c("precS","betaS","slog","sSSquared")
      
      
      samples <- coda.samples(jagsoutput3,  n.iter=nniter, n.thin=nnthin, n.burnin=floor(nniter/10),
                              variable.names=blocker2Params)
      
      par(mfrow=c(1,2))
      plot(density(samples[[1]][,1]))
      plot(density(samples[[1]][,3]))
      
})
    
})
