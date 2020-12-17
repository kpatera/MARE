examples<-list(ex1=example1,ex2=example2,ex3=example3,ex4=example4,ex5=example5)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  
  output$PplotLU <- renderPlot({
    example_input<-example_all[[input$example]]
    source(file = paste0(path,"/Functions/LogUniform.R"), local = TRUE)
    par(mfrow=c(1,2))
    plot(density(samples[[1]][,1]),main = "Posterior on Theta (LogU)",lwd=3)
    plot(density(samples[[1]][,3]),main = "Posterior on Tau (LogU)",lwd=3)
    
  })
  
  output$PplotHN <- renderPlot({
    example_input<-example_all[[input$example]]
    source(file = paste0(path,"/Functions/HalfNormal.R"), local = TRUE)
    par(mfrow=c(1,2))
    plot(density(samples[[1]][,1]),main = "Posterior on Theta (HN)",lwd=3)
    plot(density(samples[[1]][,3]),main = "Posterior on Tau (HN)",lwd=3)
    
  })
  output$exampleOut<-renderTable({
    example_all[[input$example]]
  })
  
  output$analysisOut<-renderTable({
    analysis<-data.frame(matrix(NA,7,6))
    colnames(analysis)<-c("Method","Theta","Tau^2","St.error Theta","Adj. St. error Theta","P-Value")
    analysis$Method<-Methods<-c("dl","Mantel","Peto","exact","GLMMLaplace","BayesHN","BayesLogU")
    for (i in 1:length(Methods)){
      if(any(input$checkMethod %in% Methods[i])){
        analysis[i,2:6] <- tryCatch({out<-t(hetEstimate(method = Methods[i],
                                                     data1 = example_all[[input$example]],
                                                     threshold = 0.01,
                                                     ContCor = "Constant05"))},
                                         error=function(cond) {return(NA)} 
        ) 
      }
    }
    analysis
    
  })
        
        
})
  
  