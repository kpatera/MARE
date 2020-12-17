# Routine to transform - reshape data for analysis
# Input must be an object of the form (Study - year - ai - bi - ci - di - ni1 - ni2)
reshapedata<-function(data1, method=c("summary","regression","Wregression","Expregression")){
  
  if(method=="regression"){
    datas<-data.frame(event=as.numeric(t(cbind(data1$ai, data1$ci))), 
                      n=as.numeric(t(cbind(data1$ni1, data1$ni2))), 
                      study=rep(data1$name,each=2), 
                      treat=rep(c(1,0),length(data1$name)))
    return(datas)
  }
  if(method=="Wregression"){ 
    dataas<-rep(NA,4)
    for( i in 1:dim(data1)[1]){
      datas<-matrix(c(data1[[3]][i],data1[[5]][i],
                      data1[[4]][i],data1[[6]][i]),2,2,byrow=TRUE)
      colnames(datas)<-c("E","NE")
      row.names(datas)<-c("T","NT")
      datas <-ftable(datas) ; datas<-as.data.frame(datas)
      datas <-cbind(datas,Study=rep(paste(data1[[1]][i]),dim(datas)[1]))
      dataas<-rbind(dataas,datas)
    }
    datas<-dataas[-1,]
    names(datas)[1:2]<-c("Outcome","Exposure")
    return(datas)
  }
  if(method=="Expregression"){ 
    require(reshape)
    dataas<-rep(NA,4)
    for( i in 1:dim(data1)[1]){
      datas<-matrix(c(data1[[3]][i],data1[[5]][i],
                      data1[[4]][i],data1[[6]][i]),2,2,byrow=TRUE)
      colnames(datas)<-c("E","NE")
      row.names(datas)<-c("T","NT")
      datas<-ftable(datas) ; datas<-as.data.frame(datas)
      datas<-cbind(datas,Study=rep(paste(data1[[1]][i]),dim(datas)[1]))
      dataas<-rbind(dataas,datas)
    }
    datas<-dataas[-1,]
    names(datas)[1:2]<-c("Outcome","Exposure")
    datas <- untable(datas[,c(1,2,4)], num=datas[,3])
    return(datas)
  }
  if(method=="summary") return(datas)
  
} # Function reshapedata 