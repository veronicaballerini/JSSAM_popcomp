impute_M_MC<-function(data,w,discipline,i,gender="F",year){
  M_ASN<-read.table(paste(wd,"/data/M_ms",year,".txt",sep=""))
  M_ASN<-matrix(M_ASN,nrow=10,ncol=2,byrow=TRUE)
  M_ASN<-as.numeric(ifelse(gender=="M",M_ASN[i,1],M_ASN[i,2]))
  
  data<-data[which(data$disciplines==discipline&data$GESS_year==year),]
  
  if(gender=="F"){
    xxs<-c(data$empl_F,data$unempl_F)
  }else{xxs<-c(data$empl_M,data$unempl_M)}
  
  M_MC<-NULL
  for(j in 1:length(w)){
    M_MC<-rbind(M_MC,numFNCHypergeo(xxs[1],xxs[1]+xxs[2],M_ASN,w[j])[1])
  }
  return(M_MC)
}
