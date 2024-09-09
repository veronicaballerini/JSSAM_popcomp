third_step_indepprop<-function(year,
                     gender,
                     data=datalong,
                     niter=20000,
                     burnin=10000,
                     disciplines=disciplines,
                     i,
                     sdw=1,
                     denM,
                     denw,
                     seed=310198){
  
  M_ASN<-read.table(paste(wd,"/data/M_ms",year,".txt",sep=""))
  M_ASN<-matrix(M_ASN,nrow=length(disciplines),ncol=2,byrow=TRUE)
  M_ASN<-as.numeric(ifelse(gender=="M",M_ASN[i,1],M_ASN[i,2]))
  
  w2<-read.table(paste(wd,"/output/step2_",disciplines[i],gender,"sdw",sdw,".csv",sep=""),
                 sep=",",
                 header=TRUE)
  w2<-w2[,4]
  
  alma<-data[which(data$GESS_year==(year+1)&data$disciplines==disciplines[i]),]
  
  if(gender=="M"){ 
    xxs<-as.vector(alma[c(3,5)])
  }else{
    xxs<-as.vector(alma[c(4,6)])
  }
  
  set.seed(seed)
  
  w_low<-hdi(log(w2))[1]
  w_upp<-hdi(log(w2))[2]
  
  qqnorm(log(w2))
  qqline(log(w2))
  
  hist(log(w2),freq = FALSE)
  curve(dnorm(x,mean(log(w2)),sd(log(w2))),add=TRUE)
  
  meanw <- norm.approx(w_low,w_upp)[1]
  sd_w<- norm.approx(w_low,w_upp)[2]
  
  xxs1<-as.numeric(xxs[1])
  xxs2<-as.numeric(xxs[2])
  
  M1low<-xxs1+1
  M1upp<-M_ASN-xxs2-1
  
  M1<-rep(0,niter)
  w<-rep(0,niter)
  
  M1[1]<-round(numFNCHypergeo(xxs1,xxs1+xxs2,M_ASN,mean(w2))[1])
  
  w[1]<-mean(w2)
  
  prop.sd.Me<-M1[1]/denw
  prop.sd.w<-w[1]/denw
  
  accM<-NULL
  accw<-NULL
  
  for(iter in 2:niter){
    
    last.M<-M1[iter-1]
    last.w<-w[iter-1]
    
    Mprop<-round(rtnorm(n=1,
                        mean=M1[1],
                        sd=prop.sd.Me,
                        a=xxs1+1,
                        b=M_ASN-xxs2-1))
    
    RATIO.M = exp(log(dFNCHypergeo(x=xxs1,
                                   m1=Mprop,
                                   m2=M_ASN-Mprop,
                                   n=xxs1+xxs2,
                                   odds=last.w)) +
                    log(ddunif(x=Mprop,
                               xxs1+1,
                               M_ASN-xxs2-1)) +
                    log(dtnorm(x=last.M,
                               mean=M1[1],
                               sd=prop.sd.Me,
                               a=xxs1+1,
                               b=M_ASN-xxs2-1)) -
                    log(dFNCHypergeo(x=xxs1,
                                     m1=last.M,
                                     m2=M_ASN-last.M,
                                     n=xxs1+xxs2,
                                     odds=last.w)) -
                          log(ddunif(x=last.M,
                                     xxs1+1,
                                     M_ASN-xxs2-1)) -
                          log(dtnorm(x=Mprop,
                                     mean=M1[1],
                                     sd=prop.sd.Me,
                                     a=xxs1+1,
                                     b=M_ASN-xxs2-1))
    )
    
    if(is.na(runif(1)<RATIO.M)==FALSE&runif(1)<RATIO.M){
      M1[iter]<-Mprop
      if(iter>burnin){accM<-sum(accM,1)}
    }else{
      M1[iter]<-last.M}
    
    wprop<-rlnorm(1,log(last.w)+prop.sd.w/2,prop.sd.w)
    
    RATIO.w = exp(log(dFNCHypergeo(x=xxs1,
                                   m1=M1[iter],
                                   m2=M_ASN-M1[iter],
                                   n=xxs1+xxs2,
                                   odds=wprop)) + 
                    log(dnorm(log(wprop),meanw,sd_w)) +
                    log(dlnorm(last.w,log(wprop)+prop.sd.w/2,prop.sd.w)) -
                    log(dFNCHypergeo(x=xxs1,
                                     m1=M1[iter],
                                     m2=M_ASN-M1[iter],
                                     n=xxs1+xxs2,
                                     odds=last.w)) - 
                    log(dnorm(log(last.w),meanw,sd_w)) -
                    log(dlnorm(wprop,log(last.w)+prop.sd.w/2,prop.sd.w))
    )
    
    if(runif(1)<RATIO.w){
      w[iter]<-wprop
      if(iter>burnin){accw<-sum(accw,1)}
    }else{
      w[iter]<-last.w
    }
    
    if(iter%%1000==0){
      print(paste("ITERATION No",iter,"-",disciplines[i],"-",gender,"-",year))}
    
  }
  
  Mburn<-M1[burnin:niter]
  Mthin<-Mburn[seq(1,length(Mburn),by=30)]  
  # print(acf(Mthin))
  
  wburn<-w[burnin:niter]
  wthin<-wburn[seq(1,length(wburn),by=30)]  
  # print(acf(wthin))
  
  print(plot(Mthin,type="l"))
  print(plot(wthin,type="l"))
  
  save<-cbind(Mthin,wthin)
  write.csv(save,paste(wd,"/output/step3_",year,disciplines[i],gender,".csv",sep=""))
  
  pri<-data.frame(acc_ratio_M=ifelse(is.null(accM)==T,1,accM/(niter-burnin)),
                  acc_ratio_w=accw/(niter-burnin),
                  hdi_M_low=as.numeric(hdi(Mthin)[1]),
                  hdi_M_upp=as.numeric(hdi(Mthin)[2]),
                  hdi_w_low=as.numeric(hdi(wthin)[1]),
                  hdi_w_upp=as.numeric(hdi(wthin)[2]))
  print(pri)
  
  return(list=c(Mratio=accM/(niter-burnin),
                wratio=accw/(niter-burnin),
                Mhpd=hdi(Mthin),
                wthin=hdi(wthin)))
}
