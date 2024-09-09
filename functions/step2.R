second_step<-function(gender,
                      MM,xxs,nn,
                      niter=20000,burnin=10000,
                      disciplines,
                      sdw,
                      denM,denN,denw,
                      seed){
  
  set.seed(seed)
  
  M_low<-hdi(MM$Mthin)[1]
  M_upp<-hdi(MM$Mthin)[2]
  
  Nupp<-hdi(MM$Nthin)[2]
  Nlow<-hdi(MM$Nthin)[1]
  
  meanM1 <- norm.approx(M_low,M_upp)[1]
  sdM1<- norm.approx(M_low,M_upp)[2]
  
  meanN <- norm.approx(Nlow,Nupp)[1]
  sdN <- norm.approx(Nlow,Nupp)[2]
  
  N<-rep(0,niter)
  M1<-rep(0,niter)
  w<-rep(0,niter)
  
  N[1]<-round(mean(MM$Nthin))
  M1[1]<-round(mean(MM$Mthin))
  
  w[1]<-oddsFNCHypergeo(xxs[1],round(mean(MM$Mthin)),
                        round(mean(MM$Nthin)-mean(MM$Mthin)),sum(xxs))
  
  prop.sd.N<-N[1]/denN
  prop.sd.Me<-M1[1]/denM
  prop.sd.w<-w[1]/denw
  
  accM<-NULL
  accN<-NULL
  accw<-NULL
  
  for(iter in 2:niter){
    
    last.M<-M1[iter-1]
    last.N<-N[iter-1]
    last.w<-w[iter-1]
    
    Mprop<-rtnorm(1,
                  mean=meanM1,
                  sd=prop.sd.Me,
                  a=xxs[1]+1,
                  b=last.N-xxs[2]-1)
    
    RATIO.M = exp(log(dFNCHypergeo(x=xxs[1],
                                   m1=Mprop,
                                   m2=last.N-Mprop,
                                   n=sum(xxs),
                                   odds=last.w)) +
                    log(dtnorm(x=Mprop,
                               mean=meanM1,
                               sd=sdM1,
                               a=xxs[1]+1,
                               b=last.N-xxs[2]-1)) +
                    log(dtnorm(x=last.M,
                               mean=Mprop,
                               sd=prop.sd.Me,
                               a=xxs[1]+1,
                               b=last.N-xxs[2]-1)) -
                    log(dFNCHypergeo(x=xxs[1],
                                     m1=last.M,
                                     m2=last.N-last.M,
                                     n=sum(xxs),
                                     odds=last.w)) - 
                    log(dtnorm(x=last.M,
                               mean=meanM1,
                               sd=sdM1,
                               a=xxs[1]+1,
                               b=last.N-xxs[2]-1)) -
                    log(dtnorm(x=Mprop,
                               mean=last.M,
                               sd=prop.sd.Me,
                               a=xxs[1]+1,
                               b=last.N-xxs[2]-1))
    )
    
    if(runif(1)<RATIO.M){
      M1[iter]<-Mprop
      if(iter>burnin){accM<-sum(accM,1)}
    }else{
      M1[iter]<-last.M}
    
    Nprop<-round(rtnorm(1,last.N,prop.sd.N,a=M1[iter]+xxs[2]+1))

    RATIO.N = exp(log(dFNCHypergeo(x=xxs[1],
                                   m1=M1[iter],
                                   m2=Nprop-M1[iter],
                                   n=sum(xxs),
                                   odds=last.w)) + 
                    log(dnorm(x=Nprop,
                              mean=meanN,
                              sd=sdN)) +
                    log(dtnorm(x=last.N,
                               mean=Nprop,
                               sd=prop.sd.N,
                               a=M1[iter]+xxs[2])) -
                    log(dFNCHypergeo(x=xxs[1],
                                     m1=M1[iter],
                                     m2=last.N-M1[iter],
                                     n=sum(xxs),
                                     odds=last.w)) - 
                    log(dnorm(x=last.N,
                              mean=meanN,
                              sd=sdN)) -
                    log(dtnorm(x=Nprop,
                               mean=last.N,
                               sd=prop.sd.N,
                               a=M1[iter]+xxs[2]))
    )
    
    if(runif(1)<RATIO.N){
      N[iter]<-Nprop
      if(iter>burnin){accN<-sum(accN,1)}
    }else{
      N[iter]<-last.N}
    
    wprop<-rlnorm(1,log(last.w)+prop.sd.w/2,prop.sd.w)
    
    RATIO.w = exp(log(dFNCHypergeo(x=xxs[1],
                                   m1=M1[iter],
                                   m2=N[iter]-M1[iter],
                                   n=sum(xxs),
                                   odds=wprop)) + 
                    log(dlnorm(wprop,meanlog = 0, sdlog = sdw)) +
                    log(dlnorm(last.w,log(wprop)+prop.sd.w/2,prop.sd.w)) -
                    log(dFNCHypergeo(x=xxs[1],
                                     m1=M1[iter],
                                     m2=N[iter]-M1[iter],
                                     n=sum(xxs),
                                     odds=last.w)) - 
                    log(dlnorm(last.w,meanlog = 0, sdlog = sdw)) -
                    log(dlnorm(wprop,log(last.w)+prop.sd.w/2,prop.sd.w))
    )
    
    if(runif(1)<RATIO.w){
      w[iter]<-wprop
      if(iter>burnin){accw<-sum(accw,1)}
    }else{
      w[iter]<-last.w
    }
    
    if(iter%%1000==0){
      print(paste("ITERATION No",iter))}
    
  }
  
  Mburn<-M1[burnin:niter]
  Mthin<-Mburn[seq(1,length(Mburn),by=30)]  
  print(acf(Mthin))
  
  Nburn<-N[burnin:niter]
  Nthin<-Nburn[seq(1,length(Nburn),by=30)]  
  print(acf(Nthin))
  
  wburn<-w[burnin:niter]
  wthin<-wburn[seq(1,length(wburn),by=30)]  
  print(acf(wthin))
  
  print(plot(Nthin,type="l"))
  print(plot(Mthin,type="l"))
  print(plot(wthin,type="l"))
  
  save<-cbind(Mthin,Nthin,wthin)
  write.csv(save,paste(wd,"/output/step2_",disciplines[i],gender,"sdw",sdw,".csv",sep=""))
  
  return(list=c(Mratio=accM/(niter-burnin),
                Nratio=accN/(niter-burnin),
                wratio=accw/(niter-burnin),
                Mhpd=hdi(Mthin),
                Nhpd=hdi(Nthin),
                wthin=hdi(wthin)))
}