first_step<-function(gender,MM,xxs,nn,niter,burnin,disciplines,i,denM,seed){
  set.seed(seed)
  
  M<-MM[i]
  xs<-xxs[i,]
  n<-nn[i]
  
  N<-rep(0,niter)
  Me<-rep(0,niter)
  
  N[1]<-M
  Me[1]<-round(numFNCHypergeo(xs[1],n,M,1))[1]
  
  lambda.N<-N[1]
  lambda.Me<-Me[1]
  
  low_poi<-n+2
  
  prop.sd.Me<-Me[1]/denM

  w<-1
  
  accMe<-NULL
  accN<-NULL
  
  for(iter in 2:niter){
    
    last.Me<-Me[iter-1]
    last.N<-N[iter-1]
    
    Meprop<-round(rtnorm(1,last.Me,prop.sd.Me,
                         a=xs[1]+1,
                         b=last.N-(n-xs[1]+1)))
    
    RATIO.Me = exp(log(dFNCHypergeo(x=xs[1],
                                    m1=Meprop,
                                    m2=last.N-Meprop,
                                    n=n,
                                    odds=w)) + 
                     log(ddunif(x=Meprop,
                                min=xs[1]+1,
                                max=last.N-(n-xs[1]+1))) +
                     log(dtnorm(x=last.Me,
                                mean=Meprop,
                                sd=prop.sd.Me,
                                a=xs[1]+1,
                                b=last.N-(n-xs[1]+1))) -
                     log(dFNCHypergeo(x=xs[1],
                                      m1=last.Me,
                                      m2=last.N-last.Me,
                                      n=n,
                                      odds=w)) - 
                     log(ddunif(x=last.Me,
                                min=xs[1]+1,
                                max=last.N-(n-xs[1]+1))) -
                     log(dtnorm(x=Meprop,
                                mean=last.Me,
                                sd=prop.sd.Me,
                                a=xs[1]+1,
                                b=last.N-(n-xs[1]+1)))
    )
    
    if(runif(1)<RATIO.Me){
      Me[iter]<-Meprop
      if(iter>burnin){accMe<-sum(accMe,1)}
    }else{
      Me[iter]<-last.Me}
    
    Nprop<-rpois(1,lambda.N)
    
    RATIO.N = exp(log(dFNCHypergeo(x=xs[1],
                                   m1=Me[iter],
                                   m2=Nprop-Me[iter],
                                   n=n,
                                   odds=w)) + 
                    log(dtpois(x=Nprop,
                              lambda=lambda.N,a=low_poi)) +
                    log(dpois(x=last.N,
                              lambda=lambda.N)) -
                    log(dFNCHypergeo(xs[1],
                                     Me[iter],
                                     last.N-Me[iter],
                                     n=n,
                                     odds=w)) - 
                    log(dtpois(x=last.N,
                              lambda=lambda.N,a=low_poi)) -
                    log(dpois(x=Nprop,
                              lambda=lambda.N))
    )
    
    if(runif(1)<RATIO.N){
      N[iter]<-Nprop
      if(iter>burnin){accN<-sum(accN,1)}
    }else{
      N[iter]<-last.N}
    
    if(iter%%1000==0){
      print(paste("ITERATION No",iter))}
    
  }
  
  Mburn<-Me[burnin:niter]
  Mthin<-Mburn[seq(1,length(Mburn),by=10)]  
  print(acf(Mthin))
  
  Nburn<-N[burnin:niter]
  Nthin<-Nburn[seq(1,length(Nburn),by=10)]  
  print(acf(Nthin))
  
  print(plot(Nburn,type="l"))
  print(plot(Me[burnin:niter],type="l"))
  
  save<-cbind(Mthin,Nthin)
  write.csv(save, paste(wd,"/output/step1_",disciplines[i],gender,".csv",sep=""))
  
  return(list=c(accrateN=accN/(niter-burnin),accrateM=accMe/(niter-burnin)))
}
