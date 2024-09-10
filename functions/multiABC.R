multiABC<-function(x){
  seed<-x$seed
  set.seed(seed)
  
  colors <- matrix(NA,nrow=numcol,ncol=1)
  for(i in 1:numcol){
    colors[i,] <- rbinom(1,trueColors[i],true.theta[i])
  }
  
  n <- sum(colors)
  
  M_low<-colors[1,1]+1
  
  M<-matrix(0,niter,numcol)

  M[1,1]<-lambda_M1
  M[1,2:numcol]<-round(numMFNCHypergeo(colors,n,12000,true.w)[-1])
  
  truefreq<-NULL
  for(i in 1:numcol){
    truefreq[i]<-colors[i]/sum(colors)
  }
  
  epsilon<-NULL
  m1<-rpois(100,lambda_M1)
  m2<-rdunif(1000,colors[2]+1,Nupp)
  m3<-rdunif(1000,colors[3]+1,Nupp)
  m4<-rdunif(1000,colors[4]+1,Nupp)
  m5<-rdunif(1000,colors[5]+1,Nupp)
  x1<-x2<-x3<-x4<-x5<-array(NA,c(100,1000,100))
  
  for(i in 1:length(m1)){
    for(j in 1:length(m2)){
      x1[i,j,]<-rFNCHypergeo(100,m1[i],m2[j],colors[1]+colors[2],true.w[1]/true.w[2])
    }
  }
  epsilon[1]<-quantile(abs(x1/sum(colors)-truefreq[1]),0.05)
  
  for(i in 1:length(m1)){
    for(j in 1:length(m2)){
      x2[i,j,]<-rFNCHypergeo(100,m2[j],m1[i],colors[1]+colors[2],true.w[2]/true.w[1])
    }
  }
  epsilon[2]<-quantile(abs(x2/sum(colors)-truefreq[2]),0.05)
  
  for(i in 1:length(m1)){
    for(j in 1:length(m3)){
      x3[i,j,]<-rFNCHypergeo(100,m3[j],m1[i],colors[1]+colors[3],true.w[3]/true.w[1])
    }
  }
  epsilon[3]<-quantile(abs(x3/sum(colors)-truefreq),0.05)
  
  for(i in 1:length(m1)){
    for(j in 1:length(m4)){
      x4[i,j,]<-rFNCHypergeo(100,m4[j],m1[i],colors[1]+colors[4],true.w[4]/true.w[1])
    }
  }
  epsilon[4]<-quantile(abs(x4/sum(colors)-truefreq),0.05)
  
  for(i in 1:length(m1)){
    for(j in 1:length(m5)){
      x5[i,j,]<-rFNCHypergeo(100,m5[j],m1[i],colors[1]+colors[5],true.w[5]/true.w[1])
    }
  }
  epsilon[5]<-quantile(abs(x5/sum(colors)-truefreq),0.05)
  
  rhos<-rep(10,numcol)
  
  for(iter in 2:niter){
    
    last.M<-M[iter-1,]
    iterM<-NULL
    
    rho<-rhos[1]
    while(rho>epsilon[1]){
      M.prop<-rdunif(1,M_low,M_upp)
      zeta<-rFNCHypergeo(1,M.prop,last.M[2],colors[1]+colors[2],true.w[1]/true.w[2])
      freq<-zeta/sum(colors)
      rho<-abs(freq-truefreq[1])
    }
    
    iterM[1]<-M.prop
    rm(M.prop,rho)
    
    for(i in 2:numcol){
      rho<-rhos[i]
      while(rho>epsilon[i]){
        N1m.prop <- rtpois(1,lambda=iterM[1]+M[iter-1,i],
                           a=(colors[i]+iterM[1]+1))
        # M.prop<-rdunif(1,colors[i]+1,Nupp)
        M.prop<-N1m.prop-iterM[1]
        zeta<-rFNCHypergeo(1,M.prop,last.M[1],colors[1]+colors[i],true.w[i]/true.w[1])
        freq<-zeta/sum(colors)
        rho<-abs(freq-truefreq[i])
      }
      iterM[i]<-M.prop
      rm(M.prop,rho)
    }
    M[iter,]<-iterM
    
  }
  
  print("DONE!")
  list(pM_ABC=M,pN_ABC=rowSums(M))
}