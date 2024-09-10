multiMCMC<-function(x){
  seed<-x$seed
  set.seed(seed)
  
  colors <- matrix(NA,nrow=numcol,ncol=1)
  for(i in 1:numcol){
    colors[i,] <- rbinom(1,trueColors[i],true.theta[i])
  }
  
  n <- sum(colors)
  
  M_low<-colors[1,1]+1
  
  N<-rep(0,niter)
  M<-matrix(0,niter,numcol)
  
  N[1]<-12000
  M[1,1]<-lambda_M1
  M[1,2:numcol]<-round(numMFNCHypergeo(colors,n,N[1],true.w)[-1])
  
  for(iter in 2:niter){
    
    last.M <- M[iter-1,]
    last.N <- N[iter-1]
    
    ########################## M1
    M1prop<-round(runif(1,M_low,M_upp))
    
    RATIO.M1 = exp(log(dFNCHypergeo(colors[1],M1prop,last.M[2],sum(colors[1:2]),true.w[1]/true.w[2])) +
                     log(dpois(M1prop,lambda_M1)) +
                     log(dunif(last.M[1],M_low,M_upp)) -
                     log(dFNCHypergeo(colors[1],last.M[1],last.M[2],sum(colors[1:2]),true.w[1]/true.w[2])) -
                     log(dpois(last.M[1],lambda_M1)) -
                     log(dunif(M1prop,M_low,M_upp)))
    
    if(runif(1)<RATIO.M1){
      M[iter,1]<-M1prop
    }else{
      M[iter,1]<-last.M[1]}
    
    rm(RATIO.M1)
    
    ########################## M2+
    for(m in 2:numcol){
      N1m.prop <- rtpois(1,lambda=M[iter,1]+M[iter-1,m],
                         a=(colors[m]+M[iter,1]+1))
      
      RATIO.Mm = exp(log(dFNCHypergeo(colors[1],M[iter,1],N1m.prop-M[iter,1],sum(colors[c(1,m)]),true.w[1]/true.w[m])) + 
                       log(dunif(N1m.prop+sum(last.M[-c(1,m)]),sum(colors)+2,Nupp)) + 
                       log(dtpois(M[iter,1]+last.M[m],lambda=N1m.prop,a=(colors[m]+M[iter,1]))) -
                       log(dFNCHypergeo(colors[1],M[iter,1],last.M[m],sum(colors[c(1,m)])+2,true.w[1]/true.w[m])) - 
                       log(dunif(last.N,sum(colors),Nupp)) - 
                       log(dtpois(N1m.prop,lambda=(M[iter,1]+last.M[m]),a=(colors[m]+M[iter,1])))
      )
      
      if(runif(1)<RATIO.Mm){
        M[iter,m]<-N1m.prop-M[iter,1]
      }else{
        M[iter,m]<-last.M[m]}
      
      rm(RATIO.Mm)
    }
    
    N[iter]<-sum(M[iter,])
  }
  
  pM_MCMC <- M[burnin:niter,]
  pN_MCMC <- N[burnin:niter]
  
  list(pM_MCMC=pM_MCMC,pN_MCMC=pN_MCMC)
}