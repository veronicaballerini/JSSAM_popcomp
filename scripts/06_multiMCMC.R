rm(list=ls())
setwd("D:/fisher/ReplicationPackage")

library(BiasedUrn)
library(extraDistr)

N.true=10000
numcol = 5
true.pi<-rdirichlet(1,rep(1,numcol))
trueColors<-rep(0,numcol)
for(i in 1:(numcol-1)){
  trueColors[i]<-round(true.pi[i]*N.true)
}
trueColors[numcol]<-N.true-sum(trueColors[1:(numcol-1)])

true.theta=rbeta(numcol,1,1)

true.w<-rep(0,numcol)
for(i in 1:numcol){
  true.w[i]<-true.theta[i]/(1-true.theta[i])
}

nsam <- 20

truecolors<-matrix(NA,nrow=nsam,ncol=numcol)
for(i in 1:numcol){
  truecolors[,i]<-rbinom(nsam,trueColors[i],true.theta[i])
}

n=rep(NA,nsam)
for(i in 1:nsam){
  n[i]<-sum(truecolors[i,])}

set.seed(310198)

################ MCMC #################
niter=10000
burnin=2000

lambda_M1<-trueColors[1]
M_upp<-lambda_M1+round(lambda_M1/100*20)#qpois(0.95,lambda_M1)

Nupp<-lambda_M1+20000

pM_MCMC <- list()
pN_MCMC <- list()
pp_MCMC <- list()

sdprop<-100

for(i in 1:nsam){
  M_low<-truecolors[i,1]
  
  if(i%%1==0){
    print(paste("SAMPLE",i))
  }
  
  colors<-truecolors[i,]
  
  N<-rep(0,niter)
  M<-matrix(0,niter,numcol)
  
  N[1]<-12000
  M[1,1]<-lambda_M1
  # M[,1]<-lambda_M1
  # M[1,2:numcol]<-round((N[1]-M[1,1])/(numcol-1))
  M[1,2:numcol]<-round(numMFNCHypergeo(colors,n[i],N[1],true.w)[-1])
  
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

    ########################## M2
    for(m in 2:numcol){
        N1m.prop <- rtpois(1,lambda=M[iter,1]+M[iter-1,m],
                           a=(colors[m]+M[iter,1]))
      # N1m.prop <- round(rtnorm(1,(M[iter,1]+M[iter-1,m]),sdprop,
      #                          a=M[iter,1]+colors[m]))
      
      RATIO.Mm = exp(log(dFNCHypergeo(colors[1],M[iter,1],N1m.prop-M[iter,1],sum(colors[c(1,m)]),true.w[1]/true.w[m])) + 
                       log(dunif(N1m.prop+sum(last.M[-c(1,m)]),sum(colors)+2,Nupp)) + 
                       # log(dtnorm(M[iter,1]+last.M[m],N1m.prop,sdprop,a=M[iter,1]+colors[m]))-
                       log(dtpois(M[iter,1]+last.M[m],lambda=N1m.prop,a=(colors[m]+M[iter,1]))) -
                       log(dFNCHypergeo(colors[1],M[iter,1],last.M[m],sum(colors[c(1,m)])+2,true.w[1]/true.w[m])) - 
                       log(dunif(last.N,sum(colors),Nupp)) - 
                       log(dtpois(N1m.prop,lambda=(M[iter,1]+last.M[m]),a=(colors[m]+M[iter,1])))
                       # log(dtnorm(N1m.prop,(M[iter,1]+M[iter-1,m]),sdprop,
                       #            a=M[iter,1]+colors[m]))
                       )
      
      if(runif(1)<RATIO.Mm){
        M[iter,m]<-N1m.prop-M[iter,1]
      }else{
        M[iter,m]<-last.M[m]}
      
      rm(RATIO.Mm)
    }
    
    N[iter]<-sum(M[iter,])
  }
  
  pM_MCMC[[i]] <- M[burnin:niter,]
  pN_MCMC[[i]] <- N[burnin:niter]
}

pNmcmc<-pN_MCMC
pMmcmc<-pM_MCMC

rm(list=c("pN_MCMC","pM_MCMC"))
rm(list=c("M","burnin","colors","i","iter","last.M","last.N",
          "m","M1prop","N","N1m.prop","niter","sdprop"))

pM_ABC<-list()
pN_ABC<-list()

nrep<-10000
  
for(s in 1:nsam){
  
  M<-NULL
  colors<-truecolors[s,]
  truefreq<-colors/sum(colors)
  
  truesums<-NULL
  truesums[1]<-colors[1]
  for(i in 2:numcol){
    truesums[i]<-colors[1]+colors[i]
  }
  
  epsilon<-.05
  
  r<-NULL
  r<-sum(r,1)
  
  while(r<nrep){
    
    if(r%%100==0){
      print(paste("SAMPLE N°", s, "REP N°", r))
    }
    
    # repeat{
      M.prop<-NULL
      M.prop[1]<-rpois(1,lambda_M1)
      
      p.prop<-rdirichlet(1,rep(1,numcol))
      for(i in 2:numcol){
        M.prop[i]<-rdunif(1,colors[i]+1,Nupp)
        # M.prop[i]<-round(p.prop[i]*M.prop[1]/p.prop[1])
      }
      # if(M.prop[1]>colors[1]&
      #    M.prop[2]>colors[2]&
      #    M.prop[3]>colors[3]&
      #    M.prop[4]>colors[4]&
      #    M.prop[5]>colors[5]){break}
    # }
    
    zeta<-rMFNCHypergeo(1,m=M.prop,n=sum(colors),odds=true.w)
    freq<-zeta/sum(colors)
    rho<-max(abs(freq-truefreq))
    
    if(rho<=epsilon){
      M<-rbind(M,M.prop)
      r<-sum(r,1)
    }
  }
  
  pM_ABC[[s]]<-M
  pN_ABC[[s]]<-rowSums(M)
  
}

pMabc<-pM_ABC
pNabc<-pN_ABC

rm(list=c("pN_ABC","pM_ABC"))

library(ggplot2)
library(RColorBrewer)
library(HDInterval)

meanNmcmc<-NULL
for(i in 1:length(pNmcmc)){
  meanNmcmc<-rbind(meanNmcmc,mean(pNmcmc[[i]]))
}

mean_Nmcmc<-data.frame(meanNmcmc)
mean_Nmcmc$prior<-""

ggplot(mean_Nmcmc,aes(x=mean_Nmcmc[,2], y=mean_Nmcmc[,1] , fill=mean_Nmcmc[,2])) +
  geom_violin(trim=FALSE, show.legend = FALSE) +
  labs(title = "", x = "", y="") +
  geom_hline(yintercept=N.true, linetype="dashed", color="#DEEBF7") +
  scale_fill_manual(name= "",values=c(brewer.pal(n=9,name = "GnBu")[9]))

meanNabc<-NULL
for(i in 1:length(pNabc)){
  meanNabc<-rbind(meanNabc,mean(pNabc[[i]]))
}

mean_Nabc<-data.frame(meanNabc)
mean_Nabc$prior<-""

ggplot(mean_Nabc,aes(x=mean_Nabc[,2], y=mean_Nabc[,1] , fill=mean_Nabc[,2])) +
  geom_violin(trim=FALSE, show.legend = FALSE) +
  labs(title = "", x = "", y="") +
  geom_hline(yintercept=N.true, linetype="dashed", color="#DEEBF7") +
  scale_fill_manual(name= "",values=c(brewer.pal(n=9,name = "GnBu")[9]))

Nmcmc.in.int<-NULL
for(s in 1:nsam){
  Nint<-hdi(pNmcmc[[s]])
  Ntf<-N.true>Nint[1]&N.true<Nint[2]
  Nmcmc.in.int<-rbind(Nmcmc.in.int,Ntf)
}
sum(Nmcmc.in.int)/nsam

Nabc.in.int<-NULL
for(s in 1:7){
  Nint<-hdi(pNabc[[s]])
  Ntf<-N.true>Nint[1]&N.true<Nint[2]
  Nabc.in.int<-rbind(Nabc.in.int,Ntf)
}
sum(Nabc.in.int)/7#nsam

Mmcmc.in.int<-NULL
for(s in 1:nsam){
  Mm.in.int<-NULL
  for(m in 1:numcol){
    Mmint<-hdi(pMmcmc[[s]])
    Mmtf<-trueColors[m]>Mmint[1,m]&trueColors[m]<Mmint[2,m]
    Mm.in.int<-cbind(Mm.in.int,Mmtf) 
  }
  Mmcmc.in.int<-rbind(Mmcmc.in.int,Mm.in.int)
}
colSums(Mmcmc.in.int)/nsam

Mabc.in.int<-NULL
for(s in 1:7){
  Mm.in.int<-NULL
  for(m in 1:numcol){
    Mmint<-hdi(pMabc[[s]])
    Mmtf<-trueColors[m]>Mmint[1,m]&trueColors[m]<Mmint[2,m]
    Mm.in.int<-cbind(Mm.in.int,Mmtf) 
  }
  Mabc.in.int<-rbind(Mabc.in.int,Mm.in.int)
}
colSums(Mabc.in.int)/7#nsam

meanMmcmc<-NULL
for(i in 1:length(pMmcmc)){
  meanMmcmc<-rbind(meanMmcmc,colMeans(pMmcmc[[i]]))
}
mean_Mmcmc<-as.data.frame(meanMmcmc)

meanMabc<-NULL
for(i in 1:length(pMabc)){
  meanMabc<-rbind(meanMabc,colMeans(pMabc[[i]]))
}
mean_Mabc<-as.data.frame(meanMabc)

mean_Mmcmc$M1<-""
mean_Mmcmc$M2<-""
mean_Mmcmc$M3<-""
mean_Mmcmc$M4<-""
mean_Mmcmc$M5<-""

mean_Mabc$M1<-""
mean_Mabc$M2<-""
mean_Mabc$M3<-""
mean_Mabc$M4<-""
mean_Mabc$M5<-""

colgraph<-c(7,6,5,4,3)
for(m in 1:numcol){
  print(ggplot(mean_Mmcmc,aes(x=mean_Mmcmc[,m], y=mean_Mmcmc[,m], fill=mean_Mmcmc[,numcol+m])) +
          geom_violin(trim=FALSE, show.legend = FALSE) +
          geom_hline(yintercept=trueColors[m], linetype="dashed", color="#DEEBF7") +
          labs(title = "", x = "", y="")) #+
          # scale_fill_brewer(name = "Prior on M1", palette = "Blues"))
          # scale_fill_manual(name= "",values=c(brewer.pal(n=8,name = "Blues")[colgraph[m]])))
}
for(m in 1:numcol){
  print(ggplot(mean_Mabc,aes(x=mean_Mabc[,m], y=mean_Mabc[,m], fill=mean_Mabc[,numcol+m])) +
          geom_violin(trim=FALSE, show.legend = FALSE) +
          geom_hline(yintercept=trueColors[m], linetype="dashed", color="#DEEBF7") +
          labs(title = "", x = "", y=""))# +
          # scale_fill_brewer(name = "Prior on M1", palette = "Blues"))
          # scale_fill_manual(name= "",values=c(brewer.pal(n=8,name = "GnBu")[colgraph[m]])))
}

