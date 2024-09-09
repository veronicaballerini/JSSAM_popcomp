################################################################################
#### Inferring a population composition from survey data with nonignorable #####
####      nonresponse: Borrowing information from external sources         #####
####                                                                       #####
####             Authors: Veronica Ballerini, Brunero Liseo                #####
####                                                                       #####
####   This code reproduces results of step 2 included in the paper        #####
################################################################################

rm(list=ls())
wd <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(wd)

#### Load libraries and functions
library(extraDistr)
library(BiasedUrn)
library(HDInterval)
source(paste(wd,"/functions/step2.R",sep=""))

norm.approx=function(lower,upper){	
  
  m = (lower+upper)/2
  s = upper/4 - lower/4
  
  return(c(mean=m,sd=s))
  
} 

#### Load data
load(paste(wd,"/data/finaldata.RData",sep=""))
disciplines<-rownames(nf)

alma11<-read.csv(paste(wd,"/data/almalaurea2011.csv",sep=""),sep=";")

alma11_m<-alma11[,c(1,2,4)]
alma11_f<-alma11[,c(1,3,5)]

M_f<-list()

for(i in 1:length(disciplines)){
    M_f[[i]]<-read.csv(paste(wd,"/output/step1_",disciplines[i],"F.csv",sep=""))
}

M_m<-list()

for(i in 1:length(disciplines)){
    M_m[[i]]<-read.csv(paste(wd,"/output/step1_",disciplines[i],"M.csv",sep=""))
}

Mf<-NULL
for(i in 1:length(disciplines)){
    Mf<-cbind(Mf,mean(M_f[[i]][,2]))
}

Mm<-NULL
for(i in 1:length(disciplines)){
    Mm<-cbind(Mm,mean(M_m[[i]][,2]))
}

Mf_upp<-NULL
for(i in 1:length(disciplines)){
    Mf_upp<-cbind(Mf_upp,hdi(M_f[[i]][,2])[2])
}

Mm_upp<-NULL
for(i in 1:length(disciplines)){
  Mm_upp<-cbind(Mm_upp,hdi(M_m[[i]][,2])[2])
}

Mtot<-read.table(paste(wd,"/data/M_ms2011.txt",sep=""))
Mmtot<-Mtot[c(1,3,5,7,9,11,13,15,17,19)]
Mftot<-Mtot[c(2,4,6,8,10,12,14,16,18,20)]

#### Results: Step 2

par(mfrow=c(1,2))
for(i in 1:length(disciplines)){
  qqnorm(M_f[[i]][,2])
  qqline(M_f[[i]][,2])
  
  hist(M_f[[i]][,2],freq = FALSE, 
       main=paste("Posterior of M|N -", disciplines[i], ", females"), 
       xlab="M|N")
  curve(dnorm(x,mean(M_f[[i]][,2]),sd(M_f[[i]][,2])),add=TRUE)
}

for(i in 1:length(disciplines)){
  qqnorm(M_f[[i]][,3])
  qqline(M_f[[i]][,3])
  
  hist(M_f[[i]][,3],freq = FALSE,
       main=paste("Posterior of M|N -", disciplines[i], ", males"), 
       xlab="M|N")
  curve(dnorm(x,mean(M_f[[i]][,3]),sd(M_f[[i]][,3])),add=TRUE)
}

#1 - main text 
i<-1
MM<-M_m[[i]]
xxs<-as.numeric(alma11_m[i,c(2:3)])
nn<-sum(alma11_m[i,c(2:3)])
second_step(gender="M",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            niter=30000,burnin=20000,
            sdw=1,
            denM=25,denN=25,denw=5,seed=310198)

MM<-M_f[[i]]
xxs<-as.numeric(alma11_f[i,c(2:3)])
nn<-sum(alma11_f[i,c(2:3)])
second_step(gender="F",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            niter=30000,burnin=20000,
            sdw=1,
            denM=20,denN=20,denw=5,seed=310198)

#2 - main text
i<-2
MM<-M_f[[i]]
xxs<-as.numeric(alma11_f[i,c(2:3)])
nn<-sum(alma11_f[i,c(2:3)])
second_step(gender="F",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            niter=30000,burnin=20000,
            sdw=1,
            denM=20,denN=20,denw=20,seed=310198)

MM<-M_m[[i]]
xxs<-as.numeric(alma11_m[i,c(2:3)])
nn<-sum(alma11_m[i,c(2:3)])
second_step(gender="M",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            niter=30000,burnin=20000,
            sdw=1,
            denM=20,denN=20,denw=20,seed=310198)

#3 - appendix
i<-3
MM<-M_f[[i]]
xxs<-as.numeric(alma11_f[i,c(2:3)])
nn<-sum(alma11_f[i,c(2:3)])
second_step(gender="F",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            niter=30000,burnin=20000,
            sdw=1,
            denM=150,denN=40,denw=150,seed=310198)

MM<-M_m[[i]]
xxs<-as.numeric(alma11_m[i,c(2:3)])
nn<-sum(alma11_m[i,c(2:3)])
second_step(gender="M",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            niter=30000,burnin=20000,
            sdw=1,
            denM=60,denN=40,denw=60,seed=310198)

#4 - appendix
i<-4
MM<-M_m[[i]]
xxs<-as.numeric(alma11_m[i,c(2:3)])
nn<-sum(alma11_m[i,c(2:3)])
second_step(gender="M",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            niter=30000,burnin=20000,
            sdw=1,
            denM=40,denN=20,denw=20,seed=310198)

MM<-M_f[[i]]
xxs<-as.numeric(alma11_f[i,c(2:3)])
nn<-sum(alma11_f[i,c(2:3)])
second_step(gender="F",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            niter=30000,burnin=20000,
            sdw=1,
            denM=200,denN=20,denw=200,seed=310198)

#5 - appendix
i<-5
MM<-M_f[[i]]
xxs<-as.numeric(alma11_f[i,c(2:3)])
nn<-sum(alma11_f[i,c(2:3)])
second_step(gender="F",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            niter=30000,burnin=20000,
            sdw=1,
            denM=50,denN=20,denw=50,seed=310198)

MM<-M_m[[i]]
xxs<-as.numeric(alma11_m[i,c(2:3)])
nn<-sum(alma11_m[i,c(2:3)])
second_step(gender="M",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            niter=30000,burnin=20000,
            sdw=1,
            denM=750,denN=20,denw=200,seed=310198) # unreliable results

#6 - main text
i<-6
MM<-M_f[[i]]
xxs<-as.numeric(alma11_f[i,c(2:3)])
nn<-sum(alma11_f[i,c(2:3)])
second_step(gender="F",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            niter=30000,burnin=20000,
            sdw=1,
            denM=30,denN=40,denw=7,seed=310198)

MM<-M_m[[i]]
xxs<-as.numeric(alma11_m[i,c(2:3)])
nn<-sum(alma11_m[i,c(2:3)])
second_step(gender="M",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            niter=30000,burnin=20000,
            sdw=1,
            denM=25,denN=35,denw=4,seed=310198)

#7 - appendix
i<-7
MM<-M_f[[i]]
xxs<-as.numeric(alma11_f[i,c(2:3)])
nn<-sum(alma11_f[i,c(2:3)])
second_step(gender="F",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            niter=30000,burnin=20000,
            sdw=1,
            denM=60,denN=60,denw=2,seed=310198)

MM<-M_m[[i]]
xxs<-as.numeric(alma11_m[i,c(2:3)])
nn<-sum(alma11_m[i,c(2:3)])
second_step(gender="M",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            niter=30000,burnin=20000,
            sdw=1,
            denM=30,denN=30,denw=5,seed=310198)

#8 - main text
i<-8
MM<-M_f[[i]]
xxs<-as.numeric(alma11_f[i,c(2:3)])
nn<-sum(alma11_f[i,c(2:3)])
second_step(gender="F",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            niter=30000,burnin=20000,
            sdw=1,
            denM=80,denN=60,denw=30,seed=310198)

MM<-M_m[[i]]
xxs<-as.numeric(alma11_m[i,c(2:3)])
nn<-sum(alma11_m[i,c(2:3)])
second_step(gender="M",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            niter=30000,burnin=20000,
            sdw=1,
            denM=50,denN=50,denw=15,seed=310198)

#9 - appendix
i<-9
MM<-M_f[[i]]
xxs<-as.numeric(alma11_f[i,c(2:3)])
nn<-sum(alma11_f[i,c(2:3)])
second_step(gender="F",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            sdw=1,
            denM=150,denN=25,denw=100,seed=310198)

MM<-M_m[[i]]
xxs<-as.numeric(alma11_m[i,c(2:3)])
nn<-sum(alma11_m[i,c(2:3)])
second_step(gender="M",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            sdw=1,
            denM=35,denN=30,denw=7,seed=310198) 

#10 - appendix
i<-10
MM<-M_f[[i]]
xxs<-as.numeric(alma11_f[i,c(2:3)])
nn<-sum(alma11_f[i,c(2:3)])
second_step(gender="F",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            sdw=1,
            denM=50,denN=50,denw=10,seed=310198)

MM<-M_m[[i]]
xxs<-as.numeric(alma11_m[i,c(2:3)])
nn<-sum(alma11_m[i,c(2:3)])
second_step(gender="M",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            sdw=1,
            denM=70,denN=30,denw=60,seed=310198)

#### Table 3
wwM<-matrix(NA,334,10)
for(i in 1:10){
  wwM[,i]<-read.table(paste(wd,"/output/step2_",disciplines[i],"Msdw1",".csv",sep=""),
                      sep=",",
                      header=TRUE)[,4]
}

post.mean.wwM<-apply(wwM,2,mean)
overall.meanM<-mean(post.mean.wwM)
post.median.wwM<-apply(wwM,2,median)
post.sd.wwM<-apply(wwM,2,sd)
var.withinM<-mean(post.sd.wwM^2)
var.betweenM<-mean((post.mean.wwM-overall.meanM)^2)

wwF<-matrix(NA,334,10)
for(i in 1:10){
  wwF[,i]<-read.table(paste(wd,"/output/step2_",disciplines[i],"Fsdw1",".csv",sep=""),
                      sep=",",
                      header=TRUE)[,4]
}

post.mean.wwF<-apply(wwF,2,mean)
overall.meanF<-mean(post.mean.wwF)
post.median.wwF<-apply(wwF,2,median)
post.sd.wwF<-apply(wwF,2,sd)
var.withinF<-mean(post.sd.wwF^2)
var.betweenF<-mean((post.mean.wwF-overall.meanF)^2)

overall.mean<-mean(rbind(as.matrix(post.mean.wwF),
                         as.matrix(post.mean.wwM)))
overall.median<-quantile(rbind(as.matrix(post.mean.wwF),
                               as.matrix(post.mean.wwM)))
var.within<-mean(rbind(as.matrix(post.sd.wwF^2),
                       as.matrix(post.sd.wwM^2)))
diffF<-(post.mean.wwF-overall.meanF)^2
diffM<-(post.mean.wwM-overall.meanM)^2

var.between<-mean(rbind(as.matrix(diffF),
                        as.matrix(diffM)))

tab<-rbind(post.mean.wwF,
           post.median.wwF,
           post.sd.wwF,
           post.mean.wwM,
           post.median.wwM,
           post.sd.wwM)
colnames(tab)<-disciplines
xtable(tab)
