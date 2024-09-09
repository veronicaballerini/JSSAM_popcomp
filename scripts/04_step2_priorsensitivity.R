################################################################################
#### Inferring a population composition from survey data with nonignorable #####
####      nonresponse: Borrowing information from external sources         #####
####                                                                       #####
####             Authors: Veronica Ballerini, Brunero Liseo                #####
####                                                                       #####
####           This code reproduces w prior sensitivity results            #####
################################################################################

rm(list=ls())
wd <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(wd)

#### Load libraries and functions
library(extraDistr)
library(BiasedUrn)
library(HDInterval)
source(paste(wd,"/functions/step2.R",sep=""))

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

#### Results: Step 2 - sensitivity
### sd w = 3

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
            sdw=3,
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
            sdw=3,
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
            sdw=3,
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
            sdw=3,
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
            sdw=3,
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
            sdw=3,
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
            sdw=3,
            denM=30,denN=20,denw=20,seed=310198)

MM<-M_f[[i]]
xxs<-as.numeric(alma11_f[i,c(2:3)])
nn<-sum(alma11_f[i,c(2:3)])
second_step(gender="F",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            niter=100000,burnin=90000,
            sdw=3,
            denM=100,denN=20,denw=200,seed=310198)

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
            sdw=3,
            denM=30,denN=20,denw=50,seed=310198)

MM<-M_m[[i]]
xxs<-as.numeric(alma11_m[i,c(2:3)])
nn<-sum(alma11_m[i,c(2:3)])
second_step(gender="M",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            niter=100000,burnin=90000,
            sdw=3,
            denM=200,denN=20,denw=200,seed=310198) # unreliable results

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
            sdw=3,
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
            sdw=3,
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
            sdw=3,
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
            sdw=3,
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
            sdw=3,
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
            sdw=3,
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
            sdw=3,
            denM=150,denN=25,denw=100,seed=310198)

MM<-M_m[[i]]
xxs<-as.numeric(alma11_m[i,c(2:3)])
nn<-sum(alma11_m[i,c(2:3)])
second_step(gender="M",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            sdw=3,
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
            sdw=3,
            denM=50,denN=50,denw=10,seed=310198)

MM<-M_m[[i]]
xxs<-as.numeric(alma11_m[i,c(2:3)])
nn<-sum(alma11_m[i,c(2:3)])
second_step(gender="M",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            sdw=3,
            denM=70,denN=30,denw=60,seed=310198)


### sd w = 4

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
            sdw=4,
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
            sdw=4,
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
            sdw=4,
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
            sdw=4,
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
            sdw=4,
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
            sdw=4,
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
            sdw=4,
            denM=30,denN=20,denw=20,seed=310198)

MM<-M_f[[i]]
xxs<-as.numeric(alma11_f[i,c(2:3)])
nn<-sum(alma11_f[i,c(2:3)])
second_step(gender="F",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            niter=100000,burnin=90000,
            sdw=4,
            denM=100,denN=20,denw=200,seed=310198)

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
            sdw=4,
            denM=30,denN=20,denw=50,seed=310198)

MM<-M_m[[i]]
xxs<-as.numeric(alma11_m[i,c(2:3)])
nn<-sum(alma11_m[i,c(2:3)])
second_step(gender="M",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            niter=100000,burnin=90000,
            sdw=4,
            denM=200,denN=20,denw=200,seed=310198) # unreliable results

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
            sdw=4,
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
            sdw=4,
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
            sdw=4,
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
            sdw=4,
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
            sdw=4,
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
            sdw=4,
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
            sdw=4,
            denM=150,denN=25,denw=100,seed=310198)

MM<-M_m[[i]]
xxs<-as.numeric(alma11_m[i,c(2:3)])
nn<-sum(alma11_m[i,c(2:3)])
second_step(gender="M",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            sdw=4,
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
            sdw=4,
            denM=50,denN=50,denw=10,seed=310198)

MM<-M_m[[i]]
xxs<-as.numeric(alma11_m[i,c(2:3)])
nn<-sum(alma11_m[i,c(2:3)])
second_step(gender="M",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            sdw=4,
            denM=70,denN=30,denw=60,seed=310198)


### sd w = 5

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
            sdw=5,
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
            sdw=5,
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
            sdw=5,
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
            sdw=5,
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
            sdw=5,
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
            sdw=5,
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
            sdw=5,
            denM=30,denN=20,denw=20,seed=310198)

MM<-M_f[[i]]
xxs<-as.numeric(alma11_f[i,c(2:3)])
nn<-sum(alma11_f[i,c(2:3)])
second_step(gender="F",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            niter=100000,burnin=90000,
            sdw=5,
            denM=100,denN=20,denw=200,seed=310198)

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
            sdw=5,
            denM=30,denN=20,denw=50,seed=310198)

MM<-M_m[[i]]
xxs<-as.numeric(alma11_m[i,c(2:3)])
nn<-sum(alma11_m[i,c(2:3)])
second_step(gender="M",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            niter=100000,burnin=90000,
            sdw=5,
            denM=200,denN=20,denw=200,seed=310198) # unreliable results

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
            sdw=5,
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
            sdw=5,
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
            sdw=5,
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
            sdw=5,
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
            sdw=5,
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
            sdw=5,
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
            sdw=5,
            denM=150,denN=25,denw=100,seed=310198)

MM<-M_m[[i]]
xxs<-as.numeric(alma11_m[i,c(2:3)])
nn<-sum(alma11_m[i,c(2:3)])
second_step(gender="M",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            sdw=5,
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
            sdw=5,
            denM=50,denN=50,denw=10,seed=310198)

MM<-M_m[[i]]
xxs<-as.numeric(alma11_m[i,c(2:3)])
nn<-sum(alma11_m[i,c(2:3)])
second_step(gender="M",
            MM=MM,
            xxs=xxs,
            nn=nn,
            disciplines=disciplines,
            sdw=5,
            denM=70,denN=30,denw=60,seed=310198)

