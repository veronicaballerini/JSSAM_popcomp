################################################################################
#### Inferring a population composition from survey data with nonignorable #####
####      nonresponse: Borrowing information from external sources         #####
####                                                                       #####
####             Authors: Veronica Ballerini, Brunero Liseo                #####
####                                                                       #####
####   This code reproduces results of step 1 included in the paper        #####
################################################################################

rm(list=ls())
wd <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(wd)

#### Load libraries and functions
library(extraDistr)
library(BiasedUrn)
library(xtable)
source(paste(wd,"/functions/step1.R",sep=""))

#### Load data
load(paste(wd,"/data/finaldata.RData",sep=""))
alma<-read.csv(paste(wd,"/data/almalaurea2011.csv",sep=""),sep=";")

disciplines<-rownames(nf)

### Table 2:
e_NSI_m<-as.table(round((xs_m[,1]/nm)*100,1))
e_NSI_f<-as.table(round((xs_f[,1]/nf)*100,1))

e_alma_m<-as.table(round(as.matrix((alma[,2]/
                                      (alma[,2]+alma[,4]))*100,ncol=1),1))
e_alma_f<-as.table(round(as.matrix((alma[,3]/
                                      (alma[,3]+alma[,5]))*100,ncol=1),1))

tab1_M <- cbind(e_NSI_m,e_alma_m,nm,(alma[,2]+alma[,4]),M_m)
colnames(tab1_M) <- c("E%_NSI","E%_Almal.","Tot_NSI","Tot_Almal.","Tot_SNR")

tab1_F <- cbind(e_NSI_f,e_alma_f,nf,(alma[,3]+alma[,5]),M_f)
colnames(tab1_F) <- c("E%_NSI","E%_Almal.","Tot_NSI","Tot_Almal.","Tot_SNR")

# xtable(tab1_M, type = "latex")
# xtable(tab1_F, type = "latex")
rm(alma)

#### Results: Step 1

#1 - main text
first_step("F",MM=M_f,xxs=xs_f,nn=nf,
           niter=10000,burnin=5000,
           disciplines=disciplines,i=1,
           denM=2.5,seed=310198) 
first_step("M",MM=M_m,xxs=xs_m,nn=nm,
           niter=10000,burnin=5000,
           disciplines=disciplines,i=1,
           denM=3.5,seed=310198) 

#2 - main text
first_step("F",MM=M_f,xxs=xs_f,nn=nf,
           niter=10000,burnin=5000,
           disciplines=disciplines,i=2,
           denM=3,seed=310198) 
first_step("M",MM=M_m,xxs=xs_m,nn=nm,
           niter=10000,burnin=5000,
           disciplines=disciplines,i=2,
           denM=3.5,seed=310198) 

#3 - appendix
first_step("F",MM=M_f,xxs=xs_f,nn=nf,
           niter=10000,burnin=5000,
           disciplines=disciplines,i=3,
           denM=3.5,seed=310198)
first_step("M",MM=M_m,xxs=xs_m,nn=nm,
           niter=10000,burnin=5000,
           disciplines=disciplines,i=3,
           denM=4,seed=310198)

#4 - appendix
first_step("F",MM=M_f,xxs=xs_f,nn=nf,
           niter=10000,burnin=5000,
           disciplines=disciplines,i=4,
           denM=3.1,seed=310198) 
first_step("M",MM=M_m,xxs=xs_m,nn=nm,
           niter=10000,burnin=5000,
           disciplines=disciplines,i=4,
           denM=3,seed=310198) 

#5 - appendix
first_step("F",MM=M_f,xxs=xs_f,nn=nf,
           niter=10000,burnin=5000,
           disciplines=disciplines,i=5,
           denM=3.5,seed=310198) 
first_step("M",MM=M_m,xxs=xs_m,nn=nm,
           niter=10000,burnin=5000,
           disciplines=disciplines,i=5,
           denM=4,seed=310198) 

#6 - main text
first_step("F",MM=M_f,xxs=xs_f,nn=nf,
           niter=10000,burnin=5000,
           disciplines=disciplines,i=6,
           denM=3,seed=310198) 
first_step("M",MM=M_m,xxs=xs_m,nn=nm,
           niter=10000,burnin=5000,
           disciplines=disciplines,i=6,
           denM=3,seed=310198) 

#7 - appendix
first_step("F",MM=M_f,xxs=xs_f,nn=nf,
           niter=10000,burnin=5000,
           disciplines=disciplines,i=7,
           denM=5,seed=310198) 
first_step("M",MM=M_m,xxs=xs_m,nn=nm,
           niter=10000,burnin=5000,
           disciplines=disciplines,i=7,
           denM=3,seed=310198) 

#8 - main text
first_step("F",MM=M_f,xxs=xs_f,nn=nf,
           niter=10000,burnin=5000,
           disciplines=disciplines,i=8,
           denM=5,seed=310198) 
first_step("M",MM=M_m,xxs=xs_m,nn=nm,
           niter=10000,burnin=5000,
           disciplines=disciplines,i=8,
           denM=4,seed=310198) 

#9 - appendix
first_step("F",MM=M_f,xxs=xs_f,nn=nf,
           niter=10000,burnin=5000,
           disciplines=disciplines,i=9,
           denM=3.5,seed=310198) 
first_step("M",MM=M_m,xxs=xs_m,nn=nm,
           niter=10000,burnin=5000,
           disciplines=disciplines,i=9,
           denM=3.3,seed=310198) 

#10 - appendix
first_step("F",MM=M_f,xxs=xs_f,nn=nf,
           niter=10000,burnin=5000,
           disciplines=disciplines,i=10,
           denM=4,seed=310198) 
first_step("M",MM=M_m,xxs=xs_m,nn=nm,
           niter=10000,burnin=5000,
           disciplines=disciplines,i=10,
           denM=3,seed=310198) 
