################################################################################
#### Inferring a population composition from survey data with nonignorable #####
####      nonresponse: Borrowing information from external sources         #####
####                                                                       #####
####             Authors: Veronica Ballerini, Brunero Liseo                #####
####                                                                       #####
####   This code reproduces results of step 3 included in the paper        #####
################################################################################

rm(list=ls())
wd <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(wd)

#### Load libraries and functions
library(extraDistr)
library(BiasedUrn)
library(HDInterval)
source(paste(wd,"/functions/step3.R",sep=""))
source(paste(wd,"/functions/step3_indepprop.R",sep=""))

norm.approx=function(lower,upper){	
  
  m = (lower+upper)/2
  s = upper/4 - lower/4
  
  return(c(mean=m,sd=s))
  
} 

#### Load data
load(paste(wd,"/data/finaldata.RData",sep=""))
disciplines<-rownames(nf)

datalong<-read.csv(paste(wd,"/data/data_almalaurea.csv",sep=""),
                   header=TRUE,sep=";")

#### Results: Step 3

#1 - main text
j<-1
for(year in c(2012:2020)){
  runM<-third_step(year=year,gender="M",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=30,denw=5,seed=310198)
}

for(year in c(2012:2013,2019)){
  runF<-third_step(year=year,gender="F",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=20,denw=5,seed=310198)
}

for(year in 2014:2017){
  runF<-third_step(year=year,gender="F",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=30,denw=4,seed=310198)
}

for(year in c(2018,2020)){
  runF<-third_step(year=year,gender="F",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=30,denw=5,seed=310198)
}

###################################################################

#2 - main text
j<-2

for(year in c(2012:2013)){
  runM<-third_step(year=year,gender="M",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=20,denw=15,seed=310198)
}

runM<-third_step(year=2014,gender="M",data=datalong,
                 niter=50000,burnin=30000,
                 disciplines = disciplines,i=j,sdw=1,
                 denM=40,denw=20,seed=310198)

for(year in c(2015:2016)){
  runM<-third_step(year=year,gender="M",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=45,denw=25,seed=310198) 
}

for(year in c(2017,2019)){
  runM<-third_step(year=year,gender="M",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=35,denw=20,seed=310198)
}

for(year in c(2018,2020)){
  runM<-third_step(year=year,gender="M",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=30,denw=20,seed=310198)
}


for(year in c(2012:2013,2020)){
  runF<-third_step(year=2013,gender="F",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=25,denw=20,seed=310198)
}

runF<-third_step(year=2014,gender="F",data=datalong,
                 niter=50000,burnin=30000,
                 disciplines = disciplines,i=j,sdw=1,
                 denM=55,denw=20,seed=310198)

for(year in 2015:2016){
  runF<-third_step(year=year,gender="F",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=75,denw=20,seed=310198)
}

runF<-third_step(year=2017,gender="F",data=datalong,
                 niter=50000,burnin=30000,
                 disciplines = disciplines,i=j,sdw=1,
                 denM=45,denw=20,seed=310198)

for(year in 2018:2019){
  runF<-third_step(year=year,gender="F",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=35,denw=20,seed=310198)
}

###################################################################

#3 - appendix
j<-3
for(year in 2012:2020){
  runM<-third_step(year=year,gender="M",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=50,denw=70,seed=310198)
}

for(year in 2012:2018){
  runF<-third_step(year=year,gender="F",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=120,denw=120,seed=310198)
}

for(year in 2019:2020){
  runF<-third_step(year=year,gender="F",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=80,denw=120,seed=310198)
}


###################################################################

#4 - appendix
j<-4
for(year in c(2012:2013)){
  runM<-third_step(year=year,gender="M",data=datalong,
                   niter=10000,burnin=5000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=25,denw=20,seed=310198)
}

runM<-third_step(year=2014,gender="M",data=datalong,
                 niter=10000,burnin=5000,
                 disciplines = disciplines,i=j,sdw=1,
                 denM=45,denw=30,seed=310198)

for(year in c(2015:2017)){
  runM<-third_step(year=year,gender="M",data=datalong,
                 niter=10000,burnin=5000,
                 disciplines = disciplines,i=j,sdw=1,
                 denM=40,denw=20,seed=310198)
}

for(year in c(2018:2019)){
  runM<-third_step(year=year,gender="M",data=datalong,
                   niter=10000,burnin=5000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=25,denw=20,seed=310198)
}

runM<-third_step(year=2020,gender="M",data=datalong,
                 niter=10000,burnin=5000,
                 disciplines = disciplines,i=j,sdw=1,
                 denM=30,denw=20,seed=310198)

# for(year in 2012:2013){
#   runF<-third_step_indepprop(year=year,gender="F",data=datalong,
#                                  niter=50000,burnin=30000,
#                                  disciplines = disciplines,i=j,sdw=1,
#                                  denM=50000,denw=500,seed=310198)
# }
# 
# # **** post MC
# for(year in 2014:2016){ 
#   runF<-third_step_indepprop(year=year,gender="F",data=datalong,
#                              niter=100000,burnin=80000,
#                              disciplines = disciplines,i=j,sdw=1,
#                              denM=50,denw=500,seed=310198)
# }
# 
# for(year in 2017:2020){
#   runF<-third_step_indepprop(year=year,gender="F",data=datalong,
#                              niter=50000,burnin=30000,
#                              disciplines = disciplines,i=j,sdw=1,
#                              denM=50000,denw=500,seed=310198)
# }

###################################################################

#5 - appendix
j<-5

# # Males: unreliable results - post MC
# for(year in 2012:2017){
#   runM<-third_step_indepprop(year=year,gender="M",data=datalong,
#                              niter=100000,burnin=80000,
#                              disciplines = disciplines,i=j,sdw=1,
#                              denM=3000,denw=1000,seed=310198)
# }
# 
# for(year in 2018:2020){
#   runM<-third_step_priorproposal(year=year,gender="M",data=datalong,
#                    niter=100000,burnin=50000,
#                    disciplines = disciplines,i=j,sdw=1,
#                    denw=2000,seed=310198)
# }

for(year in 2012:2020){
  runF<-third_step(year=year,gender="F",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=60,denw=60,seed=310198)
}

for(year in 2018:2020){
  runF<-third_step(year=year,gender="F",data=datalong,
                   niter=10000,burnin=5000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=35,denw=30,seed=310198)
}

###################################################################

#6 - main text
j<-6
for(year in 2012:2020){
  runM<-third_step(year=year,gender="M",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=20,denw=5,seed=310198)
}

for(year in 2012:2020){
  runF<-third_step(year=year,gender="F",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=20,denw=5,seed=310198)
}

###################################################################

#7 - appendix
j<-7
for(year in c(2012:2013,2018:2020)){
  runM<-third_step(year=year,gender="M",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=20,denw=10,seed=310198)
}

for(year in 2014:2017){
  runM<-third_step(year=year,gender="M",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=30,denw=10,seed=310198)
}

for(year in 2012:2015){
  runF<-third_step(year=year,gender="F",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=30,denw=2,seed=310198)
}

for(year in 2016:2020){
  runF<-third_step(year=year,gender="F",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=40,denw=2,seed=310198)
}

###################################################################

#8 - main text
j<-8
for(year in 2012:2013){
  runM<-third_step(year=year,gender="M",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=40,denw=15,seed=310198)
}

for(year in 2014:2020){
  runM<-third_step(year=year,gender="M",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=60,denw=10,seed=310198)
}

for(year in 2012:2020){
  runF<-third_step(year=year,gender="F",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=60,denw=20,seed=310198)
}

###################################################################

#9 - appendix
j<-9
for(year in 2012:2020){
  runM<-third_step(year=year,gender="M",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=20,denw=10,seed=310198)
}

# # Females: unreliable - post MC
# for(year in 2012:2020){
#   runF<-third_step(year=year,gender="F",data=datalong,
#                    niter=100000,burnin=80000,
#                    disciplines = disciplines,i=j,sdw=1,
#                    denM=5000,denw=100,seed=310198)
# }

###################################################################

#10 - appendix
j<-10
runM<-third_step(year=2012,gender="M",data=datalong,
                 niter=100000, burnin=30000,
                 disciplines = disciplines,i=j,sdw=1,
                 denM=60,denw=35,seed=310198)

runM<-third_step(year=2013,gender="M",data=datalong,
                 niter=100000,burnin=30000,
                 disciplines = disciplines,i=j,sdw=1,
                 denM=80,denw=35,seed=310198)

for(year in 2014:2017){
  runM<-third_step(year=year,gender="M",data=datalong,
                   niter=100000,burnin=80000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=150,denw=40,seed=310198)
}

for(year in 2018:2020){
  runM<-third_step(year=year,gender="M",data=datalong,
                   niter=100000,burnin=80000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=60,denw=40,seed=310198)
}

for(year in 2012:2020){
  runF<-third_step(year=year,gender="F",data=datalong,
                   niter=50000,burnin=30000,
                   disciplines = disciplines,i=j,sdw=1,
                   denM=30,denw=10,seed=310198)
}

