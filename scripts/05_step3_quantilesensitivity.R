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
library(xtable)
source(paste(wd,"/functions/sensitivity.R",sep=""))

norm.approx=function(lower,upper){	
  
  m = (lower+upper)/2
  s = upper/4 - lower/4
  
  return(c(mean=m,sd=s))
  
} 

#### Load data
load(paste(wd,"/data/finaldata.RData",sep=""))
disciplines<-rownames(nf)
datalong<-read.csv(paste(wd,"/data/data_almalaurea.csv",sep=""),header=TRUE,sep=";")

###################################################################
# q = 0.25 

# 1
j<-1

# M
for(year in c(2012:2013)){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=20,denw=5,seed=310198,qnt=0.25)
}

for(year in c(2014:2015)){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=30,denw=2,seed=310198,qnt=0.25)
}

for(year in c(2016:2018,2020)){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=40,denw=5,seed=310198,qnt=0.25)
}

runM<-sensitivity_step(year=2019,gender="M",data=datalong,
                       disciplines = disciplines,i=j,
                       denM=30,denw=5,seed=310198,qnt=0.25)

# F
for(year in c(2012:2013,2016)){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=20,denw=5,seed=310198,qnt=0.25)
}

for(year in c(2014:2015,2017:2020)){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                       disciplines = disciplines,i=j,
                       denM=30,denw=5,seed=310198,qnt=0.25)
}

###################################################################
# 2
j<-2

# M
for(year in 2012:2013){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=20,denw=10,seed=310198,qnt=0.25)
}

for(year in c(2014,2016:2018)){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                   disciplines = disciplines,i=j,
                   denM=40,denw=10,seed=310198,qnt=0.25)
}

runM<-sensitivity_step(year=2015,gender="M",data=datalong,
                       disciplines = disciplines,i=j,
                       denM=50,denw=10,seed=310198,qnt=0.25)

for(year in 2019:2020){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=30,denw=10,seed=310198,qnt=0.25)
}

# F
runF<-sensitivity_step(year=2012,gender="F",data=datalong,
                       disciplines = disciplines,i=j,
                       denM=20,denw=10,seed=310198,qnt=0.25)

for(year in c(2013,2018:2020)){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                   disciplines = disciplines,i=j,
                   denM=30,denw=10,seed=310198,qnt=0.25)
}

for(year in c(2014,2016)){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=50,denw=10,seed=310198,qnt=0.25)
}

runF<-sensitivity_step(year=2015,gender="F",data=datalong,
                       disciplines = disciplines,i=j,
                       denM=60,denw=10,seed=310198,qnt=0.25)

###################################################################
# 3
j<-3

# M
for(year in 2012:2020){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=30,denw=40,seed=310198,qnt=0.25)
}

# F
for(year in 2012:2015){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=120,denw=80,seed=310198,qnt=0.25)
}

for(year in 2016:2017){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                   disciplines = disciplines,i=j,
                   denM=120,denw=100,seed=310198,qnt=0.25)
}

year<-2018
runF<-sensitivity_step(year=year,gender="F",data=datalong,
                       disciplines = disciplines,i=j,
                       denM=100,denw=100,seed=310198,qnt=0.25)

for(year in 2019:2020){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                   disciplines = disciplines,i=j,
                   denM=80,denw=120,seed=310198,qnt=0.25)
}

###################################################################
# 4
j<-4

# M
runM<-sensitivity_step(year=2012,gender="M",data=datalong,
                       niter=10000,burnin=5000,
                       disciplines = disciplines,i=j,
                       denM=25,denw=20,seed=310198,qnt=0.25)

runM<-sensitivity_step(year=2013,gender="M",data=datalong,
                   niter=10000,burnin=5000,
                   disciplines = disciplines,i=j,
                   denM=30,denw=20,seed=310198,qnt=0.25)

runM<-sensitivity_step(year=2014,gender="M",data=datalong,
                 niter=10000,burnin=5000,
                 disciplines = disciplines,i=j,
                 denM=40,denw=20,seed=310198,qnt=0.25)

runM<-sensitivity_step(year=2015,gender="M",data=datalong,
                   niter=10000,burnin=5000,
                   disciplines = disciplines,i=j,
                   denM=35,denw=10,seed=310198,qnt=0.25)

runM<-sensitivity_step(year=2016,gender="M",data=datalong,
                 niter=10000,burnin=5000,
                 disciplines = disciplines,i=j,
                 denM=25,denw=10,seed=310198,qnt=0.25)

runM<-sensitivity_step(year=2017,gender="M",data=datalong,
                       niter=10000,burnin=5000,
                       disciplines = disciplines,i=j,
                       denM=30,denw=10,seed=310198,qnt=0.25)

for(year in 2018:2020){
  runM<-sensitivity_step(year=year,gender="M",
                         niter=10000,burnin=5000,
                         disciplines = disciplines,i=j,
                         denM=20,denw=10,seed=310198,qnt=0.25)
}


###################################################################
# 5
j<-5

# F
for(year in 2012:2020){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                   disciplines = disciplines,i=j,
                   denM=60,denw=30,seed=310198, qnt=0.25)
}

###################################################################
# 6
j<-6

# M
for(year in 2012:2020){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                   disciplines = disciplines,i=j,
                   denM=20,denw=5,seed=310198,qnt=0.25)
}

# F
for(year in 2012:2020){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                   disciplines = disciplines,i=j,
                   denM=20,denw=5,seed=310198,qnt=0.25)
}

###################################################################
# 7
j<-7

# M
for(year in 2012:2020){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=20,denw=5,seed=310198,qnt=0.25)
}

# F
for(year in 2012:2013){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=20,denw=2,seed=310198,qnt=0.25)
}

for(year in 2014:2020){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                   disciplines = disciplines,i=j,
                   denM=40,denw=2,seed=310198,qnt=0.25)
}

###################################################################
# 8
j<-8

# M
for(year in 2012:2015){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=30,denw=10,seed=310198,qnt=0.25)
}

for(year in 2016:2020){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=40,denw=10,seed=310198,qnt=0.25)
}

# F
for(year in 2012:2015){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=50,denw=15,seed=310198,qnt=0.25)
}

for(year in 2016:2020){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=60,denw=15,seed=310198,qnt=0.25)
}

###################################################################
# 9
j<-9

# M
for(year in 2012:2015){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=30,denw=5,seed=310198,qnt=0.25)
}

for(year in 2016:2020){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=25,denw=5,seed=310198,qnt=0.25)
}

###################################################################
# 10
j<-10

# M
for(year in c(2012:2013,2018:2020)){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=60,denw=30,seed=310198,qnt=0.25)
}

for(year in 2014:2017){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=150,denw=30,seed=310198,qnt=0.25)
}

# F
for(year in 2012:2020){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=30,denw=10,seed=310198,qnt=0.25)
}

###################################################################
# q = 0.75

# 1
j<-1

# M
for(year in c(2012:2014,2018:2019)){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=30,denw=5,seed=310198,qnt=0.75)
}

for(year in c(2016,2020)){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=40,denw=5,seed=310198,qnt=0.75)
}

for(year in c(2015,2017)){
runM<-sensitivity_step(year=year,gender="M",data=datalong,
                       disciplines = disciplines,i=j,
                       denM=50,denw=5,seed=310198,qnt=0.75)
}

# F
for(year in c(2012:2013)){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                         disciplines = disciplines, i=j,
                         denM=20,denw=5,seed=310198,qnt=0.75)
}

year<-2014
runF<-sensitivity_step(year=year,gender="F",data=datalong,
                       disciplines = disciplines,i=j,
                       denM=50,denw=5,seed=310198,qnt=0.75)

for(year in c(2015:2020)){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=40,denw=5,seed=310198,qnt=0.75)
}

###################################################################
# 2
j<-2

# M
runM<-sensitivity_step(year=2012,gender="M",data=datalong,
                       disciplines = disciplines,i=j,
                       denM=20,denw=10,seed=310198,qnt=0.75)

runM<-sensitivity_step(year=2013,gender="M",data=datalong,
                       disciplines = disciplines,i=j,
                       denM=30,denw=10,seed=310198,qnt=0.75)

runM<-sensitivity_step(year=2014,gender="M",data=datalong,
                       disciplines = disciplines,i=j,
                       denM=50,denw=10,seed=310198,qnt=0.75)

for(year in 2015:2020){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=60,denw=10,seed=310198,qnt=0.75)
}

# F
runF<-sensitivity_step(year=2012,gender="F",data=datalong,
                       disciplines = disciplines,i=j,
                       denM=30,denw=10,seed=310198,qnt=0.75)

for(year in c(2013,2018,2020)){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                         disciplines=disciplines,i=j,
                         denM=40,denw=10,seed=310198,qnt=0.75)
}

runF<-sensitivity_step(year=2014,gender="F",data=datalong,
                       disciplines = disciplines,i=j,
                       denM=60,denw=10,seed=310198,qnt=0.75)

for(year in c(2016:2017,2019)){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=70,denw=10,seed=310198,qnt=0.75)
}

runF<-sensitivity_step(year=2015,gender="F",data=datalong,
                       disciplines = disciplines,i=j,
                       denM=90,denw=10,seed=310198,qnt=0.75)


###################################################################
# 3
j<-3

# M
for(year in 2012:2020){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=30,denw=40,seed=310198,qnt=0.75)
}

# F
for(year in 2012:2015){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=200,denw=80,seed=310198,qnt=0.75)
}

for(year in 2016:2018){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=120,denw=100,seed=310198,qnt=0.75)
}
for(year in 2019:2020){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=80,denw=120,seed=310198,qnt=0.75)
}

###################################################################
# 4
j<-4

# M
runM<-sensitivity_step(year=2012,gender="M",data=datalong,
                       disciplines = disciplines,i=j,
                       denM=35,denw=20,seed=310198,qnt=0.75)

runM<-sensitivity_step(year=2013,gender="M",data=datalong,
                       disciplines = disciplines,i=j,
                       denM=30,denw=20,seed=310198,qnt=0.75)

runM<-sensitivity_step(year=2014,gender="M",data=datalong,
                       disciplines = disciplines,i=j,
                       denM=70,denw=20,seed=310198,qnt=0.75)

for(year in c(2015:2017)){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=80,denw=20,seed=310198,qnt=0.75)
}

for(year in 2018:2020){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=40,denw=10,seed=310198,qnt=0.75)
}

###################################################################
# 5
j<-5

# F
runF<-sensitivity_step(year=2012,gender="F",data=datalong,
                       disciplines = disciplines,i=j,
                       denM=90,denw=30,seed=310198,qnt=0.75)

runF<-sensitivity_step(year=2013,gender="F",data=datalong,
                       disciplines=disciplines,i=j,
                       denM=70,denw=30,seed=310198,qnt=0.75)

runF<-sensitivity_step(year=2014,gender="F",data=datalong,
                       disciplines = disciplines,i=j,
                       denM=100,denw=30,seed=310198,qnt=0.75)

runF<-sensitivity_step(year=2015,gender="F",data=datalong,
                       disciplines=disciplines,i=j,
                       denM=80,denw=30,seed=310198,qnt=0.75)

for(year in 2016:2020){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=60,denw=30,seed=310198,qnt=0.75)
}

###################################################################
# 6
j<-6

# M
for(year in 2012:2020){
  runM<-sensitivity_step(year=year,gender="M", data=datalong,
                         disciplines = disciplines,i=j,
                         denM=20,denw=5, seed=310198,qnt=0.75)
}

# F
for(year in 2012:2020){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=20,denw=5,seed=310198,qnt=0.75)
}

###################################################################
# 7
j<-7

# M
for(year in c(2012:2013,2016:2020)){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=20,denw=5,seed=310198,qnt=0.75)
}

for(year in 2014:2015){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=60,denw=5,seed=310198,qnt=0.75)
}


for(year in 2012:2013){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=20,denw=2,seed=310198,qnt=0.75)
}

for(year in 2014:2020){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=30,denw=2,seed=310198,qnt=0.75)
}

###################################################################
# 8
j<-8

# M
for(year in 2012:2014){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=30,denw=10,seed=310198,qnt=0.75)
}
for(year in 2015:2020){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=40,denw=10,seed=310198,qnt=0.75)
}

# F
for(year in 2012:2020){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=50,denw=15,seed=310198,qnt=0.75)
}

###################################################################
# 9
j<-9

# M
for(year in 2012:2015){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=30,denw=5,seed=310198,qnt=0.75)
}

for(year in 2016:2020){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=25,denw=5,seed=310198,qnt=0.75)
}

###################################################################
# 10
j<-10

# M
for(year in c(2012:2013,2018:2020)){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=70,denw=30,seed=310198,qnt=0.75)
}

for(year in 2014:2017){
  runM<-sensitivity_step(year=year,gender="M",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=250,denw=30,seed=310198,qnt=0.75)
}

# F
for(year in 2012:2020){
  runF<-sensitivity_step(year=year,gender="F",data=datalong,
                         disciplines = disciplines,i=j,
                         denM=30,denw=10,seed=310198,qnt=0.75)
}

