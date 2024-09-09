################################################################################
#### Inferring a population composition from survey data with nonignorable #####
####      nonresponse: Borrowing information from external sources         #####
####                                                                       #####
####             Authors: Veronica Ballerini, Brunero Liseo                #####
####                                                                       #####
####   This code reproduces sensitivity results in the Suppl Material      #####
################################################################################

rm(list=ls())
wd <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(wd)

#### Load libraries and functions
library(extraDistr)
library(BiasedUrn)
library(xtable)

#### Load data
load(paste(wd,"/data/finaldata.RData",sep=""))

disciplines<-rownames(nf)

rm(list=setdiff(ls(),list("disciplines","wd")))

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
