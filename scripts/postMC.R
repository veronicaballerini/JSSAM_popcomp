################################################################################
#### Inferring a population composition from survey data with nonignorable #####
####      nonresponse: Borrowing information from external sources         #####
####                                                                       #####
####             Authors: Veronica Ballerini, Brunero Liseo                #####
####                                                                       #####
####                               Post MC                                #####
################################################################################

rm(list=ls())
wd <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(wd)

#### Load libraries and functions
library(extraDistr)
library(BiasedUrn)
library(HDInterval)
source(paste(wd,"/functions/impute_MC.R",sep=""))

#### Load data
# "Communication and Publishing", i=4
comm_F_2014<-read.csv(paste(wd,"/output/step3_2014Communication and PublishingF.csv",sep=""))
comm_F_2015<-read.csv(paste(wd,"/output/step3_2015Communication and PublishingF.csv",sep=""))
comm_F_2016<-read.csv(paste(wd,"/output/step3_2016Communication and PublishingF.csv",sep=""))

# "Industrial Engineering", i=5
indeng_M_2012<-read.csv(paste(wd,"/output/step3_2012Industrial EngineeringM.csv",sep=""))
indeng_M_2013<-read.csv(paste(wd,"/output/step3_2013Industrial EngineeringM.csv",sep=""))
indeng_M_2014<-read.csv(paste(wd,"/output/step3_2014Industrial EngineeringM.csv",sep=""))
indeng_M_2015<-read.csv(paste(wd,"/output/step3_2015Industrial EngineeringM.csv",sep=""))
indeng_M_2016<-read.csv(paste(wd,"/output/step3_2016Industrial EngineeringM.csv",sep=""))
indeng_M_2017<-read.csv(paste(wd,"/output/step3_2017Industrial EngineeringM.csv",sep=""))
indeng_M_2018<-read.csv(paste(wd,"/output/step3_2018Industrial EngineeringM.csv",sep=""))
indeng_M_2019<-read.csv(paste(wd,"/output/step3_2019Industrial EngineeringM.csv",sep=""))
indeng_M_2020<-read.csv(paste(wd,"/output/step3_2020Industrial EngineeringM.csv",sep=""))

# "Political Science", i=9
polsci_F_2012<-read.csv(paste(wd,"/output/step3_2012Political ScienceF.csv",sep=""))
polsci_F_2013<-read.csv(paste(wd,"/output/step3_2013Political ScienceF.csv",sep=""))
polsci_F_2014<-read.csv(paste(wd,"/output/step3_2014Political ScienceF.csv",sep=""))
polsci_F_2015<-read.csv(paste(wd,"/output/step3_2015Political ScienceF.csv",sep=""))
polsci_F_2016<-read.csv(paste(wd,"/output/step3_2016Political ScienceF.csv",sep=""))
polsci_F_2017<-read.csv(paste(wd,"/output/step3_2017Political ScienceF.csv",sep=""))
polsci_F_2018<-read.csv(paste(wd,"/output/step3_2018Political ScienceF.csv",sep=""))
polsci_F_2019<-read.csv(paste(wd,"/output/step3_2019Political ScienceF.csv",sep=""))
polsci_F_2020<-read.csv(paste(wd,"/output/step3_2020Political ScienceF.csv",sep=""))

datalong<-read.csv(paste(wd,"/data/data_almalaurea.csv",sep=""),
                   header=TRUE,sep=";")

# MC for unreliable results
comm_F_2014$Mthin<-impute_M_MC(datalong,comm_F_2014$wthin,"Communication and Publishing",4,year=2014)
comm_F_2015$Mthin<-impute_M_MC(datalong,comm_F_2015$wthin,"Communication and Publishing",4,year=2015)
comm_F_2016$Mthin<-impute_M_MC(datalong,comm_F_2016$wthin,"Communication and Publishing",4,year=2016)

indeng_M_2012$Mthin<-impute_M_MC(datalong,indeng_M_2012$wthin,"Industrial Engineering",5,gender="M",year=2012)
indeng_M_2013$Mthin<-impute_M_MC(datalong,indeng_M_2013$wthin,"Industrial Engineering",5,gender="M",year=2013)
indeng_M_2014$Mthin<-impute_M_MC(datalong,indeng_M_2014$wthin,"Industrial Engineering",5,gender="M",year=2014)
indeng_M_2015$Mthin<-impute_M_MC(datalong,indeng_M_2015$wthin,"Industrial Engineering",5,gender="M",year=2015)
indeng_M_2016$Mthin<-impute_M_MC(datalong,indeng_M_2016$wthin,"Industrial Engineering",5,gender="M",year=2016)
indeng_M_2017$Mthin<-impute_M_MC(datalong,indeng_M_2017$wthin,"Industrial Engineering",5,gender="M",year=2017)
indeng_M_2018$Mthin<-impute_M_MC(datalong,indeng_M_2018$wthin,"Industrial Engineering",5,gender="M",year=2018)
indeng_M_2019$Mthin<-impute_M_MC(datalong,indeng_M_2019$wthin,"Industrial Engineering",5,gender="M",year=2019)
indeng_M_2020$Mthin<-impute_M_MC(datalong,indeng_M_2020$wthin,"Industrial Engineering",5,gender="M",year=2020)

polsci_F_2012$Mthin<-impute_M_MC(datalong,polsci_F_2012$wthin,"Political Science",9,year=2012)
polsci_F_2013$Mthin<-impute_M_MC(datalong,polsci_F_2013$wthin,"Political Science",9,year=2013)
polsci_F_2014$Mthin<-impute_M_MC(datalong,polsci_F_2014$wthin,"Political Science",9,year=2014)
polsci_F_2015$Mthin<-impute_M_MC(datalong,polsci_F_2015$wthin,"Political Science",9,year=2015)
polsci_F_2016$Mthin<-impute_M_MC(datalong,polsci_F_2016$wthin,"Political Science",9,year=2016)
polsci_F_2017$Mthin<-impute_M_MC(datalong,polsci_F_2017$wthin,"Political Science",9,year=2017)
polsci_F_2018$Mthin<-impute_M_MC(datalong,polsci_F_2018$wthin,"Political Science",9,year=2018)
polsci_F_2019$Mthin<-impute_M_MC(datalong,polsci_F_2019$wthin,"Political Science",9,year=2019)
polsci_F_2020$Mthin<-impute_M_MC(datalong,polsci_F_2020$wthin,"Political Science",9,year=2020)
