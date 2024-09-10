################################################################################
#### Inferring a population composition from survey data with nonignorable #####
####      nonresponse: Borrowing information from external sources         #####
####                                                                       #####
####             Authors: Veronica Ballerini, Brunero Liseo                #####
####                                                                       #####
### This code reproduces sensitivity results (step 3) in the Suppl Material ####
################################################################################

rm(list=ls())
wd <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(wd)

#### Load libraries and functions
library(ragg)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(HDInterval)

#### Load data
load(paste(wd,"/data/finaldata.RData",sep=""))
disciplines<-rownames(nf)

rm(list=setdiff(ls(),list("wd","disciplines")))

M_ASN<-list()
M_ASN_agrvet<-NULL
M_ASN_engarch<-NULL
M_ASN_eco<-NULL
M_ASN_comm<-NULL
M_ASN_indeng<-NULL
M_ASN_law<-NULL
M_ASN_hum<-NULL
M_ASN_med<-NULL
M_ASN_pol<-NULL
M_ASN_sci<-NULL

i<-NULL
for(year in 2012:2020){
  i<-sum(i,1)
  M_ASN[[i]]<-matrix(read.table(paste(wd,"/data/M_ms",year,".txt",sep="")),
                     nrow=length(disciplines),ncol=2,byrow=TRUE)
  M_ASN_agrvet<-rbind(M_ASN_agrvet,M_ASN[[i]][1,])
  M_ASN_engarch<-rbind(M_ASN_engarch,M_ASN[[i]][2,])
  M_ASN_eco<-rbind(M_ASN_eco,M_ASN[[i]][3,])
  M_ASN_comm<-rbind(M_ASN_comm,M_ASN[[i]][4,])
  M_ASN_indeng<-rbind(M_ASN_indeng,M_ASN[[i]][5,])
  M_ASN_law<-rbind(M_ASN_law,M_ASN[[i]][6,])
  M_ASN_hum<-rbind(M_ASN_hum,M_ASN[[i]][7,])
  M_ASN_med<-rbind(M_ASN_med,M_ASN[[i]][8,])
  M_ASN_pol<-rbind(M_ASN_pol,M_ASN[[i]][9,])
  M_ASN_sci<-rbind(M_ASN_sci,M_ASN[[i]][10,])
}

empl_rates<-read.csv(paste(wd,"/data/employment_rates_almalaurea.csv",sep=""),
                     header=TRUE,sep=";")

### Agricultural and Forestry sciences, Veterinary 
#M
gender<-"M"
agrvet_M1<-list()
agrvet_M3<-list()
j<-NULL
j<-cbind(j,2012)
agrvet_M_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  agrvet_M1[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[1],
                                gender,"0.25.csv",sep=""),header=TRUE)
  agrvet_M1[[i]]<-agrvet_M1[[i]][,-1]
  agrvet_M3[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[1],
                                 gender,"0.75.csv",sep=""),header=TRUE)
  agrvet_M3[[i]]<-agrvet_M3[[i]][,-1]
  agrvet_M_post[i,]<-cbind(apply(agrvet_M1[[i]], 2, mean)[1]/
                             as.numeric(M_ASN_agrvet[i,1]),
                           apply(agrvet_M1[[i]], 2, hdi)[1,1]/
                             as.numeric(M_ASN_agrvet[i,1]),
                           apply(agrvet_M1[[i]], 2, hdi)[2,1]/
                             as.numeric(M_ASN_agrvet[i,1]),
                           apply(agrvet_M3[[i]], 2, mean)[1]/
                             as.numeric(M_ASN_agrvet[i,1]),
                           apply(agrvet_M3[[i]], 2, hdi)[1,1]/
                             as.numeric(M_ASN_agrvet[i,1]),
                           apply(agrvet_M3[[i]], 2, hdi)[2,1]/
                             as.numeric(M_ASN_agrvet[i,1]))
  j<-sum(j,1)
}

empl_rates_agrvet_M<-empl_rates[which(empl_rates$disciplines=="Agricultural and forestry sciences, Veterinary"),c(1,3)]
empl_rates_agrvet_M<-empl_rates_agrvet_M[-1,]

df_M_agrvet<-data.frame(values=as.matrix(empl_rates_agrvet_M[,2]),
                        agrvet_M_post_low25=agrvet_M_post[,2],
                        agrvet_M_post_upp25=agrvet_M_post[,3],
                        agrvet_M_post_low75=agrvet_M_post[,5],
                        agrvet_M_post_upp75=agrvet_M_post[,6],
                        labels=rep(c("Posterior mean - 1st quartile",
                                     "Posterior mean - 3rd quartile",
                                     "Almalaurea value"),each=9),
                        cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/qsens_",disciplines[1],"M.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_M_agrvet, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=agrvet_M_post_low25, ymax=agrvet_M_post_upp25),
              fill="deeppink", alpha=0.2)+
  geom_ribbon(aes(ymin=agrvet_M_post_low75, ymax=agrvet_M_post_upp75),
              fill="blue", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed",
                                 "solid"))+
  theme(legend.position = "none", axis.text = element_text(size = 20)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

#F
gender<-"F"
agrvet_F1<-list()
agrvet_F3<-list()
j<-NULL
j<-cbind(j,2012)
agrvet_F_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  agrvet_F1[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[1],
                                 gender,"0.25.csv",sep=""),header=TRUE)
  agrvet_F1[[i]]<-agrvet_F1[[i]][,-1]
  agrvet_F3[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[1],
                                 gender,"0.75.csv",sep=""),header=TRUE)
  agrvet_F3[[i]]<-agrvet_F3[[i]][,-1]
  agrvet_F_post[i,]<-cbind(apply(agrvet_F1[[i]], 2, mean)[1]/
                             as.numeric(M_ASN_agrvet[i,2]),
                           apply(agrvet_F1[[i]], 2, hdi)[1,1]/
                             as.numeric(M_ASN_agrvet[i,2]),
                           apply(agrvet_F1[[i]], 2, hdi)[2,1]/
                             as.numeric(M_ASN_agrvet[i,2]),
                           apply(agrvet_F3[[i]], 2, mean)[1]/
                             as.numeric(M_ASN_agrvet[i,2]),
                           apply(agrvet_F3[[i]], 2, hdi)[1,1]/
                             as.numeric(M_ASN_agrvet[i,2]),
                           apply(agrvet_F3[[i]], 2, hdi)[2,1]/
                             as.numeric(M_ASN_agrvet[i,2]))
  j<-sum(j,1)
}

empl_rates_agrvet_F<-empl_rates[which(empl_rates$disciplines=="Agricultural and forestry sciences, Veterinary"),c(1,4)]
empl_rates_agrvet_F<-empl_rates_agrvet_F[-1,]

df_F_agrvet<-data.frame(values=as.matrix(empl_rates_agrvet_F[,2]),
                        agrvet_F_post_low25=agrvet_F_post[,2],
                        agrvet_F_post_upp25=agrvet_F_post[,3],
                        agrvet_F_post_low75=agrvet_F_post[,5],
                        agrvet_F_post_upp75=agrvet_F_post[,6],
                        labels=rep(c("Posterior mean - 1st quartile",
                                     "Posterior mean - 3rd quartile",
                                     "Almalaurea value"),each=9),
                        cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/qsens_",disciplines[1],"F.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_F_agrvet, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=agrvet_F_post_low25, ymax=agrvet_F_post_upp25),
              fill="deeppink", alpha=0.2)+
  geom_ribbon(aes(ymin=agrvet_F_post_low75, ymax=agrvet_F_post_upp75),
              fill="blue", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed",
                                 "solid"))+
  theme(legend.position = "none", axis.text = element_text(size = 20)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

### Architecture and Engineering
#M
gender<-"M"
engarch_M1<-list()
engarch_M3<-list()
j<-NULL
j<-cbind(j,2012)
engarch_M_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  engarch_M1[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[2],
                                 gender,"0.25.csv",sep=""),header=TRUE)
  engarch_M1[[i]]<-engarch_M1[[i]][,-1]
  engarch_M3[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[2],
                                 gender,"0.75.csv",sep=""),header=TRUE)
  engarch_M3[[i]]<-engarch_M3[[i]][,-1]
  engarch_M_post[i,]<-cbind(apply(engarch_M1[[i]], 2, mean)[1]/
                             as.numeric(M_ASN_engarch[i,1]),
                           apply(engarch_M1[[i]], 2, hdi)[1,1]/
                             as.numeric(M_ASN_engarch[i,1]),
                           apply(engarch_M1[[i]], 2, hdi)[2,1]/
                             as.numeric(M_ASN_engarch[i,1]),
                           apply(engarch_M3[[i]], 2, mean)[1]/
                             as.numeric(M_ASN_engarch[i,1]),
                           apply(engarch_M3[[i]], 2, hdi)[1,1]/
                             as.numeric(M_ASN_engarch[i,1]),
                           apply(engarch_M3[[i]], 2, hdi)[2,1]/
                             as.numeric(M_ASN_engarch[i,1]))
  j<-sum(j,1)
}

empl_rates_engarch_M<-empl_rates[which(empl_rates$disciplines=="Architecture and Engineering"),c(1,3)]
empl_rates_engarch_M<-empl_rates_engarch_M[-1,]

df_M_engarch<-data.frame(values=as.matrix(empl_rates_engarch_M[,2]),
                        engarch_M_post_low25=engarch_M_post[,2],
                        engarch_M_post_upp25=engarch_M_post[,3],
                        engarch_M_post_low75=engarch_M_post[,5],
                        engarch_M_post_upp75=engarch_M_post[,6],
                        labels=rep(c("Posterior mean - 1st quartile",
                                     "Posterior mean - 3rd quartile",
                                     "Almalaurea value"),each=9),
                        cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/qsens_",disciplines[2],"M.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_M_engarch, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=engarch_M_post_low25, ymax=engarch_M_post_upp25),
              fill="deeppink", alpha=0.2)+
  geom_ribbon(aes(ymin=engarch_M_post_low75, ymax=engarch_M_post_upp75),
              fill="blue", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed",
                                 "solid"))+
  theme(legend.position = "none", axis.text = element_text(size = 20)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

#F
gender<-"F"
engarch_F1<-list()
engarch_F3<-list()
j<-NULL
j<-cbind(j,2012)
engarch_F_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  engarch_F1[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[2],
                                 gender,"0.25.csv",sep=""),header=TRUE)
  engarch_F1[[i]]<-engarch_F1[[i]][,-1]
  engarch_F3[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[2],
                                 gender,"0.75.csv",sep=""),header=TRUE)
  engarch_F3[[i]]<-engarch_F3[[i]][,-1]
  engarch_F_post[i,]<-cbind(apply(engarch_F1[[i]], 2, mean)[1]/
                             as.numeric(M_ASN_engarch[i,2]),
                           apply(engarch_F1[[i]], 2, hdi)[1,1]/
                             as.numeric(M_ASN_engarch[i,2]),
                           apply(engarch_F1[[i]], 2, hdi)[2,1]/
                             as.numeric(M_ASN_engarch[i,2]),
                           apply(engarch_F3[[i]], 2, mean)[1]/
                             as.numeric(M_ASN_engarch[i,2]),
                           apply(engarch_F3[[i]], 2, hdi)[1,1]/
                             as.numeric(M_ASN_engarch[i,2]),
                           apply(engarch_F3[[i]], 2, hdi)[2,1]/
                             as.numeric(M_ASN_engarch[i,2]))
  j<-sum(j,1)
}

empl_rates_engarch_F<-empl_rates[which(empl_rates$disciplines=="Agricultural and forestry sciences, Veterinary"),c(1,4)]
empl_rates_engarch_F<-empl_rates_engarch_F[-1,]

df_F_engarch<-data.frame(values=as.matrix(empl_rates_engarch_F[,2]),
                        engarch_F_post_low25=engarch_F_post[,2],
                        engarch_F_post_upp25=engarch_F_post[,3],
                        engarch_F_post_low75=engarch_F_post[,5],
                        engarch_F_post_upp75=engarch_F_post[,6],
                        labels=rep(c("Posterior mean - 1st quartile",
                                     "Posterior mean - 3rd quartile",
                                     "Almalaurea value"),each=9),
                        cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/qsens_",disciplines[2],"F.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_F_engarch, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=engarch_F_post_low25, ymax=engarch_F_post_upp25),
              fill="deeppink", alpha=0.2)+
  geom_ribbon(aes(ymin=engarch_F_post_low75, ymax=engarch_F_post_upp75),
              fill="blue", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed",
                                 "solid"))+
  theme(legend.position = "none", axis.text = element_text(size = 20)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

### B&A, Economics, Finance
#M
gender<-"M"
eco_M1<-list()
eco_M3<-list()
j<-NULL
j<-cbind(j,2012)
eco_M_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  eco_M1[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[3],
                                 gender,"0.25.csv",sep=""),header=TRUE)
  eco_M1[[i]]<-eco_M1[[i]][,-1]
  eco_M3[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[3],
                                 gender,"0.75.csv",sep=""),header=TRUE)
  eco_M3[[i]]<-eco_M3[[i]][,-1]
  eco_M_post[i,]<-cbind(apply(eco_M1[[i]], 2, mean)[1]/
                             as.numeric(M_ASN_eco[i,1]),
                           apply(eco_M1[[i]], 2, hdi)[1,1]/
                             as.numeric(M_ASN_eco[i,1]),
                           apply(eco_M1[[i]], 2, hdi)[2,1]/
                             as.numeric(M_ASN_eco[i,1]),
                           apply(eco_M3[[i]], 2, mean)[1]/
                             as.numeric(M_ASN_eco[i,1]),
                           apply(eco_M3[[i]], 2, hdi)[1,1]/
                             as.numeric(M_ASN_eco[i,1]),
                           apply(eco_M3[[i]], 2, hdi)[2,1]/
                             as.numeric(M_ASN_eco[i,1]))
  j<-sum(j,1)
}

empl_rates_eco_M<-empl_rates[which(empl_rates$disciplines=="B&A, Economics, Finance"),c(1,3)]
empl_rates_eco_M<-empl_rates_eco_M[-1,]

df_M_eco<-data.frame(values=as.matrix(empl_rates_eco_M[,2]),
                        eco_M_post_low25=eco_M_post[,2],
                        eco_M_post_upp25=eco_M_post[,3],
                        eco_M_post_low75=eco_M_post[,5],
                        eco_M_post_upp75=eco_M_post[,6],
                        labels=rep(c("Posterior mean - 1st quartile",
                                     "Posterior mean - 3rd quartile",
                                     "Almalaurea value"),each=9),
                        cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/qsens_",disciplines[3],"M.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_M_eco, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=eco_M_post_low25, ymax=eco_M_post_upp25),
              fill="deeppink", alpha=0.2)+
  geom_ribbon(aes(ymin=eco_M_post_low75, ymax=eco_M_post_upp75),
              fill="blue", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed",
                                 "solid"))+
  theme(legend.position = "none", axis.text = element_text(size = 20)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

#F
gender<-"F"
eco_F1<-list()
eco_F3<-list()
j<-NULL
j<-cbind(j,2012)
eco_F_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  eco_F1[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[3],
                                 gender,"0.25.csv",sep=""),header=TRUE)
  eco_F1[[i]]<-eco_F1[[i]][,-1]
  eco_F3[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[3],
                                 gender,"0.75.csv",sep=""),header=TRUE)
  eco_F3[[i]]<-eco_F3[[i]][,-1]
  eco_F_post[i,]<-cbind(apply(eco_F1[[i]], 2, mean)[1]/
                             as.numeric(M_ASN_eco[i,2]),
                           apply(eco_F1[[i]], 2, hdi)[1,1]/
                             as.numeric(M_ASN_eco[i,2]),
                           apply(eco_F1[[i]], 2, hdi)[2,1]/
                             as.numeric(M_ASN_eco[i,2]),
                           apply(eco_F3[[i]], 2, mean)[1]/
                             as.numeric(M_ASN_eco[i,2]),
                           apply(eco_F3[[i]], 2, hdi)[1,1]/
                             as.numeric(M_ASN_eco[i,2]),
                           apply(eco_F3[[i]], 2, hdi)[2,1]/
                             as.numeric(M_ASN_eco[i,2]))
  j<-sum(j,1)
}

empl_rates_eco_F<-empl_rates[which(empl_rates$disciplines=="B&A, Economics, Finance"),c(1,4)]
empl_rates_eco_F<-empl_rates_eco_F[-1,]

df_F_eco<-data.frame(values=as.matrix(empl_rates_eco_F[,2]),
                        eco_F_post_low25=eco_F_post[,2],
                        eco_F_post_upp25=eco_F_post[,3],
                        eco_F_post_low75=eco_F_post[,5],
                        eco_F_post_upp75=eco_F_post[,6],
                        labels=rep(c("Posterior mean - 1st quartile",
                                     "Posterior mean - 3rd quartile",
                                     "Almalaurea value"),each=9),
                        cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/qsens_",disciplines[3],"F.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_F_eco, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=eco_F_post_low25, ymax=eco_F_post_upp25),
              fill="deeppink", alpha=0.2)+
  geom_ribbon(aes(ymin=eco_F_post_low75, ymax=eco_F_post_upp75),
              fill="blue", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed",
                                 "solid"))+
  theme(legend.position = "none", axis.text = element_text(size = 20)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

### Communication and Publishing
#M
gender<-"M"
comm_M1<-list()
comm_M3<-list()
j<-NULL
j<-cbind(j,2012)
comm_M_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  comm_M1[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[4],
                                 gender,"0.25.csv",sep=""),header=TRUE)
  comm_M1[[i]]<-comm_M1[[i]][,-1]
  comm_M3[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[4],
                                 gender,"0.75.csv",sep=""),header=TRUE)
  comm_M3[[i]]<-comm_M3[[i]][,-1]
  comm_M_post[i,]<-cbind(apply(comm_M1[[i]], 2, mean)[1]/
                             as.numeric(M_ASN_comm[i,1]),
                           apply(comm_M1[[i]], 2, hdi)[1,1]/
                             as.numeric(M_ASN_comm[i,1]),
                           apply(comm_M1[[i]], 2, hdi)[2,1]/
                             as.numeric(M_ASN_comm[i,1]),
                           apply(comm_M3[[i]], 2, mean)[1]/
                             as.numeric(M_ASN_comm[i,1]),
                           apply(comm_M3[[i]], 2, hdi)[1,1]/
                             as.numeric(M_ASN_comm[i,1]),
                           apply(comm_M3[[i]], 2, hdi)[2,1]/
                             as.numeric(M_ASN_comm[i,1]))
  j<-sum(j,1)
}

empl_rates_comm_M<-empl_rates[which(empl_rates$disciplines=="Communication and Publishing"),c(1,3)]
empl_rates_comm_M<-empl_rates_comm_M[-1,]

df_M_comm<-data.frame(values=as.matrix(empl_rates_comm_M[,2]),
                        comm_M_post_low25=comm_M_post[,2],
                        comm_M_post_upp25=comm_M_post[,3],
                        comm_M_post_low75=comm_M_post[,5],
                        comm_M_post_upp75=comm_M_post[,6],
                        labels=rep(c("Posterior mean - 1st quartile",
                                     "Posterior mean - 3rd quartile",
                                     "Almalaurea value"),each=9),
                        cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/qsens_",disciplines[4],"M.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_M_comm, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=comm_M_post_low25, ymax=comm_M_post_upp25),
              fill="deeppink", alpha=0.2)+
  geom_ribbon(aes(ymin=comm_M_post_low75, ymax=comm_M_post_upp75),
              fill="blue", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed",
                                 "solid"))+
  theme(legend.position = "none", axis.text = element_text(size = 20)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

### Industrial Engineering

#F
gender<-"F"
indeng_F1<-list()
indeng_F3<-list()
j<-NULL
j<-cbind(j,2012)
indeng_F_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  indeng_F1[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[5],
                               gender,"0.25.csv",sep=""),header=TRUE)
  indeng_F1[[i]]<-indeng_F1[[i]][,-1]
  indeng_F3[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[5],
                               gender,"0.75.csv",sep=""),header=TRUE)
  indeng_F3[[i]]<-indeng_F3[[i]][,-1]
  indeng_F_post[i,]<-cbind(apply(indeng_F1[[i]], 2, mean)[1]/
                           as.numeric(M_ASN_indeng[i,2]),
                         apply(indeng_F1[[i]], 2, hdi)[1,1]/
                           as.numeric(M_ASN_indeng[i,2]),
                         apply(indeng_F1[[i]], 2, hdi)[2,1]/
                           as.numeric(M_ASN_indeng[i,2]),
                         apply(indeng_F3[[i]], 2, mean)[1]/
                           as.numeric(M_ASN_indeng[i,2]),
                         apply(indeng_F3[[i]], 2, hdi)[1,1]/
                           as.numeric(M_ASN_indeng[i,2]),
                         apply(indeng_F3[[i]], 2, hdi)[2,1]/
                           as.numeric(M_ASN_indeng[i,2]))
  j<-sum(j,1)
}

empl_rates_indeng_F<-empl_rates[which(empl_rates$disciplines=="Industrial Engineering"),c(1,4)]
empl_rates_indeng_F<-empl_rates_indeng_F[-1,]

df_F_indeng<-data.frame(values=as.matrix(empl_rates_indeng_F[,2]),
                      indeng_F_post_low25=indeng_F_post[,2],
                      indeng_F_post_upp25=indeng_F_post[,3],
                      indeng_F_post_low75=indeng_F_post[,5],
                      indeng_F_post_upp75=indeng_F_post[,6],
                      labels=rep(c("Posterior mean - 1st quartile",
                                   "Posterior mean - 3rd quartile",
                                   "Almalaurea value"),each=9),
                      cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/qsens_",disciplines[5],"F.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_F_indeng, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=indeng_F_post_low25, ymax=indeng_F_post_upp25),
              fill="deeppink", alpha=0.2)+
  geom_ribbon(aes(ymin=indeng_F_post_low75, ymax=indeng_F_post_upp75),
              fill="blue", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed",
                                 "solid"))+
  theme(legend.position = "none", axis.text = element_text(size = 20)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

### Law and Legal sciences
#M
gender<-"M"
law_M1<-list()
law_M3<-list()
j<-NULL
j<-cbind(j,2012)
law_M_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  law_M1[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[6],
                                 gender,"0.25.csv",sep=""),header=TRUE)
  law_M1[[i]]<-law_M1[[i]][,-1]
  law_M3[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[6],
                                 gender,"0.75.csv",sep=""),header=TRUE)
  law_M3[[i]]<-law_M3[[i]][,-1]
  law_M_post[i,]<-cbind(apply(law_M1[[i]], 2, mean)[1]/
                             as.numeric(M_ASN_law[i,1]),
                           apply(law_M1[[i]], 2, hdi)[1,1]/
                             as.numeric(M_ASN_law[i,1]),
                           apply(law_M1[[i]], 2, hdi)[2,1]/
                             as.numeric(M_ASN_law[i,1]),
                           apply(law_M3[[i]], 2, mean)[1]/
                             as.numeric(M_ASN_law[i,1]),
                           apply(law_M3[[i]], 2, hdi)[1,1]/
                             as.numeric(M_ASN_law[i,1]),
                           apply(law_M3[[i]], 2, hdi)[2,1]/
                             as.numeric(M_ASN_law[i,1]))
  j<-sum(j,1)
}

empl_rates_law_M<-empl_rates[which(empl_rates$disciplines=="Law and Legal sciences"),c(1,3)]
empl_rates_law_M<-empl_rates_law_M[-1,]

df_M_law<-data.frame(values=as.matrix(empl_rates_law_M[,2]),
                        law_M_post_low25=law_M_post[,2],
                        law_M_post_upp25=law_M_post[,3],
                        law_M_post_low75=law_M_post[,5],
                        law_M_post_upp75=law_M_post[,6],
                        labels=rep(c("Posterior mean - 1st quartile",
                                     "Posterior mean - 3rd quartile",
                                     "Almalaurea value"),each=9),
                        cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/qsens_",disciplines[6],"M.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_M_law, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=law_M_post_low25, ymax=law_M_post_upp25),
              fill="deeppink", alpha=0.2)+
  geom_ribbon(aes(ymin=law_M_post_low75, ymax=law_M_post_upp75),
              fill="blue", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed",
                                 "solid"))+
  theme(legend.position = "none", axis.text = element_text(size = 20)) +
  coord_cartesian(ylim=c(.15, .80),xlim=c(2012,2020))
dev.off()

#F
gender<-"F"
law_F1<-list()
law_F3<-list()
j<-NULL
j<-cbind(j,2012)
law_F_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  law_F1[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[6],
                                 gender,"0.25.csv",sep=""),header=TRUE)
  law_F1[[i]]<-law_F1[[i]][,-1]
  law_F3[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[6],
                                 gender,"0.75.csv",sep=""),header=TRUE)
  law_F3[[i]]<-law_F3[[i]][,-1]
  law_F_post[i,]<-cbind(apply(law_F1[[i]], 2, mean)[1]/
                             as.numeric(M_ASN_law[i,2]),
                           apply(law_F1[[i]], 2, hdi)[1,1]/
                             as.numeric(M_ASN_law[i,2]),
                           apply(law_F1[[i]], 2, hdi)[2,1]/
                             as.numeric(M_ASN_law[i,2]),
                           apply(law_F3[[i]], 2, mean)[1]/
                             as.numeric(M_ASN_law[i,2]),
                           apply(law_F3[[i]], 2, hdi)[1,1]/
                             as.numeric(M_ASN_law[i,2]),
                           apply(law_F3[[i]], 2, hdi)[2,1]/
                             as.numeric(M_ASN_law[i,2]))
  j<-sum(j,1)
}

empl_rates_law_F<-empl_rates[which(empl_rates$disciplines=="Law and Legal sciences"),c(1,4)]
empl_rates_law_F<-empl_rates_law_F[-1,]

df_F_law<-data.frame(values=as.matrix(empl_rates_law_F[,2]),
                        law_F_post_low25=law_F_post[,2],
                        law_F_post_upp25=law_F_post[,3],
                        law_F_post_low75=law_F_post[,5],
                        law_F_post_upp75=law_F_post[,6],
                        labels=rep(c("Posterior mean - 1st quartile",
                                     "Posterior mean - 3rd quartile",
                                     "Almalaurea value"),each=9),
                        cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/qsens_",disciplines[6],"F.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_F_law, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=law_F_post_low25, ymax=law_F_post_upp25),
              fill="deeppink", alpha=0.2)+
  geom_ribbon(aes(ymin=law_F_post_low75, ymax=law_F_post_upp75),
              fill="blue", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed",
                                 "solid"))+
  theme(legend.position = "none", axis.text = element_text(size = 20)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

### Literature and Humanities
#M
gender<-"M"
hum_M1<-list()
hum_M3<-list()
j<-NULL
j<-cbind(j,2012)
hum_M_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  hum_M1[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[7],
                              gender,"0.25.csv",sep=""),header=TRUE)
  hum_M1[[i]]<-hum_M1[[i]][,-1]
  hum_M3[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[7],
                              gender,"0.75.csv",sep=""),header=TRUE)
  hum_M3[[i]]<-hum_M3[[i]][,-1]
  hum_M_post[i,]<-cbind(apply(hum_M1[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_hum[i,1]),
                        apply(hum_M1[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_hum[i,1]),
                        apply(hum_M1[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_hum[i,1]),
                        apply(hum_M3[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_hum[i,1]),
                        apply(hum_M3[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_hum[i,1]),
                        apply(hum_M3[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_hum[i,1]))
  j<-sum(j,1)
}

empl_rates_hum_M<-empl_rates[which(empl_rates$disciplines=="Literature and Humanities"),c(1,3)]
empl_rates_hum_M<-empl_rates_hum_M[-1,]

df_M_hum<-data.frame(values=as.matrix(empl_rates_hum_M[,2]),
                     hum_M_post_low25=hum_M_post[,2],
                     hum_M_post_upp25=hum_M_post[,3],
                     hum_M_post_low75=hum_M_post[,5],
                     hum_M_post_upp75=hum_M_post[,6],
                     labels=rep(c("Posterior mean - 1st quartile",
                                  "Posterior mean - 3rd quartile",
                                  "Almalaurea value"),each=9),
                     cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/qsens_",disciplines[7],"M.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_M_hum, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=hum_M_post_low25, ymax=hum_M_post_upp25),
              fill="deeppink", alpha=0.2)+
  geom_ribbon(aes(ymin=hum_M_post_low75, ymax=hum_M_post_upp75),
              fill="blue", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed",
                                 "solid"))+
  theme(legend.position = "none", axis.text = element_text(size = 20)) +
  coord_cartesian(ylim=c(.15, .80),xlim=c(2012,2020))
dev.off()

#F
gender<-"F"
hum_F1<-list()
hum_F3<-list()
j<-NULL
j<-cbind(j,2012)
hum_F_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  hum_F1[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[7],
                              gender,"0.25.csv",sep=""),header=TRUE)
  hum_F1[[i]]<-hum_F1[[i]][,-1]
  hum_F3[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[7],
                              gender,"0.75.csv",sep=""),header=TRUE)
  hum_F3[[i]]<-hum_F3[[i]][,-1]
  hum_F_post[i,]<-cbind(apply(hum_F1[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_hum[i,2]),
                        apply(hum_F1[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_hum[i,2]),
                        apply(hum_F1[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_hum[i,2]),
                        apply(hum_F3[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_hum[i,2]),
                        apply(hum_F3[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_hum[i,2]),
                        apply(hum_F3[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_hum[i,2]))
  j<-sum(j,1)
}

empl_rates_hum_F<-empl_rates[which(empl_rates$disciplines=="Literature and Humanities"),c(1,4)]
empl_rates_hum_F<-empl_rates_hum_F[-1,]

df_F_hum<-data.frame(values=as.matrix(empl_rates_hum_F[,2]),
                     hum_F_post_low25=hum_F_post[,2],
                     hum_F_post_upp25=hum_F_post[,3],
                     hum_F_post_low75=hum_F_post[,5],
                     hum_F_post_upp75=hum_F_post[,6],
                     labels=rep(c("Posterior mean - 1st quartile",
                                  "Posterior mean - 3rd quartile",
                                  "Almalaurea value"),each=9),
                     cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/qsens_",disciplines[7],"F.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_F_hum, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=hum_F_post_low25, ymax=hum_F_post_upp25),
              fill="deeppink", alpha=0.2)+
  geom_ribbon(aes(ymin=hum_F_post_low75, ymax=hum_F_post_upp75),
              fill="blue", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed",
                                 "solid"))+
  theme(legend.position = "none", axis.text = element_text(size = 20)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

### Medicine, Dentistry, Pharmacy
#M
gender<-"M"
med_M1<-list()
med_M3<-list()
j<-NULL
j<-cbind(j,2012)
med_M_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  med_M1[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[8],
                              gender,"0.25.csv",sep=""),header=TRUE)
  med_M1[[i]]<-med_M1[[i]][,-1]
  med_M3[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[8],
                              gender,"0.75.csv",sep=""),header=TRUE)
  med_M3[[i]]<-med_M3[[i]][,-1]
  med_M_post[i,]<-cbind(apply(med_M1[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_med[i,1]),
                        apply(med_M1[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_med[i,1]),
                        apply(med_M1[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_med[i,1]),
                        apply(med_M3[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_med[i,1]),
                        apply(med_M3[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_med[i,1]),
                        apply(med_M3[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_med[i,1]))
  j<-sum(j,1)
}

empl_rates_med_M<-empl_rates[which(empl_rates$disciplines=="Medicine, Dentistry, Pharmacy"),c(1,3)]
empl_rates_med_M<-empl_rates_med_M[-1,]

df_M_med<-data.frame(values=as.matrix(empl_rates_med_M[,2]),
                     med_M_post_low25=med_M_post[,2],
                     med_M_post_upp25=med_M_post[,3],
                     med_M_post_low75=med_M_post[,5],
                     med_M_post_upp75=med_M_post[,6],
                     labels=rep(c("Posterior mean - 1st quartile",
                                  "Posterior mean - 3rd quartile",
                                  "Almalaurea value"),each=9),
                     cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/qsens_",disciplines[8],"M.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_M_med, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=med_M_post_low25, ymax=med_M_post_upp25),
              fill="deeppink", alpha=0.2)+
  geom_ribbon(aes(ymin=med_M_post_low75, ymax=med_M_post_upp75),
              fill="blue", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed",
                                 "solid"))+
  theme(legend.position = "none", axis.text = element_text(size = 20)) +
  coord_cartesian(ylim=c(.15, .80),xlim=c(2012,2020))
dev.off()

#F
gender<-"F"
med_F1<-list()
med_F3<-list()
j<-NULL
j<-cbind(j,2012)
med_F_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  med_F1[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[8],
                              gender,"0.25.csv",sep=""),header=TRUE)
  med_F1[[i]]<-med_F1[[i]][,-1]
  med_F3[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[8],
                              gender,"0.75.csv",sep=""),header=TRUE)
  med_F3[[i]]<-med_F3[[i]][,-1]
  med_F_post[i,]<-cbind(apply(med_F1[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_med[i,2]),
                        apply(med_F1[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_med[i,2]),
                        apply(med_F1[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_med[i,2]),
                        apply(med_F3[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_med[i,2]),
                        apply(med_F3[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_med[i,2]),
                        apply(med_F3[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_med[i,2]))
  j<-sum(j,1)
}

empl_rates_med_F<-empl_rates[which(empl_rates$disciplines=="Medicine, Dentistry, Pharmacy"),c(1,4)]
empl_rates_med_F<-empl_rates_med_F[-1,]

df_F_med<-data.frame(values=as.matrix(empl_rates_med_F[,2]),
                     med_F_post_low25=med_F_post[,2],
                     med_F_post_upp25=med_F_post[,3],
                     med_F_post_low75=med_F_post[,5],
                     med_F_post_upp75=med_F_post[,6],
                     labels=rep(c("Posterior mean - 1st quartile",
                                  "Posterior mean - 3rd quartile",
                                  "Almalaurea value"),each=9),
                     cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/qsens_",disciplines[8],"F.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_F_med, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=med_F_post_low25, ymax=med_F_post_upp25),
              fill="deeppink", alpha=0.2)+
  geom_ribbon(aes(ymin=med_F_post_low75, ymax=med_F_post_upp75),
              fill="blue", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed",
                                 "solid"))+
  theme(legend.position = "none", axis.text = element_text(size = 20)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

### Political Science
#M
gender<-"M"
pol_M1<-list()
pol_M3<-list()
j<-NULL
j<-cbind(j,2012)
pol_M_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  pol_M1[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[9],
                              gender,"0.25.csv",sep=""),header=TRUE)
  pol_M1[[i]]<-pol_M1[[i]][,-1]
  pol_M3[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[9],
                              gender,"0.75.csv",sep=""),header=TRUE)
  pol_M3[[i]]<-pol_M3[[i]][,-1]
  pol_M_post[i,]<-cbind(apply(pol_M1[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_pol[i,1]),
                        apply(pol_M1[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_pol[i,1]),
                        apply(pol_M1[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_pol[i,1]),
                        apply(pol_M3[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_pol[i,1]),
                        apply(pol_M3[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_pol[i,1]),
                        apply(pol_M3[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_pol[i,1]))
  j<-sum(j,1)
}

empl_rates_pol_M<-empl_rates[which(empl_rates$disciplines=="Political Science"),c(1,3)]
empl_rates_pol_M<-empl_rates_pol_M[-1,]

df_M_pol<-data.frame(values=as.matrix(empl_rates_pol_M[,2]),
                     pol_M_post_low25=pol_M_post[,2],
                     pol_M_post_upp25=pol_M_post[,3],
                     pol_M_post_low75=pol_M_post[,5],
                     pol_M_post_upp75=pol_M_post[,6],
                     labels=rep(c("Posterior mean - 1st quartile",
                                  "Posterior mean - 3rd quartile",
                                  "Almalaurea value"),each=9),
                     cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/qsens_",disciplines[9],"M.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_M_pol, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=pol_M_post_low25, ymax=pol_M_post_upp25),
              fill="deeppink", alpha=0.2)+
  geom_ribbon(aes(ymin=pol_M_post_low75, ymax=pol_M_post_upp75),
              fill="blue", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed",
                                 "solid"))+
  theme(legend.position = "none", axis.text = element_text(size = 20)) +
  coord_cartesian(ylim=c(.15, .80),xlim=c(2012,2020))
dev.off()

### Science and IT
#M
gender<-"M"
sci_M1<-list()
sci_M3<-list()
j<-NULL
j<-cbind(j,2012)
sci_M_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  sci_M1[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[10],
                              gender,"0.25.csv",sep=""),header=TRUE)
  sci_M1[[i]]<-sci_M1[[i]][,-1]
  sci_M3[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[10],
                              gender,"0.75.csv",sep=""),header=TRUE)
  sci_M3[[i]]<-sci_M3[[i]][,-1]
  sci_M_post[i,]<-cbind(apply(sci_M1[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_sci[i,1]),
                        apply(sci_M1[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_sci[i,1]),
                        apply(sci_M1[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_sci[i,1]),
                        apply(sci_M3[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_sci[i,1]),
                        apply(sci_M3[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_sci[i,1]),
                        apply(sci_M3[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_sci[i,1]))
  j<-sum(j,1)
}

empl_rates_sci_M<-empl_rates[which(empl_rates$disciplines=="Science and IT"),c(1,3)]
empl_rates_sci_M<-empl_rates_sci_M[-1,]

df_M_sci<-data.frame(values=as.matrix(empl_rates_sci_M[,2]),
                     sci_M_post_low25=sci_M_post[,2],
                     sci_M_post_upp25=sci_M_post[,3],
                     sci_M_post_low75=sci_M_post[,5],
                     sci_M_post_upp75=sci_M_post[,6],
                     labels=rep(c("Posterior mean - 1st quartile",
                                  "Posterior mean - 3rd quartile",
                                  "Almalaurea value"),each=9),
                     cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/qsens_",disciplines[10],"M.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_M_sci, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=sci_M_post_low25, ymax=sci_M_post_upp25),
              fill="deeppink", alpha=0.2)+
  geom_ribbon(aes(ymin=sci_M_post_low75, ymax=sci_M_post_upp75),
              fill="blue", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed",
                                 "solid"))+
  theme(legend.position = "none", axis.text = element_text(size = 20)) +
  coord_cartesian(ylim=c(.15, .80),xlim=c(2012,2020))
dev.off()

#F
gender<-"F"
sci_F1<-list()
sci_F3<-list()
j<-NULL
j<-cbind(j,2012)
sci_F_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  sci_F1[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[10],
                              gender,"0.25.csv",sep=""),header=TRUE)
  sci_F1[[i]]<-sci_F1[[i]][,-1]
  sci_F3[[i]]<-read.csv(paste(wd,"/output/sensitivity_",j,disciplines[10],
                              gender,"0.75.csv",sep=""),header=TRUE)
  sci_F3[[i]]<-sci_F3[[i]][,-1]
  sci_F_post[i,]<-cbind(apply(sci_F1[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_sci[i,2]),
                        apply(sci_F1[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_sci[i,2]),
                        apply(sci_F1[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_sci[i,2]),
                        apply(sci_F3[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_sci[i,2]),
                        apply(sci_F3[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_sci[i,2]),
                        apply(sci_F3[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_sci[i,2]))
  j<-sum(j,1)
}

empl_rates_sci_F<-empl_rates[which(empl_rates$disciplines=="Science and IT"),c(1,4)]
empl_rates_sci_F<-empl_rates_sci_F[-1,]

df_F_sci<-data.frame(values=as.matrix(empl_rates_sci_F[,2]),
                     sci_F_post_low25=sci_F_post[,2],
                     sci_F_post_upp25=sci_F_post[,3],
                     sci_F_post_low75=sci_F_post[,5],
                     sci_F_post_upp75=sci_F_post[,6],
                     labels=rep(c("Posterior mean - 1st quartile",
                                  "Posterior mean - 3rd quartile",
                                  "Almalaurea value"),each=9),
                     cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/qsens_",disciplines[10],"F.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_F_sci, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=sci_F_post_low25, ymax=sci_F_post_upp25),
              fill="deeppink", alpha=0.2)+
  geom_ribbon(aes(ymin=sci_F_post_low75, ymax=sci_F_post_upp75),
              fill="blue", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed",
                                 "solid"))+
  theme(legend.position = "none", axis.text = element_text(size = 20)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()
