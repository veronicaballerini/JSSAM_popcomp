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
agrvet_M<-list()
j<-NULL
j<-cbind(j,2012)
agrvet_M_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  agrvet_M[[i]]<-read.csv(paste(wd,"/output/step3_",j,disciplines[1],
                                gender,".csv",sep=""),header=TRUE)
  agrvet_M[[i]]<-agrvet_M[[i]][,-1]
  agrvet_M_post[i,]<-cbind(apply(agrvet_M[[i]], 2, mean)[1]/
                             as.numeric(M_ASN_agrvet[i,1]),
                           apply(agrvet_M[[i]], 2, hdi)[1,1]/
                             as.numeric(M_ASN_agrvet[i,1]),
                           apply(agrvet_M[[i]], 2, hdi)[2,1]/
                             as.numeric(M_ASN_agrvet[i,1]),
                           apply(agrvet_M[[i]], 2, mean)[2],
                           apply(agrvet_M[[i]], 2, hdi)[1,2],
                           apply(agrvet_M[[i]], 2, hdi)[2,2])
  j<-sum(j,1)
}

empl_rates_agrvet_M<-empl_rates[which(empl_rates$disciplines=="Agricultural and forestry sciences, Veterinary"),c(1,3)]
empl_rates_agrvet_M<-empl_rates_agrvet_M[-1,]

df_M_agrvet<-data.frame(values=rbind(as.matrix(agrvet_M_post[,1]),
                                   as.matrix(empl_rates_agrvet_M[,2])),
                      agrvet_M_post_low=agrvet_M_post[,2],
                      agrvet_M_post_upp=agrvet_M_post[,3],
                      labels=rep(c("Posterior mean",
                                 "Almalaurea value"),each=9),
                      cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/step3 - M_",disciplines[1],".jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_M_agrvet, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=agrvet_M_post_low, ymax=agrvet_M_post_upp),
              fill="grey2", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed"))+
  theme(legend.position = "none", axis.text = element_text(size = 50)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

#F
gender<-"F"
agrvet_F<-list()
j<-NULL
j<-cbind(j,2012)
agrvet_F_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  agrvet_F[[i]]<-read.csv(paste(wd,"/output/step3_",j,disciplines[1],
                                gender,".csv",sep=""),header=TRUE)
  agrvet_F[[i]]<-agrvet_F[[i]][,-1]
  agrvet_F_post[i,]<-cbind(apply(agrvet_F[[i]], 2, mean)[1]/
                             as.numeric(M_ASN_agrvet[i,2]),
                           apply(agrvet_F[[i]], 2, hdi)[1,1]/
                             as.numeric(M_ASN_agrvet[i,2]),
                           apply(agrvet_F[[i]], 2, hdi)[2,1]/
                             as.numeric(M_ASN_agrvet[i,2]),
                           apply(agrvet_F[[i]], 2, mean)[2],
                           apply(agrvet_F[[i]], 2, hdi)[1,2],
                           apply(agrvet_F[[i]], 2, hdi)[2,2])
  j<-sum(j,1)
}

empl_rates_agrvet_F<-empl_rates[which(empl_rates$disciplines=="Agricultural and forestry sciences, Veterinary"),c(1,4)]
empl_rates_agrvet_F<-empl_rates_agrvet_F[-1,]

df_F_agrvet<-data.frame(values=rbind(as.matrix(agrvet_F_post[,1]),
                                   as.matrix(empl_rates_agrvet_F[,2])),
                      agrvet_F_post_low=agrvet_F_post[,2],
                      agrvet_F_post_upp=agrvet_F_post[,3],
                      labels=rep(c("Posterior mean",
                                   "Almalaurea value"),each=9),
                      cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/step3 - F_",disciplines[1],".jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_F_agrvet, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=agrvet_F_post_low, ymax=agrvet_F_post_upp),
              fill="grey2", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed"))+
  theme(legend.position = "none", axis.text = element_text(size = 50)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

### Architecture and Engineering
#M
gender<-"M"
engarch_M<-list()
j<-NULL
j<-cbind(j,2012)
engarch_M_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  engarch_M[[i]]<-read.csv(paste(wd,"/output/step3_",j,disciplines[2],
                                gender,".csv",sep=""),header=TRUE)
  engarch_M[[i]]<-engarch_M[[i]][,-1]
  engarch_M_post[i,]<-cbind(apply(engarch_M[[i]], 2, mean)[1]/
                             as.numeric(M_ASN_engarch[i,1]),
                           apply(engarch_M[[i]], 2, hdi)[1,1]/
                             as.numeric(M_ASN_engarch[i,1]),
                           apply(engarch_M[[i]], 2, hdi)[2,1]/
                             as.numeric(M_ASN_engarch[i,1]),
                           apply(engarch_M[[i]], 2, mean)[2],
                           apply(engarch_M[[i]], 2, hdi)[1,2],
                           apply(engarch_M[[i]], 2, hdi)[2,2])
  j<-sum(j,1)
}

empl_rates_engarch_M<-empl_rates[which(empl_rates$disciplines=="Architecture and Engineering"),c(1,3)]
empl_rates_engarch_M<-empl_rates_engarch_M[-1,]

df_M_engarch<-data.frame(values=rbind(as.matrix(engarch_M_post[,1]),
                                     as.matrix(empl_rates_engarch_M[,2])),
                        engarch_M_post_low=engarch_M_post[,2],
                        engarch_M_post_upp=engarch_M_post[,3],
                        labels=rep(c("Posterior mean",
                                     "Almalaurea value"),each=9),
                        cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/step3 - M_",disciplines[2],".jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_M_engarch, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=engarch_M_post_low, ymax=engarch_M_post_upp),
              fill="grey2", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed"))+
  theme(legend.position = "none", axis.text = element_text(size = 50)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

#F
gender<-"F"
engarch_F<-list()
j<-NULL
j<-cbind(j,2012)
engarch_F_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  engarch_F[[i]]<-read.csv(paste(wd,"/output/step3_",j,disciplines[2],
                                gender,".csv",sep=""),header=TRUE)
  engarch_F[[i]]<-engarch_F[[i]][,-1]
  engarch_F_post[i,]<-cbind(apply(engarch_F[[i]], 2, mean)[1]/
                             as.numeric(M_ASN_engarch[i,2]),
                           apply(engarch_F[[i]], 2, hdi)[1,1]/
                             as.numeric(M_ASN_engarch[i,2]),
                           apply(engarch_F[[i]], 2, hdi)[2,1]/
                             as.numeric(M_ASN_engarch[i,2]),
                           apply(engarch_F[[i]], 2, mean)[2],
                           apply(engarch_F[[i]], 2, hdi)[1,2],
                           apply(engarch_F[[i]], 2, hdi)[2,2])
  j<-sum(j,1)
}

empl_rates_engarch_F<-empl_rates[which(empl_rates$disciplines=="Architecture and Engineering"),c(1,4)]
empl_rates_engarch_F<-empl_rates_engarch_F[-1,]

df_F_engarch<-data.frame(values=rbind(as.matrix(engarch_F_post[,1]),
                                     as.matrix(empl_rates_engarch_F[,2])),
                        engarch_F_post_low=engarch_F_post[,2],
                        engarch_F_post_upp=engarch_F_post[,3],
                        labels=rep(c("Posterior mean",
                                     "Almalaurea value"),each=9),
                        cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/step3 - F_",disciplines[2],".jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_F_engarch, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=engarch_F_post_low, ymax=engarch_F_post_upp),
              fill="grey2", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed"))+
  theme(legend.position = "none", axis.text = element_text(size = 50)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

### B&A, Economics, Finance 
#M
gender<-"M"
eco_M<-list()
j<-NULL
j<-cbind(j,2012)
eco_M_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  eco_M[[i]]<-read.csv(paste(wd,"/output/step3_",j,disciplines[3],
                             gender,".csv",sep=""),header=TRUE)
  eco_M[[i]]<-eco_M[[i]][,-1]
  eco_M_post[i,]<-cbind(apply(eco_M[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_eco[i,1]),
                           apply(eco_M[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_eco[i,1]),
                           apply(eco_M[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_eco[i,1]),
                           apply(eco_M[[i]], 2, mean)[2],
                           apply(eco_M[[i]], 2, hdi)[1,2],
                           apply(eco_M[[i]], 2, hdi)[2,2])
  j<-sum(j,1)
}

empl_rates_eco_M<-empl_rates[which(empl_rates$disciplines=="B&A, Economics, Finance"),c(1,3)]
empl_rates_eco_M<-empl_rates_eco_M[-1,]

df_M_eco<-data.frame(values=rbind(as.matrix(eco_M_post[,1]),
                                   as.matrix(empl_rates_eco_M[,2])),
                      eco_M_post_low=eco_M_post[,2],
                      eco_M_post_upp=eco_M_post[,3],
                      labels=rep(c("Posterior mean",
                                   "Almalaurea value"),each=9),
                      cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/step3 - M_",disciplines[3],".jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_M_eco, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=eco_M_post_low, ymax=eco_M_post_upp),
              fill="grey2", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed"))+
  theme(legend.position = "none", axis.text = element_text(size = 50)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

#F
gender<-"F"
eco_F<-list()
j<-NULL
j<-cbind(j,2012)
eco_F_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  eco_F[[i]]<-read.csv(paste(wd,"/output/step3_",j,disciplines[3],
                             gender,".csv",sep=""),header=TRUE)
  eco_F[[i]]<-eco_F[[i]][,-1]
  eco_F_post[i,]<-cbind(apply(eco_F[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_eco[i,2]),
                        apply(eco_F[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_eco[i,2]),
                        apply(eco_F[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_eco[i,2]),
                        apply(eco_F[[i]], 2, mean)[2],
                        apply(eco_F[[i]], 2, hdi)[1,2],
                        apply(eco_F[[i]], 2, hdi)[2,2])
  j<-sum(j,1)
}

empl_rates_eco_F<-empl_rates[which(empl_rates$disciplines=="B&A, Economics, Finance"),c(1,4)]
empl_rates_eco_F<-empl_rates_eco_F[-1,]

df_F_eco<-data.frame(values=rbind(as.matrix(eco_F_post[,1]),
                                  as.matrix(empl_rates_eco_F[,2])),
                     eco_F_post_low=eco_F_post[,2],
                     eco_F_post_upp=eco_F_post[,3],
                     labels=rep(c("Posterior mean",
                                  "Almalaurea value"),each=9),
                     cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/step3 - F_",disciplines[3],".jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_F_eco, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=eco_F_post_low, ymax=eco_F_post_upp),
              fill="grey2", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed"))+
  theme(legend.position = "none", axis.text = element_text(size = 50)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

### Communication and Publishing
#M
gender<-"M"
comm_M<-list()
j<-NULL
j<-cbind(j,2012)
comm_M_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  comm_M[[i]]<-read.csv(paste(wd,"/output/step3_",j,disciplines[4],
                             gender,".csv",sep=""),header=TRUE)
  comm_M[[i]]<-comm_M[[i]][,-1]
  comm_M_post[i,]<-cbind(apply(comm_M[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_comm[i,1]),
                        apply(comm_M[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_comm[i,1]),
                        apply(comm_M[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_comm[i,1]),
                        apply(comm_M[[i]], 2, mean)[2],
                        apply(comm_M[[i]], 2, hdi)[1,2],
                        apply(comm_M[[i]], 2, hdi)[2,2])
  j<-sum(j,1)
}

empl_rates_comm_M<-empl_rates[which(empl_rates$disciplines=="Communication and Publishing"),c(1,3)]
empl_rates_comm_M<-empl_rates_comm_M[-1,]

df_M_comm<-data.frame(values=rbind(as.matrix(comm_M_post[,1]),
                                  as.matrix(empl_rates_comm_M[,2])),
                     comm_M_post_low=comm_M_post[,2],
                     comm_M_post_upp=comm_M_post[,3],
                     labels=rep(c("Posterior mean",
                                  "Almalaurea value"),each=9),
                     cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/step3 - M_",disciplines[4],".jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_M_comm, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  # theme(legend.title = element_blank(),
  #       legend.position = c(0.8, 0.2))+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=comm_M_post_low, ymax=comm_M_post_upp),
              fill="grey2", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed"))+
  theme(legend.position = "none", axis.text = element_text(size = 50)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

#F
gender<-"F"
comm_F<-list()
j<-NULL
j<-cbind(j,2012)
comm_F_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  comm_F[[i]]<-read.csv(paste(wd,"/output/step3_",j,disciplines[4],
                             gender,".csv",sep=""),header=TRUE)
  comm_F[[i]]<-comm_F[[i]][,-1]
  comm_F_post[i,]<-cbind(apply(comm_F[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_comm[i,2]),
                        apply(comm_F[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_comm[i,2]),
                        apply(comm_F[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_comm[i,2]),
                        apply(comm_F[[i]], 2, mean)[2],
                        apply(comm_F[[i]], 2, hdi)[1,2],
                        apply(comm_F[[i]], 2, hdi)[2,2])
  j<-sum(j,1)
}

empl_rates_comm_F<-empl_rates[which(empl_rates$disciplines=="Communication and Publishing"),c(1,4)]
empl_rates_comm_F<-empl_rates_comm_F[-1,]

df_F_comm<-data.frame(values=rbind(as.matrix(comm_F_post[,1]),
                                  as.matrix(empl_rates_comm_F[,2])),
                     comm_F_post_low=comm_F_post[,2],
                     comm_F_post_upp=comm_F_post[,3],
                     labels=rep(c("Posterior mean",
                                  "Almalaurea value"),each=9),
                     cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/step3 - F_",disciplines[4],".jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_F_comm, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=comm_F_post_low, ymax=comm_F_post_upp),
              fill="grey2", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed"))+
  theme(legend.position = "none", axis.text = element_text(size = 50)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

### Industrial and Information Engineering
#M
gender<-"M"
indeng_M<-list()
j<-NULL
j<-cbind(j,2012)
indeng_M_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  indeng_M[[i]]<-read.csv(paste(wd,"/output/step3_",j,disciplines[5],
                             gender,".csv",sep=""),header=TRUE)
  indeng_M[[i]]<-indeng_M[[i]][,-1]
  indeng_M_post[i,]<-cbind(apply(indeng_M[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_indeng[i,1]),
                        apply(indeng_M[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_indeng[i,1]),
                        apply(indeng_M[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_indeng[i,1]),
                        apply(indeng_M[[i]], 2, mean)[2],
                        apply(indeng_M[[i]], 2, hdi)[1,2],
                        apply(indeng_M[[i]], 2, hdi)[2,2])
  j<-sum(j,1)
}

empl_rates_indeng_M<-empl_rates[which(empl_rates$disciplines=="Industrial Engineering"),c(1,3)]
empl_rates_indeng_M<-empl_rates_indeng_M[-1,]

df_M_indeng<-data.frame(values=rbind(as.matrix(indeng_M_post[,1]),
                                  as.matrix(empl_rates_indeng_M[,2])),
                     indeng_M_post_low=indeng_M_post[,2],
                     indeng_M_post_upp=indeng_M_post[,3],
                     labels=rep(c("Posterior mean",
                                  "Almalaurea value"),each=9),
                     cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/step3 - M_",disciplines[5],".jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_M_indeng, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=indeng_M_post_low, ymax=indeng_M_post_upp),
              fill="grey2", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed"))+
  theme(legend.position = "none", axis.text = element_text(size = 50)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

#F
gender<-"F"
indeng_F<-list()
j<-NULL
j<-cbind(j,2012)
indeng_F_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  indeng_F[[i]]<-read.csv(paste(wd,"/output/step3_",j,disciplines[5],
                             gender,".csv",sep=""),header=TRUE)
  indeng_F[[i]]<-indeng_F[[i]][,-1]
  indeng_F_post[i,]<-cbind(apply(indeng_F[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_indeng[i,2]),
                        apply(indeng_F[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_indeng[i,2]),
                        apply(indeng_F[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_indeng[i,2]),
                        apply(indeng_F[[i]], 2, mean)[2],
                        apply(indeng_F[[i]], 2, hdi)[1,2],
                        apply(indeng_F[[i]], 2, hdi)[2,2])
  j<-sum(j,1)
}

empl_rates_indeng_F<-empl_rates[which(empl_rates$disciplines=="Industrial Engineering"),c(1,4)]
empl_rates_indeng_F<-empl_rates_indeng_F[-1,]

df_F_indeng<-data.frame(values=rbind(as.matrix(indeng_F_post[,1]),
                                  as.matrix(empl_rates_indeng_F[,2])),
                     indeng_F_post_low=indeng_F_post[,2],
                     indeng_F_post_upp=indeng_F_post[,3],
                     labels=rep(c("Posterior mean",
                                  "Almalaurea value"),each=9),
                     cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/step3 - F_",disciplines[5],".jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_F_indeng, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=indeng_F_post_low, ymax=indeng_F_post_upp),
              fill="grey2", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed"))+
  theme(legend.position = "none", axis.text = element_text(size = 50)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

### Law and Legal sciences 
#M
gender<-"M"
law_M<-list()
j<-NULL
j<-cbind(j,2012)
law_M_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  law_M[[i]]<-read.csv(paste(wd,"/output/step3_",j,disciplines[6],
                             gender,".csv",sep=""),header=TRUE)
  law_M[[i]]<-law_M[[i]][,-1]
  law_M_post[i,]<-cbind(apply(law_M[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_law[i,1]),
                        apply(law_M[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_law[i,1]),
                        apply(law_M[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_law[i,1]),
                        apply(law_M[[i]], 2, mean)[2],
                        apply(law_M[[i]], 2, hdi)[1,2],
                        apply(law_M[[i]], 2, hdi)[2,2])
  j<-sum(j,1)
}

empl_rates_law_M<-empl_rates[which(empl_rates$disciplines=="Law and Legal sciences"),c(1,3)]
empl_rates_law_M<-empl_rates_law_M[-1,]

df_M_law<-data.frame(values=rbind(as.matrix(law_M_post[,1]),
                                  as.matrix(empl_rates_law_M[,2])),
                     law_M_post_low=law_M_post[,2],
                     law_M_post_upp=law_M_post[,3],
                     labels=rep(c("Posterior mean",
                                  "Almalaurea value"),each=9),
                     cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/step3 - M_",disciplines[6],".jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_M_law, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=law_M_post_low, ymax=law_M_post_upp),
              fill="grey2", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed"))+
  theme(legend.position = "none", axis.text = element_text(size = 50)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

#F
gender<-"F"
law_F<-list()
j<-NULL
j<-cbind(j,2012)
law_F_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  law_F[[i]]<-read.csv(paste(wd,"/output/step3_",j,disciplines[6],
                             gender,".csv",sep=""),header=TRUE)
  law_F[[i]]<-law_F[[i]][,-1]
  law_F_post[i,]<-cbind(apply(law_F[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_law[i,2]),
                        apply(law_F[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_law[i,2]),
                        apply(law_F[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_law[i,2]),
                        apply(law_F[[i]], 2, mean)[2],
                        apply(law_F[[i]], 2, hdi)[1,2],
                        apply(law_F[[i]], 2, hdi)[2,2])
  j<-sum(j,1)
}

empl_rates_law_F<-empl_rates[which(empl_rates$disciplines=="Law and Legal sciences"),c(1,4)]
empl_rates_law_F<-empl_rates_law_F[-1,]

df_F_law<-data.frame(values=rbind(as.matrix(law_F_post[,1]),
                                  as.matrix(empl_rates_law_F[,2])),
                     law_F_post_low=law_F_post[,2],
                     law_F_post_upp=law_F_post[,3],
                     labels=rep(c("Posterior mean",
                                  "Almalaurea value"),each=9),
                     cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/step3 - F_",disciplines[6],".jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_F_law, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=law_F_post_low, ymax=law_F_post_upp),
              fill="grey2", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed"))+
  theme(legend.position = "none", axis.text = element_text(size = 50)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

### Literature 
#M
gender<-"M"
lit_M<-list()
j<-NULL
j<-cbind(j,2012)
lit_M_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  lit_M[[i]]<-read.csv(paste(wd,"/output/step3_",j,disciplines[7],
                             gender,".csv",sep=""),header=TRUE)
  lit_M[[i]]<-lit_M[[i]][,-1]
  lit_M_post[i,]<-cbind(apply(lit_M[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_hum[i,1]),
                        apply(lit_M[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_hum[i,1]),
                        apply(lit_M[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_hum[i,1]),
                        apply(lit_M[[i]], 2, mean)[2],
                        apply(lit_M[[i]], 2, hdi)[1,2],
                        apply(lit_M[[i]], 2, hdi)[2,2])
  j<-sum(j,1)
}

empl_rates_lit_M<-empl_rates[which(empl_rates$disciplines=="Literature and Humanities"),c(1,3)]
empl_rates_lit_M<-empl_rates_lit_M[-1,]

df_M_lit<-data.frame(values=rbind(as.matrix(lit_M_post[,1]),
                                  as.matrix(empl_rates_lit_M[,2])),
                     lit_M_post_low=lit_M_post[,2],
                     lit_M_post_upp=lit_M_post[,3],
                     labels=rep(c("Posterior mean",
                                  "Almalaurea value"),each=9),
                     cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/step3 - M_",disciplines[7],".jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_M_lit, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=lit_M_post_low, ymax=lit_M_post_upp),
              fill="grey2", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed"))+
  theme(legend.position = "none", axis.text = element_text(size = 50)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

#F
gender<-"F"
lit_F<-list()
j<-NULL
j<-cbind(j,2012)
lit_F_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  lit_F[[i]]<-read.csv(paste(wd,"/output/step3_",j,disciplines[7],
                             gender,".csv",sep=""),header=TRUE)
  lit_F[[i]]<-lit_F[[i]][,-1]
  lit_F_post[i,]<-cbind(apply(lit_F[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_hum[i,2]),
                        apply(lit_F[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_hum[i,2]),
                        apply(lit_F[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_hum[i,2]),
                        apply(lit_F[[i]], 2, mean)[2],
                        apply(lit_F[[i]], 2, hdi)[1,2],
                        apply(lit_F[[i]], 2, hdi)[2,2])
  j<-sum(j,1)
}
colnames(lit_F_post)<-c("er_post_mean","er_low","er_upp",
                        "w_post_mean","w_low","w_upp")

empl_rates_lit_F<-empl_rates[which(empl_rates$disciplines=="Literature and Humanities"),c(1,4)]
empl_rates_lit_F<-empl_rates_lit_F[-1,]


df_F_lit<-data.frame(values=rbind(as.matrix(lit_F_post[,1]),
                                  as.matrix(empl_rates_lit_F[,2])),
                     lit_F_post_low=lit_F_post[,2],
                     lit_F_post_upp=lit_F_post[,3],
                     labels=rep(c("Posterior mean",
                                  "Almalaurea value"),each=9),
                     cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/step3 - F_",disciplines[7],".jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_F_lit, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=lit_F_post_low, ymax=lit_F_post_upp),
              fill="grey2", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed"))+
  theme(legend.position = "none", axis.text = element_text(size = 50)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

### Medicine, Dentistry, Pharmacy 
#M
gender<-"M"
med_M<-list()
j<-NULL
j<-cbind(j,2012)
med_M_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  med_M[[i]]<-read.csv(paste(wd,"/output/step3_",j,disciplines[8],
                             gender,".csv",sep=""),header=TRUE)
  med_M[[i]]<-med_M[[i]][,-1]
  med_M_post[i,]<-cbind(apply(med_M[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_med[i,1]),
                        apply(med_M[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_med[i,1]),
                        apply(med_M[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_med[i,1]),
                        apply(med_M[[i]], 2, mean)[2],
                        apply(med_M[[i]], 2, hdi)[1,2],
                        apply(med_M[[i]], 2, hdi)[2,2])
  j<-sum(j,1)
}

empl_rates_med_M<-empl_rates[which(empl_rates$disciplines=="Medicine, Dentistry, Pharmacy"),c(1,3)]
empl_rates_med_M<-empl_rates_med_M[-1,]


df_M_med<-data.frame(values=rbind(as.matrix(med_M_post[,1]),
                                  as.matrix(empl_rates_med_M[,2])),
                     med_M_post_low=med_M_post[,2],
                     med_M_post_upp=med_M_post[,3],
                     labels=rep(c("Posterior mean",
                                  "Almalaurea value"),each=9),
                     cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/step3 - M_",disciplines[8],".jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_M_med, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=med_M_post_low, ymax=med_M_post_upp),
              fill="grey2", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed"))+
  theme(legend.position = "none", axis.text = element_text(size = 50)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

#F
gender<-"F"
med_F<-list()
j<-NULL
j<-cbind(j,2012)
med_F_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  med_F[[i]]<-read.csv(paste(wd,"/output/step3_",j,disciplines[8],
                             gender,".csv",sep=""),header=TRUE)
  med_F[[i]]<-med_F[[i]][,-1]
  med_F_post[i,]<-cbind(apply(med_F[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_med[i,2]),
                        apply(med_F[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_med[i,2]),
                        apply(med_F[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_med[i,2]),
                        apply(med_F[[i]], 2, mean)[2],
                        apply(med_F[[i]], 2, hdi)[1,2],
                        apply(med_F[[i]], 2, hdi)[2,2])
  j<-sum(j,1)
}

empl_rates_med_F<-empl_rates[which(empl_rates$disciplines=="Medicine, Dentistry, Pharmacy"),c(1,4)]
empl_rates_med_F<-empl_rates_med_F[-1,]

df_F_med<-data.frame(values=rbind(as.matrix(med_F_post[,1]),
                                  as.matrix(empl_rates_med_F[,2])),
                     med_F_post_low=med_F_post[,2],
                     med_F_post_upp=med_F_post[,3],
                     labels=rep(c("Posterior mean",
                                  "Almalaurea value"),each=9),
                     cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/step3 - F_",disciplines[8],".jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_F_med, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=med_F_post_low, ymax=med_F_post_upp),
              fill="grey2", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed"))+
  theme(legend.position = "none", axis.text = element_text(size = 50)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

### Political Science
#M
gender<-"M"
pol_M<-list()
j<-NULL
j<-cbind(j,2012)
pol_M_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  pol_M[[i]]<-read.csv(paste(wd,"/output/step3_",j,disciplines[9],
                             gender,".csv",sep=""),header=TRUE)
  pol_M[[i]]<-pol_M[[i]][,-1]
  pol_M_post[i,]<-cbind(apply(pol_M[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_pol[i,1]),
                        apply(pol_M[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_pol[i,1]),
                        apply(pol_M[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_pol[i,1]),
                        apply(pol_M[[i]], 2, mean)[2],
                        apply(pol_M[[i]], 2, hdi)[1,2],
                        apply(pol_M[[i]], 2, hdi)[2,2])
  j<-sum(j,1)
}

empl_rates_pol_M<-empl_rates[which(empl_rates$disciplines=="Political Science"),c(1,3)]
empl_rates_pol_M<-empl_rates_pol_M[-1,]


df_M_pol<-data.frame(values=rbind(as.matrix(pol_M_post[,1]),
                                  as.matrix(empl_rates_pol_M[,2])),
                     pol_M_post_low=pol_M_post[,2],
                     pol_M_post_upp=pol_M_post[,3],
                     labels=rep(c("Posterior mean",
                                  "Almalaurea value"),each=9),
                     cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/step3 - M_",disciplines[9],".jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_M_pol, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=pol_M_post_low, ymax=pol_M_post_upp),
              fill="grey2", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed"))+
  theme(legend.position = "none", axis.text = element_text(size = 50)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

#F
gender<-"F"
pol_F<-list()
j<-NULL
j<-cbind(j,2012)
pol_F_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  pol_F[[i]]<-read.csv(paste(wd,"/output/step3_",j,disciplines[9],
                             gender,".csv",sep=""),header=TRUE)
  pol_F[[i]]<-pol_F[[i]][,-1]
  pol_F_post[i,]<-cbind(apply(pol_F[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_pol[i,2]),
                        apply(pol_F[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_pol[i,2]),
                        apply(pol_F[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_pol[i,2]),
                        apply(pol_F[[i]], 2, mean)[2],
                        apply(pol_F[[i]], 2, hdi)[1,2],
                        apply(pol_F[[i]], 2, hdi)[2,2])
  j<-sum(j,1)
}

empl_rates_pol_F<-empl_rates[which(empl_rates$disciplines=="Political Science"),c(1,4)]
empl_rates_pol_F<-empl_rates_pol_F[-1,]

df_F_pol<-data.frame(values=rbind(as.matrix(pol_F_post[,1]),
                                  as.matrix(empl_rates_pol_F[,2])),
                     pol_F_post_low=pol_F_post[,2],
                     pol_F_post_upp=pol_F_post[,3],
                     labels=rep(c("Posterior mean",
                                  "Almalaurea value"),each=9),
                     cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/step3 - F_",disciplines[9],".jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_F_pol, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=pol_F_post_low, ymax=pol_F_post_upp),
              fill="grey2", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed"))+
  theme(legend.position = "none", axis.text = element_text(size = 50)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

### Science and IT
#M
gender<-"M"
sci_M<-list()
j<-NULL
j<-cbind(j,2012)
sci_M_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  sci_M[[i]]<-read.csv(paste(wd,"/output/step3_",j,disciplines[10],
                             gender,".csv",sep=""),header=TRUE)
  sci_M[[i]]<-sci_M[[i]][,-1]
  sci_M_post[i,]<-cbind(apply(sci_M[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_sci[i,1]),
                        apply(sci_M[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_sci[i,1]),
                        apply(sci_M[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_sci[i,1]),
                        apply(sci_M[[i]], 2, mean)[2],
                        apply(sci_M[[i]], 2, hdi)[1,2],
                        apply(sci_M[[i]], 2, hdi)[2,2])
  j<-sum(j,1)
}

empl_rates_sci_M<-empl_rates[which(empl_rates$disciplines=="Science and IT"),c(1,3)]
empl_rates_sci_M<-empl_rates_sci_M[-1,]


df_M_sci<-data.frame(values=rbind(as.matrix(sci_M_post[,1]),
                                  as.matrix(empl_rates_sci_M[,2])),
                     sci_M_post_low=sci_M_post[,2],
                     sci_M_post_upp=sci_M_post[,3],
                     labels=rep(c("Posterior mean",
                                  "Almalaurea value"),each=9),
                     cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/step3 - M_",disciplines[10],".jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_M_sci, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=sci_M_post_low, ymax=sci_M_post_upp),
              fill="grey2", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed"))+
  theme(legend.position = "none", axis.text = element_text(size = 50)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()

#F
gender<-"F"
sci_F<-list()
j<-NULL
j<-cbind(j,2012)
sci_F_post<-matrix(NA,nrow=9,ncol=6)
for(i in 1:9){
  sci_F[[i]]<-read.csv(paste(wd,"/output/step3_",j,disciplines[10],
                             gender,".csv",sep=""),header=TRUE)
  sci_F[[i]]<-sci_F[[i]][,-1]
  sci_F_post[i,]<-cbind(apply(sci_F[[i]], 2, mean)[1]/
                          as.numeric(M_ASN_sci[i,2]),
                        apply(sci_F[[i]], 2, hdi)[1,1]/
                          as.numeric(M_ASN_sci[i,2]),
                        apply(sci_F[[i]], 2, hdi)[2,1]/
                          as.numeric(M_ASN_sci[i,2]),
                        apply(sci_F[[i]], 2, mean)[2],
                        apply(sci_F[[i]], 2, hdi)[1,2],
                        apply(sci_F[[i]], 2, hdi)[2,2])
  j<-sum(j,1)
}

empl_rates_sci_F<-empl_rates[which(empl_rates$disciplines=="Science and IT"),c(1,4)]
empl_rates_sci_F<-empl_rates_sci_F[-1,]

df_F_sci<-data.frame(values=rbind(as.matrix(sci_F_post[,1]),
                                  as.matrix(empl_rates_sci_F[,2])),
                     sci_F_post_low=sci_F_post[,2],
                     sci_F_post_upp=sci_F_post[,3],
                     labels=rep(c("Posterior mean",
                                  "Almalaurea value"),each=9),
                     cohort=seq(2012,2020))

jpeg(file=paste(wd,"/figures/step3 - F_",disciplines[10],".jpeg",sep=""),width = 1600, height = 1200)
ggplot(df_F_sci, aes(x=cohort, y=values))+ 
  geom_line(aes(linetype=labels),size=1) +
  theme_light()+
  labs(x = "", y = "") +
  geom_ribbon(aes(ymin=sci_F_post_low, ymax=sci_F_post_upp),
              fill="grey2", alpha=0.2)+
  scale_linetype_manual(values=c("solid",
                                 "dashed"))+
  theme(legend.position = "none", axis.text = element_text(size = 50)) +
  coord_cartesian(ylim=c(.15, .8),xlim=c(2012,2020))
dev.off()
