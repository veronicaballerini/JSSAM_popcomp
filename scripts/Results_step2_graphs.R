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
library(ggplot2)
library(dplyr)
library(RColorBrewer)

#### Load data
load(paste(wd,"/data/finaldata.RData",sep=""))
disciplines<-rownames(nf)

rm(list=setdiff(ls(),list("wd","disciplines")))

w2_M<-list()
for(i in 1:length(disciplines)){
  w2_M[[i]]<-read.csv(paste(wd,"/output/step2_",disciplines[i],"Msdw1.csv",sep=""))
  w2_M[[i]]<-w2_M[[i]][,-1]
  jpeg(file=paste(wd,"/extra_figures/trace_step2",disciplines[i],"Mw.jpeg",sep=""),width = 1600, height = 1200)
  print(plot(w2_M[[i]][,3],type="l",main=paste(disciplines[i],"- M"),ylab="w"))
  dev.off()
}

w2_F<-list()
for(i in 1:length(disciplines)){
  w2_F[[i]]<-read.csv(paste(wd,"/output/step2_",disciplines[i],"Fsdw1.csv",sep=""))
  w2_F[[i]]<-w2_F[[i]][,-1]
  jpeg(file=paste(wd,"/extra_figures/trace_step2",disciplines[i],"Fw.jpeg",sep=""),width = 1600, height = 1200)
  print(plot(w2_F[[i]][,3],type="l"))
  dev.off()
}

# 1
df.w_agrvet<-data.frame(value=rbind(as.matrix(w2_F[[1]][,3]),
                                    as.matrix(w2_M[[1]][,3])),
                        labels=rep(c("Females","Males"),each=length(w2_F[[1]][,3])))

jpeg(file=paste(wd,"/figures/step2",disciplines[1],".jpeg",sep=""),width = 1600, height = 1200)
print(ggplot(df.w_agrvet,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 50)) +
  labs(title = "", x = "", y=""))
dev.off()

# 2 
df.w_engarch<-data.frame(value=rbind(as.matrix(w2_F[[2]][,3]),
                                    as.matrix(w2_M[[2]][,3])),
                        labels=rep(c("Females","Males"),each=length(w2_F[[2]][,3])))

jpeg(file=paste(wd,"/figures/step2",disciplines[2],".jpeg",sep=""),width = 1600, height = 1200)
ggplot(df.w_engarch,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 50)) +
  labs(title = "", x = "", y="")
dev.off()

# 3
df.w_eco<-data.frame(value=rbind(as.matrix(w2_F[[3]][,3]),
                                    as.matrix(w2_M[[3]][,3])),
                        labels=rep(c("Females","Males"),each=length(w2_F[[3]][,3])))

jpeg(file=paste(wd,"/figures/step2",disciplines[3],".jpeg",sep=""),width = 1600, height = 1200)
print(ggplot(df.w_eco,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 50)) +
  labs(title = "", x = "", y=""))
dev.off()

# 4
df.w_comm<-data.frame(value=rbind(as.matrix(w2_F[[4]][,3]),
                                    as.matrix(w2_M[[4]][,3])),
                        labels=rep(c("Females","Males"),each=length(w2_F[[4]][,3])))

jpeg(file=paste(wd,"/figures/step2",disciplines[4],".jpeg",sep=""),width = 1600, height = 1200)
print(ggplot(df.w_comm,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 50)) +
  labs(title = "", x = "", y=""))
dev.off()

# 5
df.w_indeng<-data.frame(value=rbind(as.matrix(w2_F[[5]][,3]),
                                    as.matrix(w2_M[[5]][,3])),
                        labels=rep(c("Females","Males"),each=length(w2_F[[5]][,3])))

jpeg(file=paste(wd,"/figures/step2",disciplines[5],".jpeg",sep=""),width = 1600, height = 1200)
print(ggplot(df.w_indeng,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 50)) +
  labs(title = "", x = "", y=""))
dev.off()

# 6
df.w_law<-data.frame(value=rbind(as.matrix(w2_F[[6]][,3]),
                                    as.matrix(w2_M[[6]][,3])),
                        labels=rep(c("Females","Males"),
                                   each=length(w2_F[[6]][,3])))

jpeg(file=paste(wd,"/figures/step2",disciplines[6],".jpeg",sep=""),width = 1600, height = 1200)
print(ggplot(df.w_law,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 50)) +
  labs(title = "", x = "", y=""))
dev.off()

# 7
df.w_lit<-data.frame(value=rbind(as.matrix(w2_F[[7]][,3]),
                                    as.matrix(w2_M[[7]][,3])),
                        labels=rep(c("Females","Males"),
                                   each=length(w2_F[[7]][,3])))

jpeg(file=paste(wd,"/figures/step2",disciplines[7],".jpeg",sep=""),width = 1600, height = 1200)
print(ggplot(df.w_lit,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 50)) +
  labs(title = "", x = "", y=""))
dev.off()

# 8
df.w_med<-data.frame(value=rbind(as.matrix(w2_F[[8]][,3]),
                                    as.matrix(w2_M[[8]][,3])),
                        labels=rep(c("Females","Males"),
                                   each=length(w2_F[[8]][,3])))

jpeg(file=paste(wd,"/figures/step2",disciplines[8],".jpeg",sep=""),width = 1600, height = 1200)
print(ggplot(df.w_med,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 50)) +
  labs(title = "", x = "", y=""))
dev.off()

# 9
df.w_pol<-data.frame(value=rbind(as.matrix(w2_F[[9]][,3]),
                                    as.matrix(w2_M[[9]][,3])),
                        labels=rep(c("Females","Males"),
                                   each=length(w2_F[[9]][,3])))

jpeg(file=paste(wd,"/figures/step2",disciplines[9],".jpeg",sep=""),width = 1600, height = 1200)
print(ggplot(df.w_pol,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 50)) +
  labs(title = "", x = "", y=""))
dev.off()

# 10
df.w_sci<-data.frame(value=rbind(as.matrix(w2_F[[10]][,3]),
                                    as.matrix(w2_M[[10]][,3])),
                        labels=rep(c("Females","Males"),
                                   each=length(w2_F[[10]][,3])))

jpeg(file=paste(wd,"/figures/step2",disciplines[10],".jpeg",sep=""),width = 1600, height = 1200)
print(ggplot(df.w_sci,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 50)) +
  labs(title = "", x = "", y=""))
dev.off()
