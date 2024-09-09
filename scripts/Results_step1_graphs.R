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
library(ggplot2)
library(dplyr)
library(RColorBrewer)

#### Load data
load(paste(wd,"/data/finaldata.RData",sep=""))
disciplines<-rownames(nf)

rm(list=setdiff(ls(),list("wd","disciplines")))

#### Graphs Step 1
N1_M<-list()
post_mean_N_M<-NULL
post_mean_M_M<-NULL

par(mfrow=c(1,1))
for(i in 1:length(disciplines)){
  N1_M[[i]]<-read.csv(paste(wd,"/output/step1_",disciplines[i],"M.csv",sep=""))
  N1_M[[i]]<-N1_M[[i]][,-1]
  jpeg(file=paste(wd,"/extra_figures/trace_step1",disciplines[i],"M.jpeg",sep=""),width = 1600, height = 1200)
  print(plot(N1_M[[i]][,2],type="l",main=paste(disciplines[i],"- M"),ylab="M"))
  dev.off()
  post_mean_N_M<-rbind(post_mean_N_M,mean(N1_M[[i]][,2]))
  post_mean_M_M<-rbind(post_mean_M_M,mean(N1_M[[i]][,1]))
}

N1_F<-list()
post_mean_N_F<-NULL
post_mean_M_F<-NULL
for(i in 1:length(disciplines)){
  N1_F[[i]]<-read.csv(paste(wd,"/output/step1_",disciplines[i],"F.csv",sep=""))
  N1_F[[i]]<-N1_F[[i]][,-1]
  jpeg(file=paste(wd,"/extra_figures/trace_step1",disciplines[i],"F.jpeg",sep=""),width = 1600, height = 1200)
  print(plot(N1_F[[i]][,2],type="l",main=paste(disciplines[i],"- F"),ylab="M"))
  dev.off()
  post_mean_N_F<-rbind(post_mean_N_F,mean(N1_F[[i]][,2]))
  post_mean_M_F<-rbind(post_mean_M_F,mean(N1_F[[i]][,1]))  
}

# 1
df<-data.frame(value=rbind(as.matrix(N1_F[[1]][,1]/post_mean_N_F[1]),
                                  as.matrix(N1_M[[1]][,1]/post_mean_N_M[1])),
                      gender=rep(c("Females","Males"),
                                 each=length(N1_F[[1]][,1])))

jpeg(file=paste(wd,"/figures/step1",disciplines[1],".jpeg",sep=""),width = 1600, height = 1200)
print(ggplot(df, aes(x=value, group=gender))+ 
  geom_density(aes(colour=factor(gender)),
               size=1.1,alpha=.6)+
  theme_light()+
  theme(legend.position = "none", axis.text = element_text(size=50)) +
  labs(title = "", x = "", y="") +
  scale_color_manual(name= "",values=c(brewer.pal(n=9,name = "Greys")[c(4,7)])))
dev.off()

# 2
df<-data.frame(value=rbind(as.matrix(N1_F[[2]][,1]/post_mean_N_F[2]),
                                  as.matrix(N1_M[[2]][,1]/post_mean_N_M[2])),
                      gender=rep(c("Females","Males"),
                                 each=length(N1_F[[2]][,1])))

jpeg(file=paste(wd,"/figures/step1",disciplines[2],".jpeg",sep=""),width = 1600, height = 1200)
print(ggplot(df, aes(x=value, group=gender))+ 
  geom_density(aes(colour=factor(gender)),
               size=1.2,alpha=.6)+
  theme_light()+
  theme(legend.position = "none", axis.text = element_text(size=50)) +
  labs(title = "", x = "", y="") +
  scale_color_manual(name= "",values=c(brewer.pal(n=9,name = "Greys")[c(4,7)])))
dev.off()

# 3
df<-data.frame(value=rbind(as.matrix(N1_F[[3]][,1]/post_mean_N_F[3]),
                               as.matrix(N1_M[[3]][,1]/post_mean_N_M[3])),
                   gender=rep(c("Females","Males"),
                              each=length(N1_F[[3]][,1])))

jpeg(file=paste(wd,"/figures/step1",disciplines[3],".jpeg",sep=""),width = 1600, height = 1200)
print(ggplot(df, aes(x=value, group=gender))+ 
  geom_density(aes(colour=factor(gender)),
               size=1.2,alpha=.6)+
  theme_light()+
  theme(legend.position = "none", axis.text = element_text(size=50)) +
  labs(title = "", x = "", y="") +
  scale_color_manual(name= "",values=c(brewer.pal(n=9,name = "Greys")[c(4,7)])))
dev.off()

# 4
df<-data.frame(value=rbind(as.matrix(N1_F[[4]][,1]/post_mean_N_F[4]),
                               as.matrix(N1_M[[4]][,1]/post_mean_N_M[4])),
                   gender=rep(c("Females","Males"),
                              each=length(N1_F[[4]][,1])))

jpeg(file=paste(wd,"/figures/step1",disciplines[4],".jpeg",sep=""),width = 1600, height = 1200)
print(ggplot(df, aes(x=value, group=gender))+ 
  geom_density(aes(colour=factor(gender)),
               size=1.2,alpha=.6)+
  theme_light()+
  theme(legend.position = "none", axis.text = element_text(size = 50)) +
  labs(title = "", x = "", y="") +
  scale_color_manual(name= "",values=c(brewer.pal(n=9,name = "Greys")[c(4,7)])))
dev.off()

# 5
df<-data.frame(value=rbind(as.matrix(N1_F[[5]][,1]/post_mean_N_F[5]),
                               as.matrix(N1_M[[5]][,1]/post_mean_N_M[5])),
                   gender=rep(c("Females","Males"),
                              each=length(N1_F[[5]][,1])))

jpeg(file=paste(wd,"/figures/step1",disciplines[5],".jpeg",sep=""),width = 1600, height = 1200)
print(ggplot(df, aes(x=value, group=gender))+ 
  geom_density(aes(colour=factor(gender)),
               size=1.2,alpha=.6)+
  theme_light()+
  theme(legend.position = "none", axis.text = element_text(size=50)) +
  labs(title = "", x = "", y="") +
  scale_color_manual(name= "",values=c(brewer.pal(n=9,name = "Greys")[c(4,7)])))
dev.off()

# 6

df<-data.frame(value=rbind(as.matrix(N1_F[[6]][,1]/post_mean_N_F[6]),
                           as.matrix(N1_M[[6]][,1]/post_mean_N_M[6])),
               gender=rep(c("Females","Males"),
                          each=length(N1_F[[6]][,1])))

jpeg(file=paste(wd,"/figures/step1",disciplines[6],".jpeg",sep=""),width = 1600, height = 1200)
print(ggplot(df, aes(x=value, group=gender))+ 
  geom_density(aes(colour=factor(gender)),
               size=1.2,alpha=.6)+
  theme_light()+
  theme(legend.position = "none", axis.text = element_text(size=50)) +
  labs(title = "", x = "", y="") +
  scale_color_manual(name= "",values=c(brewer.pal(n=9,name = "Greys")[c(4,7)])))
dev.off()

# 7
df<-data.frame(value=rbind(as.matrix(N1_F[[7]][,1]/post_mean_N_F[7]),
                           as.matrix(N1_M[[7]][,1]/post_mean_N_M[7])),
               gender=rep(c("Females","Males"),
                          each=length(N1_F[[7]][,1])))

jpeg(file=paste(wd,"/figures/step1",disciplines[7],".jpeg",sep=""),width = 1600, height = 1200)
print(ggplot(df, aes(x=value, group=gender))+ 
  geom_density(aes(colour=factor(gender)),
               size=1.2,alpha=.6)+
  theme_light()+
  theme(legend.position = "none", axis.text = element_text(size=50)) +
  labs(title = "", x = "", y="") +
  scale_color_manual(name= "",values=c(brewer.pal(n=9,name = "Greys")[c(4,7)])))
dev.off()

# 8
df<-data.frame(value=rbind(as.matrix(N1_F[[8]][,1]/post_mean_N_F[8]),
                           as.matrix(N1_M[[8]][,1]/post_mean_N_M[8])),
               gender=rep(c("Females","Males"),
                          each=length(N1_F[[8]][,1])))

jpeg(file=paste(wd,"/figures/step1",disciplines[8],".jpeg",sep=""),width = 1600, height = 1200)
print(ggplot(df, aes(x=value, group=gender))+ 
  geom_density(aes(colour=factor(gender)),
               size=1.2,alpha=.6)+
  theme_light()+
  theme(legend.position = "none", axis.text = element_text(size=50)) +
  labs(title = "", x = "", y="") +
  scale_color_manual(name= "",values=c(brewer.pal(n=9,name = "Greys")[c(4,7)])))
dev.off()

# 9
df<-data.frame(value=rbind(as.matrix(N1_F[[9]][,1]/post_mean_N_F[9]),
                           as.matrix(N1_M[[9]][,1]/post_mean_N_M[9])),
               gender=rep(c("Females","Males"),
                          each=length(N1_F[[9]][,1])))

jpeg(file=paste(wd,"/figures/step1",disciplines[9],".jpeg",sep=""),width = 1600, height = 1200)
print(ggplot(df, aes(x=value, group=gender))+ 
  geom_density(aes(colour=factor(gender)),
               size=1.2,alpha=.6)+
  theme_light()+
  theme(legend.position = "none", axis.text = element_text(size=50)) +
  labs(title = "", x = "", y="") +
  scale_color_manual(name= "",values=c(brewer.pal(n=9,name = "Greys")[c(4,7)])))
dev.off()

# 10
df<-data.frame(value=rbind(as.matrix(N1_F[[10]][,1]/post_mean_N_F[10]),
                           as.matrix(N1_M[[10]][,1]/post_mean_N_M[10])),
               gender=rep(c("Females","Males"),
                          each=length(N1_F[[10]][,1])))

jpeg(file=paste(wd,"/figures/step1",disciplines[10],".jpeg",sep=""),width = 1600, height = 1200)
ggplot(df, aes(x=value, group=gender))+ 
  geom_density(aes(colour=factor(gender)),
               size=1.2,alpha=.6)+
  theme_light()+
  theme(legend.position = "none", axis.text = element_text(size=50)) +
  labs(title = "", x = "", y="") +
  scale_color_manual(name= "",values=c(brewer.pal(n=9,name = "Greys")[c(4,7)]))
dev.off()
