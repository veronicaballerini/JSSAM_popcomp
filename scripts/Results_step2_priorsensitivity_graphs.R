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
library(ggplot2)
library(RColorBrewer)

#### Load data
load(paste(wd,"/data/finaldata.RData",sep=""))
disciplines<-rownames(nf)

w2_Msdw1<-list()
for(i in 1:length(disciplines)){
  w2_Msdw1[[i]]<-read.csv(paste(wd,"/output/step2_",disciplines[i],
                                "M","sdw1.csv",sep=""))
  w2_Msdw1[[i]]<-w2_Msdw1[[i]][,-1]
  print(plot(w2_Msdw1[[i]][,3],type="l"))
}

w2_Msdw3<-list()
for(i in 1:length(disciplines)){
  w2_Msdw3[[i]]<-read.csv(paste(wd,"/output/step2_",disciplines[i],
                                "M","sdw3.csv",sep=""))
  w2_Msdw3[[i]]<-w2_Msdw3[[i]][,-1]
  print(plot(w2_Msdw3[[i]][,3],type="l"))
}

w2_Msdw4<-list()
for(i in 1:length(disciplines)){
  w2_Msdw4[[i]]<-read.csv(paste(wd,"/output/step2_",disciplines[i],
                                "M","sdw4.csv",sep=""))
  w2_Msdw4[[i]]<-w2_Msdw4[[i]][,-1]
  print(plot(w2_Msdw4[[i]][,3],type="l"))
}

w2_Msdw5<-list()
for(i in 1:length(disciplines)){
  w2_Msdw5[[i]]<-read.csv(paste(wd,"/output/step2_",disciplines[i],
                                "M","sdw5.csv",sep=""))
  w2_Msdw5[[i]]<-w2_Msdw5[[i]][,-1]
  print(plot(w2_Msdw5[[i]][,3],type="l"))
}

w2_Fsdw1<-list()
for(i in 1:length(disciplines)){
  w2_Fsdw1[[i]]<-read.csv(paste(wd,"/output/step2_",disciplines[i],
                                "F","sdw1.csv",sep=""))
  w2_Fsdw1[[i]]<-w2_Fsdw1[[i]][,-1]
  print(plot(w2_Fsdw1[[i]][,3],type="l"))
}

w2_Fsdw3<-list()
for(i in 1:length(disciplines)){
  w2_Fsdw3[[i]]<-read.csv(paste(wd,"/output/step2_",disciplines[i],
                                "F","sdw3.csv",sep=""))
  w2_Fsdw3[[i]]<-w2_Fsdw3[[i]][,-1]
  print(plot(w2_Fsdw3[[i]][,3],type="l"))
}

w2_Fsdw4<-list()
for(i in 1:length(disciplines)){
  w2_Fsdw4[[i]]<-read.csv(paste(wd,"/output/step2_",disciplines[i],
                                "F","sdw4.csv",sep=""))
  w2_Fsdw4[[i]]<-w2_Fsdw4[[i]][,-1]
  print(plot(w2_Fsdw4[[i]][,3],type="l"))
}

w2_Fsdw5<-list()
for(i in 1:length(disciplines)){
  w2_Fsdw5[[i]]<-read.csv(paste(wd,"/output/step2_",disciplines[i],
                                "F","sdw5.csv",sep=""))
  w2_Fsdw5[[i]]<-w2_Fsdw5[[i]][,-1]
  print(plot(w2_Fsdw5[[i]][,3],type="l"))
}

df.w_agrvet_F<-data.frame(value=rbind(as.matrix(w2_Fsdw1[[1]][,3]),
                                      as.matrix(w2_Fsdw3[[1]][,3]),
                                      as.matrix(w2_Fsdw4[[1]][,3]),
                                      as.matrix(w2_Fsdw5[[1]][,3])),
                        labels=rep(c("1","3",
                                     "4","5"),
                                   each=length(w2_Fsdw1[[1]][,3])))
df.w_agrvet_M<-data.frame(value=rbind(as.matrix(w2_Msdw1[[1]][,3]),
                                      as.matrix(w2_Msdw3[[1]][,3]),
                                      as.matrix(w2_Msdw4[[1]][,3]),
                                      as.matrix(w2_Msdw5[[1]][,3])),
                          labels=rep(c("1","3",
                                       "4","5"),
                                     each=length(w2_Msdw1[[1]][,3])))


# i = 1
jpeg(file=paste(wd,"/figures/sens_",disciplines[1],"F.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df.w_agrvet_F,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  labs(title = "Agricolture and Forestry, F", x = expression(tau), y=expression(w))
dev.off()

jpeg(file=paste(wd,"/figures/sens_",disciplines[1],"M.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df.w_agrvet_M,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  labs(title = "Agricolture and Forestry, M", x = expression(tau), y=expression(w))
dev.off()

# i = 2
df.w_engarch_F<-data.frame(value=rbind(as.matrix(w2_Fsdw1[[2]][,3]),
                                     as.matrix(w2_Fsdw3[[2]][,3]),
                                     as.matrix(w2_Fsdw4[[2]][,3]),
                                     as.matrix(w2_Fsdw5[[2]][,3])),
                         labels=rep(c("1","3",
                                      "4","5"),
                                    each=length(w2_Fsdw1[[2]][,3])))
df.w_engarch_M<-data.frame(value=rbind(as.matrix(w2_Msdw1[[2]][,3]),
                                       as.matrix(w2_Msdw3[[2]][,3]),
                                       as.matrix(w2_Msdw4[[2]][,3]),
                                       as.matrix(w2_Msdw5[[2]][,3])),
                           labels=rep(c("1","3",
                                        "4","5"),
                                      each=length(w2_Msdw1[[2]][,3])))

jpeg(file=paste(wd,"/figures/sens_",disciplines[2],"F.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df.w_engarch_F,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  labs(title = "Architecture and Engineering, F", x = expression(tau), y=expression(w))
dev.off()

jpeg(file=paste(wd,"/figures/sens_",disciplines[2],"M.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df.w_engarch_M,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  labs(title = "Architecture and Engineering, M", x = expression(tau), y=expression(w))
dev.off()

# 3
df.w_eco_F<-data.frame(value=rbind(as.matrix(w2_Fsdw1[[3]][,3]),
                                 as.matrix(w2_Fsdw3[[3]][,3]),
                                 as.matrix(w2_Fsdw4[[3]][,3]),
                                 as.matrix(w2_Fsdw5[[3]][,3])),
                     labels=rep(c("1","3","4","5"),
                                each=length(w2_Fsdw1[[3]][,3])))
df.w_eco_M<-data.frame(value=rbind(as.matrix(w2_Msdw1[[3]][,3]),
                                   as.matrix(w2_Msdw3[[3]][,3]),
                                   as.matrix(w2_Msdw4[[3]][,3]),
                                   as.matrix(w2_Msdw5[[3]][,3])),
                       labels=rep(c("1","3","4","5"),
                                  each=length(w2_Msdw1[[3]][,3])))

jpeg(file=paste(wd,"/figures/sens_",disciplines[3],"F.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df.w_eco_F,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  labs(title = "B&A, Economics, Finance, F", x = expression(tau), y=expression(w))
dev.off()

jpeg(file=paste(wd,"/figures/sens_",disciplines[3],"M.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df.w_eco_M,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  labs(title = "B&A, Economics, Finance, M", x = expression(tau), y=expression(w))
dev.off()

# 4
df.w_comm_F<-data.frame(value=rbind(as.matrix(w2_Fsdw1[[4]][,3]),
                                  as.matrix(w2_Fsdw3[[4]][,3]),
                                  as.matrix(w2_Fsdw4[[4]][,3]),
                                  as.matrix(w2_Fsdw5[[4]][,3])),
                      labels=rep(c("1","3","4","5"),
                                 each=length(w2_Fsdw1[[4]][,3])))
df.w_comm_M<-data.frame(value=rbind(as.matrix(w2_Msdw1[[4]][,3]),
                                    as.matrix(w2_Msdw3[[4]][,3]),
                                    as.matrix(w2_Msdw4[[4]][,3]),
                                    as.matrix(w2_Msdw5[[4]][,3])),
                        labels=rep(c("1","3","4","5"),
                                   each=length(w2_Msdw1[[4]][,3])))

jpeg(file=paste(wd,"/figures/sens_",disciplines[4],"F.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df.w_comm_F,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  ylim(c(0,2000))+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  labs(title = "Communication and Publishing, F", x = expression(tau), y=expression(w))
dev.off()

jpeg(file=paste(wd,"/figures/sens_",disciplines[4],"M.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df.w_comm_M,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  labs(title = "Communication and Publishing, M", x = expression(tau), y=expression(w))
dev.off()

# 5
df.w_indeng_F<-data.frame(value=rbind(as.matrix(w2_Fsdw1[[5]][,3]),
                                    as.matrix(w2_Fsdw3[[5]][,3]),
                                    as.matrix(w2_Fsdw4[[5]][,3]),
                                    as.matrix(w2_Fsdw5[[5]][,3])),
                        labels=rep(c("1","3","4","5"),
                                   each=length(w2_Fsdw1[[5]][,3])))
df.w_indeng_M<-data.frame(value=rbind(as.matrix(w2_Msdw1[[5]][,3]),
                                      as.matrix(w2_Msdw3[[5]][,3]),
                                      as.matrix(w2_Msdw4[[5]][,3]),
                                      as.matrix(w2_Msdw5[[5]][,3])),
                          labels=rep(c("1","3","4","5"),
                                     each=length(w2_Msdw1[[5]][,3])))

jpeg(file=paste(wd,"/figures/sens_",disciplines[5],"F.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df.w_indeng_F,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  labs(title = "Industrial Engineering, F", x = expression(tau), y=expression(w))
dev.off()

jpeg(file=paste(wd,"/figures/sens_",disciplines[5],"M.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df.w_indeng_M,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  ylim(c(0,5000))+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  labs(title = "Industrial Engineering, M", x = expression(tau), y=expression(w))
dev.off()

# 6
df.w_law_F<-data.frame(value=rbind(as.matrix(w2_Fsdw1[[6]][,3]),
                                   as.matrix(w2_Fsdw3[[6]][,3]),
                                   as.matrix(w2_Fsdw4[[6]][,3]),
                                   as.matrix(w2_Fsdw5[[6]][,3])),
                     labels=rep(c("1","3","4","5"),
                                each=length(w2_Fsdw1[[6]][,3])))
df.w_law_M<-data.frame(value=rbind(as.matrix(w2_Msdw1[[6]][,3]),
                                   as.matrix(w2_Msdw3[[6]][,3]),
                                   as.matrix(w2_Msdw4[[6]][,3]),
                                   as.matrix(w2_Msdw5[[6]][,3])),
                       labels=rep(c("1","3","4","5"),
                                  each=length(w2_Msdw1[[6]][,3])))

jpeg(file=paste(wd,"/figures/sens_",disciplines[6],"F.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df.w_law_F,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  labs(title = "Law and Legal Studies, F", x = expression(tau), y=expression(w))
dev.off()

jpeg(file=paste(wd,"/figures/sens_",disciplines[6],"M.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df.w_law_M,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  labs(title = "Law and Legal Studies, M", x = expression(tau), y=expression(w))
dev.off()

# 7
df.w_lit_F<-data.frame(value=rbind(as.matrix(w2_Fsdw1[[7]][,3]),
                                 as.matrix(w2_Fsdw3[[7]][,3]),
                                 as.matrix(w2_Fsdw4[[7]][,3]),
                                 as.matrix(w2_Fsdw5[[7]][,3])),
                     labels=rep(c("1","3","4","5"),
                                each=length(w2_Fsdw1[[7]][,3])))
df.w_lit_M<-data.frame(value=rbind(as.matrix(w2_Msdw1[[7]][,3]),
                                   as.matrix(w2_Msdw3[[7]][,3]),
                                   as.matrix(w2_Msdw4[[7]][,3]),
                                   as.matrix(w2_Msdw5[[7]][,3])),
                       labels=rep(c("1","3","4","5"),
                                  each=length(w2_Msdw1[[7]][,3])))

jpeg(file=paste(wd,"/figures/sens_",disciplines[7],"F.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df.w_lit_F,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  labs(title = "Literature and Humanities, F", x = expression(tau), y=expression(w))
dev.off()

jpeg(file=paste(wd,"/figures/sens_",disciplines[7],"M.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df.w_lit_M,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  labs(title = "Literature and Humanities, M", x = expression(tau), y=expression(w))
dev.off()

# 8
df.w_med_F<-data.frame(value=rbind(as.matrix(w2_Fsdw1[[8]][,3]),
                                   as.matrix(w2_Fsdw3[[8]][,3]),
                                   as.matrix(w2_Fsdw4[[8]][,3]),
                                   as.matrix(w2_Fsdw5[[8]][,3])),
                     labels=rep(c("1","3","4","5"),
                                each=length(w2_Fsdw1[[8]][,3])))
df.w_med_M<-data.frame(value=rbind(as.matrix(w2_Msdw1[[8]][,3]),
                                   as.matrix(w2_Msdw3[[8]][,3]),
                                   as.matrix(w2_Msdw4[[8]][,3]),
                                   as.matrix(w2_Msdw5[[8]][,3])),
                       labels=rep(c("1","3","4","5"),
                                  each=length(w2_Msdw1[[8]][,3])))

jpeg(file=paste(wd,"/figures/sens_",disciplines[8],"F.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df.w_med_F,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  labs(title = "Medicine, Dentistry, Pharmacy, F", x = expression(tau), y=expression(w))
dev.off()

jpeg(file=paste(wd,"/figures/sens_",disciplines[8],"M.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df.w_med_M,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  labs(title = "Medicine, Dentistry, Pharmacy, M", x = expression(tau), y=expression(w))
dev.off()

# 9
df.w_pol_F<-data.frame(value=rbind(as.matrix(w2_Fsdw1[[9]][,3]),
                                 as.matrix(w2_Fsdw3[[9]][,3]),
                                 as.matrix(w2_Fsdw4[[9]][,3]),
                                 as.matrix(w2_Fsdw5[[9]][,3])),
                     labels=rep(c("1","3","4","5"),
                                each=length(w2_Fsdw1[[9]][,3])))
df.w_pol_M<-data.frame(value=rbind(as.matrix(w2_Msdw1[[9]][,3]),
                                   as.matrix(w2_Msdw3[[9]][,3]),
                                   as.matrix(w2_Msdw4[[9]][,3]),
                                   as.matrix(w2_Msdw5[[9]][,3])),
                       labels=rep(c("1","3","4","5"),
                                  each=length(w2_Msdw1[[9]][,3])))

jpeg(file=paste(wd,"/figures/sens_",disciplines[9],"F.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df.w_pol_F,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  ylim(c(0,300))+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  labs(title = "Political Science, F", x = expression(tau), y=expression(w))
dev.off()

jpeg(file=paste(wd,"/figures/sens_",disciplines[9],"M.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df.w_pol_M,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  labs(title = "Political Science, M", x = expression(tau), y=expression(w))
dev.off()

# 10
df.w_sci_F<-data.frame(value=rbind(as.matrix(w2_Fsdw1[[10]][,3]),
                                 as.matrix(w2_Fsdw3[[10]][,3]),
                                 as.matrix(w2_Fsdw4[[10]][,3]),
                                 as.matrix(w2_Fsdw5[[10]][,3])),
                     labels=rep(c("1","3","4","5"),
                                each=length(w2_Fsdw1[[10]][,3])))
df.w_sci_M<-data.frame(value=rbind(as.matrix(w2_Msdw1[[10]][,3]),
                                   as.matrix(w2_Msdw3[[10]][,3]),
                                   as.matrix(w2_Msdw4[[10]][,3]),
                                   as.matrix(w2_Msdw5[[10]][,3])),
                       labels=rep(c("1","3","4","5"),
                                  each=length(w2_Msdw1[[10]][,3])))

jpeg(file=paste(wd,"/figures/sens_",disciplines[10],"F.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df.w_sci_F,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  labs(title = "Science and IT, F", x = expression(tau), y=expression(w))
dev.off()

jpeg(file=paste(wd,"/figures/sens_",disciplines[10],"M.jpeg",sep=""),width = 1600, height = 1200)
ggplot(df.w_sci_M,aes(x=labels, y=value, fill=labels)) +
  theme_light()+
  geom_boxplot(fill="gray")+
  geom_hline(yintercept = 1) + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  labs(title = "Science and IT, M", x = expression(tau), y=expression(w))
dev.off()
