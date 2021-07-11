library(tidyverse)
datum=read.csv("OocyteSize.csv")

#mixed effects model for difference in egg size average Feret diameter in resistant vs resilient
library(nlme)
datum$BleachingStatus=as.factor(datum$BleachingStatus)
results=lme(Average~BleachingStatus,data=datum,random=~1|Individual)
summary(results)
intervals(results)

#mixed effects model for difference in egg area in resistant vs. resilient
results2=lme(Area~BleachingStatus,data=datum,random=~1|Individual)
summary(results2)
intervals(results2)

#boxplot for oocyte area in resistant vs resilient
#to add in points + geom_point(mapping=aes(x=BleachingStatusStatus,y=Area))
oocytearea <- ggplot(data=datum)+
  geom_boxplot(mapping=aes(x=BleachingStatus,y=Area,fill=BleachingStatus))+
  labs(x="Bleaching Status",y=bquote('Oocyte Area'~(mu*m)))+
  theme_classic()+
  scale_fill_brewer(palette="Dark2")+
  theme(legend.position="none")
#oocytearea

#boxplot for Feret diameter averages in resistant vs resilient
#to add in points + geom_point(mapping=aes(x=BleachingStatus,y=Average))
oocytediam <- ggplot(data=datum)+
  geom_boxplot(mapping=aes(x=BleachingStatus,y=Average,fill=BleachingStatus))+
  labs(x="Bleaching Status",y=bquote('Average Feret diameter'~(mu*m)))+
  theme_classic()+
  scale_fill_brewer(palette="Dark2")+
  theme(legend.position="none")+
  geom_point(mapping=aes(x=BleachingStatus,y=Average))
#oocytediam

#histogram of egg size distribution with Feret diameter
oocytediamhist <- ggplot(data=datum,aes(x=Average,fill=BleachingStatus))+
  geom_histogram(binwidth=10,alpha=.4,position="identity")+
  theme_classic(base_size=12)+
  scale_fill_manual(name="Survival Strategy",values=c("#7570B3","#1B9E77"),labels=c("recovered","resistant"))+
  labs(x=bquote('Oocyte Average Diameter'~(mu*m)),y="count")+
  geom_vline(aes(xintercept=312.03),color="#1B9E77")+
  geom_vline(aes(xintercept=309.75),color="#7570B3")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 9))
oocytediamhist

#density plot of egg size distribution with Feret diameter
oocytediamdens <- ggplot(data=datum,aes(x=Average,fill=BleachingStatus))+
  geom_density(alpha=.3)+
  theme_classic()+
  scale_fill_brewer(name="Bleaching Status",palette="Dark2")+
  labs(x=bquote('Average Feret diameter'~(mu*m)),y="density")+
  geom_vline(aes(xintercept=312.23),color="#CC6600")+
  geom_vline(aes(xintercept=307.59),color="#009999")
#oocytediamdens

#scatterplot of egg size vs colony size for resistant vs resilient
datum2=read.csv("OocyteSize2Avgs.csv")
sizesize <- ggplot(data=datum2,aes(x=ColonySize,y=AvgEgg,color=Status))+
  geom_point()+
  theme_classic()+
  scale_color_brewer(name="Bleaching Status",palette="Dark2")+
  labs(x=bquote('Colony Size'~(cm^2)),y=bquote('Average Egg Size'~(mu*m^2)))
#sizesize

#scatterplot of egg size and protein concentration for resistant vs resilient
proteinsize <- ggplot(data=datum2,aes(x=Protein,y=AvgEgg,color=Status))+
  geom_point()+
  theme_classic()+
  scale_color_brewer(name="Bleaching Status",palette="Dark2")+
  labs(x=bquote('Protein'~(mu*g/cm^2)),y=bquote('Average Egg Size'~(mu*m^2)))
#proteinsize

  