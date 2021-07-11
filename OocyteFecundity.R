library(tidyverse)
setwd("/Users/lumosmaximma/Desktop/coral/histology")
datum=read.csv("OocyteFecundity.csv")

#generalized linear model with Poisson distribution for difference in fecundity eggs/polyp between resistant vs resilient
datum$BleachingStatus=as.factor(datum$BleachingStatus)
results=glm(Average~BleachingStatus,data=datum,family=poisson)
summary(results)
exp(confint(results))

#generalized linear model with Poisson distribution for difference in fecundity with all zeroes removed
datum3=read.csv("FecundityZeroRemove.csv")
datum3$BleachingStatus=as.factor(datum3$BleachingStatus)
results4=glm(Average~BleachingStatus,data=datum3,family=poisson)
summary(results4)
exp(confint(results4))

#generalized linear model with Poisson distribution for difference in fecundity with immature removed
datum4=read.csv("FecundityImmatureRemoved.csv")
datum4$BleachingStatus=as.factor(datum4$BleachingStatus)
results5=glm(Average~BleachingStatus,data=datum4,family=poisson)
summary(results5)
exp(confint(results5))

#t test for difference in average fecundity between resistant and resilient
results2=lm(Average~BleachingStatus,data=datum)
summary(results2)
confint(results2)

#boxplot of fecundity for recovered vs. resistant (color fill and black dots, no jitter)
fecundity <- ggplot(data=datum)+
  geom_boxplot(mapping=aes(x=BleachingStatus,y=Average,fill=BleachingStatus),width=0.6)+
  labs(x="Survival Strategy",y="Relative Fecundity (average oocytes/polyp/slide)")+
  theme_classic(base_size=12)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))+
  scale_fill_manual(values=c("#7570B3","#1B9E77"))+
  geom_point(mapping=aes(x=BleachingStatus,y=Average))+
  theme(legend.position="none")+
  scale_x_discrete(labels=c("resilient"="recovered","resistant"="resistant"))
#fecundity

#boxplot of fecundity for recovered vs. resistant with no fill and color WITH JITTER
fecundity <- ggplot(data=datum)+
  geom_boxplot(mapping=aes(x=BleachingStatus,y=Average),width=0.6)+
  labs(x="Survival Strategy",y="Relative Fecundity (average oocytes/polyp/slide)")+
  theme_classic(base_size=12)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))+
  geom_jitter(mapping=aes(x=BleachingStatus,y=Average,color=BleachingStatus,fill=BleachingStatus),shape=21,color="black",size=2.9,width=0.08)+
  scale_fill_manual(values=c("#7570B3","#1B9E77"))+
  theme(legend.position="none")+
  scale_x_discrete(labels=c("resilient"="recovered","resistant"="resistant"))
fecundity

#scatterplot of colony size vs average fecundity for resistant and resilient
datum2=read.csv("AvgFecundityandSize.csv")
fecundsize <- ggplot(datum2,aes(x=ColonySize,y=AvgFecundity,color=BleachingStatus))+
  geom_point()+
  theme_classic()+ 
  geom_smooth(method="glm",aes(fill=BleachingStatus))+
  scale_fill_manual(values=c("#7570B3","#1B9E77"),name="Survival Strategy",labels=c("recovered","resistant"))+
  scale_color_manual(values=c("#7570B3","#1B9E77"),name="Survival Strategy",labels=c("recovered","resistant"))+
  scale_x_continuous(bquote('Colony Area'~(cm^2)))+
  labs(y="Relative Fecundity (average oocytes/polyp/slide)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))
#possibly change the units for relative fecundity?
fecundsize

#generalized linear model with Poisson distribution for difference in fecundity over size between resistant vs resilient
datum2$BleachingStatus=as.factor(datum2$BleachingStatus)
results3=glm(AvgFecundity~BleachingStatus+ColonySize,data=datum2,family=poisson)
summary(results3)
exp(confint(results3))

#scatterplot of fecundity vs protein measurement for resistant vs resilient
fecundprotein <- ggplot(datum2,aes(x=Protein,y=AvgFecundity,color=BleachingStatus))+
  geom_point()+
  theme_classic()+
  scale_color_manual(name="Bleaching Status",values=c("#7570B3","#1B9E77"))+
  labs(x=bquote('Protein Content'~(mu*g/cm^2)),y="Relative Fecundity (average oocytes/polyp/slide)")
#fecundprotein

#fecundity and protein correlation
results5=lm(AvgFecundity~Protein+BleachingStatus+Protein:BleachingStatus,data=datum2)
#summary(results5)

#testing for an interaction in size/bleaching/fecundity data: not significant
datum2$BleachingStatus=as.factor(datum2$BleachingStatus)
results3=glm(AvgFecundity~BleachingStatus+ColonySize+(BleachingStatus*ColonySize),data=datum2,family=poisson)
summary(results3)

#linear regression just for resistant
head(datum2)
resistant=datum2[datum2$BleachingStatus=="resistant",]
res_lm=lm(AvgFecundity~ColonySize,data=resistant)
summary(res_lm)

#linear regression for just recovered
recovered=datum2[datum2$BleachingStatus=="resilient",]
rec_lm=lm(AvgFecundity~ColonySize,data=recovered)
summary(rec_lm)

#glm with poisson distribution including depth as a factor
datum$depth_m=as.factor(datum$depth_m)
results5=glm(Average~BleachingStatus+depth_m,data=datum,family=poisson)
summary(results5)
exp(confint(results5))

#poisson regression just for resistant
head(datum2)
resistant=datum2[datum2$BleachingStatus=="resistant",]
res_poi=glm(AvgFecundity~ColonySize,data=resistant,family=poisson)
summary(res_poi)

#poisson regression for just recovered
recovered=datum2[datum2$BleachingStatus=="resilient",]
rec_poi=glm(AvgFecundity~ColonySize,data=recovered,family=poisson)
summary(rec_poi)