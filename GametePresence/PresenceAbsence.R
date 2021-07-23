library(tidyverse)
setwd("/Users/lumosmaximma/Desktop/coral/histology")
datum=read.csv("GametePresence.csv")

#presence/absence of eggs in resistant vs resilient corals: glm binomial distribution
datum$BleachingStatus=as.factor(datum$BleachingStatus)
results=glm(Eggs~BleachingStatus,data=datum,family=binomial)
summary(results)
exp(confint(results))

#presence/absence of sperm in resistant vs resilient corals: glm binomial distribution
datum$BleachingStatus=as.factor(datum$BleachingStatus)
results2=glm(Sperm~BleachingStatus,data=datum,family=binomial)
summary(results2)
exp(confint(results2))

#presence/absence of gametes in resistant vs resilient corals: glm binomial distribution
datum$BleachingStatus=as.factor(datum$BleachingStatus)
results3=glm(Gametes~BleachingStatus,data=datum,family=binomial)
summary(results3)
exp(confint(results3))

#proportion of resistant vs resilient with eggs present
dfegg <- data.frame(Strategy=c("recovered", "resistant"),Proportion=c(0.3333,0.9231))
eggspresent <- ggplot(data=dfegg,aes(x=Strategy,y=Proportion,fill=Strategy))+
  geom_bar(stat="identity",width=0.6)+
  theme_classic(base_size=12)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))+
  scale_fill_manual(values=c("#7570B3","#1B9E77"))+
  theme(legend.position="none")+
  coord_cartesian(ylim = c(0.00,1.00))+
  labs(x="Survival Strategy",y="Proportion of colonies containing oocytes")+
  coord_flip(xlim=c(0.00,1.00))
eggspresent

#proportion of resistant vs resilient with sperm present
dfsperm <- data.frame(Strategy=c("recovered", "resistant"),Proportion=c(0.3810,0.9231))
spermpresent <- ggplot(data=dfsperm,aes(x=Strategy,y=Proportion,fill=Strategy))+
  geom_bar(stat="identity",width=0.6)+
  theme_classic(base_size=12)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))+
  scale_fill_manual(values=c("#7570B3","#1B9E77"))+
  theme(legend.position="none")+
  coord_cartesian(ylim = c(0.00,1.00))+
  labs(x="Survival Strategy",y="Proportion of colonies containing spermatocytes")+
  coord_flip()
spermpresent

#proportion of resistant vs resilient with gametes present
datum$Gametes=as.factor(datum$Gametes)
gametespresent <- ggplot(data=datum)+
  geom_bar(mapping=aes(x=BleachingStatus,fill=Gametes),position="fill")+
  theme_classic(base_size=15)+
  scale_fill_brewer(name="Gametes",labels=c("Absent","Present"),palette="Dark2")+
  labs(x="Survival Strategy",y="Proportion")
#gametespresent

#scatterplot of egg presence vs colony size in resistant vs resilient
probeggs <- ggplot(datum,aes(x=Size,y=Eggs2,color=BleachingStatus))+
  geom_point()+
  theme_classic(base_size=12)+
  scale_color_brewer(name="Survival Strategy",palette="Dark2")+
  #geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE)+
  labs(x=bquote('Colony Area'~(cm^2)),y="Probability of Containing Oocytes")
  #NEED TO CHANGE SURVIVAL STRAT TO BE RECOVERED AND RESIST
probeggs

#glm with binomial distribution for differences in prob of containing eggs over size is resis vs resil
datum$BleachingStatus=as.factor(datum$BleachingStatus)
results4=glm(Eggs~BleachingStatus+Size,data=datum,family=binomial)
summary(results4)
exp(confint(results4))

#scatterplot of sperm presence vs colony size in resistant vs resilient
probsperm <- ggplot(datum,aes(x=Size,y=Sperm,color=BleachingStatus))+
  geom_point()+
  theme_classic(base_size=15)+
  scale_color_brewer(name="Survival Strategy",palette="Dark2")+
  labs(x=bquote('Colony Area'~(cm^2)),y="Probability of Containing Spermatocytes")+
  scale_x_discrete(labels=c("resilient"="recovered","resistant"="resistant"))
#probsperm

#weird boxplot with containing gametes as x and size as y
datum$Eggs=as.factor(datum$Eggs)
probeggsboxplot <- ggplot(data=datum)+
  geom_boxplot(mapping=aes(x=Eggs,y=Size,fill=BleachingStatus),width=0.8)+
  geom_jitter(mapping=aes(x=Eggs,y=Size,color=BleachingStatus,fill=BleachingStatus),shape=21,color="black",size=3.0,width=0.08)+
  theme_classic(base_size=12)+
  scale_fill_manual(values=c("#7570B3","#1B9E77"),name="Survival Strategy",labels=c("recovered","resistant"))+
  labs(y=bquote('Colony Area'~(cm^2)),x="Probability of Containing Oocytes")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))+
  coord_flip()
probeggsboxplot