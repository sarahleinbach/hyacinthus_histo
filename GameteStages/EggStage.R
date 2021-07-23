library(tidyverse)
datum=read.csv("EggStages.csv")

#distribution of egg stages as histogram (ugly)
eggstagehist <- ggplot(data=datum,aes(x=Stage,fill=BleachingStatus))+
  geom_histogram(binwidth=1,alpha=.5,position="identity")+
  theme_classic()+
  scale_fill_brewer(name="Survival Strategy",palette="Dark2")+
  labs(x="Egg Stage",y="count")
eggstagehist

#density plot of egg stage distribution
eggstagedens <- ggplot(data=datum,aes(x=Stage,fill=BleachingStatus))+
  geom_density(alpha=.3)+
  theme_classic()+
  scale_fill_brewer(name="Survival Strategy",palette="Dark2")+
  labs(x="Stage",y="density")+
  scale_x_discrete(labels=c("resilient"="recovered","resistant"="resistant"))
eggstagedens

#proportion of colonies with each egg stage
datum$Stage=as.factor(datum$Stage)
eggstages <- ggplot(data=datum)+
  geom_bar(mapping=aes(x=BleachingStatus,fill=Stage),position="fill")+
  theme_classic()+
  scale_fill_brewer(name="Oocyte Stage",labels=c("Stage II","Stage III","Stage IV"),palette="Dark2")+
  labs(x="Survival Strategy",y="Proportion")+
  scale_x_discrete(labels=c("resilient"="recovered","resistant"="resistant"))
eggstages

#loglinear model differences in egg stages between resistant and resilient
datum2=read.csv("EggStagesRComp.csv")
library(lme4)
results=glmer(Number~EggStage+BleachingStatus+EggStage:BleachingStatus+(1|Sample),data=datum2,family=poisson)
summary(results)

#loglinear model differences in sperm stages between resistant and resilient
datum3=read.csv("SpermStages.csv")
library(lme4)
results=glmer(Number~Stage+BleachingStatus+Stage:BleachingStatus+(1|Sample),data=datum3,family=poisson)
summary(results)





