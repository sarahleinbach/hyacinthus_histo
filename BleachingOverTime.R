#stacked bar chart showing proportion of bleaching scores over time
library(tidyverse)
setwd("/Users/lumosmaximma/Desktop")
datum=read.csv("Ahya_SizeColor_PileMooringEdited.csv")
datum$color_bin=as.factor(datum$color_bin)
datum$month=as.factor(datum$month)
recovery <- ggplot(data=datum)+
  geom_bar(mapping=aes(x=month,fill=color_bin),position="fill",width=0.8)+
  theme_classic(base_size=12)+
  theme(legend.title=element_text(size=10))+
  scale_fill_brewer(labels=c("1 - stark white","2","3","4","5 - healthy"),palette="YlOrBr",name="Bleaching Severity in 2019")+
  scale_x_discrete(labels=c("Amay"="May","Baugust"="August","O"="October"))+
  labs(x="Month",y="Proportion of Colonies")
recovery
