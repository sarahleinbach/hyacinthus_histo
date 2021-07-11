#boxplot of protein concentration normalized to surface area for recovered vs. resistant 
library(tidyverse)
library(scales)
setwd("/Users/lumosmaximma/Desktop/coral/histology")

datum=read.csv("SymbiontProteinEtcTemp.csv")
proteinSA <- ggplot(data=datum)+
  geom_boxplot(mapping=aes(x=Status,y=Protein.SA..ug.cm2.),width=0.6)+
  labs(x="Survival Strategy",y=bquote('Total Protein Concentration'~(mu*g/cm^2)))+
  theme_classic(base_size=11)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))+
  scale_fill_manual(values=c("#7570B3","#1B9E77"))+
  geom_jitter(mapping=aes(x=Status,y=Protein.SA..ug.cm2.,color=Status,fill=Status),shape=21,color="black",size=2.9,width=0.08)+
  theme(legend.position="none")+
  scale_x_discrete(labels=c("resilient"="recovered","resistant"="resistant"))
proteinSA

#t test between healthy vs recovered protein concentration/SA
results=lm(Protein.SA..ug.cm2.~Status,data=datum)
summary(results)
confint(results)

#boxplot of symbiont counts normalized to surface area for recovered vs. healthy
scientific <- function(x){
  ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", " %*% 10^", scientific_format()(x)))))
}
symbiontSA <- ggplot(data=datum)+
  geom_boxplot(mapping=aes(x=Status,y=SymbiontPerSA),width=0.6)+
  labs(x="Survival Strategy",y=bquote('Microalgal Density'~(symbionts/cm^2)))+
  theme_classic(base_size=11)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))+
  scale_fill_manual(values=c("#7570B3","#1B9E77"))+
  geom_jitter(mapping=aes(x=Status,y=SymbiontPerSA,color=Status,fill=Status),shape=21,color="black",size=2.9,width=0.08)+
  theme(legend.position="none")+
  scale_x_discrete(labels=c("resilient"="recovered","resistant"="resistant"))+
  scale_y_continuous(breaks=seq(2.5e5,1.0e6,2.5e5), label=scientific)
symbiontSA

#t test of symbionts per SA in resistant vs recovered
results2=lm(SymbiontPerSA~Status,data=datum)
summary(results2)
confint(results2)

#boxplot of carb concentration normalized to surface area for recovered vs. resistant 
carbSA <- ggplot(data=datum)+
  geom_boxplot(mapping=aes(x=Status,y=CarbPerSA),width=0.6,outlier.shape=NA)+
  labs(x="Survival Strategy",y=bquote('Total Carbohydrate Concentration'~(mu*g/cm^2)))+
  theme_classic(base_size=11)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))+
  scale_fill_manual(values=c("#7570B3","#1B9E77"))+
  geom_jitter(mapping=aes(x=Status,y=CarbPerSA,color=Status,fill=Status),shape=21,color="black",size=2.9,width=0.08)+
  theme(legend.position="none")+
  scale_x_discrete(labels=c("resilient"="recovered","resistant"="resistant"))
carbSA

#t test between healthy vs recovered carb concentration/SA
results3=lm(CarbPerSA~Status,data=datum)
summary(results3)
confint(results3)

#protein/carb and size correlation
datum=read.csv("Histology Supplemental Data File.csv")
results8=lm(protein_normSA~colony_area,data=datum)
summary(results8)
results9=lm(carb_normSA~colony_area,data=datum)
summary(results9)


