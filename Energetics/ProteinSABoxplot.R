# CODE FROM LEINBACH ET AL. 2021 - Energetic and reproductive costs of coral recovery in divergent bleaching responses   
# large sections of commented out code are analyses/graphs not included in the paper OR model outputs - model outputs are indented
# only model outputs from results in the paper are included in the script
# NOTE: depth and heat stress response were found to be highly collinear in this data (singularities were present); including depth in the model 
#       did not change any estimates of effect or P values so we did not include it

library(tidyverse)
library(scales)
setwd("/Users/lumosmaximma/Desktop/coral/histology")

datum=read.csv("SymbiontProteinEtcTemp.csv")
proteinSA <- ggplot(data=datum)+
  geom_boxplot(mapping=aes(x=Status,y=ProteinPerSA),width=0.6)+
  labs(x="Survival Strategy",y=bquote('Total Protein Concentration'~(mu*g/cm^2)))+
  theme_classic(base_size=11)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))+
  scale_fill_manual(values=c("#7570B3","#1B9E77"))+
  geom_jitter(mapping=aes(x=Status,y=ProteinPerSA,color=Status,fill=Status),shape=21,color="black",size=2.9,width=0.08)+
  theme(legend.position="none")+
  scale_x_discrete(labels=c("resilient"="recovered","resistant"="resistant"))
proteinSA

#t test between healthy vs recovered protein concentration/SA
results=lm(ProteinPerSA~Status,data=datum)
summary(results)
      #MODEL RESULTS
          # Residuals:
          #   Min       1Q   Median       3Q      Max 
          # -186.575  -78.146    0.395   58.474  220.625 
          # 
          # Coefficients:
          #                  Estimate Std. Error t value Pr(>|t|)    
          # (Intercept)       323.95      22.95  14.115 8.72e-15 ***
          # Statusresistant   123.09      37.48   3.284   0.0026 ** 
          #   ---
          #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
          # 
          # Residual standard error: 102.6 on 30 degrees of freedom
          # Multiple R-squared:  0.2645,	Adjusted R-squared:   0.24 
          # F-statistic: 10.79 on 1 and 30 DF,  p-value: 0.002603
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
      #MODEL RESULTS
          # Residuals:
          #   Min      1Q  Median      3Q     Max 
          # -324420 -110267   19216   79055  548743 
          # 
          # Coefficients:
          #                  Estimate Std. Error t value Pr(>|t|)    
          # (Intercept)       467153      41696  11.204 3.04e-12 ***
          # Statusresistant    -3517      68089  -0.052    0.959    
          # ---
          #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
          # 
          # Residual standard error: 186500 on 30 degrees of freedom
          # Multiple R-squared:  8.894e-05,	Adjusted R-squared:  -0.03324 
          # F-statistic: 0.002668 on 1 and 30 DF,  p-value: 0.9591
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
      #MODEL RESULTS
          # Residuals:
          #   Min      1Q  Median      3Q     Max 
          # -455.87 -226.23  -42.04  159.45  704.87 
          # 
          # Coefficients:
          #                   Estimate Std. Error t value Pr(>|t|)    
          # (Intercept)       883.73      72.69   12.16 4.02e-13 ***
          # Statusresistant   250.48     118.70    2.11   0.0433 *  
          #   ---
          #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
          # 
          # Residual standard error: 325.1 on 30 degrees of freedom
          # Multiple R-squared:  0.1292,	Adjusted R-squared:  0.1002 
          # F-statistic: 4.453 on 1 and 30 DF,  p-value: 0.04329
confint(results3)

#protein and size correlation: increasing colony size significantly increases SA normalized protein concentration
datum2=read.csv("Histology Supplemental Data File.csv")
results8=lm(protein_normSA~colony_area,data=datum2)
summary(results8)

#carb and size correlation: increasing colony size increases SA normalized carb concentration but not significantly
results9=lm(carb_normSA~colony_area,data=datum2)
summary(results9)
      
