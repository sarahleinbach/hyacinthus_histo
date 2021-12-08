# CODE FROM LEINBACH ET AL. 2021 - Energetic and reproductive costs of coral recovery in divergent bleaching responses   
# large sections of commented out code are analyses/graphs not included in the paper OR model outputs - model outputs are indented
# only model outputs from results in the paper are included in the script

setwd("/Users/lumosmaximma/Desktop/coral/histology")
library(tidyverse)
datum=read.csv("OocyteSize.csv")

# #mixed effects model for difference in egg size average Feret diameter in resistant vs resilient, WITHOUT depth as a factor
# library(nlme)
# datum$BleachingStatus=as.factor(datum$BleachingStatus)
# results=lme(Average~BleachingStatus,data=datum,random=~1|Individual)
# summary(results)
# intervals(results)

#mixed effects model for difference in egg size average Feret diameter in resistant vs resilient, WITH depth as a factor
library(nlme)
datum$BleachingStatus=as.factor(datum$BleachingStatus)
resultsdep=lme(Average~BleachingStatus+depth_m,data=datum,random=~1|Individual)
summary(resultsdep)
      # MODEL RESULTS
          # Linear mixed-effects model fit by REML
          #   AIC      BIC    logLik
          # 5793.763 5815.109 -2891.882
          # 
          # Random effects:
          #   Formula: ~1 | Individual
          #           (Intercept) Residual
          # StdDev:    24.97496 54.88429
          # 
          # Fixed effects: Average ~ BleachingStatus + depth_m 
          #                             Value Std.Error  DF   t-value p-value
          # (Intercept)              328.2387  43.02272 502  7.629426  0.0000
          # BleachingStatusresistant  -6.8365  27.95672  26 -0.244539  0.8087
          # depth_m                   -2.5052   3.34534  26 -0.748876  0.4607
          # Correlation: 
          #   (Intr) BlchnS
          # BleachingStatusresistant -0.963       
          # depth_m                  -0.963  0.884
          # 
          # Standardized Within-Group Residuals:
          #   Min           Q1          Med           Q3          Max 
          # -3.939460721 -0.635876108 -0.007739699  0.644627961  2.796582749 
          # 
          # Number of Observations: 531
          # Number of Groups: 29 
intervals(resultsdep)  
      # MODEL RESULTS
          # Approximate 95% confidence intervals
          # Fixed effects:
          #                              lower       est.      upper
          # (Intercept)              243.711889 328.238670 412.765451
          # BleachingStatusresistant -64.302355  -6.836502  50.629351
          # depth_m                   -9.381677  -2.505242   4.371193
          # attr(,"label")
          # [1] "Fixed effects:"
          # 
          # Random Effects:
          #   Level: Individual 
          #                   lower     est.    upper
          # sd((Intercept)) 17.24257 24.97496 36.17491
          # 
          # Within-group standard error:
          #   lower     est.    upper 
          # 51.59245 54.88429 58.38617

# #mixed effects model for difference in egg area in resistant vs. resilient
# results2=lme(Area~BleachingStatus,data=datum,random=~1|Individual)
# summary(results2)
# intervals(results2)

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

# #boxplot for oocyte area in resistant vs resilient
# #to add in points + geom_point(mapping=aes(x=BleachingStatusStatus,y=Area))
# oocytearea <- ggplot(data=datum)+
#   geom_boxplot(mapping=aes(x=BleachingStatus,y=Area,fill=BleachingStatus))+
#   labs(x="Bleaching Status",y=bquote('Oocyte Area'~(mu*m)))+
#   theme_classic()+
#   scale_fill_brewer(palette="Dark2")+
#   theme(legend.position="none")
# oocytearea

# #boxplot for Feret diameter averages in resistant vs resilient
# #to add in points + geom_point(mapping=aes(x=BleachingStatus,y=Average))
# oocytediam <- ggplot(data=datum)+
#   geom_boxplot(mapping=aes(x=BleachingStatus,y=Average,fill=BleachingStatus))+
#   labs(x="Bleaching Status",y=bquote('Average Feret diameter'~(mu*m)))+
#   theme_classic()+
#   scale_fill_brewer(palette="Dark2")+
#   theme(legend.position="none")+
#   geom_point(mapping=aes(x=BleachingStatus,y=Average))
# oocytediam

# #density plot of egg size distribution with Feret diameter
# oocytediamdens <- ggplot(data=datum,aes(x=Average,fill=BleachingStatus))+
#   geom_density(alpha=.3)+
#   theme_classic()+
#   scale_fill_brewer(name="Bleaching Status",palette="Dark2")+
#   labs(x=bquote('Average Feret diameter'~(mu*m)),y="density")+
#   geom_vline(aes(xintercept=312.23),color="#CC6600")+
#   geom_vline(aes(xintercept=307.59),color="#009999")
# oocytediamdens

# #scatterplot of egg size vs colony size for resistant vs resilient
# datum2=read.csv("OocyteSize2Avgs.csv")
# sizesize <- ggplot(data=datum2,aes(x=ColonySize,y=AvgEgg,color=Status))+
#   geom_point()+
#   theme_classic()+
#   scale_color_brewer(name="Bleaching Status",palette="Dark2")+
#   labs(x=bquote('Colony Size'~(cm^2)),y=bquote('Average Egg Size'~(mu*m^2)))
# sizesize

# #scatterplot of egg size and protein concentration for resistant vs resilient
# proteinsize <- ggplot(data=datum2,aes(x=Protein,y=AvgEgg,color=Status))+
#   geom_point()+
#   theme_classic()+
#   scale_color_brewer(name="Bleaching Status",palette="Dark2")+
#   labs(x=bquote('Protein'~(mu*g/cm^2)),y=bquote('Average Egg Size'~(mu*m^2)))
# proteinsize

  
