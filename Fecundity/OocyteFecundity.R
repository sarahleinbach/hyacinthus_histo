# CODE FROM LEINBACH ET AL. 2021 - Coral recovery, but at a cost: energetic and reproductive consequences of divergent bleaching responses   
# large sections of commented out code are analyses/graphs not included in the paper OR model outputs - model outputs are indented
# only model outputs from results in the paper are included in the script
# NOTE: although depth was never significant in our models, after subsequent analyses we found that depth and heat stress response 
#     were highly collinear; therefore, we included depth in the models in order to control for the effects of collinearity 

library(tidyverse)
library(car)
setwd("/Users/lumosmaximma/Desktop/coral/histology")
datum=read.csv("OocyteFecundity.csv")

# #generalized linear model with Poisson distribution for difference in fecundity eggs/polyp between resistant vs resilient, WITHOUT depth in model
# datum$BleachingStatus=as.factor(datum$BleachingStatus)
# results=glm(Average~BleachingStatus,data=datum,family=poisson)
# summary(results) 
# exp(confint(results))
# #resistant significantly more fecund (p = 7.7e-06)

#generalized linear model with Poisson distribution for difference in fecundity eggs/polyp between resistant vs resilient, WITH depth in model
datum$BleachingStatus=as.factor(datum$BleachingStatus)
datum$depth_m=as.factor(datum$depth_m)
resultsdep=glm(Average~BleachingStatus+depth_m,data=datum,family=poisson)
summary(resultsdep) 
      # MODEL RESULTS
          # Deviance Residuals: 
          #   Min        1Q    Median        3Q       Max  
          # -2.35702  -1.29857  -0.06712   0.64268   2.04538  
          # 
          # Coefficients:
          #                          Estimate Std. Error z value Pr(>|z|)  
          # (Intercept)               -0.4055     0.6124  -0.662   0.5079  
          # BleachingStatusresistant   1.4271     0.6262   2.279   0.0227 *
          # depth_m14                  0.2348     0.6669   0.352   0.7247  
          # ---
          #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
          # 
          # (Dispersion parameter for poisson family taken to be 1)
          # 
          # Null deviance: 85.407  on 41  degrees of freedom
          # Residual deviance: 61.296  on 39  degrees of freedom
          # AIC: Inf
          # Number of Fisher Scoring iterations: 6
exp(confint(resultsdep))
      # MODEL RESULTS
          #                             2.5 %    97.5 %
          # (Intercept)              0.1492662  1.813561
          # BleachingStatusresistant 1.4727361 18.936362
          # depth_m14                0.3971542  6.048069
vif(resultsdep)
      # BleachingStatus depth_m 
      # 5.162053        5.162053
      # depth and heat stress response highly collinear 

# #generalized linear model with Poisson distribution for difference in fecundity with all zeroes removed
# datum3=read.csv("FecundityZeroRemove.csv")
# datum3$BleachingStatus=as.factor(datum3$BleachingStatus)
# results4=glm(Average~BleachingStatus,data=datum3,family=poisson)
# summary(results4)
# exp(confint(results4))
# #no significant difference if remove all zeros (p = 0.27); removing 0s removes a biologically significant effect

# #generalized linear model with Poisson distribution for difference in fecundity with immature removed
# datum4=read.csv("FecundityImmatureRemoved.csv")
# datum4$BleachingStatus=as.factor(datum4$BleachingStatus)
# results5=glm(Average~BleachingStatus,data=datum4,family=poisson)
# summary(results5)
# exp(confint(results5))
# #resistant still more fecund (p = 4.01e-06)

# #t test for difference in average fecundity between resistant and resilient
# results2=lm(Average~BleachingStatus,data=datum)
# summary(results2)
# confint(results2)
# #resistant significantly more fecund (p = 2.7e-05)

#boxplot of fecundity for recovered vs. resistant
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
fecundsize

#generalized linear model with Poisson distribution for difference in fecundity over size between resistant vs resilient
datum2$BleachingStatus=as.factor(datum2$BleachingStatus)
results3=glm(AvgFecundity~BleachingStatus+ColonySize,data=datum2,family=poisson)
summary(results3)
      # MODEL RESULTS
          # Deviance Residuals: 
          #   Min        1Q    Median        3Q       Max  
          # -2.52466  -1.26613   0.01577   0.66051   2.05155  
          # 
          # Coefficients:
          #                           Estimate Std. Error z value Pr(>|z|)    
          # (Intercept)              -0.3180090  0.2622696  -1.213 0.225311    
          # BleachingStatusresistant  1.0374285  0.2934734   3.535 0.000408 ***
          # ColonySize                0.0004726  0.0002943   1.606 0.108231    
          # ---
          #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
          # 
          # (Dispersion parameter for poisson family taken to be 1)
          # 
          # Null deviance: 81.777  on 40  degrees of freedom
          # Residual deviance: 57.272  on 38  degrees of freedom
          # AIC: Inf
          # 
          # Number of Fisher Scoring iterations: 6
exp(confint(results3))
      # MODEL RESULTS
          #                            2.5 %   97.5 %
          # (Intercept)              0.4198067 1.180590
          # BleachingStatusresistant 1.6159503 5.144236
          # ColonySize               0.9998837 1.001039

# #scatterplot of fecundity vs protein measurement for resistant vs resilient
# fecundprotein <- ggplot(datum2,aes(x=Protein,y=AvgFecundity,color=BleachingStatus))+
#   geom_point()+
#   theme_classic()+
#   scale_color_manual(name="Bleaching Status",values=c("#7570B3","#1B9E77"))+
#   labs(x=bquote('Protein Content'~(mu*g/cm^2)),y="Relative Fecundity (average oocytes/polyp/slide)")
# #fecundprotein
# #very few colonies measured both for histology and protein so not a very informative visual

# #fecundity and protein correlation: no correlation
# results5=lm(AvgFecundity~Protein+BleachingStatus+Protein:BleachingStatus,data=datum2)
# #summary(results5) 

#testing for an interaction in size/bleaching/fecundity data: not significant
datum2$BleachingStatus=as.factor(datum2$BleachingStatus)
results4=glm(AvgFecundity~BleachingStatus+ColonySize+(BleachingStatus*ColonySize),data=datum2,family=poisson)
summary(results4)

#glm with poisson distribution including depth as a factor and an interaction
datum$depth_m=as.factor(datum$depth_m)
results7=glm(Average~BleachingStatus+depth_m+BleachingStatus:depth_m,data=datum,family=poisson)
summary(results7)
exp(confint(results7))
#singularities present, interaction NA
#variables are collinear

#linear regression just for resistant
head(datum2)
resistant=datum2[datum2$BleachingStatus=="resistant",]
res_lm=lm(AvgFecundity~ColonySize,data=resistant)
summary(res_lm)
      # MODEL RESULTS
          # Residuals:
          #   Min      1Q  Median      3Q     Max 
          # -3.2709 -0.5156  0.3235  0.7979  1.5472 
          # 
          # Coefficients:
          #              Estimate Std. Error t value Pr(>|t|)   
          # (Intercept) 1.8875500  0.5189497   3.637  0.00175 **
          # ColonySize  0.0014871  0.0007162   2.076  0.05167 . 
          # ---
          #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
          # 
          # Residual standard error: 1.34 on 19 degrees of freedom
          # Multiple R-squared:  0.1849,	Adjusted R-squared:  0.1421 
          # F-statistic: 4.311 on 1 and 19 DF,  p-value: 0.05167

#linear regression for just recovered
recovered=datum2[datum2$BleachingStatus=="resilient",]
rec_lm=lm(AvgFecundity~ColonySize,data=recovered)
summary(rec_lm)
      # MODEL RESULTS
          # Residuals:
          #   Min      1Q  Median      3Q     Max 
          # -0.8553 -0.8522 -0.8477  0.9801  2.4830 
          # 
          # Coefficients:
          #              Estimate Std. Error t value Pr(>|t|)
          # (Intercept)  8.559e-01  5.394e-01   1.587     0.13
          # ColonySize  -1.862e-05  1.429e-03  -0.013     0.99
          # 
          # Residual standard error: 1.288 on 18 degrees of freedom
          # Multiple R-squared:  9.434e-06,	Adjusted R-squared:  -0.05555 
          # F-statistic: 0.0001698 on 1 and 18 DF,  p-value: 0.9897

# #poisson regression just for resistant
# head(datum2)
# resistant=datum2[datum2$BleachingStatus=="resistant",]
# res_poi=glm(AvgFecundity~ColonySize,data=resistant,family=poisson)
# summary(res_poi)

# #poisson regression for just recovered
# recovered=datum2[datum2$BleachingStatus=="resilient",]
# rec_poi=glm(AvgFecundity~ColonySize,data=recovered,family=poisson)
# summary(rec_poi)

# #testing effect of depth on fecundity
# testdepth = glm(Average~depth_m,data=datum,family=poisson)
# summary(testdepth)
# #significant, shallower depths are more fecund (p = 0.0009); obviously bc that's where the resistant ones were!