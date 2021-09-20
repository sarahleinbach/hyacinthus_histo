# CODE FROM LEINBACH ET AL. 2021 - Coral recovery, but at a cost: energetic and reproductive consequences of divergent bleaching responses   
# large sections of commented out code are analyses/graphs not included in the paper OR model outputs - model outputs are indented
# only model outputs from results in the paper are included in the script

setwd("/Users/lumosmaximma/Desktop/coral/histology")
library(tidyverse)
datum=read.csv("EggStages.csv")

# #distribution of egg stages as histogram (ugly)
# eggstagehist <- ggplot(data=datum,aes(x=Stage,fill=BleachingStatus))+
#   geom_histogram(binwidth=1,alpha=.5,position="identity")+
#   theme_classic()+
#   scale_fill_brewer(name="Survival Strategy",palette="Dark2")+
#   labs(x="Egg Stage",y="count")
# eggstagehist

# #density plot of egg stage distribution
# eggstagedens <- ggplot(data=datum,aes(x=Stage,fill=BleachingStatus))+
#   geom_density(alpha=.3)+
#   theme_classic()+
#   scale_fill_brewer(name="Survival Strategy",palette="Dark2")+
#   labs(x="Stage",y="density")+
#   scale_x_discrete(labels=c("resilient"="recovered","resistant"="resistant"))
# eggstagedens

# #proportion of colonies with each egg stage
# datum$Stage=as.factor(datum$Stage)
# eggstages <- ggplot(data=datum)+
#   geom_bar(mapping=aes(x=BleachingStatus,fill=Stage),position="fill")+
#   theme_classic()+
#   scale_fill_brewer(name="Oocyte Stage",labels=c("Stage II","Stage III","Stage IV"),palette="Dark2")+
#   labs(x="Survival Strategy",y="Proportion")+
#   scale_x_discrete(labels=c("resilient"="recovered","resistant"="resistant"))
# eggstages

# #loglinear model differences in egg stages between resistant and resilient, WITHOUT depth in model
# datum2=read.csv("EggStagesRComp.csv")
# library(lme4)
# results=glmer(Number~EggStage+BleachingStatus+EggStage:BleachingStatus+(1|Sample),data=datum2,family=poisson)
# summary(results)

#loglinear model differences in egg stages between resistant and resilient, WITH depth in model
datum2=read.csv("EggStagesRComp.csv")
library(lme4)
results2=glmer(Number~EggStage+BleachingStatus+depth_m+EggStage:BleachingStatus+(1|Sample),data=datum2,family=poisson)
summary(results2)
      # MODEL RESULTS
          # AIC      BIC   logLik deviance df.resid 
          # 1332.5   1347.3   -660.2   1320.5       81 
          # 
          # Scaled residuals: 
          #   Min     1Q Median     3Q    Max 
          # -2.929 -2.727 -1.915  2.910  6.723 
          # 
          # Random effects:
          #   Groups Name        Variance Std.Dev.
          #   Sample (Intercept) 0.2116   0.46    
          # Number of obs: 87, groups:  Sample, 29
          # 
          # Fixed effects:
          #                                   Estimate Std. Error z value Pr(>|z|)   
          # (Intercept)                        2.52227    0.87738   2.875  0.00404 **
          # EggStage                           0.03227    0.12587   0.256  0.79767   
          # BleachingStatusresistant          -0.27137    0.66247  -0.410  0.68208   
          # depth_m                           -0.10319    0.06137  -1.681  0.09270 . 
          # EggStage:BleachingStatusresistant -0.00186    0.13846  -0.013  0.98928   
          # ---
          #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
          # 
          # Correlation of Fixed Effects:
          #   (Intr) EggStg BlchnS dpth_m
          # EggStage    -0.433                     
          # BlchngSttsr -0.922  0.574              
          # depth_m     -0.869  0.000  0.689       
          # EggStg:BlcS  0.394 -0.909 -0.631  0.000

# #loglinear model differences in sperm stages between resistant and resilient, WITHOUT depth in model
# datum3=read.csv("SpermStages.csv")
# library(lme4)
# results3=glmer(Number~Stage+BleachingStatus+Stage:BleachingStatus+(1|Sample),data=datum3,family=poisson)
# summary(results3)

#loglinear model differences in sperm stages between resistant and resilient, WITH depth in model
datum3=read.csv("SpermStages.csv")
library(lme4)
results4=glmer(Number~Stage+BleachingStatus+depth_m+Stage:BleachingStatus+(1|Sample),data=datum3,family=poisson)
summary(results4)
      # MODEL RESULTS
          #   AIC      BIC   logLik deviance df.resid 
          # 288.2    301.1   -138.1    276.2       58 
          # 
          # Scaled residuals: 
          #   Min      1Q  Median      3Q     Max 
          # -2.9095 -0.6292 -0.1153  0.5016  4.1384 
          # 
          # Random effects:
          #   Groups Name        Variance Std.Dev.
          # Sample (Intercept) 0.03779  0.1944  
          # Number of obs: 64, groups:  Sample, 32
          # 
          # Fixed effects:
          #                                  Estimate Std. Error z value Pr(>|z|)    
          # (Intercept)                     7.379e+00  7.340e-01  10.053  < 2e-16 ***
          # Stage                          -4.185e+00  5.804e-01  -7.210 5.59e-13 ***
          # BleachingStatusresistant       -2.902e-01  7.381e-01  -0.393    0.694    
          # depth_m                         7.606e-07  3.313e-02   0.000    1.000    
          # Stage:BleachingStatusresistant  1.396e-01  6.704e-01   0.208    0.835    
          # ---
          #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
          # 
          # Correlation of Fixed Effects:
          #   (Intr) Stage  BlchnS dpth_m
          # Stage       -0.803                     
          # BlchngSttsr -0.864  0.798              
          # depth_m     -0.581  0.000  0.353       
          # Stg:BlchngS  0.695 -0.866 -0.922  0.000
