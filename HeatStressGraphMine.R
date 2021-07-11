library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(grDevices)
library(scales)

setwd("/Users/lumosmaximma/Desktop/coral/MooreaTemp")

ggplot(thermal_mean_and_2019_sub, aes(x=date, y=mean_daily_temp, fill=timeframe))+
  geom_line(aes(y=mean_daily_temp, color=timeframe))+
  geom_ribbon(aes(ymin=mean_daily_temp - sd, ymax=mean_daily_temp + sd), alpha=.2)+
  geom_hline(linetype = "longdash", yintercept=29, color="black",size=0.6)+
  scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%b"))+
  scale_color_manual(values=c("#4480AE","#99341B"))+
  labs(x="", y=expression("Temperature " ( degree*C)))+
  theme_classic()+
  theme(axis.text.x = element_text(colour="black", size=12), 
        axis.text.y = element_text(colour="black", size=14))+
  theme(axis.title=element_text(size=14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
