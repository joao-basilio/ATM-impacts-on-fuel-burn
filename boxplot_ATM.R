rm(list=ls())

library(ggplot2)
library(tidyr)
library(ggthemes)
library(dplyr)
library(forcats)
library(ggpubr)
library(cowplot)
library(tidyverse)
library(hrbrthemes)
library(viridis)


#--2019------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))## ** this file location as wd **

load("data2017/dataset2017.rda")
dataset2017$year <- "2017"
dataset2017$empresa_ONE <- NULL

load("data2019/dataset2019.rda")
dataset2019$year <- "2019"

df <- rbind(dataset2017, dataset2019)%>%
  select(VIC, VID, HFI_climb, HFI_desc, HFI_cruise, year, indicat)%>%
  gather(variable, value, -c(year, indicat))%>%
  group_by(variable, year)%>%
  subset(value<quantile(value, probs = 0.99, na.rm = T))

df1 <- rbind(dataset2017, dataset2019)

####################################################################

HE <- df %>% subset(str_detect(variable, "HFI_"))

ggplot(HE, aes(year,value))+
  geom_boxplot(position="dodge",aes(fill=year))+
  facet_wrap(~variable, ncol=3,scales = "free")+
  labs(y = "Path stretch (nautical miles)")+
  theme(legend.position = "none") +coord_flip()
  

VE <- df %>% subset(str_detect(variable, "VI"))

ggplot(VE, aes(year,value))+
  geom_boxplot(position="dodge",aes(fill=year))+
  facet_wrap(~variable, ncol=3,scales = "free")+
  labs(y = "% of climb or descent distance")+
  theme(legend.position = "none")+coord_flip()




x <- boxplot(dataset2019$VID)

NROW(subset(dataset2019,VID == 0))/NROW(dataset2019)
NROW(subset(dataset2017,VID == 0))/NROW(dataset2017)

NROW(subset(dataset2019,VIC == 0))/NROW(dataset2019)
NROW(subset(dataset2017,VIC == 0))/NROW(dataset2017)


NROW(subset(df1,df1$VID == 0))/NROW(df1)

