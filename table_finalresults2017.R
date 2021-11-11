rm(list=ls())

#------------------------------------------------------------------------

library(jtools)
library(huxtable)
library(officer)
library(flextable)
library(tidyverse)
library(coefplot)
library(IDPmisc)

#------------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))## ** this file location as wd **
load("data2017/dataset2017.rda")

#--tira Inf, NA e NaN------------------------------------------------------

#dataset2017 <- NaRV.omit(dataset2017)

#---novas variaveis -------------------------------------------------------

dataset2017$TOW_proxy <- dataset2017$TOW+(dataset2017$fuel*0.05*0.72)
dataset2017$route <- as.factor(dataset2017$route)
dataset2017$periodo_dia <- as.factor(dataset2017$periodo_dia)
dataset2017$equip <- as.factor(dataset2017$equip)
dataset2017$regist <- as.factor(dataset2017$regist)
dataset2017$landing_threshould <- as.numeric(dataset2017$landing_threshould)


#--- threshold -------------------------------------------------------

#hist(as.numeric(dataset2017$takeoff_threshould))
# threshould #####

dataset2017 <- dataset2017%>%
  mutate(landing_threshould = as.numeric(landing_threshould))%>%
  mutate(landing_threshould = ifelse(dest == 'CGH' & (landing_threshould >= 80 & landing_threshould <= 260), 170,landing_threshould))%>%
  mutate(landing_threshould = ifelse(dest == 'CGH' & (landing_threshould > 260 | landing_threshould <  80 ), 350,landing_threshould))%>%
  
  mutate(landing_threshould = ifelse(dest == 'GRU' & (landing_threshould > 0   & landing_threshould < 180) , 090,landing_threshould))%>%
  mutate(landing_threshould = ifelse(dest == 'GRU' & (landing_threshould < 360 & landing_threshould > 180) , 270,landing_threshould))%>%
  
  mutate(landing_threshould = ifelse(dest == 'SSA' & (landing_threshould > 125 & landing_threshould < 215) , 170,landing_threshould))%>%
  mutate(landing_threshould = ifelse(dest == 'SSA' & (landing_threshould > 305 | landing_threshould < 45)  , 350,landing_threshould))%>%
  mutate(landing_threshould = ifelse(dest == 'SSA' & (landing_threshould > 45  & landing_threshould < 145) , 100,landing_threshould))%>%
  mutate(landing_threshould = ifelse(dest == 'SSA' & (landing_threshould > 215 & landing_threshould < 305) , 280,landing_threshould))%>%
  
  mutate(landing_threshould = ifelse(dest == 'REC' & (landing_threshould > 90  & landing_threshould < 270) , 180,landing_threshould))%>%
  mutate(landing_threshould = ifelse(dest == 'REC' & (landing_threshould < 90  | landing_threshould > 270) , 350,landing_threshould))%>%
  
  mutate(landing_threshould = ifelse(dest == 'CWB' & (landing_threshould < 30  | landing_threshould > 311) , 330,landing_threshould))%>%
  mutate(landing_threshould = ifelse(dest == 'CWB' & (landing_threshould < 311 & landing_threshould > 260) , 290,landing_threshould))%>%
  mutate(landing_threshould = ifelse(dest == 'CWB' & (landing_threshould < 260 & landing_threshould > 130) , 150,landing_threshould))%>%
  mutate(landing_threshould = ifelse(dest == 'CWB' & (landing_threshould < 129 & landing_threshould > 30)  , 110,landing_threshould))%>%
  
  mutate(landing_threshould = ifelse(dest == 'POA' & (landing_threshould < 200 & landing_threshould > 20)  , 110,landing_threshould))%>%
  mutate(landing_threshould = ifelse(dest == 'POA' & (landing_threshould < 20  | landing_threshould > 200) , 290,landing_threshould))%>%
  
  mutate(landing_threshould = ifelse(dest == 'CNF' & (landing_threshould < 250 & landing_threshould > 70)  , 160,landing_threshould))%>%
  mutate(landing_threshould = ifelse(dest == 'CNF' & (landing_threshould < 70  | landing_threshould > 250) , 340,landing_threshould))%>%
  
  mutate(landing_threshould = ifelse(dest == 'BSB' & (landing_threshould < 200 & landing_threshould > 20)  , 110,landing_threshould))%>%
  mutate(landing_threshould = ifelse(dest == 'BSB' & (landing_threshould < 359 & landing_threshould > 200) , 290,landing_threshould))%>%
  
  mutate(landing_threshould = ifelse(dest == 'SDU' & (landing_threshould <= 110 | landing_threshould > 290) , 020,landing_threshould))%>%
  mutate(landing_threshould = ifelse(dest == 'SDU' & (landing_threshould <= 290 & landing_threshould > 110) , 200,landing_threshould))%>%
  
  ## takeoff ------------------------------
  
  mutate(takeoff_threshould = as.numeric(takeoff_threshould))%>%
  mutate(takeoff_threshould = ifelse(dest == 'CGH' & (takeoff_threshould >= 80 & takeoff_threshould <= 260), 170,takeoff_threshould))%>%
  mutate(takeoff_threshould = ifelse(dest == 'CGH' & (takeoff_threshould > 260 | takeoff_threshould <  80 ), 350,takeoff_threshould))%>%
  
  mutate(takeoff_threshould = ifelse(dest == 'GRU' & (takeoff_threshould > 0   & takeoff_threshould <= 180) , 090,takeoff_threshould))%>%
  mutate(takeoff_threshould = ifelse(dest == 'GRU' & (takeoff_threshould < 360 & takeoff_threshould > 180) , 270,takeoff_threshould))%>%
  
  mutate(takeoff_threshould = ifelse(dest == 'SSA' & (takeoff_threshould > 125 & takeoff_threshould < 215) , 170,takeoff_threshould))%>%
  mutate(takeoff_threshould = ifelse(dest == 'SSA' & (takeoff_threshould > 305 | takeoff_threshould < 45)  , 350,takeoff_threshould))%>%
  mutate(takeoff_threshould = ifelse(dest == 'SSA' & (takeoff_threshould > 45  & takeoff_threshould < 145) , 100,takeoff_threshould))%>%
  mutate(takeoff_threshould = ifelse(dest == 'SSA' & (takeoff_threshould > 215 & takeoff_threshould < 305) , 280,takeoff_threshould))%>%
  
  mutate(takeoff_threshould = ifelse(dest == 'REC' & (takeoff_threshould > 90  & takeoff_threshould < 270) , 180,takeoff_threshould))%>%
  mutate(takeoff_threshould = ifelse(dest == 'REC' & (takeoff_threshould < 90  | takeoff_threshould > 270) , 350,takeoff_threshould))%>%
  
  mutate(takeoff_threshould = ifelse(dest == 'CWB' & (takeoff_threshould < 30  | takeoff_threshould > 311) , 330,takeoff_threshould))%>%
  mutate(takeoff_threshould = ifelse(dest == 'CWB' & (takeoff_threshould <= 311 & takeoff_threshould > 260) , 290,takeoff_threshould))%>%
  mutate(takeoff_threshould = ifelse(dest == 'CWB' & (takeoff_threshould < 260 & takeoff_threshould > 130) , 150,takeoff_threshould))%>%
  mutate(takeoff_threshould = ifelse(dest == 'CWB' & (takeoff_threshould < 129 & takeoff_threshould > 30)  , 110,takeoff_threshould))%>%
  
  mutate(takeoff_threshould = ifelse(dest == 'POA' & (takeoff_threshould < 200 & takeoff_threshould > 20)  , 110,takeoff_threshould))%>%
  mutate(takeoff_threshould = ifelse(dest == 'POA' & (takeoff_threshould < 20  | takeoff_threshould > 200) , 290,takeoff_threshould))%>%
  
  mutate(takeoff_threshould = ifelse(dest == 'CNF' & (takeoff_threshould <= 250 & takeoff_threshould > 70)  , 160,takeoff_threshould))%>%
  mutate(takeoff_threshould = ifelse(dest == 'CNF' & (takeoff_threshould <= 70  | takeoff_threshould > 250) , 340,takeoff_threshould))%>%
  
  mutate(takeoff_threshould = ifelse(dest == 'BSB' & (takeoff_threshould < 200 & takeoff_threshould > 20)  , 110,takeoff_threshould))%>%
  mutate(takeoff_threshould = ifelse(dest == 'BSB' & (takeoff_threshould < 359 & takeoff_threshould > 200) , 290,takeoff_threshould))%>%
  
  mutate(takeoff_threshould = ifelse(dest == 'SDU' & (takeoff_threshould <= 110 | takeoff_threshould > 290) , 020,takeoff_threshould))%>%
  mutate(takeoff_threshould = ifelse(dest == 'SDU' & (takeoff_threshould <= 290 & takeoff_threshould > 110) , 200,takeoff_threshould))
  
baseALL <- dataset2017


#x <- as.data.frame(unique(dataset2017$landing_threshould))

#------------------------------------------------------------------------
# resultados fuel
#------------------------------------------------------------------------
#---B738 -------------------------------------------------------

baseB738 <- dataset2017 %>% slice(which(equip == 'B738'))

fitB738 <- lm(fuel ~ VIC+VID+HFI_climb+HFI_cruise+HFI_desc+
                age+TOW_proxy:route+periodo_dia+
                route,data = baseB738)

#---A320 -------------------------------------------------------

baseA320 <- dataset2017%>% slice(which(equip == 'A320'))

fitA320 <- lm(fuel ~ VIC+VID+HFI_climb+HFI_cruise+HFI_desc+
                age+TOW_proxy:route+periodo_dia+empresa_TAM+
                route, data = baseA320)

#---A319 -------------------------------------------------------

baseA319 <- dataset2017%>% slice(which(equip == 'A319'))

fitA319 <- lm(fuel ~ VIC+VID+HFI_climb+HFI_cruise+HFI_desc+
                age+TOW_proxy:route+periodo_dia+empresa_TAM+
                route, data = baseA319)

#---All -------------------------------------------------------

fit <- lm(fuel ~ VIC+VID+HFI_climb+HFI_cruise+HFI_desc+
            age+TOW_proxy:route+equip:route+periodo_dia+empresa_TAM,
          data = dataset2017, na.action = na.omit)


#-----------------------------------------------------------------
#--Analises fuel consumption
#----------------------------------------------------------------

ALL <- as.data.frame(confint(fit, c('VIC','VID', 'HFI_climb','HFI_cruise', 'HFI_desc'), level=0.95))

B738 <- confint(fitB738, c('VIC','VID', 'HFI_climb', 'HFI_cruise', 'HFI_desc'), level=0.95)

A320 <- confint(fitA320, c('VIC','VID', 'HFI_climb', 'HFI_cruise', 'HFI_desc'), level=0.95)

A319 <- confint(fitA319, c('VIC','VID', 'HFI_climb', 'HFI_cruise', 'HFI_desc'), level=0.95)

tudo <- as.data.frame(cbind(B738, A320, A319, ALL))

colnames(tudo) <- c("B738_L","B738_U","A320_L","A320_U","A319_L","A319_U","ALL_L","ALL_U")


##B738-------------------------------------------------------
baseB738$VIC_L <- baseB738$VIC*B738[1,1]
baseB738$VIC_U <- baseB738$VIC*B738[1,2]
baseB738$VID_L <- baseB738$VID*B738[2,1]
baseB738$VID_U <- baseB738$VID*B738[2,2]
baseB738$HFI_climb_L <- baseB738$HFI_climb*B738[3,1]
baseB738$HFI_climb_U <- baseB738$HFI_climb*B738[3,2]
baseB738$HFI_cruise_L <- baseB738$HFI_cruise*B738[4,1]
baseB738$HFI_cruise_U <- baseB738$HFI_cruise*B738[4,2]
baseB738$HFI_desc_L <- baseB738$HFI_desc*B738[5,1]
baseB738$HFI_desc_U <- baseB738$HFI_desc*B738[5,2]

baseB738 <- baseB738[,c('VIC_L','VIC_U', 'VID_L','VID_U', 'HFI_climb_L',
                        'HFI_climb_U', 'HFI_cruise_L', 'HFI_cruise_U' , 
                        'HFI_desc_L' ,'HFI_desc_U')]

baseB738 <- baseB738 %>%
  na.omit()%>%
  summarise(VIC_L = mean(VIC_L),VIC_U = mean(VIC_U), VID_L = mean(VID_L),
            VID_U = mean(VID_U), HFI_climb_L = mean(HFI_climb_L),
            HFI_climb_U = mean(HFI_climb_U), HFI_cruise_L = mean(HFI_cruise_L),
            HFI_cruise_U = mean(HFI_cruise_U) , HFI_desc_L = mean(HFI_desc_L) , HFI_desc_U = mean(HFI_desc_U))

##A320-------------------------------------------------------
baseA320$VIC_L <- baseA320$VIC*A320[1,1]
baseA320$VIC_U <- baseA320$VIC*A320[1,2]
baseA320$VID_L <- baseA320$VID*A320[2,1]
baseA320$VID_U <- baseA320$VID*A320[2,2]
baseA320$HFI_climb_L <- baseA320$HFI_climb*A320[3,1]
baseA320$HFI_climb_U <- baseA320$HFI_climb*A320[3,2]
baseA320$HFI_cruise_L <- baseA320$HFI_cruise*A320[4,1]
baseA320$HFI_cruise_U <- baseA320$HFI_cruise*A320[4,2]
baseA320$HFI_desc_L <- baseA320$HFI_desc*A320[5,1]
baseA320$HFI_desc_U <- baseA320$HFI_desc*A320[5,2]

baseA320 <- baseA320[,c('VIC_L','VIC_U', 'VID_L','VID_U', 'HFI_climb_L',
                        'HFI_climb_U', 'HFI_cruise_L', 'HFI_cruise_U' , 
                        'HFI_desc_L' ,'HFI_desc_U')]

baseA320 <- baseA320 %>%
  na.omit()%>%
  summarise(VIC_L = mean(VIC_L),VIC_U = mean(VIC_U), VID_L = mean(VID_L),
            VID_U = mean(VID_U), HFI_climb_L = mean(HFI_climb_L),
            HFI_climb_U = mean(HFI_climb_U), HFI_cruise_L = mean(HFI_cruise_L),
            HFI_cruise_U = mean(HFI_cruise_U) , HFI_desc_L = mean(HFI_desc_L) , HFI_desc_U = mean(HFI_desc_U))

##A319-------------------------------------------------------
baseA319$VIC_L <- baseA319$VIC*A319[1,1]
baseA319$VIC_U <- baseA319$VIC*A319[1,2]
baseA319$VID_L <- baseA319$VID*A319[2,1]
baseA319$VID_U <- baseA319$VID*A319[2,2]
baseA319$HFI_climb_L <- baseA319$HFI_climb*A319[3,1]
baseA319$HFI_climb_U <- baseA319$HFI_climb*A319[3,2]
baseA319$HFI_cruise_L <- baseA319$HFI_cruise*A319[4,1]
baseA319$HFI_cruise_U <- baseA319$HFI_cruise*A319[4,2]
baseA319$HFI_desc_L <- baseA319$HFI_desc*A319[5,1]
baseA319$HFI_desc_U <- baseA319$HFI_desc*A319[5,2]

baseA319 <- baseA319[,c('VIC_L','VIC_U', 'VID_L','VID_U', 'HFI_climb_L',
                        'HFI_climb_U', 'HFI_cruise_L', 'HFI_cruise_U' , 
                        'HFI_desc_L' ,'HFI_desc_U')]

baseA319 <- baseA319 %>%
  na.omit()%>%
  summarise(VIC_L = mean(VIC_L),VIC_U = mean(VIC_U), VID_L = mean(VID_L),
            VID_U = mean(VID_U), HFI_climb_L = mean(HFI_climb_L),
            HFI_climb_U = mean(HFI_climb_U), HFI_cruise_L = mean(HFI_cruise_L),
            HFI_cruise_U = mean(HFI_cruise_U) , HFI_desc_L = mean(HFI_desc_L) , HFI_desc_U = mean(HFI_desc_U))

##ALL-------------------------------------------------------
baseALL$VIC_L <- baseALL$VIC*ALL[1,1]
baseALL$VIC_U <- baseALL$VIC*ALL[1,2]
baseALL$VID_L <- baseALL$VID*ALL[2,1]
baseALL$VID_U <- baseALL$VID*ALL[2,2]
baseALL$HFI_climb_L <- baseALL$HFI_climb*ALL[3,1]
baseALL$HFI_climb_U <- baseALL$HFI_climb*ALL[3,2]
baseALL$HFI_cruise_L <- baseALL$HFI_cruise*ALL[4,1]
baseALL$HFI_cruise_U <- baseALL$HFI_cruise*ALL[4,2]
baseALL$HFI_desc_L <- baseALL$HFI_desc*ALL[5,1]
baseALL$HFI_desc_U <- baseALL$HFI_desc*ALL[5,2]

baseALL <- baseALL[,c('VIC_L','VIC_U', 'VID_L','VID_U', 'HFI_climb_L',
                      'HFI_climb_U', 'HFI_cruise_L', 'HFI_cruise_U' , 
                      'HFI_desc_L' ,'HFI_desc_U')]

baseALL <- baseALL %>%
  na.omit()%>%
  summarise(VIC_L = mean(VIC_L),VIC_U = mean(VIC_U), VID_L = mean(VID_L),
            VID_U = mean(VID_U), HFI_climb_L = mean(HFI_climb_L),
            HFI_climb_U = mean(HFI_climb_U), HFI_cruise_L = mean(HFI_cruise_L),
            HFI_cruise_U = mean(HFI_cruise_U) , HFI_desc_L = mean(HFI_desc_L) , HFI_desc_U = mean(HFI_desc_U))

## juntar ----------------------------------

resultados <- rbind(baseB738, baseA320, baseA319, baseALL) 
resultados <- resultados %>% 
  mutate(Total_L = (VIC_L+VID_L+HFI_climb_L+HFI_cruise_L+HFI_desc_L),
         Total_U = (VIC_U+VID_U+HFI_climb_U+HFI_cruise_U+HFI_desc_U))%>%
  mutate_if(is.numeric, round)

resultados <- resultados %>%
  mutate(VIC = paste(VIC_L,VIC_U,sep = "  -  "),
         VID = paste(VID_L, VID_U, sep = "  -  "),
         HFI_climb = paste(HFI_climb_L,HFI_climb_U,sep = "  -  "),
         HFI_desc = paste(HFI_desc_L,HFI_desc_U,sep = "  -  "),
         HFI_cruise = paste(HFI_cruise_L,HFI_cruise_U,sep = "  -  "),
         Total = paste(Total_L,Total_U,sep = "  -  "))

resultados <- as.data.frame(t(resultados[,c('VIC', 'VID', 'HFI_climb', 'HFI_desc',
                            'HFI_cruise','Total')]))
  
names(resultados) <- c("B738","A320","A319","ALL")
  
ft <- flextable(resultados)

save_as_docx("my table" = ft, path = "table_results2017.docx")

