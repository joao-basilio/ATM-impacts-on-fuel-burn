rm(list=ls())

library(jtools)
library(huxtable)
library(officer)
library(flextable)
library(tidyverse)
library(coefplot)
library(IDPmisc)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))## ** this file location as wd **


#########################################################
# 2019
########################################################
load("data2019/dataset2019.rda")

#--tira Inf, NA e NaN------------------

dataset2019 <- NaRV.omit(dataset2019)

#---novas variaveis -------------------

dataset2019$TOW_proxy <- dataset2019$TOW+(dataset2019$fuel*0.05*0.72)
dataset2019$route <- as.factor(dataset2019$route)
dataset2019$periodo_dia <- as.factor(dataset2019$periodo_dia)
dataset2019$equip <- as.factor(dataset2019$equip)
dataset2019$matricula <- as.factor(dataset2019$matricula)
baseALL <- dataset2019


#------------------------------------
# resultados fuel
#------------------------------------
#---B738 -------------------------------------------------------

baseB738 <- dataset2019 %>% slice(which(equip == 'B738'))

fitB738 <- lm(fuel ~ VIC+VID+HFI_climb+HFI_cruise+HFI_desc+
                age+TOW_proxy:route+periodo_dia+
                route,data = baseB738)

#---A320 -------------------------------------------------------

baseA320 <- dataset2019%>% slice(which(equip == 'A320'))

fitA320 <- lm(fuel ~ VIC+VID+HFI_climb+HFI_cruise+HFI_desc+
                age+TOW_proxy:route+periodo_dia+empresa_TAM+
                route, data = baseA320)

#---A319 -------------------------------------------------------

baseA319 <- dataset2019%>% slice(which(equip == 'A319'))

fitA319 <- lm(fuel ~ VIC+VID+HFI_climb+HFI_cruise+HFI_desc+
                age+TOW_proxy:route+periodo_dia+empresa_TAM+
                route, data = baseA319)

#---All -------------------------------------------------------

fit <- lm(fuel ~ VIC+VID+HFI_climb+HFI_cruise+HFI_desc+
            age+TOW_proxy:route+equip:route+periodo_dia+empresa_TAM,
          data = dataset2019, na.action = na.omit)


#-----------------------------
#--Analises fuel consumption
#----------------------------

ALL <- confint(fit, c('VIC','VID', 'HFI_climb', 'HFI_cruise', 'HFI_desc'), level=0.95)

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
                        'HFI_desc_L' ,'HFI_desc_U', 'orig','dest')]


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
                        'HFI_desc_L' ,'HFI_desc_U', 'orig','dest')]


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
                        'HFI_desc_L' ,'HFI_desc_U', 'orig','dest')]


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
                      'HFI_desc_L' ,'HFI_desc_U', 'orig','dest')]

resultados <- rbind(baseB738, baseA320, baseA319, baseALL) %>%
  mutate(Route = paste(orig,dest,sep = "_to_"),
         Total_L = (VIC_L+VID_L+HFI_climb_L+HFI_cruise_L+HFI_desc_L),
         Total_U = (VIC_U+VID_U+HFI_climb_U+HFI_cruise_U+HFI_desc_U))%>%
  group_by(Route)%>%
  dplyr::summarise(VIC_L = mean(VIC_L),VIC_U = mean(VIC_U), VID_L = mean(VID_L),
            VID_U = mean(VID_U), HFI_climb_L = mean(HFI_climb_L),
            HFI_climb_U = mean(HFI_climb_U), HFI_cruise_L = mean(HFI_cruise_L),
            HFI_cruise_U = mean(HFI_cruise_U) , HFI_desc_L = mean(HFI_desc_L) , 
            HFI_desc_U = mean(HFI_desc_U), Total_L = mean(Total_L),
            Total_U = mean(Total_U))

## juntar ----------------------------------


resultados[,2:13] <- trunc(resultados[,2:13])

resultados <- resultados %>%
  arrange(Total_U)%>%
  mutate(VIC = paste(VIC_L,VIC_U,sep = "   -   "),
         VID = paste(VID_L, VID_U, sep = "   -   "),
         HFI_climb = paste(HFI_climb_L,HFI_climb_U,sep = "   -   "),
         HFI_cruise = paste(HFI_cruise_L,HFI_cruise_U,sep = "   -   "),
         HFI_desc = paste(HFI_desc_L,HFI_desc_U,sep = "   -   "),
         Total = paste(Total_L,Total_U,sep = "   -   "))

resultados <- resultados[,c('Route', 'VIC', 'VID', 'HFI_climb', 'HFI_cruise',
                            'HFI_desc','Total')]

ft <- flextable(resultados)

save_as_docx("my table" = ft, 
             path = "table_results per route 2019.docx")


#########################################################
# 2017
########################################################
load("data2017/dataset2017.rda")

#--tira Inf, NA e NaN------------------

dataset2017 <- NaRV.omit(dataset2017)

#---novas variaveis -------------------

dataset2017$TOW_proxy <- dataset2017$TOW+(dataset2017$fuel*0.05*0.72)
dataset2017$route <- as.factor(dataset2017$route)
dataset2017$periodo_dia <- as.factor(dataset2017$periodo_dia)
dataset2017$equip <- as.factor(dataset2017$equip)
dataset2017$matricula <- as.factor(dataset2017$matricula)
baseALL <- dataset2017


#------------------------------------
# resultados fuel
#------------------------------------
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

#-----------------------------
#--Analises fuel consumption
#----------------------------

ALL <- confint(fit, c('VIC','VID', 'HFI_climb', 'HFI_cruise', 'HFI_desc'), level=0.95)

B738 <- confint(fitB738, c('VIC','VID', 'HFI_climb','HFI_cruise', 'HFI_desc'), level=0.95)

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
                        'HFI_desc_L' ,'HFI_desc_U', 'orig','dest')]


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
                        'HFI_desc_L' ,'HFI_desc_U', 'orig','dest')]


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
                        'HFI_desc_L' ,'HFI_desc_U', 'orig','dest')]


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
                      'HFI_desc_L' ,'HFI_desc_U', 'orig','dest')]

resultados <- rbind(baseB738, baseA320, baseA319, baseALL) %>%
  mutate(Route = paste(orig,dest,sep = "_to_"),
         Total_L = (VIC_L+VID_L+HFI_climb_L+HFI_cruise_L+HFI_desc_L),
         Total_U = (VIC_U+VID_U+HFI_climb_U+HFI_cruise_U+HFI_desc_U))%>%
  group_by(Route)%>%
  dplyr::summarise(VIC_L = mean(VIC_L),VIC_U = mean(VIC_U), VID_L = mean(VID_L),
                   VID_U = mean(VID_U), HFI_climb_L = mean(HFI_climb_L),
                   HFI_climb_U = mean(HFI_climb_U), HFI_cruise_L = mean(HFI_cruise_L),
                   HFI_cruise_U = mean(HFI_cruise_U) , HFI_desc_L = mean(HFI_desc_L) , 
                   HFI_desc_U = mean(HFI_desc_U), Total_L = mean(Total_L),
                   Total_U = mean(Total_U))

## juntar ----------------------------------


resultados[,2:13] <- trunc(resultados[,2:13])

resultados <- resultados %>%
  arrange(Total_U)%>%
  mutate(VIC = paste(VIC_L,VIC_U,sep = "   -   "),
         VID = paste(VID_L, VID_U, sep = "   -   "),
         HFI_climb = paste(HFI_climb_L,HFI_climb_U,sep = "   -   "),
         HFI_cruise = paste(HFI_cruise_L,HFI_cruise_U,sep = "   -   "),
         HFI_desc = paste(HFI_desc_L,HFI_desc_U,sep = "   -   "),
         Total = paste(Total_L,Total_U,sep = "   -   "))

resultados <- resultados[,c('Route', 'VIC', 'VID', 'HFI_climb', 'HFI_cruise',
                            'HFI_desc','Total')]

ft <- flextable(resultados)

save_as_docx("my table" = ft, 
             path = "table_results per route 2017.docx")
