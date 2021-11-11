rm(list=ls())

library(jtools)
library(huxtable)
library(officer)
library(flextable)
library(tidyverse)
library(coefplot)
library(IDPmisc)
library(ggplot2)
library(ggridges)
library(ggpubr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))## ** this file location as wd **
load("data2017/dataset2017.rda")
dataset2017$empresa_ONE <- NULL


#--tira Inf, NA e NaN------------------

dataset2017 <- NaRV.omit(dataset2017)

#---novas variaveis -------------------

dataset2017$TOW_proxy <- dataset2017$TOW+(dataset2017$fuel*0.05*0.72)
dataset2017$route <- as.factor(dataset2017$route)
dataset2017$periodo_dia <- as.factor(dataset2017$periodo_dia)
dataset2017$equip <- as.factor(dataset2017$equip)
baseALL <- dataset2017

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

baseALL <- dataset2017%>%
  mutate(equip = "ALL")



ALL <- fit$coefficients[c("VIC","VID", "HFI_climb", "HFI_desc","HFI_cruise")]

B738 <- fitB738$coefficients[c("VIC","VID", "HFI_climb", "HFI_desc","HFI_cruise")]

A320 <- fitA320$coefficients[c("VIC","VID", "HFI_climb", "HFI_desc","HFI_cruise")]

A319 <- fitA319$coefficients[c("VIC","VID", "HFI_climb", "HFI_desc","HFI_cruise")]

tudo <- as.data.frame(cbind(B738, A320, A319, ALL))

colnames(tudo) <- c("B738","A320","A319","ALL")

dfs <- list(baseB738,baseA320,baseA319,baseALL)

coeftimesvar <- function(x){
  x <- x %>%
    select('VIC', 'VID', 'HFI_climb', 'HFI_cruise','HFI_desc', 'orig' , 'dest', 'equip', 'fuel')%>%
    mutate(Route = paste(orig,dest, sep = "_to_"))%>%
    mutate(VIC = VIC*fitB738$coefficients["VIC"],
           VID = VIC*fitB738$coefficients["VID"],
           HFI_climb = HFI_climb*fitB738$coefficients["HFI_climb"],
           HFI_cruise = HFI_cruise*fitB738$coefficients["HFI_cruise"],
           HFI_desc = HFI_desc*fitB738$coefficients["HFI_desc"])%>%
    mutate(Total = VIC+VID+HFI_climb+HFI_desc+HFI_cruise, Route = Route,
           Aircraft = equip)
  return(x)
  
}

result <- dfs %>%
  lapply(coeftimesvar)
## join ----------------------------------

RESULTADOS2017 <- bind_rows(result) %>%
  mutate(year = "2017")


#--------------------------------------------------------------------
#--------------------------------------------------------------------
#                              2019
#--------------------------------------------------------------------
#--------------------------------------------------------------------

load("data2019/dataset2019.rda")


#--tira Inf, NA e NaN------------------

dataset2019 <- NaRV.omit(dataset2019)

#---novas variaveis -------------------

dataset2019$TOW_proxy <- dataset2019$TOW+(dataset2019$fuel*0.05*0.72)
dataset2019$route <- as.factor(dataset2019$route)
dataset2019$periodo_dia <- as.factor(dataset2019$periodo_dia)
dataset2019$equip <- as.factor(dataset2019$equip)
baseALL <- dataset2019

#------------------------------------------------------------------------
# resultados fuel
#------------------------------------------------------------------------
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


baseALL <- dataset2019%>%
  mutate(equip = "ALL")


ALL <- fit$coefficients[c("VIC","VID", "HFI_climb", "HFI_desc","HFI_cruise")]

B738 <- fitB738$coefficients[c("VIC","VID", "HFI_climb", "HFI_desc","HFI_cruise")]

A320 <- fitA320$coefficients[c("VIC","VID", "HFI_climb", "HFI_desc","HFI_cruise")]

A319 <- fitA319$coefficients[c("VIC","VID", "HFI_climb", "HFI_desc","HFI_cruise")]

tudo <- as.data.frame(cbind(B738, A320, A319, ALL))

colnames(tudo) <- c("B738","A320","A319","ALL")

dfs <- list(baseB738,baseA320,baseA319,baseALL)

coeftimesvar <- function(x){
x <- x %>%
    select('VIC', 'VID', 'HFI_climb', 'HFI_cruise','HFI_desc', 'orig' , 'dest', 'equip', 'fuel')%>%
    mutate(Route = paste(orig,dest, sep = "_to_"))%>%
    mutate(VIC = VIC*fitB738$coefficients["VIC"],
           VID = VIC*fitB738$coefficients["VID"],
           HFI_climb = HFI_climb*fitB738$coefficients["HFI_climb"],
           HFI_cruise = HFI_cruise*fitB738$coefficients["HFI_cruise"],
           HFI_desc = HFI_desc*fitB738$coefficients["HFI_desc"])%>%
    mutate(Total = VIC+VID+HFI_climb+HFI_desc+HFI_cruise, Route = Route,
           Aircraft = equip)
 return(x)

}

result <- dfs %>%
  lapply(coeftimesvar)

## juntar ----------------------------------

RESULTADOS2019 <- bind_rows(result)%>%
  mutate(year = "2019")


RESULTADOS <- rbind(RESULTADOS2017, RESULTADOS2019)%>%
  filter(Total < quantile(Total, 0.99) & Total > quantile(Total, 0.01))


#--------------------------------------------------------------------
#--------------------------------------------------------------------
#                             IMAGEM
#--------------------------------------------------------------------
# cdf --------------------------------------------------------------------
library(plyr)
mu <- ddply(RESULTADOS, c("Aircraft", "year"), summarise, grp.mean=mean((Total/fuel)*100))


ggplot(RESULTADOS, aes(x = Total)) +
  stat_ecdf(aes(color = year,linetype = year), 
            geom = "step", size = 1.5) +
  facet_grid(~factor(Aircraft, levels = c("B738","A320","A319","ALL"))) +
  labs(y = "Cumulative Percent",x =  "Additional fuel (liters)")


#----------
# ggplot(RESULTADOS, aes(x = Total,y = ..density.. ,
#                        fill=year, color=year)) +
#   stat_ecdf(position="identity", alpha=0.5)+
#   facet_grid(.~factor(Aircraft, levels = c("B738", "A320", "A319", "ALL"))) 

# pdf -------------------------------------------------------


ggplot(RESULTADOS, aes(x = (Total/fuel)*100,y = ..density.. ,
                       fill=year, color=year)) +
  geom_histogram(position="identity", alpha=0.5)+
  facet_grid(.~factor(Aircraft, levels = c("B738", "A320", "A319", "ALL"))) +
  
  geom_vline(data=mu, aes(xintercept=grp.mean, color = year), linetype="dashed")+
  
  labs(x =  "% of trip fuel")+
  
  scale_y_continuous(labels = scales::percent)




## pdf e cdf -------------------------------------------------------------


ggarrange(
  x,y, nrow = 2,
  labels = c("(A) Cumulative Distribution", "(B) Relative frequency")
) 



#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
##                             rotas
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#-- total cada ineficiencia------------------------------------------------------------------

rotas <- RESULTADOS %>%
  select(VIC, VID, HFI_climb, HFI_desc, HFI_cruise, year, Route)%>%
  gather(variable, value, -c(year, Route))%>%
  dplyr::group_by(Route,variable, year)%>%
  dplyr::summarise(mean_inef = mean(value), total_inef = sum(value))

rotas2017x <- rotas %>% subset(year == "2017")
rotas2019x <- rotas %>% subset(year == "2019")

#----- imagem total cada ineficiencia-------------------------------------------------------


ggplot(rotas2017x,aes(y = total_inef, x = reorder(Route, -total_inef), fill = variable)) + 
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  labs(y = "liters of fuel",x =  "Route", title = "2017")


#----------------------------------------------------------------

ggplot(rotas2019x) + 
  geom_bar(aes(y = total_inef, x =  reorder(Route, -total_inef), fill = variable),
           stat="identity")+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  labs(y = "liters of fuel",x =  "Route", title = "2019")




##- media de cada ineficiencia por rota ############################

ggplot(rotas2017x) + 
  geom_bar(aes(y = mean_inef, x = reorder(Route, -mean_inef), fill = variable),
           stat="identity")+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  labs(y = "liters of fuel",x =  "Route")


ggplot(rotas2019x) + 
  geom_bar(aes(y = mean_inef, x = reorder(Route, -mean_inef), fill = variable),
           stat="identity")+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  labs(y = "liters of fuel",x =  "Route")



