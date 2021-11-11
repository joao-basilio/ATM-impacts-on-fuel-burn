rm(list=ls())

library(data.table)
library(purrr)
library(geosphere)
library(ggplot2)
library(tidyverse)
library(foreign)
library(fastDummies)
library(iterators)
library(lubridate)
library(SEAsic)
library(rlang)
library(olsrr)
library(readxl)
library(haven)
library(janitor)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))## ** this file location as wd **

load("data2017/trajectories_2017.rda")
total_all <- track_data2 %>% subset(lon <= 180)%>%
  #--apenas 20 main routes
  mutate(route = paste(orig,dest, sep = ""))%>%
  subset(grepl("GRUCNF|CNFGRU|CGHCNF|CWBGRU|CWBCGH|GRUREC|CNFCGH|CGHPOA|POACGH|CGHCWB|POAGRU|GRUPOA|RECGRU|GRUCWB|SDUCGH|CGHSDU|CGHBSB|BSBCGH|SSAGRU|GRUSSA" , route))

##Decents#####################################################################
VID <- subset(total_all, alt > 0 & Distance2 <= 40 & Distance2 >= 5)

#-- exclusion box - not used --------------------------------------
# setDT(VID)[, teste:= max(alt), indicat]
# VID$teste <- VID$teste*0.9
# 
# VID <- VID %>% 
#   group_by(indicat) %>%
#   slice(which(!(alt > teste)))
# 
# setDT(VID)[, position:= which.max(alt), indicat]
# 
# VID <- VID %>% 
#   group_by(indicat) %>%
#   slice(which(!(row_number() < position)))%>%
#   slice(which(!(unix-lag(unix)>100)))%>%
#   filter(unix-lag(unix)<100)
##--------------------------------------------------------------

VID <- VID %>%
  group_by(indicat, route) %>%
  dplyr::arrange(unix, .by_group = TRUE)%>%
  mutate(EV = alt)%>%
  mutate(EV = ifelse(EV < lag(EV)+150 & EV > lag(EV)-150 & unix-lag(unix)>30 |((lag(alt)-alt)/(unix-lag(unix))) < 5, unix-lag(unix),0))%>%
  mutate(EV = ifelse(is.na(EV),0,EV))%>%
  slice(which(!(EV>100)))%>%
  mutate(dista10 = ifelse(alt < 10000, distHaversine(cbind(lon, lat), cbind(lag(lon),lag(lat))),0))%>%
  mutate(dista10 = ifelse(is.na(dista10),0, dista10))%>%
  mutate(dista = distHaversine(cbind(lon, lat), cbind(lag(lon),lag(lat))))%>%
  mutate(dista = ifelse(is.na(dista),0, dista))%>%
  mutate(dista.x = ifelse(EV > 0, dista,0))%>%
  mutate(dista.x10 = ifelse(EV > 0, dista10,0))

ID <- VID%>%  
  summarise(equip = equip[1], landing_threshould = last(head), dist1 = sum(dista, na.rm = T),
            VID = (sum(dista.x, na.rm = T)/sum(dista, na.rm = T))*100,
            VID10 = (sum(dista.x10, na.rm = T)/sum(dista10, na.rm = T))*100,
            time_desc = (last(unix)-first(unix))/60,
            HFI_desc = (sum(dista,na.rm = T) - (distHaversine(cbind(first(lon),first(lat)), cbind(last(lon),last(lat)))))*0.000539957) 

time_d <- VID %>% group_by(indicat, route)%>%
  arrange(unix, .by_group = TRUE)%>%
  summarise(time_d = (last(unix)-first(unix))/60, equip = equip[1])%>%
  group_by(route,equip)%>%
  summarise(min_time_d = min(time_d))

ID <- merge(ID,time_d, by = c("route", "equip"))
ID$excess_desc <- ID$time_desc - ID$min_time_d 

#Climbs############################################################

VIC <- subset(total_all, Distance1 <= 40 & alt > 0 & Distance1 >= 2)

#--- exclusion box - not used ---------------------

# setDT(VIC)[, teste:= max(alt), indicat]
#VIC$teste <- VIC$teste*0.9

# VIC <- VIC %>% 
#   group_by(indicat) %>%
#   slice(which(!(alt > teste)))

#setDT(VIC)[, position:= which.max(alt), indicat]

# VIC <- VIC %>% 
#   group_by(indicat) %>%
#   dplyr::arrange(unix) %>%
#   arrange(indicat) %>%
#   slice(which(!(row_number() > position)))%>%
#   slice(which(!(unix-lag(unix)>100)))%>%
#   filter(unix-lag(unix)<100)
#---------------------------------------------------------

VIC <- VIC %>%
  group_by(indicat, route) %>%
  dplyr::arrange(unix, .by_group = TRUE)%>% 
  mutate(EV = alt)%>%
  mutate(EV = ifelse(EV < lag(EV)+150 & EV > lag(EV)-150 & unix-lag(unix)>30 | ((alt-lag(alt))/(unix-lag(unix))) < 5, unix-lag(unix), 0 ) )%>%
  mutate(EV = ifelse(is.na(EV),0,EV))%>%
  slice(which(!(EV>100)))%>%
  mutate(dista10 = ifelse(alt < 10000, distHaversine(cbind(lon, lat), cbind(lag(lon),lag(lat))),0))%>%
  mutate(dista10 = ifelse(is.na(dista10), 0, dista10) )%>%
  mutate(dista = distHaversine(cbind(lon, lat), cbind(lag(lon),lag(lat))))%>%
  mutate(dista = ifelse(is.na(dista), 0, dista) )%>%
  mutate(dista.x = ifelse(EV > 0, dista,0))%>%
  mutate(dista.x10 = ifelse(EV > 0, dista10,0))

IC <- VIC%>%  
  summarise(equip = equip[1], takeoff_threshould = first(head), dist2 = sum(dista, na.rm = T), 
            VIC = (sum(dista.x, na.rm = T)/sum(dista, na.rm = T))*100,
            VIC10 = (sum(dista.x10, na.rm = T)/sum(dista10, na.rm = T))*100,
            time_climb = (last(unix)-first(unix))/60,
            HFI_climb = (sum(dista,na.rm = T) - (distHaversine(cbind(first(lon),first(lat)), cbind(last(lon),last(lat)))))*0.000539957,
            dest = dest[1], orig = orig[1], id_icao = id_icao[1],
            time = time[1],
            Day = substr(first(time), 1,10), Hora = substr(first(time), 12,13),
            minuto = substr(first(time), 15,16),equip = equip[1],
            regist = str_remove_all(regist[1],"-"))

time_c <- VIC%>% group_by(indicat, route)%>% 
  summarise(time_c = (last(unix)-first(unix))/60, equip = equip[1])%>%
  group_by(route,equip)%>%
  summarise(min_time_c = min(time_c))

IC <- merge(IC,time_c, by = c("route", "equip"))  
IC$excess_climb <- IC$time_climb - IC$min_time_c 

##Cruise###############################################################

CR <- subset(total_all, Distance1 >= 40 & Distance2 >= 40)%>%
  group_by(indicat, route)%>%
  arrange(unix, .by_group = TRUE)%>%
  mutate(dista = distHaversine(cbind(lon, lat), cbind(lag(lon),lag(lat))))

CRI <- CR%>%
  summarise(equip = equip[1], time_cruise = (last(unix)-first(unix))/60,
            HFI_cruise = (sum(dista,na.rm = T) - (distHaversine(cbind(first(lon),first(lat)), cbind(last(lon),last(lat)))))*0.000539957)

time_cr <- CR%>% group_by(indicat, route)%>%
  arrange(unix, .by_group = TRUE)%>%
  summarise(time_cr = (last(unix)-first(unix))/60, equip = equip[1])%>%
  group_by(route,equip)%>%
  summarise(min_time_cr = min(time_cr))

CRI <- merge(CRI,time_cr, by = c("route", "equip"))
CRI$excess_cruise <- CRI$time_cruise - CRI$min_time_cr

## anac ##################################################

anac1 <- read.table("data2017/basica2017-09.txt", sep = ";", header = T)
anac2 <- read.table("data2017/basica2017-10.txt", sep = ";", header = T)
anac3 <- read.table("data2017/basica2017-11.txt", sep = ";", header = T)

dados_anac <- rbind(anac3, anac2, anac1)
rm(anac3, anac2, anac1)

dados_anac <- dados_anac%>%
  transmute(Day = as.character(dt_partida_real),
            Hora = as.character(substr(hr_partida_real,1,2)),
            TOW = kg_peso,
            id_icao = paste(sg_empresa_icao,nr_voo,sep =""),
            empresa = as.character(sg_empresa_icao),
            distance = km_distancia*0.539957,
            fuel = lt_combustivel,
            route = paste(sg_iata_origem,sg_iata_destino, sep = ""))

## rab ############################################################

rab <- read_excel("data2017/rab112017.xlsx") 
rab <- rab %>% subset(CATEGORIA == "TPR") %>% subset(ASSENTOS > 50)%>%
  subset("TRIP. MÍN." > 1)%>%
  select(MARCA, 'ANO FAB')%>%
  rename(regist = MARCA, ANO_FAB =  'ANO FAB')

## variables to MERGE by ############################################################

IC$indroute <- paste(IC$indicat, IC$route)
ID$indroute <- paste(ID$indicat, ID$route)
CRI$indroute <- paste(CRI$indicat, CRI$route)

ID$route <- NULL
ID$equip <- NULL
CRI$route <- NULL
CRI$equip <- NULL

### merge #########################################################

# trajectories ------
dataset2017 <- merge(IC,ID, by = 'indroute')
dataset2017 <- merge(dataset2017,CRI, by = 'indroute')

# anac ------
dataset2017$id_icao <- as.character(dataset2017$id_icao)
dataset2017 <- merge(dataset2017, dados_anac, by = c("id_icao","Day", "route"))

# rab -------
dataset2017 <- merge(dataset2017, rab, by = "regist")
  
# not merged -----
#x <- anti_join(IC, dataset2017, by = c("id_icao","Day"))

#--apenas empresas brasileiras----------------------------------------

dataset2017$equip <- as.character(dataset2017$equip)
dataset2017 <- subset(dataset2017, grepl("A20N|B738|E195|B763|B737|A320|A319|A321",equip))
dataset2017 <- subset(dataset2017, grepl("AZU|GLO|TAM|ONE",empresa))

## other variables #################################################

dataset2017$mileage <- dataset2017$fuel/dataset2017$distance
dataset2017$age <- (2017 - dataset2017$ANO_FAB)
dataset2017$TOW_proxy <- dataset2017$TOW + (dataset2017$fuel*0.79)

#dummies-----------------------------

 dataset2017 <- fastDummies::dummy_cols(dataset2017, 
                                      select_columns = "route")
 dataset2017 <- fastDummies::dummy_cols(dataset2017, 
                                      select_columns = "empresa")

# period of the day ########################################################

dataset2017 <- dataset2017 %>%
  mutate(hora = as.numeric(Hora.x))%>%
  mutate(periodo = ifelse(hora <= 6, 1,(ifelse(hora > 6 & hora < 13, 2,
                                               ifelse(hora > 12 & hora < 19, 3,
                                                      ifelse(hora >= 18, 4,0))))))%>%
  mutate(periodo = as.character(periodo))%>%
  mutate(Day = gsub("-","",Day))%>%
  mutate(periodo_dia = paste(Day, periodo, sep = "_"))


#transform to character------------

dataset2017$orig <- as.character(dataset2017$orig)
dataset2017$dest <- as.character(dataset2017$dest)


#iata to icao#################################
source("iata_to_icao.R")

dataset2017$dest_icao <- iata_to_icao(dataset2017$dest)
dataset2017$orig_icao <- iata_to_icao(dataset2017$orig)

#METAR###########################################################

#iata to icao--------------------------
source("iata_to_icao.R")

dataset2017$dest_icao <- iata_to_icao(dataset2017$dest)
dataset2017$orig_icao <- iata_to_icao(dataset2017$orig)
#--------------------------------------

metar <- read_csv("data2017/asos2017.csv")

metar$valid <- as.POSIXct(metar$valid, format = "%Y-%m-%d %H:%M", tz = "UTC")
metar$valid <- format(metar$valid , tz="America/Sao_Paulo",usetz=TRUE)
metar$date <- as.POSIXct(metar$valid, format = "%Y-%m-%d %H", tz = "America/Sao_Paulo")

metar <- metar%>%
  mutate(Day = as.Date(valid), hora = hour(valid))%>%
  mutate(Day = as.character(Day), hora = as.numeric(hora))%>%
  mutate(Day = str_remove_all(Day,"-"))%>% 
  distinct(Day, hora, station, .keep_all = TRUE)

metar_orig <- metar %>% select(Day, hora, station, tmpc)%>%
  transmute(orig_icao = station, TO = as.numeric(tmpc), Day = Day, hora = hora)

metar_dest <- metar %>% select(Day, hora, station, tmpc)%>%
  transmute(dest_icao = station, TD = as.numeric(tmpc), Day = Day, hora = hora)

rm(metar)

dataset2017 <- merge(dataset2017, metar_orig, by = c("Day", "hora", "orig_icao"))
dataset2017 <- merge(dataset2017, metar_dest, by = c("Day", "hora", "dest_icao"))%>%
  mutate(TM = (TD+TO)/2)
#y <- anti_join(VID, VID_GRU, by = "indicat")

#----salva---------------------------------------------------------



save(dataset2017,file = "data2017/dataset2017.rda")
