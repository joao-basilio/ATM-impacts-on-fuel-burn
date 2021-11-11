rm(list=ls())

library(dplyr)
library(stringr)
library(leaflet)
library(RColorBrewer)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))## ** this file location as wd **

load("data2019/trajectories_2019.rda")
load("data2019/dataset2019.rda")

airports <- unique(dataset2019$dest)
  
dataset2019 <- dataset2019 %>% select(indicat,orig,dest,route,VID,VIC,HFI_climb,HFI_desc,HFI_cruise)
dataset2019 <- subset(dataset2019,grepl("GRUCNF|CNFGRU|CGHCNF|CWBGRU|CWBCGH|GRUREC|CNFCGH|CGHPOA|POACGH|CGHCWB|POAGRU|GRUPOA|RECGRU|GRUCWB|SDUCGH|CGHSDU|CGHBSB|BSBCGH|SSAGRU|GRUSSA" , route))

dados0 <- merge(dataset2019, track_data2,by = c("indicat", "orig","dest"))
dados0 <- dados0 %>%
  subset(str_detect(route,"CWBCGH"))%>%
  #subset(equip_A320 == 1)%>%
  #subset(Distance1 > 5 | Distance2 > 5 )%>% 
  arrange(indicat,time) 
#dados0 <- dados0[1:10000,]

rm(dataset2019)
rm(track_data2)

aeroportos <- read.delim("~/tese/Tese/base_nova/airports_world.txt")%>%
  mutate(IATA = str_remove(IATA, "'"))%>%
  mutate(IATA = str_remove(IATA, "'"))%>%
  subset(str_detect(IATA,paste(airports, collapse = "|")))%>%
  subset(str_detect(IATA,"CGH|CWB"))
rm(airports)

## cruise #############################################################

dados <- dados0%>%
  subset(Distance1 > 40 & Distance2 > 40)%>%
  filter(HFI_cruise < quantile(dados0$HFI_cruise, probs = (0.98)))

x <- nrow(dados0 %>%
  filter(HFI_cruise > quantile(dados0$HFI_cruise, probs = (0.98)))%>%
  group_by(indicat) %>%tally())

rampcols <- colorRampPalette(colors = c("green","orange", "red"))(20)
pal <- colorNumeric(palette = rampcols, domain = dados$HFI_cruise)

m <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addTiles() %>%
  #addMarkers(aeroportos$lon, aeroportos$lat, popup = as.character(aeroportos$IATA), label = as.character(aeroportos$IATA))%>%
  #addControl(rr, position = "bottomleft")%>%
  #setView( lng = -46.6, lat = -23.63, zoom = 8.5) %>%
  #fitBounds(lng1 = -47.47, lat1 = -22.911,lng2 = -45.9, lat2 = -24.2)%>%
  clearBounds()%>%
  addProviderTiles("CartoDB.DarkMatter")
  #addLegend("bottomleft", pal = colorid_arr, values = arr_CGH$clusterid,
            #group = arr_CGH$clusterid, title = "Cluster")


 m <- addLegend(m, "bottomright", pal = pal, values = dados$HFI_cruise,
                title = "HFI_cruise </br> (NM)")

for (j in unique(dados$indicat)){
    idx2 <- which(dados$indicat==j)
    dados2 <- dados[idx2,]%>% arrange(time)
    
    
    m <- addPolylines(m, lng = dados2$lon, lat = dados2$lat, 
                      color = pal(dados2$HFI_cruise), 
                      weight = 0.2, opacity = 0.1) 
    print(j)
}

#for (i in 1:NROW(aeroportos)){
#  m <- addCircles(m,lng = aeroportos[i,"lon"], lat = aeroportos[i,"lat"], radius = 74080,
#                  fill = F, color = "white", weight = 1)
#}
m

## climb #############################################################

dados <- dados0%>%
  subset(Distance1 < 40 & Distance1 > 2)

rampcols <- colorRampPalette(colors = c("green","orange", "red"))(20)
pal <- colorNumeric(palette = rampcols, domain = dados$HFI_climb)

m <- addLegend(m, "bottomleft", pal = pal, values = dados$HFI_climb,
               title = "HFI_climb (NM)")

for (j in unique(dados$indicat)){
  idx2 <- which(dados$indicat==j)
  dados2 <- dados[idx2,]
  
  
  m <- addPolylines(m, lng = dados2$lon, lat = dados2$lat, 
                    color = pal(dados2$HFI_climb), 
                    weight = 0.2, opacity = 0.1) 
  print(j)
}

for (i in 1:NROW(aeroportos)){
  m <- addCircles(m,lng = aeroportos[i,"lon"], lat = aeroportos[i,"lat"], radius = 74080,
                  fill = F, color = "white", weight = 1)%>%
    addCircles(m,lng = aeroportos[i,"lon"], lat = aeroportos[i,"lat"], radius = 3704,
               fill = F, color = "white", weight = 1)
}
m

## desc #############################################################

dados <- dados0%>%
  subset(Distance2 < 40 & Distance2 > 2)

rampcols <- colorRampPalette(colors = c("green","orange", "red"))(20)
pal <- colorNumeric(palette = rampcols, domain = dados$HFI_desc)

m <- addLegend(m, "topleft", pal = pal, values = dados$HFI_desc,
               title = "HFI_desc (NM)")

#"HFI_desc & </br> HFI_climb </br> (%)"

for (j in unique(dados$indicat)){
  idx2 <- which(dados$indicat==j)
  dados2 <- dados[idx2,]
  
  
  m <- addPolylines(m, lng = dados2$lon, lat = dados2$lat, 
                    color = pal(dados2$HFI_desc), 
                    weight = 0.2, opacity = 0.1) 
  print(j)
}

for (i in 1:NROW(aeroportos)){
  m <- addCircles(m,lng = aeroportos[i,"lon"], lat = aeroportos[i,"lat"], radius = 74080,
                  fill = F, color = "white", weight = 1)%>%
    addCircles(m,lng = aeroportos[i,"lon"], lat = aeroportos[i,"lat"], radius = 9260,
               fill = F, color = "white", weight = 1)
}
m
