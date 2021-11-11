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

load("data2019/dataset2019.rda")
dataset2019 <- dataset2019%>%
  group_by(route)%>%
  tally()%>% mutate(Percent = n/sum(n))%>%
  mutate(Percent = round(Percent, digits = 2))

load("data2017/dataset2017.rda")
dataset2017 <- dataset2017%>%
  group_by(route)%>%
  tally()%>% mutate(Percent = n/sum(n))%>%
  mutate(Percent = round(Percent, digits = 2))

rotas <- merge(dataset2019, dataset2017, by =  "route")%>%
  arrange(n.x)

ft = flextable(rotas)

save_as_docx("my table" = ft, path = "table_frequency of routes.docx")

