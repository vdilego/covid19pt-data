
#---------------------------------------------------------------------------
# Reading in data from Portugal by age and sex -----------------------------
# --------------------------------------------------------------------------

library(readr)
library(tidyverse)
library(stringr)
library(grepl)
library(ggplot2)
library(lubridate)
library(RcppRoll)
library(padr)
library(scales)
library(grDevices)
library(paletteer)

data <- read_csv("data.csv")

names(data)
# keeping only total country and then reshaping for extracting the age groups

data_2<-data %>%
  select(1:3,matches(c("^c.*\\_f$", "^c.*\\_m$")))%>%
  pivot_longer(!1:3, names_to = "Age", values_to = "Cases") %>%
  mutate(Sex=ifelse(grepl("\\_f$", Age, ignore.case = T), "Female","Male")) %>%
  separate(Age, c("key","Age", "value"), "_", extra = "merge") %>%
  filter(!Age%in%c("f","m","desconhecidos")) %>%
  select(-c(4,6)) %>%
  rename(Total=confirmados,Date=data,Date_Time=data_dados) %>%
  mutate(Date=lubridate::parse_date_time(Date,"dmy"))%>%
  arrange (Date) %>%
  mutate(Week=week(Date),
         Month=month(Date),
         Year=year(Date)) %>%
  group_by(Age,Sex,Week) %>%
  mutate(Sum_Cases_w = sum(Cases))

write_excel_csv(data_2,"Portugal_long.csv")


# graphing long

g_tile<- ggplot(data_2 , aes(x=Date, y=Age, fill=Sum_Cases_w))+
   theme_classic()+
geom_tile()+
    scale_fill_distiller(palette = "Spectral")+
   scale_y_discrete(name="", expand=c(0,0))+
   facet_grid(.~Sex)

options(scipen=999)
tiff(file="portugal_age_sex.tiff",
     width=15, height=7, units="in", res=100)
ggplot(data_2 , aes(x=Date, y=Age, fill=Sum_Cases_w))+
  theme_classic(base_size = 16)+
  geom_tile()+
  scale_fill_distiller(palette = "Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  facet_grid(.~Sex)
dev.off()
