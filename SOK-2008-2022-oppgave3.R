library(readr)
library(tidyverse)
library(data.table)
library(PxWebApiData)
library(janitor)
library(readxl)

SSB <- ApiData("https://data.ssb.no/api/v0/no/table/11155/",
               Kjonn ="0",
               Alder = c("15-24", "20-64"),
               UtdNivaa = "TOT",
               ContentsCode = "Arbeidsledige (prosent)",
               Tid = "2020")


SSB <- as_tibble(SSB[[2]])

M1 <- ggplot (SSB, aes (x = Alder, y = value, fill = Alder)) +
  geom_col(col = "white") +
  geom_text(aes(label = value), vjust = -0.5) + 
  theme(axis.title.y = element_blank()) +
  labs(title = "Arbeidsledighet i prosent")
M1



library(OECD)  
library(ggplot2)    
library(tidyverse)  
library(dplyr)  
library(ggrepel) 


dsets<-get_datasets()
search_dataset("wage",dsets)
search_dataset("unemployment",dsets)



minwage <- get_dataset("MIN2AVE",
                       filter = "USA+CAN+FRA+GBR+DEU+NZL", 
                       pre_formatted = TRUE)

minwage2019 <- subset(minwage, Time < 2019 & Time >2007 & SERIES=="MEDIAN")
minwage2007_2019 <- subset(minwage2019, Time>2007)


unempl <- get_dataset("MIG_NUP_RATES_GENDER",
                      filter = "USA+CAN+FRA+GBR+DEU+NZL", 
                      pre_formatted = TRUE)


unempl2019 <- subset(unempl, Time<2019 & RATE=="U_RATE" & BIRTH=="NB" & GENDER=="TOT")
unempl2007_2019 <- subset(unempl2019, Time>2007)


minwage_unempl <-left_join(minwage2007_2019, unempl2007_2019, by=c("COUNTRY","Time"))


complete_minwage_unempl <- na.omit(minwage_unempl)


complete_minwage_unempl$MinWage_0 <-as.numeric(complete_minwage_unempl$ObsValue.x) 
complete_minwage_unempl$UnEmpl <-as.numeric(complete_minwage_unempl$ObsValue.y)


complete_minwage_unempl$MinWage <- complete_minwage_unempl$MinWage_0 * 100



M2 <- ggplot(data=complete_minwage_unempl,aes(x=UnEmpl,y=MinWage, group=COUNTRY, color=COUNTRY)) + 
  geom_line(aes(group=COUNTRY), size=1) +
  geom_point(size=2.5) +
  labs(x = "Unemployment" , y ="Minimum Wage")  + 
  theme(legend.position="none")+
  geom_label_repel(
    data=complete_minwage_unempl %>% group_by(COUNTRY) %>% 
      filter(UnEmpl==min(UnEmpl)), 
    aes(UnEmpl, MinWage, fill = factor(COUNTRY), label = sprintf('%s', COUNTRY)), 
    color = "black", 
    fill = "white") 
M2

