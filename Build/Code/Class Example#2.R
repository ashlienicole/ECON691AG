#Class Example 2- Web data

rm(list=ls())

library(tidyverse)
library(rvest)

state<-c("illinois")

URL<-paste0("https://www.nytimes.com/elections/2016/results/illinois")

webpage<-read_html(URL)

tables<- webpage %>%
  html_elements("table")

results<- html_table(tables[[2]])

results2<-results %>%
  rename("County" = 'Vote by county') %>%
  mutate(Clinton = as.numeric(gsub(",", "", Clinton)),
         Trump = as.numeric(gsub(",", "",Trump)),
         pClinton = Clinton/(Clinton+Trump),
         pTrump = 1 - pClinton)

results2$County[which(results2$County=="DeWitt")]<-"De Witt"
results2$County[which(results2$County=="JoDaviess")]<-"Jo Daviess"

library(tidycensus)

var<-c("B01001_001", "B01001_002", "B02001_002", "B02001_003")

acs<- get_acs(geography = "county",
              variables = var,
              year = 2020, 
              state = 17,
              geometry = TRUE)

map <- acs %>%
  select(GEOID, geometry)

bridge <- acs %>%
  select(GEOID, NAME)
bridge$geometry<-NULL

bridge2<-bridge%>%
  mutate(NAME = trimws(gsub("County, Illinois", "", NAME)))%>%
  distinct()%>%
  rename("County" = "NAME")%>%
  full_join(.,results2, by="County")

core <- acs %>%
  select(-c("NAME", "moe"))
core$geometry<-NULL

core <- core %>%
  pivot_wider(names_from = variable, values_from = estimate)%>%
  rename("TotPop" = "B01001_001",
         "Male" = "B01001_002",
         "White" = "B02001_002",
         "Black" = "B02001_003")%>%
  mutate(pMale = Male/TotPop,
         pWhite = White/TotPop, 
         PBlack = Black/TotPop)%>%
  full_join(map, ., by="GEOID")%>%
  full_join(., bridge2, by="GEOID")
