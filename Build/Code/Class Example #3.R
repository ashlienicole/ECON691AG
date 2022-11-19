#Econ 691 Homework 2

rm(list=ls())

library(tidyverse)
library(tidycensus)

#Create vote Data####
votes<-read.csv("./Data/Vote Data.csv")

v.cast<- votes %>%
  filter(year==2020) %>%
  group_by(county_fips) %>%
  summarise(cast=sum(candidatevotes))

states<- c("IL", "MO", "KS")

main<-votes %>%
  filter(year==2020) %>%
  filter(state_po %in% states) %>%
  filter(party == "DEMOCRAT" | party == "REPUBLICAN") %>%
  mutate(cand = case_when(party=="DEMOCRAT" ~ "Biden", 
                          party=="REPUBLICAN" ~ "Trump"),
         percent = candidatevotes/totalvotes) %>%
  pivot_wider(id_cols = county_fips, names_from = cand, 
              values_from = c(percent, candidatevotes)) %>%
  rename(Biden = candidatevotes_Biden,
         Trump = candidatevotes_Trump, 
         pBiden = percent_Biden,
         pTrump = percent_Trump,
         GEOID = county_fips) %>%
  mutate(GEOID = as.character(GEOID))


#Census Data####

var<-c("B01001_001", "B01001_002", "B02001_002", 
       "B02001_003", "B01002_002", "B01002_003")

acs<- get_acs(geography = "county",
              variables = var,
              year = 2020, 
              state = states,
              geometry = FALSE)

census <- acs %>%
  pivot_wider(id_cols = GEOID, names_from = variable, 
              values_from = estimate) %>%
  rename("TotPop" = "B01001_001",
         "Male" = "B01001_002",
         "White" = "B02001_002",
         "Black" = "B02001_003", 
         "Age_M" = "B01002_002",
         "Age_F" = "B01002_003") %>%
  mutate(pMale = Male/TotPop,
         pWhite = White/TotPop, 
         pBlack = Black/TotPop)

acs<- get_acs(geography = "county",
              variables = var,
              year = 2020, 
              state = states,
              geometry = TRUE)
  
map <- acs %>%
  select(GEOID, geometry) %>%
  distinct()
  
#COVID-19 Data####

covid<-read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

covid2 <- covid %>%
  filter(fips %in% census$GEOID) %>%
  mutate(Date = as.Date(date, "%Y-%m-%d"),
         GEOID = as.character(fips))%>%
  arrange(fips, Date) %>%
  group_by(fips) %>%
  summarise_all(last)

#Merge Data####
 core<- map %>%
  left_join(., main, by="GEOID") %>%
  left_join(., covid2, by="GEOID") %>%
  left_join(., census, by="GEOID")

#Regressions####

mod1<- lm(cases~pMale+pBlack+Age_M+Age_F, data=core)

core2<-core %>%
  mutate(cases = cases/10000,
         Biden2 = Biden/10000,
         Trump2 = Trump/10000)

mod2<- lm(cases~pMale+pBlack+Age_M+Age_F+factor(state), data=core2)

mod3<- lm(cases~pMale+pBlack+Age_M+Age_F+factor(state)-1, data=core2)

mod4<- lm(cases~pMale+pBlack+Age_M+Age_F+factor(state)+Trump2, data=core2)

library(stargazer)

stargazer(core2, type=)