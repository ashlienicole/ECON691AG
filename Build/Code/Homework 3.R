#Econ 691 Homework 2

rm(list=ls())

library(tidyverse)
library(tidycensus)
library(stargazer)


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
  left_join(., census, by="GEOID") %>%
  mutate(PerCapitaCases = cases/TotPop,
         PerCapitaDeaths = deaths/TotPop)
summary(core)

#Regressions####

VotesTrump1 <- lm(Trump ~ pMale + state, data = core)

VotesTrump2 <- lm(Trump ~ pMale + pWhite + state, data = core)

VotesTrump3 <- lm(Trump ~ pMale + pWhite + Age_M + 
                    Age_F + state, data = core)

PCCases1 <- lm(PerCapitaCases ~ pMale + state, data = core)

PCCases2 <- lm(PerCapitaCases ~ pMale + pWhite + state,
               data = core)

PCCases3 <- lm(PerCapitaCases ~ pMale + pWhite + Age_M + Age_F + 
                 Trump + state, data = core)

PCDeaths1 <- lm(PerCapitaDeaths ~ pMale + state, data = core)

PCDeaths2 <- lm(PerCapitaDeaths ~ pMale + pWhite+ state, 
                data = core)

PCDeaths3 <- lm(PerCapitaDeaths ~ pMale + pWhite + Age_M + Age_F +
                  Trump + state, data = core)

#Tables####

stargazer(core, type = "text", title = "Summary Statistics", 
          out = "SummaryStats.txt")

stargazer(VotesTrump3, type = "text", title = "Percentage of Trump Votes", 
          out = "Trump.txt")

stargazer(PCCases3, type = "text", title = "Per Capita Cases", 
          out = "Cases.txt")

stargazer(PCDeaths3, type = "text", title = "Per Capita Deaths", 
          out = "Deaths.txt")


