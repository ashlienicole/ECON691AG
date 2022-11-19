#Homework 2
#November 18, 2022
#Ashlie Garner 

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
  pivot_wider(id_cols = c(county_fips,state_po, county_name), 
              names_from = cand, values_from = c(percent, candidatevotes)) %>%
  rename(Biden = candidatevotes_Biden,
         Trump = candidatevotes_Trump, 
         pctBiden = percent_Biden,
         pctTrump = percent_Trump,
         GEOID = county_fips,
         State = state_po,
         County = county_name) %>%
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
  mutate(pctMale = Male/TotPop,
         pctWhite = White/TotPop, 
         pctBlack = Black/TotPop)

acs<- get_acs(geography = "county",
              variables = var,
              year = 2020, 
              state = states,
              geometry = TRUE)

map <- acs %>%
  select(GEOID, geometry) %>%
  distinct()

#Merge Data####
core<- map %>%
  left_join(., main, by="GEOID") %>%
  left_join(., census, by="GEOID")

#Plots####
ggplot(core) + geom_sf(aes(fill = pctBiden))
ggplot(core) + geom_sf(aes(fill = pctTrump,)) 
ggplot(core) + geom_sf(aes(fill = Age_M))


