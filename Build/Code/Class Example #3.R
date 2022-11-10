#Econ 691 Homework 2

rm(list=ls())

library(tidyverse)
library(tidycensus)

votes<-read.csv("./Data/Vote Data.csv")

levels(factor(votes$candidate))

v.cast<- votes %>%
  filter(year==2020) %>%
  group_by(county_fips) %>%
  summarise(cast=sum(candidatevotes))

states<- c("IL", "IN", "MI")

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
         GEOID = county_fips)

