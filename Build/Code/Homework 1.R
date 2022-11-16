#Econ 691 Homework#1
#Ashlie Garner
#Oct 26, 2022

rm(list=ls())

library(tidyverse)

library(ggplot2)

delta<-function(x){
  temp<-((x-lag(x))/lag(x))
  return(round(temp,4))
}

DIF<-function(x){
  temp<-(x-lag(x))
  return(round(temp,4))
}

covid<-read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

covid.IL <- covid %>%
  filter(state=="Illinois" & county=="Cook") %>%
  mutate(new_cases=DIF(cases),
         new_deaths=DIF(deaths))%>%
  mutate(pc_cases=delta(cases),
         pc_deaths=delta(deaths))%>%
  mutate(pc_deaths=ifelse(is.infinite(pc_deaths), NA, pc_deaths),
         Date=as.Date(date, "%Y-%m-%d"))%>%
  filter(date>="2021-01-01")    

#PC_Plot<-ggplot(data=covid.IL,aes(x=date,y1=pc_cases,y2=pc_deaths), geom_plot())

#Using the ggplot is fine, but this is how the code should read. JRG
PC_Plot<-ggplot(data=covid.IL) +
  geom_line(aes(x=Date,y=pc_deaths))
#Then doing the same for the pc_cases. JRG
PC_Plot<-ggplot(data=covid.IL) +
  geom_line(aes(x=Date,y=pc_cases))

print(PC_Plot)
                