
## Libraries needed
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggalluvial)

source('phenology.R')

observed<-read_sheet("https://docs.google.com/spreadsheets/d/1IeWQPtXPJ-MrIua0wZswSJNRnU3bvmkDKtTc5lBzEbk/edit?usp=sharing")%>%
  mutate(numericmonth=ifelse(Month=="april", 4, 
                             ifelse(Month=="may", 5, 
                                    ifelse(Month=="june", 6, 
                                           ifelse(Month=="july", 7, 8)))))%>%
  group_by(numericmonth, block, orchard_age, management, seedmix)%>%
  mutate(host=`Host Plant`)%>%
  mutate(Morphospecies=ifelse(Morphospecies%in%c("beetle", "butterfly", "earwig", "true bug", "wasp", "other bee", "green bee"), "other", Morphospecies))%>%
  mutate(seedmix=ifelse(seedmix=="annual", "annuals", ifelse(seedmix=="perennial", "perennials", seedmix)))%>%
  filter(seedmix!="megamix"&management!="unmanaged")%>%
  mutate(type=ifelse(host%in%c("amsinckia", "clarkia", "collomia", "lotus", "epilobium", "gilia", "plectritis", "sanguisorba"), "annuals", "weed"))%>%
  mutate(type=ifelse(host%in%c("achillea", "eriophyllum", "geum", "agoseris", "lomatium", "potentilla", "prunella", "viola"), "perennials", type))%>%
  mutate(type=ifelse(host%in%c("vetch", "clover"), "industry", type))%>%
  group_by(orchard_age, block, management, type, Morphospecies)%>%
  summarize(count=sum(Count))

observed$type<-factor(observed$type, levels=c("perennials", "weed", "industry", "annuals"))
observed$Morphospecies<-factor(observed$Morphospecies, levels=c("small black bee", "honeybee", "syrphid", "other fly", "big black bee", "bumblebee", "other"))


ggplot(observed, aes(axis1 = type, axis2 = Morphospecies, y = count)) +
  scale_x_discrete(limits = c("Pollinator Taxa", "Host Plant"), expand = c(.05, .05)) +
  geom_alluvium(aes(fill = type)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal()#+
  facet_wrap(~management)
  
  
collected<-read_csv("collectedpollinators_2021.csv")%>%
  mutate(orchardage=`Orchard Age`)%>%
  mutate(numericmonth=ifelse(Month=="april", 4, 
                             ifelse(Month=="may", 5, 
                                    ifelse(Month=="june", 6, 
                                           ifelse(Month=="july", 7, 8)))))%>%
  mutate(host=`Host Plant`)%>%
  mutate(seedmix=`Seed Mix`)%>%
  mutate(seedmix=ifelse(seedmix=="annual", "annuals", ifelse(seedmix=="perennial", "perennials", seedmix)))%>%
  mutate(type=ifelse(host%in%c("amsinckia", "clarkia", "collomia", "lotus", "epilobium", "gilia", "plectritis", "sanguisorba"), "annuals", "weed"))%>%
  mutate(type=ifelse(host%in%c("achillea", "eriophyllum", "geum", "agoseris", "lomatium", "potentilla", "prunella", "viola"), "perennials", type))%>%
  mutate(type=ifelse(host%in%c("vetch", "clover"), "industry", type))%>%
  dplyr::select(host, type, `Order`, Genus, Notes)%>%
  unique()



ggplot(collected, aes(axis1 = host, axis2 = Genus)) +
  scale_x_discrete(limits = c("Pollinator Taxa", "Host Plant"), expand = c(.05, .05)) +
  geom_alluvium(aes(fill = host)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal()+
facet_wrap(~type)
