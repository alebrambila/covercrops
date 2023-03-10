
## Libraries needed
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggalluvial)
library(RColorBrewer)

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
  mutate(type=ifelse(host%in%c("amsinckia", "clarkia", "collomia", "lotus", "epilobium", "gilia", "plectritis", "sanguisorba"), "annuals", "weeds"))%>%
  mutate(type=ifelse(host%in%c("achillea", "eriophyllum", "geum", "agoseris", "lomatium", "potentilla", "prunella", "viola"), "perennials", type))%>%
  mutate(type=ifelse(host%in%c("vetch", "clover"), "conventional", type))%>%
  group_by(orchard_age, block, management, type, Morphospecies)%>%
  summarize(count=sum(Count))%>%
  spread(Morphospecies, count, fill=0)%>%
  gather(Morphospecies, count, 5:11)

observed$type<-factor(observed$type, levels=c("perennials", "weeds", "conventional", "annuals"))
observed$Morphospecies<-factor(observed$Morphospecies, levels=c("small black bee", "honeybee", "syrphid", "other fly", "big black bee", "bumblebee", "other"))


ggplot(observed, aes(axis1 = type, axis2 = Morphospecies, y = count)) +
  scale_x_discrete(limits = c("Pollinator Taxa", "Host Plant"), expand = c(.05, .05)) +
  geom_alluvium(aes(fill = type)) +
  geom_stratum() +
  scale_fill_brewer(palette = "Dark2")+
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_classic()+
  theme(legend.position = "none")+xlab("")+ylab("visitations")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=(element_blank()), text=element_text(size=15))

ggplot(observed, aes(x=type, y=count)) +geom_boxplot()+facet_wrap(~Morphospecies, scales="free")

ggplot(subset(observed, orchard_age==15), aes(x=Morphospecies, y=count)) +geom_boxplot()+facet_wrap(~type, scales="free")
#how many of each group did it attract out of the total number of individuals found in that group?
observed.prop<-subset(observed, orchard_age==15)%>%
  group_by(Morphospecies)%>%
  mutate(total=sum(count))%>%
  ungroup()%>%
  mutate(prop=count/total)

ggplot(observed.prop, aes(x=Morphospecies, y=prop)) +geom_boxplot()+facet_wrap(~type, scales="free")


## stats ##

#model 1: morpho species within and across seed mixes (absolupte)
test<-subset(observed.prop, Morphospecies=="honeybee")%>%
  mutate(trt=as.factor(paste(type, Morphospecies, sep="_")))%>%ungroup()%>%
  mutate(id=as.factor(paste(block, management, sep="_")))
mod<-lme(data=subset(test), prop~trt, random=~1|block/management)

cld(glht(mod, mcp(trt="Tukey")))
emmeans(mod, pairwise~trt)

observed.tot<-subset(observed)%>%
  group_by(Morphospecies, type)%>%
  summarize(total=sum(count))%>%
  group_by(Morphospecies)%>%
  mutate(m.total=sum(total))%>%
  ungroup()%>%
  mutate(prop=total/m.total)

  



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
  unique()%>%
  mutate(host=ifelse(host=="weed", Notes, host))

capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

collected$host <- capFirst(collected$host)


ggplot(subset(collected, host!="NANA"), aes(axis1 = host, axis2 = Genus)) +
  scale_x_discrete(limits = c("Pollinator Taxa", "Host Plant"), expand = c(.05, .05)) +
  geom_alluvium(aes(fill = host)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_classic()+
  theme(legend.position = "none")+xlab("")+ylab("visitations")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=(element_blank()), text=element_text(size=10))+
facet_wrap(~type, scales="free")+
  scale_y_continuous(limits=c(0,50))
