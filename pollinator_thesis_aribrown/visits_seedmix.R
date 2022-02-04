## POLLINATOR THESIS PROJECT FIGURES
## Pollinator visitations by seed mix


## Libraries needed
library(tidyverse)
library(ggplot2)
library(dplyr)

library(nlme)
library(multcomp)

#############################
#############################
#############################
#############################
## Import data
## Clean up column names and morphospecies groups

observedpollinators <- read.csv("observedpollinators_2021.csv") %>%
  ##Updated morphospecies categories combine 'ant' and 'wasp' into a 'Hymenoptera' group,
  ##as well as combining 'aphid' into 'true bug' and 'mosquito' into 'other fly'
  mutate(Morphospecies=ifelse(Morphospecies=="ant", "hymenoptera", ifelse(Morphospecies=="wasp", 
                                                             "hymenoptera",
                                                             ifelse(Morphospecies=="aphid","true bug", 
                                                                    ifelse(Morphospecies=="mosquito","other fly", Morphospecies)))))
  
names(observedpollinators) <- c ("Month", "Orchard.Age", "Block", "Management", "Seed.Mix", "Host.Plant", "Morphospecies", "Number", "Notes")

#############################
#############################

## First I'll filter the observed pollinator dataset to find total pollinator visits for each seed mix
## I want to count all pollinator visits within a seed mix by each management subplot, and by what
## morphospecies were visiting that seed mix


seedmixabundances <- observedpollinators %>%
  dplyr::select(Orchard.Age, Block, Management, Seed.Mix, Morphospecies, Number) %>%
  group_by(Orchard.Age, Block, Seed.Mix, Management, Morphospecies) %>%
  summarise(Number = sum(Number))

## For graphing this, I want to do a stacked bar plot
ggplot(seedmixabundances, aes(fill=Morphospecies, x=Seed.Mix)) + 
  geom_bar(stat="count", position="stack") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(x="Seed Mix", y="Pollinator Visitation")
#############################
#############################
## Graphing version 2: I want to do something similar to what I did with richness
## where I group observations by the actual plant they were observed on, not just seed mix,
## so that I can account for escapee plants.

annualspecies <- c("collomia", "amsinckia", "clarkia", "epilobium", "gilia",
                   "lotus", "plectritis", "sanguisorba", "annual")

perennialspecies <- c("achillea", "agoseris", "lomatium", "potentilla",
                      "prunella", "viola", "geum", "eriophyllum", "perennial")

industryspecies <- c("barley", "oats", "vetch", "clover", "industry")

taxonomicobservations<-observedpollinators%>%
  mutate(seedmix2=ifelse(Host.Plant%in%perennialspecies, "perennial",
                         ifelse(Host.Plant%in%annualspecies, "annual", 
                                ifelse(Host.Plant%in%industryspecies, "industry", 
                                       ifelse(Host.Plant=="weed","weed", Host.Plant)))))%>%
  ungroup()%>%
  group_by(Orchard.Age, Block, Management, Morphospecies, seedmix2)%>%
  summarise(Number = sum(Number))

ggplot(taxonomicobservations, aes(fill=Morphospecies, x=seedmix2)) + 
  geom_bar(stat="count", position="stack") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(x="Seed Mix", y="Pollinator Visitation")
#add in a full plot list to account for zeroes
plots <- read.csv("canopy_cover.csv")%>%
  filter(year==2021)%>% #have to do this here
  mutate(seedmix=ifelse(seedmix=="annuals", "annual", ifelse(seedmix=="perennials", "perennial", seedmix)))%>%
  mutate(management=ifelse(management=="scraped", "flailscrape", ifelse(management=="flailed", "flail", "unmanaged")))%>%
   dplyr::select(1:4)
names(plots) <- c("Orchard.Age", "Block", "Management", "Seed.Mix")

newplots<-plots%>%  #convert to species types
  mutate(seedmix2=ifelse(Seed.Mix=="control", "weed", Seed.Mix))%>%
  dplyr::select(-Seed.Mix)%>%
  filter(seedmix2!="megamix")



taxonomicobservations1<-taxonomicobservations%>%
  group_by(Orchard.Age, Block, Management, seedmix2)%>%
  summarize(Number=sum(Number))

taxonomicobservations1<-full_join(newplots, taxonomicobservations1)%>% #need zeroes
  mutate(Number=ifelse(is.na(Number), 0, Number))



ggplot(subset(taxonomicobservations1, seedmix2!="megamix"), aes(x=seedmix2, y=Number)) +
  geom_boxplot(aes(fill=as.factor(Orchard.Age))) +
  labs(x="Host Plant Groups", y="Pollinator Visitations (per management plot)") +
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=28))
#############################
#############################
## Table 2 for the cover crop paper draft
## Finding the preferred host for each morphospecies, e.g. finding what plant species
## was visited most by each morphospecies group

morphospecieshosts <- observedpollinators %>%
  group_by(Morphospecies, Host.Plant)%>%
  summarise(Number = sum(Number))

#############################
#############################
## Statistics

##First, I want to test if the means are different for total pollinator visitations in each seed mix
## category. I'll create a dataset first of all the visitations (not divided up my morphospecies)
## per seed mix, and then compare the means with the 'aov' function
aov <- aov(Number~seedmix2, data=taxonomicobservations1)
summary(aov)

## Results: P < 0.05, so I'll go ahead with a Tukey test
tukey <- TukeyHSD(aov, conf.level=.95)

## Tukey results: There's a significant difference between the perennial & megamix and perennial & control
## pollinator visitations

#############################
#############################
## Statistics for 'taxonomicobservations'

#seedmixmeans2<-observedpollinators%>%
#  mutate(seedmix2=ifelse(Host.Plant%in%perennialspecies, "perennial",
##                         ifelse(Host.Plant%in%annualspecies, "annual", 
#                                ifelse(Host.Plant%in%industryspecies, "industry", 
 #                                      ifelse(Host.Plant=="weed","weed", Host.Plant)))))%>%
#  ungroup()%>%
#  group_by(Orchard.Age, Block, Management, seedmix2)%>%
#  summarise(Number = sum(Number))


#aov <- aov(Number~seedmix2, data=seedmixmeans2)
#summary(aov)

## Results: P < 0.05, so I'll go ahead with a Tukey test
#tukey2 <- TukeyHSD(aov, conf.level=.95)

## When examining observations by plant species, there is a significant difference between
## perennials and annuals, and perennials and weeds

#############################
#############################
## Statistics for 'taxonomicobservations'
## Including orchard age

aovorchard <- aov(Number~seedmix2*as.factor(Orchard.Age), data=taxonomicobservations1)
tukey3 <- TukeyHSD(aovorchard, conf.level=.95)
smm3<-taxonomicobservations1%>%
 mutate(trt= as.factor(paste(seedmix2, Orchard.Age, sep="_")))%>%
  ungroup()

aov3<-aov(Number~trt, data=smm3)

#COMPACT LETTER DISPLAY
cld(glht(aov3, mcp(trt="Tukey"))) 

#mm<-lme(Number~paste(seedmix2, as.factor(Orchard.Age)), data=seedmixmeans2)
#tuk3<-glht(aovorchard, linft=mcp(func="Tukey"))
#cld(tuk3)

#MM<-lme(lrr~func, random = ~1|site, data = subset(inter, metric=="Shannon"&!is.na(difference)))
#summary(glht(MM, linfct=mcp(func="Tukey")))
#cld(glht(MM, linfct=mcp(func="Tukey")))

#############################
#############################
## Statistics for plants within the perennial seed mix
## From tukey2 and tukey3, it's clear that there's a significant
## difference in pollinator visitations to the species in the perennial seed mix
## as compared to the other seed mixes. So, I'm going to compare the individual
## species to see what stands out.

## Creating the data set for this:
perennialmeans <- observedpollinators%>%
  mutate(seedmix2=ifelse(Host.Plant%in%perennialspecies, "perennial",
                         ifelse(Host.Plant%in%annualspecies, "annual", 
                                ifelse(Host.Plant%in%industryspecies, "industry", 
                                       ifelse(Host.Plant=="weed","weed", Host.Plant)))))%>%
  ungroup()%>%
  group_by(Orchard.Age, Block, Management, seedmix2, Host.Plant)%>%
  summarise(Number = sum(Number)) %>%
  mutate(Host.Plant=as.factor(Host.Plant))%>%
  ungroup()%>%
  spread(Host.Plant, Number, fill=0)
pereninalmeans1<-full_join(newplots, perennialmeans)%>%
  filter(seedmix2!="megamix"&!is.na(seedmix2))%>%
  dplyr::select(-20)%>%
  gather(Host.Plant, Number, 5:19)%>%
  mutate(Number=ifelse(is.na(Number), 0, Number))%>%
  filter(Host.Plant!="barley")%>%
  mutate(Host.Plant=as.factor(Host.Plant))
test<-group_by(pereninalmeans1, Orchard.Age, Host.Plant)%>%
  summarize(test=sum(Number))%>%
  filter(Host.Plant!="barley")

perennialmeans2<-left_join(pereninalmeans1, test)%>%
  filter(test!=0)

#plants with all zeroes not shown
ggplot(perennialmeans2, aes(x=Host.Plant, y=Number)) +
    geom_jitter(size=.5)+geom_boxplot(aes(fill=seedmix2), position="identity") +  facet_grid(~Orchard.Age, scales="fixed")+
    labs(x="Host Plant Species", y="Pollinator Visitation (per management plot)")+
       theme(axis.text.x=element_text(angle = -90, hjust = 0))
  
  
## Stat tests
    
#across all
aovperennials <- aov(Number~Host.Plant, data=pereninalmeans1)
tukey4 <- TukeyHSD(aovperennials, conf.level=.95)
cld(glht(aovperennials, mcp(Host.Plant="Tukey"))) #only achillea significantly higher

#across all
aovperennials.15 <- aov(Number~Host.Plant, data=subset(pereninalmeans1, Orchard.Age==15))
tukey4 <- TukeyHSD(aovperennials, conf.level=.95)
cld(glht(aovperennials.15, mcp(Host.Plant="Tukey"))) #only achillea

#across all
aovperennials.40 <- aov(Number~Host.Plant, data=subset(pereninalmeans1, Orchard.Age==40))
tukey4 <- TukeyHSD(aovperennials, conf.level=.95)
cld(glht(aovperennials.40, mcp(Host.Plant="Tukey"))) #only geum significanlty higher

#across all
aovperennial.60 <- aov(Number~Host.Plant, data=subset(pereninalmeans1, Orchard.Age==60))
tukey4 <- TukeyHSD(aovperennials, conf.level=.95)
cld(glht(aovperennial.60, mcp(Host.Plant="Tukey"))) #only geum sig higher

## Nothing stands out as significant, so I'll do another test within orchard ages
aovperennialorchard <- aov(Number~Host.Plant*as.factor(Orchard.Age), data=perennialmeans)
tukey5 <- TukeyHSD(aovperennialorchard, conf.level=.95)