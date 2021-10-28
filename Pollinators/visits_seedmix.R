## POLLINATOR THESIS PROJECT FIGURES
## Pollinator visitations by seed mix


## Libraries needed
library(tidyverse)
library(ggplot2)
library(dplyr)

#############################
#############################
#############################
#############################
## Import data
## Clean up column names and columns

observedpollinators <- read.csv("observedpollinators_2021.csv")
names(observedpollinators) <- c ("Month", "Orchard.Age", "Block", "Management", "Seed.Mix", "Host.Plant", "Morphospecies", "Number", "Notes")

#############################
#############################

## First I'll filter the observed pollinator dataset to find total pollinator visits for each seed mix
## I want to count all pollinator visits within a seed mix by each management subplot, and by what
## morphospecies were visiting that seed mix



seedmixabundances <- observedpollinators %>%
  select(Orchard.Age, Block, Management, Seed.Mix, Morphospecies, Number) %>%
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
                   "lotus", "plectritis", "sanguisorba")

perennialspecies <- c("achillea", "agoseris", "lomatium", "potentilla",
                      "prunella", "viola", "geum", "eriophyllum")

industryspecies <- c("barley", "oats", "vetch", "clover")

taxonomicobservations<-observedpollinators%>%
  mutate(seedmix2=ifelse(Host.Plant%in%perennialspecies, "perennial",
                         ifelse(Host.Plant%in%annualspecies, "annual", 
                                ifelse(Host.Plant%in%industryspecies, "industry", 
                                       ifelse(Host.Plant=="weed","weed", Host.Plant)))))%>%
  ungroup()%>%
  group_by(Orchard.Age, Block, Management, Morphospecies, seedmix2)%>%
  summarise(Number = sum(Number))

#add in a full plot list to account for zeroes
plots <- read.csv("canopy_cover.csv")%>%
  filter(year==2021)%>% #have to do this here
  mutate(seedmix=ifelse(seedmix=="annuals", "annual", ifelse(seedmix=="perennials", "perennial", seedmix)))%>%
  dplyr::select(1:4)
names(plots) <- c("Orchard.Age", "Block", "Management", "Seed.Mix")

newplots<-plots%>%  #convert to species types
  mutate(seedmix2=ifelse(Seed.Mix=="control", "weed", Seed.Mix))%>%
  dplyr::select(-Seed.Mix)

taxonomicobservations<-full_join(newplots, taxonomicobservations)%>% #need zeroes
 mutate(Number=ifelse(is.na(Number), 0, Number))

ggplot(taxonomicobservations, aes(fill=Morphospecies, x=seedmix2)) + 
  geom_bar(stat="count", position="stack") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(x="Seed Mix", y="Pollinator Visitation")

ggplot(seedmixmeans2, aes(x=seedmix2, y=Number)) +
  geom_boxplot(aes(fill=as.factor(Orchard.Age)))

ggplot(seedmixmeans2, aes(x=as.factor(Orchard.Age), y=Number)) +
  geom_boxplot(aes(fill=as.factor(seedmix2)))

#############################
#############################
## Statistics

##First, I want to test if the means are different for total pollinator visitations in each seed mix
## category. I'll create a dataset first of all the visitations (not divided up my morphospecies)
## per seed mix, and then compare the means with the 'aov' function
seedmixmeans <- observedpollinators %>%
  group_by(Orchard.Age, Block, Seed.Mix) %>%
  summarise(Number = sum(Number))

aov <- aov(Number~Seed.Mix, data=seedmixmeans)
summary(aov)

## Results: P < 0.05, so I'll go ahead with a Tukey test
tukey <- TukeyHSD(aov, conf.level=.95)

## Tukey results: There's a significant difference between the perennial & megamix and perennial & control
## pollinator visitations

#############################
#############################
## Statistics for 'taxonomicobservations'

seedmixmeans2<-observedpollinators%>%
  mutate(seedmix2=ifelse(Host.Plant%in%perennialspecies, "perennial",
                         ifelse(Host.Plant%in%annualspecies, "annual", 
                                ifelse(Host.Plant%in%industryspecies, "industry", 
                                       ifelse(Host.Plant=="weed","weed", Host.Plant)))))%>%
  ungroup()%>%
  group_by(Orchard.Age, Block, seedmix2)%>%
  summarise(Number = sum(Number))

aov <- aov(Number~seedmix2, data=seedmixmeans2)
summary(aov)

## Results: P < 0.05, so I'll go ahead with a Tukey test
tukey2 <- TukeyHSD(aov, conf.level=.95)

## When examining observations by plant species, there is a significant difference between
## perennials and annuals, and perennials and weeds

#############################
#############################
## Statistics for 'taxonomicobservations'
## Including orchard age

aovorchard <- aov(Number~seedmix2*as.factor(Orchard.Age), data=seedmixmeans2)
tukey3 <- TukeyHSD(aovorchard, conf.level=.95)
