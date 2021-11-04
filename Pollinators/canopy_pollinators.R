## POLLINATOR THESIS PROJECT FIGURES
## Canopy cover vs. pollinator abundances


## Libraries needed
library(tidyverse)
library(ggplot2)
library(dplyr)
library(nlme)
library(multcomp)

#############################
#############################
## Import data
## Clean up column names and columns

observedpollinators <- read.csv("observedpollinators_2021.csv")%>%
  mutate(Management=ifelse(Management=="flailscrape", "scraped", ifelse(Management=="flail", "flailed", Management)))%>%
  mutate(Seed.Mix=ifelse(Seed.Mix=="annuals", "annual", ifelse(Seed.Mix=="perennials", "perennial", Seed.Mix)))
names(observedpollinators) <- c ("Month", "Orchard.Age", "Block", "Management", "Seed.Mix", "Host.Plant", "Morphospecies", "Number", "Notes")
#before proceeding to merge you'll need to summarize this down to the month level example below
observedpollinators<-observedpollinators%>%
  group_by(Orchard.Age, Block, Management, Seed.Mix)%>%
  summarize(Number=sum(Number))

canopycover <- read.csv("canopy_cover.csv")%>%
  filter(year==2021)%>% #have to do this here
  mutate(seedmix=ifelse(seedmix=="annuals", "annual", ifelse(seedmix=="perennials", "perennial", seedmix)))
names(canopycover) <- c("Orchard.Age", "Block", "Management", "Seed.Mix", "Canopy", "Year")
#############################
#############################

## I want to compare pollinator abundance and canopy cover, and I'd also like to keep the
## orchard age variable in there so that I can see if that has an effect (since canopy
## cover tended to vary with age)
## I also want to filter out 2020 data, since pollinators were only collected in 2021
#why? canopy is at the seeding plot level
#A: Maybe I can just select for canopy instead? I just want to include the canopy values in the
#'canopyvisits' dataset for the plot

canopyvisits <- full_join(observedpollinators, canopycover) %>%
  mutate(Number=ifelse(is.na(Number), 0, Number))%>%
  group_by(Orchard.Age, Block, Canopy, Seed.Mix) %>% 
  summarise(Number = sum(Number))

## Plot

ggplot(canopyvisits, aes(x=Canopy, y=Number, colour=as.factor(Orchard.Age))) +
  geom_point() +
  labs(x="Canopy Cover", y = "Insect Visitations") +
  stat_smooth()

#############################
#############################

## Statistics
## I'm using a mixed model. 'lme' uses t-tests and f-tests to find the significance between
## canopy cover and insect visits. Canopy cover is a random effect.

mmcanopy<-lme(Number~Canopy, random = ~1|Block/Orchard.Age, data = canopyvisits, na.action=na.omit)
summary(mmcanopy)
anova(mmcanopy)

mmcanopy2<-lme(Number~Seed.Mix+Canopy, random = ~1|Block/Orchard.Age, data = canopyvisits, na.action=na.omit)
summary(mmcanopy2)
anova(mmcanopy2) 
#seed mix yes, canopy no

## testing canopy cover by orchard age
mmcanopy3<-lme(Number~Orchard.Age+Canopy, random = ~1|Block/Orchard.Age, data = canopyvisits, na.action=na.omit)
summary(mmcanopy3)
anova(mmcanopy3)