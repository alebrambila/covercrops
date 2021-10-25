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

observedpollinators <- read.csv("observedpollinators_2021.csv")
names(observedpollinators) <- c ("Month", "Orchard.Age", "Block", "Management", "Seed.Mix", "Host.Plant", "Morphospecies", "Number", "Notes")

canopycover <- read.csv("canopy_cover.csv")
names(canopycover) <- c("Orchard.Age", "Block", "Management", "Seed.Mix", "Canopy", "Year")
#############################
#############################

## I want to compare pollinator abundance and canopy cover, and I'd also like to keep the
## orchard age variable in there so that I can see if that has an effect (since canopy
## cover tended to vary with age)
## I also want to filter out 2020 data, since pollinators were only collected in 2021

canopyvisits <- merge(x=observedpollinators, y=canopycover,by=c("Orchard.Age","Block"),all=TRUE) %>%
  filter(Year==2021) %>%
  group_by(Orchard.Age, Block, Canopy) %>%
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

mmcanopy<-lme(Number~Canopy, random = ~1|Orchard.Age, data = canopyvisits, na.action=na.omit)
summary(mmcanopy)
anova(mmcanopy)

mmcanopy<-lme(Number~Canopy, random = ~1|Block/Orchard.Age, data = canopyvisits, na.action=na.omit)
summary(mmcanopy)
anova(mmcanopy)
