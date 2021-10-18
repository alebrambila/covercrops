## POLLINATOR THESIS PROJECT FIGURES
## Edge distance vs. pollinator visitations


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
edgedistances <- read.csv("block_edge_distances.csv")
names(edgedistances) <- c("Orchard.Age", "Block", "Distance")

#############################
#############################
##Comparing block distance from orchard edge and pollinator visitations

## Merging the datasets and counting up observed pollinator visits by sub-plot/seed mix
blockdensity <- merge(x=observedpollinators, y=edgedistances,by=c("Orchard.Age","Block"),all=TRUE) %>%
  group_by(Orchard.Age, Block, Seed.Mix, Distance) %>%
  summarise(Number = sum(Number))

cleanblockdensity <- as.data.frame(blockdensity)
cleanblockdensity[is.na(cleanblockdensity)] = 0

ggplot(cleanblockdensity, aes(x=Distance, y=Number, colour=Orchard.Age)) +
  geom_point() +
  labs(x="Distance From Edge(m)", y = "Insect Visitations") +
  stat_smooth()

#############################
#############################
## Statistics
## I'm using a mixed model. 'lme' uses t-tests and f-tests to find the significance between
## edge distance and insect visits. Edge distance (block placement) is a random effect.

mmedgedistance<-lme(Number~Distance, random = ~1|Distance, data = cleanblockdensity, na.action=na.omit)
summary(mmcanopy)
anova(mmcanopy)
