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
## Statistics

##First, I want to test if the means are different for total pollinator visitations in each seed mix
## category. I'll create a dataset first of all the visitations (not divided up my morphospecies)
## per seed mix, and then compare the means with the 'aov' function
seedmixmeans <- observedpollinators %>%
  select(Orchard.Age, Block, Management, Seed.Mix, Number) %>%
  group_by(Orchard.Age, Block, Seed.Mix) %>%
  summarise(Number = sum(Number))

aov <- aov(Number~Seed.Mix, data=seedmixmeans)
summary(aov)

## Results: P < 0.05, so I'll go ahead with a Tukey test
tukey <- TukeyHSD(aov, conf.level=.95)

## Tukey results: There's a significant difference between the perennial & megamix and perennial & control
## pollinator visitations