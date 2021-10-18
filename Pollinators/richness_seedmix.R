## POLLINATOR THESIS PROJECT FIGURES
## Taxonomic richness by seed mix


## Libraries needed
library(tidyverse)
library(ggplot2)
library(dplyr)

#############################
#############################
## Import data
## Clean up column names and columns

collectedpollinators <- read.csv("collectedpollinators_combined.csv")
names(collectedpollinators) <- c ("Year","Day", "Month", "Orchard.Age", "Block",
                                  "Management", "Seed.Mix", "Host.Plant", "Order", "Genus", "Notes", 
                                  "Weed.Id", "Count")
#############################
#############################

## First, I need to filter the collected pollinators dataset to find the taxonomic
## richness for each seed mix
## To do this, I don't actually want to use the 'Seed.Mix' column, since it's just
## the sub-plot that the insect was collected in and might have escapee plants in it.
## Instead, I should be able to filter by the actual plant that an insect was collected on
## and put it in the corresponding seed mix category
## E.g., if an insect was collected off of Achillea that was in the 'annual' subplot, I'll
## just look at the plant identity it was collected off of when adding it to the taxonomic
## richness value for the perennial seed mix -- so it doesn't matter that it was collected
## in an annual sub-plot.

annualspecies <- c("collomia", "amsinckia", "clarkia", "epilobium", "gilia",
                    "lotus", "plectritis", "sanguisorba")

perennialspecies <- c("achillea", "agoseris", "lomatium", "potentilla",
                                                      "prunella", "viola", "geum", "eriophyllum")

industryspecies <- c("barley", "oats", "vetch", "clover")

taxonomicrichness <- collectedpollinators %>%
  select(Genus, Host.Plant) %>%
  mutate(seedmix=ifelse(Host.Plant==perennialspecies, "perennial",
                        ifelse(Host.Plant==annualspecies, "annual", 
                               ifelse(Host.Plant==industryspecies, "industry", 
                                      ifelse(Host.Plant=="weed","control", Host.Plant)))))
  
#############################
#############################

## Plot
## I want to do a box plot with error bars