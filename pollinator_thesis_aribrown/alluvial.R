## POLLINATOR THESIS PROJECT FIGURES
## Alluvial plots by seed mix

## Libraries needed
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggalluvial)

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
## Alluvial chart for plants in the annual seed mix and their associated pollinators
annualalluvial <- collectedpollinators %>%
  filter(Host.Plant %in% c("collomia", "amsinckia", "clarkia", "epilobium", "gilia",
                           "lotus", "plectritis", "sanguisorba")) %>%
  mutate(plantpollinators=paste(Host.Plant,Genus, sep = "_")) %>%
  group_by(Host.Plant,Genus,Count,plantpollinators) %>%
  summarise()

ggplot(data = annualalluvial,
       aes(axis1 = Genus, axis2 = Host.Plant,
           y = Count)) +
  scale_x_discrete(limits = c("Pollinator Taxa", "Host Plant"), expand = c(.05, .05)) +
  geom_alluvium(aes(fill = Genus)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal()

## Alluvial chart for plants in the perennial seed mix and their associated pollinators
perennialalluvial <- collectedpollinators %>%
  filter(Host.Plant %in% c("achillea", "agoseris", "lomatium", "potentilla",
                           "prunella", "viola", "geum", "eriophyllum")) %>%
  mutate(plantpollinators=paste(Host.Plant,Genus, sep = "_")) %>%
  group_by(Host.Plant,Genus,Count,plantpollinators) %>%
  summarise()

ggplot(data = perennialalluvial,
       aes(axis1 = Host.Plant, axis2 = Genus,
           y = Count)) +
  scale_x_discrete(limits = c("Host Plant", "Pollinator Taxa"), expand = c(.05, .05)) +
  geom_alluvium(aes(fill = Host.Plant)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20))

## Alluvial chart for plants in the industry seed mix and their associated pollinators
industryalluvial <- collectedpollinators %>%
  filter(Host.Plant %in% c("barley", "oats", "vetch", "clover")) %>%
  mutate(plantpollinators=paste(Host.Plant,Genus, sep = "_")) %>%
  group_by(Host.Plant,Genus,Count,plantpollinators) %>%
  summarise()

ggplot(data = industryalluvial,
       aes(axis1 = Genus, axis2 = Host.Plant,
           y = Count)) +
  scale_x_discrete(limits = c("Pollinator Taxa", "Host Plant"), expand = c(.05, .05)) +
  geom_alluvium(aes(fill = Genus)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal()

## Alluvial chart for control plants (weeds) and their associated pollinators
controlalluvial <- collectedpollinators %>%
  filter(Host.Plant == "weed") %>%
  mutate(plantpollinators=paste(Weed.Id,Genus, sep = "_")) %>%
  group_by(Weed.Id,Genus,Count,plantpollinators) %>%
  summarise()

ggplot(data = controlalluvial,
       aes(axis1 = Genus, axis2 = Weed.Id,
           y = Count)) +
  scale_x_discrete(limits = c("Pollinator Taxa", "Weed.Id"), expand = c(.05, .05)) +
  geom_alluvium(aes(fill = Genus)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal()
