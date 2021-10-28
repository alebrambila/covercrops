## POLLINATOR THESIS PROJECT FIGURES
## Pollinator taxon richness versus within-plot floral diversity


## Libraries needed
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(googlesheets4)

#############################
#############################
## Import data
## Clean up column names and columns

collectedpollinators <- read.csv("collectedpollinators_combined.csv")
names(collectedpollinators) <- c ("Year","Day", "Month", "Orchard.Age", "Block",
                                  "Management", "Seed.Mix", "Host.Plant", "Order", "Genus", "Notes", 
                                  "Weed.Id", "Count")

## Below -- I'm using Alejandro's code to import and clean up the phenology dataset
#set the theme for all plots to black and white
theme_set(theme_classic())
gs4_deauth()

phenology <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1RrV3xTE2EgWgrY5LCbAAWNRDw7NDvgUvRTSsW6cJHjs/edit?usp=sharing")
april<-select(phenology, 1:4, 5:10)%>%
  mutate(lastflower=as.numeric(NA), fruit=as.numeric(NA), senesence=as.numeric(NA),  flailed=as.numeric(NA),  scraped=as.numeric(NA),  regrowth=as.numeric(NA),  postflowerveg=as.numeric(NA))
may<-  select(phenology, 1:4, 11:16)%>%
  mutate(lastflower=as.numeric(NA), fruit=as.numeric(NA), senesence=as.numeric(NA),  flailed=as.numeric(NA),  scraped=as.numeric(NA),  regrowth=as.numeric(NA),  postflowerveg=as.numeric(NA))
june<- select(phenology, 1:4, 17:24)%>%
  mutate(germination=as.numeric(NA), flailed=as.numeric(NA),  scraped=as.numeric(NA),  regrowth=as.numeric(NA),  postflowerveg=as.numeric(NA))
july<- select(phenology, 1:4, 25:32)%>%
  mutate(seedling=as.numeric(NA), germination=as.numeric(NA),  scraped=as.numeric(NA),  regrowth=as.numeric(NA),  postflowerveg=as.numeric(NA))
august<- select(phenology, 1:4, 33:44)%>%
  mutate(germination=as.numeric(NA))
april[is.na(april)] <- 0
may[is.na(may)] <- 0
june[is.na(june)] <- 0
july[is.na(july)] <- 0
august[is.na(august)] <- 0


colnames(april)<-c("orchardage", "block", "management", "species", "date", "germination", "seedling", "vegetative", "firstflower", "flowering", "lastflower", "fruit", "senesence", "flailed", "scraped", "regrowth", "postflowerveg")
colnames(may)<-c("orchardage", "block", "management", "species", "date", "germination", "seedling", "vegetative", "firstflower", "flowering", "lastflower", "fruit", "senesence", "flailed", "scraped", "regrowth", "postflowerveg")
colnames(june)<-c("orchardage", "block", "management", "species", "date", "seedling", "vegetative", "firstflower", "flowering", "lastflower", "fruit", "senesence", "germination", "flailed", "scraped", "regrowth", "postflowerveg")
colnames(july)<-c("orchardage", "block", "management", "species", "date", "vegetative", "firstflower", "flowering", "lastflower", "fruit", "senesence", "flailed", "seedling", "germination", "scraped", "regrowth", "postflowerveg")
colnames(august)<-c("orchardage", "block", "management", "species", "date", "seedling", "vegetative", "postflowerveg", "firstflower", "flowering", "lastflower", "fruit", "senesence", "regrowth", "flailed", "scraped", "germination")

phenologyclean<-rbind(april, may, june, july, august)%>%
  mutate(germination=ifelse(germination==100, 1, germination))%>%
  mutate(seedling=ifelse(seedling==100, 1, seedling))%>%
  mutate(vegetative=ifelse(vegetative==100, 1, vegetative))%>%
  mutate(firstflower=ifelse(firstflower==100, 1, firstflower))%>%
  mutate(flowering=ifelse(flowering==100, 1, flowering))%>%
  mutate(lastflower=ifelse(lastflower==100, 1, lastflower))%>%
  mutate(fruit=ifelse(fruit==100, 1, fruit))%>%
  mutate(senesence=ifelse(senesence==100, 1, senesence))%>%
  mutate(flailed=ifelse(flailed==100, 1, flailed))
#############################
#############################
## First, I'll find the taxonomic richness of each subplot
## I'll do this by grouping on the subplot level (a management treatment within 
## a block within an orchard age) and then finding the total number of unique genera within the subplot
## I'll output this in a new column called 'pollinatorrichness'

pollinatorsubplotrichness <- collectedpollinators %>%
  filter(Year == 2021) %>%
  select(Month, Orchard.Age, Block, Management, Genus) %>%
  group_by(Month, Orchard.Age, Block, Management) %>%
  mutate(pollinatorrichness=length(unique(Genus)))

## I'll do a test to make sure that it's counting up the unique genera correctly. Looks like its working!
testpollinatorsubplot <- pollinatorsubplotrichness %>%
  filter(Orchard.Age == 15, Block == 2, Management == "flail")

## Next, I want to do the same thing but for floral species richness.
## I want to again group by subplot, but then select the species of plants that were producing flowers
## (in the flowering, first flower, or last flower category) and then find the total number of unique
## flowering species.
## I also want to rename some columns so that they match the pollinator dataset,
## and rename the management values so that they match up with the pollinator dataset

plantsubplotrichness <- phenologyclean %>%
  filter(firstflower==1|flowering==1|lastflower==1)%>%
  group_by(orchardage, block, management, date)%>%
  summarize(richness=length(unique(species))) %>%
  mutate(management=ifelse(management=="flailed", "flail", management)) %>%
  mutate(management=ifelse(management=="scraped", "flailscrape", management)) %>%
  mutate(date=ifelse(date=="4", "april", date))

names(plantsubplotrichness) <- c ("Orchard.Age", "Block",
                                  "Management", "Month", "richness")

#############################
#############################

## Graphing
## I want to compare x = subplot floral diversity; y = subplot pollinator richness
## First, I'll want to join the plant & pollinator subplot richness datasets by 
## orchard age, block, management, and month (date)


##LINE 496