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


#add in a full plot list to account for zeroes
plots <- read.csv("canopy_cover.csv")%>%
  filter(year==2021)%>% #have to do this here
  mutate(seedmix=ifelse(seedmix=="annuals", "annual", ifelse(seedmix=="perennials", "perennial", seedmix)))%>%
  dplyr::select(1:4)
names(plots) <- c("Orchard.Age", "Block", "Management", "Seed.Mix")

collectedpollinators_combined <- read.csv("collectedpollinators_combined.csv")
names(collectedpollinators_combined) <- c("Year", "Day","Month", "Orchard.Age","Block","Management",
                                          "Seed.Mix","Host.Plant","Order","Genus", "Notes",
                                          "Weed.ID", "Count")

collectedpollinators <- collectedpollinators_combined%>%
  mutate(Management=ifelse(Management=="flailscrape"|Management=="scraped", "scraped", ifelse(Management=="flail"|Management=="flailed", "flailed", "unmanaged")))%>%
  mutate(Seed.Mix=ifelse(Seed.Mix=="annuals"|Seed.Mix=="annual", "annual", ifelse(Seed.Mix=="perennials"|Seed.Mix=="perennial", "perennial", Seed.Mix)))%>%
  group_by(Year, Orchard.Age, Block, Management, Seed.Mix, Host.Plant, Order, Genus)%>%
  summarize(Count=sum(Count))
seedmix.rich<-collectedpollinators%>% #this is just the richness in each plot (ignoring what the species is)
  ungroup()%>%
  group_by(Orchard.Age, Block, Management, Seed.Mix)%>%
 summarize(richness=length(unique(Genus)))
seedmix.rich<-full_join(plots, seedmix.rich)%>%
  mutate(richness=ifelse(is.na(richness), 0, richness))

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

taxonomicrichness<-collectedpollinators%>% #this is just the richness in each plot (ignoring what the species is)
  mutate(seedmix2=ifelse(Host.Plant%in%perennialspecies, "perennial",
                         ifelse(Host.Plant%in%annualspecies, "annual", 
                                ifelse(Host.Plant%in%industryspecies, "industry", 
                                       ifelse(Host.Plant=="weed","weed", Host.Plant)))))%>%
  ungroup()%>%
  group_by(Orchard.Age, Block, Management, seedmix2)%>%
  summarize(richness=length(unique(Genus)))

newplots<-plots%>%  #convert to species types
  mutate(seedmix2=ifelse(Seed.Mix=="control", "weed", Seed.Mix))%>%
  dplyr::select(-Seed.Mix)

taxonomicrichness<-full_join(newplots, taxonomicrichness)%>% #need zeroes
  mutate(richness=ifelse(is.na(richness), 0, richness))



#############################
#############################

## Plot
## I want to do a box plot with error bars

#version 1: the actual seedmix
ggplot(subset(seedmix.rich, Seed.Mix!="megamix"), aes(x=Seed.Mix, y=richness))+geom_boxplot()+ #how did megamix get in here?
  facet_wrap(~Orchard.Age)

#version 2: based on species type
ggplot(subset(taxonomicrichness, seedmix2!="megamix"), aes(x=seedmix2, y=richness))+geom_boxplot() ##+ #how did megamix get in here?
  facet_wrap(~Orchard.Age)


#############################
#############################
## Statistics
## I want to do a similar stat test (anova and Tukey) to what I did for visitations to see
## if there's statistically significant differences in richness between seed mixes.

seedmixrichnessmeans<-taxonomicrichness%>%
ungroup()%>%
    group_by(Orchard.Age, Block, seedmix2)
  
aov <- aov(richness~seedmix2, data=seedmixrichnessmeans)
summary(aov)

## issue -- megamix shouldn't be in here; it's all zeroes
  
tukey <- TukeyHSD(aov, conf.level=.95)

## there is a statistically significant difference between perennials and industry, and
## perennials and weeds