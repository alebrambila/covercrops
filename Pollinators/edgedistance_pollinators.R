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

#add in a full plot list to account for zeroes
plots <- read.csv("canopy_cover.csv")%>%
  filter(year==2021)%>% #have to do this here
  mutate(seedmix=ifelse(seedmix=="annuals", "annual", ifelse(seedmix=="perennials", "perennial", seedmix)))%>%
  dplyr::select(1:4)
names(plots) <- c("Orchard.Age", "Block", "Management", "Seed.Mix")

observedpollinators <- read.csv("observedpollinators_2021.csv")%>%
  mutate(Management=ifelse(Management=="flailscrape", "scraped", ifelse(Management=="flail", "flailed", Management)))
names(observedpollinators) <- c ("Month", "Orchard.Age", "Block", "Management", "Seed.Mix", "Host.Plant", "Morphospecies", "Number", "Notes")
observedpollinators<-full_join(plots, observedpollinators)%>% #need to get zeroes
  mutate(Number=ifelse(is.na(Number), 0, Number))


edgedistances <- read.csv("block_edge_distances.csv")
names(edgedistances) <- c("Orchard.Age", "Block", "Distance")


#############################

#############################
#############################
##Comparing block distance from orchard edge and pollinator visitations

## Merging the datasets and counting up observed pollinator visits by sub-plot/seed mix
blockdensity <- merge(x=observedpollinators, y=edgedistances,by=c("Orchard.Age","Block"),all=TRUE) %>%
  group_by(Orchard.Age, Block, Management, Seed.Mix, Distance) %>% #added back Management, you dont need to analyze it but they are separate plots (replication)
  summarise(Number = sum(Number))

cleanblockdensity <- as.data.frame(blockdensity)%>%
  ungroup()

ggplot(cleanblockdensity, aes(x=Distance, y=Number)) +
  geom_point(aes(colour=as.factor(Orchard.Age))) +
  labs(x="Distance From Edge (m)", y = "Insect Visitations (per management plot)") +
  stat_smooth()

#############################
#############################
## Statistics
## I'm using a mixed model. 'lme' uses t-tests and f-tests to find the significance between
## edge distance and insect visits. Edge distance (block placement) is a random effect.

mmedgedistance<-lme(Number~Distance, random =  ~1|Management, data=cleanblockdensity) 
summary(mmedgedistance)
anova(mmedgedistance)#significant when you ignore orchard age


mmedgedistance<-lme(Number~Distance, random = list( ~1|Management, ~1|Orchard.Age), data = cleanblockdensity, na.action=na.omit)
summary(mmedgedistance)
anova(mmedgedistance) #not when you include it
