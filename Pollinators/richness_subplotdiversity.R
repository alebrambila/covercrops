## POLLINATOR THESIS PROJECT FIGURES
## Pollinator taxon richness versus within-plot floral diversity


## Libraries needed
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(googlesheets4)

## Functions

#function to calculate standard error
calcSE<-function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}

#############################
#############################
## Import data for pollinators
## Clean up column names and columns

collectedpollinators <- read.csv("collectedpollinators_combined.csv")
names(collectedpollinators) <- c ("year","day", "month", "orchard_age", "block",
                                  "management", "seed_mix", "host_plant", "order", "genus", "notes", 
                                  "weed_id", "count")

pollinatorsubplotrichness <- collectedpollinators %>%
  filter(year == 2021) %>%
  mutate(orchard_age = as.factor(orchard_age)) %>%
  mutate(numericmonth = ifelse(month=="april", 4,
                               ifelse(month=="may", 5, ifelse(month=="june", 6, ifelse(month=="july", 7, 8)))))%>%
  select(numericmonth, orchard_age, block, management, genus) %>%
  mutate(management=ifelse(management=="flail", "flailed", management)) %>%
  mutate(management=ifelse(management=="flailscrape", "scraped", management)) %>%
  mutate(management=ifelse(management=="no management", "unmanaged", management)) %>%
  group_by(numericmonth, orchard_age, block, management) %>%
  summarise(pollinatorrichness=length(unique(genus)))

## Plant/floral data
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

floral<-read_csv("floral.csv")%>%
  gather(species, infloresences, 4:26)%>%
  filter(!is.na(infloresences))%>%
  mutate(orchardage=as.factor(orchardage))

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

phenology_long0<-rbind(april, may, june, july, august)%>%
  mutate(germination=ifelse(germination==100, 1, germination))%>%
  mutate(seedling=ifelse(seedling==100, 1, seedling))%>%
  mutate(vegetative=ifelse(vegetative==100, 1, vegetative))%>%
  mutate(firstflower=ifelse(firstflower==100, 1, firstflower))%>%
  mutate(flowering=ifelse(flowering==100, 1, flowering))%>%
  mutate(lastflower=ifelse(lastflower==100, 1, lastflower))%>%
  mutate(fruit=ifelse(fruit==100, 1, fruit))%>%
  mutate(senesence=ifelse(senesence==100, 1, senesence))%>%
  mutate(flailed=ifelse(flailed==100, 1, flailed))

## Canopy data -- for making the 'plot key' data set

canopy<-read_csv("canopy_cover.csv")%>%
  #weighted adjustment for blocks 4-6 of 15 that are shaded by xmas trees
  mutate(canopy=ifelse(orchard_age==15&block>3&seedmix!='annuals'&seedmix!='perennials', canopy*.4+90*.6, canopy))%>%
  mutate(canopy=ifelse(orchard_age==15&block<3&seedmix!='annuals'&seedmix!='perennials', canopy*.4+60*.6, canopy))

#############################
#############################
## Dataset manipulation and joining
## From the 'subplotrichness_phenologybase' file

plotkey<-canopy%>%
  group_by(orchard_age, block, management, seedmix)%>%
  summarize()
pk2<-plotkey%>%group_by(orchard_age, block, management)%>%summarize()

#observed pollinators
op<-read_sheet("https://docs.google.com/spreadsheets/d/1IeWQPtXPJ-MrIua0wZswSJNRnU3bvmkDKtTc5lBzEbk/edit?usp=sharing")%>%
  mutate(numericmonth=ifelse(Month=="april", 1, 
                             ifelse(Month=="may", 2, 
                                    ifelse(Month=="june", 3, 
                                           ifelse(Month=="july", 4, 5)))))%>%
  group_by(numericmonth, block, orchard_age, management)%>%
  summarize(richness=length(unique(Morphospecies)), abundance=sum(Count))

op1<-full_join(pk2, op)

op2<-select(op1, -abundance)%>%spread(numericmonth, richness, fill=0)%>%
  gather(numericmonth, richness, `1`, `2`, `3`, `4`, `5`)%>%
  select(-4)
op3<-left_join(mutate(op2, numericmonth=as.numeric(numericmonth)), op1)%>%
  mutate(abundance=ifelse(is.na(abundance), 0, abundance))

opsum<-op3%>%
  group_by(orchard_age, numericmonth)%>%
  summarize(meanrich=mean(richness), serich=calcSE(richness), meanabun=mean(abundance), seabun=calcSE(abundance))

#number of flowers available in each plot
phenflor<-left_join(mutate(phenology_long0, orchardage=as.factor(orchardage)), floral)%>%
  mutate(anyflower=ifelse(firstflower==1|flowering==1|lastflower==1, 1, 0))%>%
  filter(anyflower==1)%>%
  mutate(infloresences=ifelse(flowering==1, infloresences, infloresences*.1))%>%
  select(1:5, 18)%>%
  mutate(infloresences=ifelse(is.na(infloresences),1,infloresences))

phenflorsum<-phenflor%>%
  group_by(orchardage, date, block, management)%>%
  summarize(flor=sum(infloresences))%>%
  mutate(orchard_age=orchardage)%>%
  ungroup()%>%
  select(-orchardage)

#add in zeroes and summarize for SE
phenflorsum1<-left_join(mutate(pk2, orchard_age=as.factor(orchard_age)), phenflorsum)%>%
  spread(date, flor, fill=0)%>%
  gather(numericmonth, flor, `4`, `5`, `6`, `7`, `8`)%>%
  select(-4)

#unique target spp flowering per plot (richness)
phenology_richness<-phenology_long0%>%
  filter(firstflower==1|flowering==1|lastflower==1)%>%
  group_by(orchardage, block, management, date)%>%
  summarize(richness=length(unique(species)))

#add in zeroes and summarize for SE
phenrich<-left_join(mutate(pk2, orchard_age=as.numeric(orchard_age)), 
                    mutate(phenology_richness, orchard_age=as.numeric(orchardage)))%>%
  spread(date, richness, fill=0)%>%
  gather(numericmonth, richness, `4`, `5`, `6`, `7`, `8`)%>%
  select(-4, -5)

#correlations between pollinators and plants (abun/rich)
florsum<-full_join(phenflorsum1, mutate(phenrich, orchard_age=as.factor(orchard_age)))
op4<-mutate(op3, insectrich=richness, insectabun=abundance)%>%
  mutate(numericmonth=numericmonth+3)%>%
  select(-richness, -abundance)

cors<-left_join(mutate(florsum, numericmonth=as.numeric(numericmonth)), mutate(op4, orchard_age=as.factor(orchard_age)))%>%
  ungroup()

cors0<-full_join(cors, pollinatorsubplotrichness) %>%
  mutate(pollinatorrichness=ifelse(is.na(pollinatorrichness), 0, pollinatorrichness)) %>%
  ungroup() 

#############################
#############################

## Graphing
## From the 'subplotrichness_phenologybase' file

ggplot(cors0, aes(x=richness, y=pollinatorrichness, color=orchard_age))+
  geom_jitter()+#stat_regline_equation()+
  #geom_point(aes(y=insectrich), shape=2)+
  geom_smooth(method="lm", se=F)+stat_cor()+
  xlab("unique target species flowering per plot")+ylab("collected insect richness per plot")

## Stat results - significant correlation of insect richness & plant richness across all
## orchard ages