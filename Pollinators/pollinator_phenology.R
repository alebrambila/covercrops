## POLLINATOR THESIS PROJECT FIGURES
## Plant/pollinator phenology

#############################
#############################
## Libraries & functions

## Libraries needed
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(googlesheets4)
library(nlme)

## Functions needed
#function to calculate standard error
calcSE<-function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}
#############################
#############################
## Importing and cleaning up data -- for plants

## I'm using Alejandro's code from the 'phenology' R file.
## Code is below for importing plant/floral data, and creating a column
## for floral abundance values

#set the theme for all plots to black and white
theme_set(theme_classic())
gs4_deauth()

phenology <- read_sheet("https://docs.google.com/spreadsheets/d/1RrV3xTE2EgWgrY5LCbAAWNRDw7NDvgUvRTSsW6cJHjs/edit?usp=sharing")
april<-dplyr::select(phenology, 1:4, 5:10)%>%
  mutate(lastflower=as.numeric(NA), fruit=as.numeric(NA), senesence=as.numeric(NA),  flailed=as.numeric(NA),  scraped=as.numeric(NA),  regrowth=as.numeric(NA),  postflowerveg=as.numeric(NA))
may<-  dplyr::select(phenology, 1:4, 11:16)%>%
  mutate(lastflower=as.numeric(NA), fruit=as.numeric(NA), senesence=as.numeric(NA),  flailed=as.numeric(NA),  scraped=as.numeric(NA),  regrowth=as.numeric(NA),  postflowerveg=as.numeric(NA))
june<- dplyr::select(phenology, 1:4, 17:24)%>%
  mutate(germination=as.numeric(NA), flailed=as.numeric(NA),  scraped=as.numeric(NA),  regrowth=as.numeric(NA),  postflowerveg=as.numeric(NA))
july<- dplyr::select(phenology, 1:4, 25:32)%>%
  mutate(seedling=as.numeric(NA), germination=as.numeric(NA),  scraped=as.numeric(NA),  regrowth=as.numeric(NA),  postflowerveg=as.numeric(NA))
august<- dplyr::select(phenology, 1:4, 33:44)%>%
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

canopy<-read_csv("canopy_cover.csv")%>%
  #weighted adjustment for blocks 4-6 of 15 that are shaded by xmas trees
  mutate(canopy=ifelse(orchard_age==15&block>3&seedmix!='annuals'&seedmix!='perennials', canopy*.4+90*.6, canopy))%>%
  mutate(canopy=ifelse(orchard_age==15&block<3&seedmix!='annuals'&seedmix!='perennials', canopy*.4+60*.6, canopy))


plotkey<-canopy%>%
  group_by(orchard_age, block, management, seedmix)%>%
  summarize()

floral<-read_csv("floral.csv")%>%
  gather(species, infloresences, 4:26)%>%
  filter(!is.na(infloresences))%>%
  mutate(orchardage=as.factor(orchardage))

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

#richness and abundance per plot per month
ip3<-read_sheet("https://docs.google.com/spreadsheets/d/10q37avMnA0x4wG69Joh1eMKdMAqsc7Zs4zIIKT1MNRg/edit?usp=sharing")%>%
  mutate(numericmonth=ifelse(Month=="april", 1, 
                             ifelse(Month=="may", 2, 
                                    ifelse(Month=="june", 3, 
                                           ifelse(Month=="july", 4, 5)))))%>%
  group_by(numericmonth, block, orchard_age, management)%>%
  summarize(richness=length(unique(Genus)), abundance=n())
pk2<-plotkey%>%group_by(orchard_age, block, management)%>%summarize()
ip3<-left_join(pk2, ip3)
ip4<-dplyr::select(ip3, -abundance)%>%spread(numericmonth, richness, fill=0)%>%
  gather(numericmonth, richness, `1`, `2`, `3`, `4`, `5`)%>%
  dplyr::select(-4)
cp<-left_join(mutate(ip4, numericmonth=as.numeric(numericmonth)), ip3)%>%
  mutate(abundance=ifelse(is.na(abundance), 0, abundance))

phenflor<-left_join(mutate(phenology_long0, orchardage=as.factor(orchardage)), floral)%>%
  mutate(anyflower=ifelse(firstflower==1|flowering==1|lastflower==1, 1, 0))%>%
  filter(anyflower==1)%>%
  mutate(infloresences=ifelse(flowering==1, infloresences, infloresences*.1))%>%
  dplyr::select(1:5, 18)%>%
  mutate(infloresences=ifelse(is.na(infloresences),1,infloresences))

#number of flowers available in each plot
phenflorsum<-phenflor%>%
  group_by(orchardage, date, block, management)%>%
  summarize(flor=sum(infloresences))%>%
  mutate(orchard_age=orchardage)%>%
  ungroup()%>%
  dplyr::select(-orchardage)

#add in zeroes and summarize for SE
phenflorsum1<-left_join(mutate(pk2, orchard_age=as.factor(orchard_age)), phenflorsum)%>%
  spread(date, flor, fill=0)%>%
  gather(numericmonth, flor, `4`, `5`, `6`, `7`, `8`)%>%
  dplyr::select(-4)%>%
  mutate(numericmonth=as.factor(numericmonth))

pfsumsum<-phenflorsum1%>%
  group_by(orchard_age, numericmonth)%>%
  summarize(meanflor=mean(flor), seflor=calcSE(flor))
#############################
#############################
## Graphing
## Using Alejandro's code for this -- line 135
## Showing floral abundance per plot over time  (from April - August 2021)

ggplot()+
  geom_jitter(data=phenflorsum1, aes(x=as.numeric(numericmonth), y=flor, color=as.factor(orchard_age)), width=.2, alpha=.5, size=.7)+
  ylab("floral abundance per plot")+
  geom_point(size=2, data=pfsumsum, aes(x=as.numeric(numericmonth), y=meanflor, color=as.factor(orchard_age)))+
  geom_line(size=1, data=pfsumsum, aes(x=as.numeric(numericmonth), y=meanflor, color=as.factor(orchard_age)))+
  geom_errorbar(data=pfsumsum, width=.1, aes(x=as.numeric(numericmonth), ymin=meanflor-seflor, ymax=meanflor+seflor, color=as.factor(orchard_age)))
#############################
#############################
## Importing and cleaning up data -- for pollinators

## I'm using Alejandro's code from the 'phenology' R file.
## Code is below for importing pollinator data, and creating a column
## for pollinator abundance values

#observed pollinators
op<-read_sheet("https://docs.google.com/spreadsheets/d/1IeWQPtXPJ-MrIua0wZswSJNRnU3bvmkDKtTc5lBzEbk/edit?usp=sharing")%>%
  mutate(numericmonth=ifelse(Month=="april", 1, 
                             ifelse(Month=="may", 2, 
                                    ifelse(Month=="june", 3, 
                                           ifelse(Month=="july", 4, 5)))))%>%
  group_by(numericmonth, block, orchard_age, management)%>%
  summarize(richness=length(unique(Morphospecies)), abundance=sum(Count))
op1<-full_join(pk2, op)

op2<-dplyr::select(op1, -abundance)%>%spread(numericmonth, richness, fill=0)%>%
  gather(numericmonth, richness, `1`, `2`, `3`, `4`, `5`)%>%
  dplyr::select(-4)
op3<-left_join(mutate(op2, numericmonth=as.numeric(numericmonth)), op1)%>%
  mutate(numericmonth=as.factor(numericmonth))%>%
  mutate(abundance=ifelse(is.na(abundance), 0, abundance))

opsum<-op3%>%
  group_by(orchard_age, numericmonth)%>%
  summarize(meanrich=mean(richness), serich=calcSE(richness), meanabun=mean(abundance), seabun=calcSE(abundance))

#############################
#############################
## Graphing
## Using Alejandro's code for this -- line 439
## Showing pollinator abundance per plot over time  (from April - August 2021)

ggplot()+
  geom_jitter(data=op3, aes(x=numericmonth, y=abundance, color=as.factor(orchard_age)), width=.2, alpha=.5, size=.7)+
  ylab("insect observations per plot")+
  geom_point(size=2, data=opsum, aes(x=numericmonth, y=meanabun, color=as.factor(orchard_age)))+
  geom_line(size=1, data=opsum, aes(x=numericmonth, y=meanabun, color=as.factor(orchard_age)))+
  geom_errorbar(data=opsum, width=.1, aes(x= numericmonth, ymin=meanabun-seabun, ymax=meanabun+seabun, color=as.factor(orchard_age)))

#############################
#############################
## Statistics
## I want to find what the significantly highest month is within each orchard for
## both pollinator visitations & floral abundance
## I'll use a mixed model to do this

## Floral abundance mixed model: 'lme' uses t-tests and f-tests to find the significance between
## month and floral abundance within each orchard age. Orchard age is a random effect.

# I did a separate model for each orchard age, so you no longer need the effect of orchard age in the model. 
# managment is random because we are controlling for its effect. 
mmfloralphenology.15<-lme(flor~numericmonth, random =  ~1|management, data=subset(phenflorsum1, orchard_age==15)) 
library(lsmeans)
lsmeans(mmfloralphenology.15,
                      pairwise ~ numericmonth,
                      adjust="tukey") # p-vals show you differences between different groups: ex. youcan see 6 is significantly different fron all others

mmfloralphenology.40<-lme(flor~numericmonth, random =  ~1|management, data=subset(phenflorsum1, orchard_age==40)) 
lsmeans(mmfloralphenology.40,
        pairwise ~ numericmonth,
        adjust="tukey")

mmfloralphenology.60<-lme(flor~numericmonth, random =  ~1|management, data=subset(phenflorsum1, orchard_age==60 )) 
lsmeans(mmfloralphenology.60,
        pairwise ~ numericmonth,
        adjust="tukey") #as expected here month 6 and 5 are not sig different, 

#COMPACT LETTER DISPLAY
cld(glht(mmfloralphenology.15, mcp(numericmonth="Tukey"))) 
cld(glht(mmfloralphenology.40, mcp(numericmonth="Tukey"))) 
cld(glht(mmfloralphenology.60, mcp(numericmonth="Tukey"))) 

## Pollinator abundance mixed model

mmpollinatorphenology<-lme(abundance~as.factor(numericmonth), random =  ~1|orchard_age, data=op3) 
summary(mmpollinatorphenology)
anova(mmpollinatorphenology)# now its significant, for some reason it had to be a factor here (which it is). I think what was happening was that it was trying for a linear relationship with the numbers which didnt fit since it's a hump. 

#break apart by orchard
mmpollinatorphenology.15<-lme(abundance~numericmonth, random =  ~1|management, data=subset(op3, orchard_age==15)) 
summary(mmpollinatorphenology.15)
lsmeans(mmpollinatorphenology.15,
        pairwise ~ numericmonth,
        adjust="tukey") #showing significant pairings now

mmpollinatorphenology.40<-lme(abundance~numericmonth, random =  ~1|management, data=subset(op3, orchard_age==40)) 
summary(mmpollinatorphenology.40)
lsmeans(mmpollinatorphenology.40,
        pairwise ~ numericmonth,
        adjust="tukey") 

mmpollinatorphenology.60<-lme(abundance~numericmonth, random =  ~1|management, data=subset(op3, orchard_age==60)) 
summary(mmpollinatorphenology.60)
lsmeans(mmpollinatorphenology.60,
        pairwise ~ numericmonth,
        adjust="tukey") 

#COMPACT LETTER DISPLAY - POLLINATORS
cld(glht(mmpollinatorphenology.15, mcp(numericmonth="Tukey"))) 
cld(glht(mmpollinatorphenology.40, mcp(numericmonth="Tukey"))) 
cld(glht(mmpollinatorphenology.60, mcp(numericmonth="Tukey"))) 
