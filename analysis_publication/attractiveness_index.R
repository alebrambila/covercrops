library(tidyverse)
library(tidyr)
library(ggpubr)
library(googlesheets4)
library(dplyr)

##############
## floral abundance per management plot per month from phenology script

## creating 'plotkey'
# canopy cover
canopy0<-read.csv("canopy_cover.csv")
names(canopy0)<-c("orchard_age","block","management","seedmix","canopy","year")
  #weighted adjustment for blocks 4-6 of 15 that are shaded by xmas trees
  canopy<-canopy0%>%
  mutate(canopy=ifelse(orchard_age==15&block>3&seedmix!='annuals'&seedmix!='perennials', canopy*.4+90*.6, canopy))%>%
  mutate(canopy=ifelse(orchard_age==15&block<3&seedmix!='annuals'&seedmix!='perennials', canopy*.4+60*.6, canopy))
  
plotkey<-canopy%>%
    group_by(orchard_age, block, management, seedmix)%>%
    summarize()

## floral abundance (from phenology script)
#set the theme for all plots to black and white
theme_set(theme_classic())
gs4_deauth()

phenology <- read_sheet("https://docs.google.com/spreadsheets/d/1RrV3xTE2EgWgrY5LCbAAWNRDw7NDvgUvRTSsW6cJHjs/edit?usp=sharing")
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


floral0<-read.csv("floral.csv")
names(floral0)<-c("orchardage", "block","management","amsmen","plecon","colgra","clapur","epiden","gilcap","sanann","lotpur","achmil","geumac",
                  "pruvul","potgra","erilan","agogra","viopra","lomnud","barley","oats","vetch","clover","cartum","fesroe","dancal")

floral<-floral0%>%
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
ip4<-select(ip3, -abundance)%>%spread(numericmonth, richness, fill=0)%>%
  gather(numericmonth, richness, `1`, `2`, `3`, `4`, `5`)%>%
  select(-4)
cp<-left_join(mutate(ip4, numericmonth=as.numeric(numericmonth)), ip3)%>%
  mutate(abundance=ifelse(is.na(abundance), 0, abundance))

cpsum<-cp%>%
  group_by(orchard_age, numericmonth)%>%
  summarize(meanrich=mean(richness), serich=calcSE(richness), meanabun=mean(abundance), seabun=calcSE(abundance))


#add up all of the 'flowering' plots by species, orchard, date
phenology_count<-phenology_long0%>%
  group_by(orchardage, species, date)%>%
  summarize(flowering=sum(firstflower, flowering, lastflower))%>%
  filter(date>3)

phenflor<-left_join(mutate(phenology_long0, orchardage=as.factor(orchardage)), floral)%>%
  mutate(anyflower=ifelse(firstflower==1|flowering==1|lastflower==1, 1, 0))%>%
  filter(anyflower==1)%>%
  mutate(infloresences=ifelse(flowering==1, infloresences, infloresences*.1))%>%
  select(1:5, 18)%>%
  mutate(infloresences=ifelse(is.na(infloresences),1,infloresences))

#add in zeroes
phenflor_spread<-phenflor%>%
  mutate(id=paste(date, orchardage, block, management, sep="_"))%>%
  spread(species, infloresences, fill=0)%>%
  gather(species, infloresences, 6:27)%>%
  mutate(seedmix=ifelse(species%in%c("amsmen", "clapur", "colgra", "lotpur", "epiden", "gilcap", "plecon", "sanann"), "annuals", NA))%>%
  mutate(seedmix=ifelse(species%in%c("achmil", "erilan", "geumac", "agogra", "lomnud", "potgra", "pruvul", "viopra"), "perennials", seedmix))%>%
  mutate(seedmix=ifelse(species%in%c("vetch", "clover"), "industry", seedmix))%>%
  filter(!is.na(seedmix))%>%
  mutate(id=paste(id, seedmix, sep="_")) %>%
  mutate(rem=paste(management, date, sep="_"))%>%
  filter(!rem%in%c("flailed_7", "flailed_8", "scraped_7", "scraped_8")) #these will be zero, remove them.

#number of flowers available in each seed mix per plot (over all time!)
phenflorsum0<-phenflor_spread%>%
  group_by(orchardage, block, management, species)%>% #ALEJANDRO changed seedmix to species
  summarize(flor=sum(infloresences))%>%
  mutate(orchard_age=orchardage)%>%
  ungroup()%>%
  select(-orchardage)

#############
## pollinator abundance per plot per month
#observed pollinators (per seed mix per management plot, over all time)
op<-read_sheet("https://docs.google.com/spreadsheets/d/1IeWQPtXPJ-MrIua0wZswSJNRnU3bvmkDKtTc5lBzEbk/edit?usp=sharing")%>%
  mutate(numericmonth=ifelse(Month=="april", 4, 
                             ifelse(Month=="may", 5, 
                                    ifelse(Month=="june", 6, 
                                           ifelse(Month=="july", 7, 8))))) %>%
  mutate(seedmix=ifelse(seedmix=="annual", "annuals", ifelse(seedmix=="perennial", "perennials", seedmix)))%>%
  mutate(morphospecies=ifelse(Morphospecies=="ant", "hymenoptera", ifelse(Morphospecies=="wasp", 
                                                                          "hymenoptera",
                                                                          ifelse(Morphospecies=="aphid","true bug", 
                                                                                 ifelse(Morphospecies=="mosquito","other fly", Morphospecies))))) %>%
  group_by(block, orchard_age, management, `Host Plant`, morphospecies)%>%
  summarize(richness=length(unique(morphospecies)), abundance=sum(Count))
##^^ updating morphospecies categories based on Ari's supplemental thesis tables

op1<-left_join(plotkey, op)
op2<-select(op1, -richness)%>%
  mutate(abundance=ifelse(is.na(abundance), 0, abundance)) %>%
  #mutate(morphospecies=ifelse(is.na(morphospecies), 0, morphospecies))%>%
  filter(seedmix!="megamix") %>%
  select(-seedmix)%>%
  unique()
op2$orchard_age<-factor(op2$orchard_age, levels=c(15, 60, 40))
op_spread<-spread(op2, morphospecies, abundance, fill=0)%>%
  gather(morphospecies, abundance, 6:19)%>%
  select(-5)%>%
  mutate(species=(ifelse(`Host Plant`=="achillea", "achmil", 
                         ifelse(`Host Plant`=="eriophyllum", "erilan", 
                                ifelse(`Host Plant`=="gilia", "gilcap", 
                                       ifelse(`Host Plant`=="prunella", "pruvul", 
                                              ifelse(`Host Plant`=="epilobium", "epiden", 
                  ifelse(`Host Plant`=="clarkia", "clapur", 
                         ifelse(`Host Plant`=="plectritis", "plecon", 
                                ifelse(`Host Plant`=="collomia", "colgra", 
                                       ifelse(`Host Plant`=="geum", "geumac", 
                                              ifelse(`Host Plant`=="lomatium", "lomnut", 
                  ifelse(`Host Plant`=="amsinckia", "amsmen", 
                         ifelse(`Host Plant`=="potentilla", "potgra", `Host Plant`))))))))))))))%>%
  select(-4)
##note: need to change species names in pollinators data to species codes to match phenology


#############
## find attractiveness index 
## join phenflorsum (number infloresences per plot per month) and op (pollinator visits per plot per month)

attindex_plot<-full_join(phenflorsum0, op_spread) %>%
  spread(morphospecies, abundance, fill=0) %>%
  gather(morphospecies, abundance, 7:19)%>%
  mutate(abundance=ifelse(is.na(abundance), 0, abundance)) %>%
  mutate(flor=ifelse(is.na(flor), 0, flor)) %>%
  select(-6, -7)%>% #this is at the plot level
  group_by(morphospecies, species)%>%
  filter(flor!=0)%>%
  mutate(attractiveness=abundance/flor)%>%
  filter(!is.na(attractiveness))%>%
  summarize(meanattractiveness=mean(attractiveness))

#viz
ggplot(attindex_plot, aes(x=morphospecies, y=meanattractiveness)) +
  geom_jitter(aes(color=species), width=.1)+
  scale_y_continuous(trans='log10')

attindex_block<-full_join(phenflorsum0, op_spread) %>%
  spread(morphospecies, abundance, fill=0) %>%
  gather(morphospecies, abundance, 7:19)%>%
  mutate(abundance=ifelse(is.na(abundance), 0, abundance)) %>%
  mutate(flor=ifelse(is.na(flor), 0, flor)) %>%
  select(-6, -7)%>%
  group_by(orchard_age, block, species, morphospecies)%>%
  summarize(abundance=sum(abundance), flor=sum(flor)) %>%#this is at the block level
  filter(flor!=0)%>%
  filter(!is.na(species))%>%
  group_by(morphospecies, species)%>%
  mutate(attractiveness=abundance/flor)%>%
  filter(!is.na(attractiveness))%>%
  summarize(meanattractiveness=mean(attractiveness))

#viz
ggplot(attindex_block, aes(x=morphospecies, y=meanattractiveness)) +
  geom_jitter(aes(color=species), width=.1)+
  scale_y_continuous(trans='log10')

attindex_orchard<-full_join(phenflorsum0, op_spread) %>%
  spread(morphospecies, abundance, fill=0) %>%
  gather(morphospecies, abundance, 7:19)%>%
  mutate(abundance=ifelse(is.na(abundance), 0, abundance)) %>%
  mutate(flor=ifelse(is.na(flor), 0, flor)) %>%
  select(-6, -7)%>%
  group_by(orchard_age, species, morphospecies)%>%
  summarize(abundance=sum(abundance), flor=sum(flor)) %>%#this is at the block level
  filter(flor!=0)%>%
  filter(!is.na(species))%>%
  group_by(morphospecies, species)%>%
  mutate(attractiveness=abundance/flor)%>%
  filter(!is.na(attractiveness))%>%
  summarize(meanattractiveness=mean(attractiveness)) #this is at the block level

#viz
ggplot(attindex_orchard, aes(x=morphospecies, y=meanattractiveness)) +
  geom_jitter(aes(color=species), width=.1)+
  scale_y_continuous(trans='log10')

attindex_all<-full_join(phenflorsum0, op_spread) %>%
  spread(morphospecies, abundance, fill=0) %>%
  gather(morphospecies, abundance, 7:19)%>%
  mutate(abundance=ifelse(is.na(abundance), 0, abundance)) %>%
  mutate(flor=ifelse(is.na(flor), 0, flor)) %>%
  select(-6, -7)%>%
  group_by(species, morphospecies)%>%
  summarize(abundance=sum(abundance), flor=sum(flor)) %>%#this is at the block level
  filter(flor!=0)%>%
  filter(!is.na(species))%>%
  group_by(morphospecies, species)%>%
  mutate(attractiveness=abundance/flor)%>%
  filter(!is.na(attractiveness))%>%
  summarize(meanattractiveness=mean(attractiveness)) #across the whole thing

#viz
ggplot(attindex_all, aes(x=morphospecies, y=meanattractiveness)) +
  geom_jitter(aes(color=species), width=.1)+
  scale_y_continuous(trans='log10')



## attractiveness index = # pollinator visits for each morphospecies / # flowers
## ^^ pollinators and flowers are both per seed mix per management plot summed over all time

attindex0 <- attindex%>%
  group_by(morphospecies, seedmix)%>%
  mutate(attractiveness=abundance/flor)
  summarize(meanattractiveness=mean(attractiveness))

#############
##TO DO 3/24/2022:
  ##Change plant species names to species codes in observed pollinator dataset
    #DONE
  ##Add zeroes to observed pollinator dataset (use spread function to add '0' to management plots
    #DONE
  ##with no pollinator observations)
  ##Re-join plant floral count data and observed pollinator data, fill in zeroes, and re-calculate
    #DONE
  ##attractiveness index for each plant species
    #Did this at a few different levels of aggregation, it looks sorta consistent, and the numbers are waaay lower than before. (.1 to .0001)
  ## do you want to still try with cover? 
  