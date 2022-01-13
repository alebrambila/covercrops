#phenology

##read library in tidyverse
library(tidyverse)
library(ggpubr)
library(googlesheets4)

source('cover.R')

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

phenology_countsum<-phenology_count%>%
  group_by(orchardage, date)%>%
  summarize(flowering=sum(flowering))

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
  mutate(id=paste(id, seedmix, sep="_"))

#number of flowers available in each plot
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

pfsumsum<-phenflorsum1%>%
  group_by(orchard_age, numericmonth)%>%
  summarize(meanflor=mean(flor), seflor=calcSE(flor))

  

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

prsum<-phenrich%>%
  group_by(orchard_age, numericmonth)%>%
  summarize(meanrich=mean(richness), serich=calcSE(richness))

ggplot()+
  geom_jitter(data=phenrich, aes(x=as.numeric(numericmonth), y=richness, color=as.factor(orchard_age)), width=.2, alpha=.5, size=.7)+
  ylab("unique target species flowering per plot")+
  geom_point(size=2, data=prsum, aes(x=as.numeric(numericmonth), y=meanrich, color=as.factor(orchard_age)))+
  geom_line(size=1, data=prsum, aes(x=as.numeric(numericmonth), y=meanrich, color=as.factor(orchard_age)))+
  geom_errorbar(data=prsum, width=.1, aes(x=as.numeric(numericmonth), ymin=meanrich-serich, ymax=meanrich+serich, color=as.factor(orchard_age)))




#unique target spp flowering per plot/floral (shannon)
#spread phenflor and calculate shannon div of each plot over time 

library(codyn)
phenflor_shan<-left_join(phenflor_spread, community_diversity(phenflor_spread, abundance.var="infloresences", replicate.var="id", metric = c("Shannon")))%>%
  select(1:4, 8, 9)%>%
  unique()
phenflor_shan$orchardage<-factor(phenflor_shan$orchardage, levels=c("15", "60", "40"))

#SHANNON DIVERSITY OF FLOWERS WITHIN SPECEIS MIX X ORCHARD AGE
ggplot(phenflor_shan, aes(x=date, (Shannon), color=seedmix))+
  geom_rect(aes(xmin=3.5, xmax=6.5, ymin=0, ymax=1.3), color="white", fill="grey90", alpha=.3)+
  geom_jitter(width=.2, alpha=.5, size=.7)+
  ylab("Shannon diversity of floral resources")+
  stat_summary(geom="line", aes(color=seedmix), fun="mean", size=1)+
  stat_summary(geom="point", aes(color=seedmix), fun="mean", size=1.5)+
  #  geom_point(size=2, data=pfsumsum, aes(x=as.numeric(numericmonth), y=meanflor, color=as.factor(orchard_age)))+
  #  geom_line(size=1, data=pfsumsum, aes(x=as.numeric(numericmonth), y=meanflor, color=as.factor(orchard_age)))+
  # geom_errorbar(data=pfsumsum, width=.1, aes(x=as.numeric(numericmonth), ymin=meanflor-seflor, ymax=meanflor+seflor, color=as.factor(orchard_age)))+
  facet_wrap(~orchardage)

phenflor_abun<-phenflor_spread%>%
  group_by(orchardage, block, management, date, seedmix)%>%
  summarize(infloresences=sum(infloresences))

phenflor_abun$orchardage<-factor(phenflor_abun$orchardage, levels=c("15", "60", "40"))
ggplot(data=phenflor_abun, aes(x=as.numeric(date), y=infloresences+1, color=as.factor(seedmix)))+
  geom_rect(aes(xmin=3.5, xmax=6.5, ymin=1, ymax=1500), color="white", fill="grey90", alpha=.3)+
  stat_summary(geom="line", aes(group=interaction(species)), fun="mean", data=phenflor_spread, alpha=.25, size=.45)+
  geom_jitter(width=.2, alpha=.5, size=.7)+
  ylab("floral abundance per plot")+
  stat_summary(geom="line", aes(color=seedmix), fun="mean", size=1)+
  stat_summary(geom="point", aes(color=seedmix), fun="mean", size=1.5)+
  facet_wrap(~orchardage)+ scale_y_continuous(trans='log10')

#sanity check management
phenflor_abun$orchardage<-factor(phenflor_abun$orchardage, levels=c("15", "60", "40"))
ggplot(data=phenflor_abun, aes(x=as.numeric(date), y=infloresences+1, color=as.factor(seedmix)))+
  stat_summary(geom="line", aes(group=interaction(species)), fun="mean", data=phenflor_spread, alpha=.25, size=.45)+
  geom_jitter(width=.2, alpha=.5, size=.7)+
  ylab("floral abundance per plot")+
  stat_summary(geom="line", aes(color=seedmix), fun="mean", size=1)+
  stat_summary(geom="point", aes(color=seedmix), fun="mean", size=1.5)+
  facet_grid(management~orchardage)+ scale_y_continuous(trans='log10')


phenology_long<-phenology_long0%>%
  group_by(orchardage, management, species, date)%>%
  summarize(#germination=sum(germination),
    # seedling=sum(seedling, germination),
    vegetative=sum(seedling, germination, vegetative, postflowerveg),
    #firstflower=sum(firstflower),
    flowering=sum(flowering, firstflower, lastflower),
    # lastflower=sum(lastflower),
    fruit=sum(fruit),
    senesence=sum(senesence),
    flailed=sum(flailed),
    scraped=sum(scraped),
    #  postflowerveg=sum(postflowerveg),
    regrowth=sum(regrowth),
    count=n())%>%
  group_by(orchardage, species, date)%>%  ## used to group by management, took it off to see what all together looks like
  mutate(total=sum(vegetative,  flowering,  fruit, senesence, flailed, scraped, regrowth))%>%
  # mutate(germination=germination/total)%>%
  # mutate(seedling=seedling/total)%>%
  mutate(vegetative=vegetative/total)%>%
  # mutate(firstflower=firstflower/total)%>%
  mutate(flowering=flowering/total)%>%
  #  mutate(lastflower=lastflower/total)%>%
  mutate(fruit=fruit/total)%>%
  mutate(senesence=senesence/total)%>%
  mutate(flailed=flailed/total)%>%
  mutate(scraped=scraped/total)%>%
  #  mutate(postflowerveg=postflowerveg/total)%>%
  mutate(regrowth=regrowth/total)%>%
  filter(total!=0)
phenology_long[is.na(phenology_long)] <- 0

phenology_long$orchardage<-factor(phenology_long$orchardage, levels=c("15", "60", "40"))


phenology_xl<-phenology_long%>%
  dplyr::select(-count, -total)%>%
  gather(stage, proportion, 5:11)

phenology_xl$stage<-factor(phenology_xl$stage, levels=c("seedling", "vegetative", "flowering", "fruit","postflowerveg",  "senesence", "flailed", "scraped", "regrowth"))
#phenology_xl$management<-factor(phenology_xl$management, levels=c("unmanaged", "flailed", "scraped"))


#simplify groups (germination/seedling, veg, firstflower/flowering/lastflower, fruit, senesence, flailed)
#order factor


#achillea
ggplot(subset(phenology_xl, species=="achmil"), aes(x = date, y=proportion, fill = stage)) +
  geom_density(position = "fill",  stat="identity") +
  scale_fill_manual(values=c("lightgreen", "darkgreen", "goldenrod","darkorange", "green3", "grey80", "grey30", "grey50", "dodgerblue")) +
  facet_grid(management~orchardage) +ggtitle("achillea") +xlab("month")+ylab("proportion of plots/stage")

#prunella
ggplot(subset(phenology_xl, species=="pruvul"), aes(x = date, y=proportion, fill = stage)) +
  geom_density(position = "fill",  stat="identity") +
  scale_fill_manual(values=c("lightgreen", "darkgreen", "goldenrod","darkorange", "green3", "grey80", "grey30", "grey50", "dodgerblue")) +
  facet_grid(management~orchardage) +ggtitle("prunella") +xlab("month")+ylab("proportion of plots/stage")

#amsinckia (unmanaged 40 wk 7 flailed?)
ggplot(subset(phenology_xl, species=="amsmen"), aes(x = date, y=proportion, fill = stage)) +
  geom_density(position = "fill",  stat="identity") +
  scale_fill_manual(values=c("lightgreen", "darkgreen", "goldenrod","darkorange", "green3", "grey80", "grey30", "grey50", "dodgerblue")) +
  facet_grid(management~orchardage) +ggtitle("amsinckia") +xlab("month")+ylab("proportion of plots/stage")

#epiden (15, 7, flailed?)
ggplot(subset(phenology_xl, species=="epiden"), aes(x = date, y=proportion, fill = stage)) +
  geom_density(position = "fill",  stat="identity") +
  scale_fill_manual(values=c("lightgreen", "darkgreen", "goldenrod","darkorange", "green3", "grey80", "grey30", "grey50", "dodgerblue")) +
  facet_grid(management~orchardage) +ggtitle("epilobium") +xlab("month")+ylab("proportion of plots/stage")

#geumac
ggplot(subset(phenology_xl, species=="geumac"), aes(x = date, y=proportion, fill = stage)) +
  geom_density(position = "fill",  stat="identity") +
  scale_fill_manual(values=c("lightgreen", "darkgreen", "goldenrod","darkorange", "grey80", "grey30")) +
  facet_grid(management~orchardage) +ggtitle("geum") +xlab("month")+ylab("proportion of plots/stage")

#potgra
ggplot(subset(phenology_xl, species=="potgra"), aes(x = date, y=proportion, fill = stage)) +
  geom_density(position = "fill",  stat="identity") +
  scale_fill_manual(values=c("lightgreen", "darkgreen", "goldenrod","darkorange", "grey80", "grey30")) +
  facet_grid(management~orchardage) +ggtitle("potentilla") +xlab("month")+ylab("proportion of plots/stage")

#plectritis
ggplot(subset(phenology_xl, species=="plecon"), aes(x = date, y=proportion, fill = stage)) +
  geom_density(position = "fill",  stat="identity") +
  scale_fill_manual(values=c("lightgreen", "darkgreen", "goldenrod","darkorange", "grey80", "grey30")) +
  facet_grid(management~orchardage) +ggtitle("plectritis") +xlab("month")+ylab("proportion of plots/stage")

#clarkia (flailed in 15, 7)
ggplot(subset(phenology_xl, species=="clapur"), aes(x = date, y=proportion, fill = stage)) +
  geom_density(position = "fill",  stat="identity") +
  scale_fill_manual(values=c("lightgreen", "darkgreen", "goldenrod","darkorange", "grey80", "grey30")) +
  facet_grid(management~orchardage) +ggtitle("clarkia") +xlab("month")+ylab("proportion of plots/stage")

library(gridExtra)
#phenology publication figure
grid.arrange(
  ggplot(subset(phenology_xl, species %in% c("achmil", "amsmen","clapur", 
                                             "colgra", "epiden", "erilan", 
                                             "geumac")), 
         aes(x = date, y=proportion, fill = stage)) +
    geom_bar( stat="identity") +
    scale_fill_manual(values=c("darkgreen", "goldenrod","darkorange", "grey80", "grey30", "grey50", "dodgerblue")) +
    facet_grid(orchardage~species) +xlab("")+ylab("proportion of plots/stage"),
  ggplot(subset(phenology_xl, species %in% c("gilcap", "lomnud", 
                                             "lotpur", "plecon", "potgra", 
                                             "pruvul", "sanann", "clover")), 
         aes(x = date, y=proportion, fill = stage)) +
    geom_bar( stat="identity") +
    scale_fill_manual(values=c("darkgreen", "goldenrod","darkorange","grey80", "grey30", "grey50", "dodgerblue")) +
    facet_grid(orchardage~species)  +xlab("month")+ylab("proportion of plots/stage"),
  nrow=2)


#plant side of phenology figure
#order by phenology
phenology_xl$species<-factor(phenology_xl$species, levels=c("plecon", "achmil", "amsmen", "lomnud", "colgra","geunmac","sanann","erilan", "pruvul", "gilcap", "lotpur", "potgra", "epiden", "clapur"))

ggplot(subset(phenology_xl, species %in% c("plecon", "achmil", "amsmen",  "lomnud","geunmac","erilan",  
                                           "colgra", "sanann", "pruvul", "epiden", "clapur", "gilcap", 
                                           "potgra", "lotpur")), 
       aes(x = date, y=proportion, fill = stage)) +
  geom_bar( stat="identity") +
  scale_fill_manual(values=c("darkgreen", "goldenrod","darkorange", "grey80", "grey30", "grey50", "dodgerblue")) +
  facet_grid(species~orchardage)  +xlab("month")+ylab("proportion of plots/stage")



#insect side
insect_phenology<-read_csv("collectedpollinators_2021.csv")%>%
  mutate(orchardage=`Orchard Age`)%>%
  group_by(Month, `Order`, Genus, Count, `orchardage`)%>%
  summarize(orchardtotal=sum(Count))%>%
  mutate(numericmonth=ifelse(Month=="april", 1, ifelse(Month=="may", 2, ifelse(Month=="june", 3, ifelse(Month=="july", 4, 5)))))%>%
  group_by(Genus, Order, Month)%>%
  mutate(total40=sum(orchardtotal), total60=ifelse(orchardage==60|orchardage==15, orchardtotal, 0))%>%
  mutate(total60=sum(total60), total15=ifelse(orchardage==15, orchardtotal, 0))

insect_phenology$orchardage<-factor(insect_phenology$orchardage, levels=c(15, 60, 40))
insect_phenology$Month<-factor(insect_phenology$Month, levels=c("april", "may", "june", "july", "august"))
insect_phenology$Genus<-factor(insect_phenology$Genus, levels=c("Panurginus", "Coleoptera", "Diptera", "Osmia", 
                                                                "Dermaptera", "Eupeodes", "Hymenoptera", "Hylaeus", 
                                                                "Halictus", "Andrena", "Ceratina", "Eucera", 
                                                                "Lasioglossum", "Platycheirus", "Anthidium", "Ashmeadiella",
                                                                "Bombus", "Eristalis", "Heriades", "Nomada",
                                                                "Sphaerophoria", "Toxomerus", "Thysanoptera", "Syritta",
                                                                "Hemiptera"))


ggplot(insect_phenology, 
       aes(x = Month, y=orchardtotal)) +
  geom_bar( stat="identity", aes(fill=as.factor(orchardage))) +
  facet_wrap(~Genus, scales="free")+  scale_y_continuous(limits=c(0,45))  +xlab("month")+ylab("total")+
  scale_x_discrete(breaks=levels(factor(insect_phenology$Month)), limits=c("april", "may", "june", "july", "august"))


ip2<-select(insect_phenology, 1, 2, 3, 8, 9, 10, 7, orchardage)%>%
  filter(orchardage==15)%>%
  gather(orchardage, total, 4:6)%>%
  ungroup()

poll_div<-read_sheet("https://docs.google.com/spreadsheets/d/10q37avMnA0x4wG69Joh1eMKdMAqsc7Zs4zIIKT1MNRg/edit?usp=sharing")%>%
  filter(Year==2021)%>%
  mutate(seedmix=ifelse(seedmix=="annual", "annuals", ifelse(seedmix=="perennial", "perennials", seedmix)))
 
poll_div1<-full_join(plotkey, poll_div)%>%
  select(1:5, 7, 9, 10, 12)%>%
  mutate(numericmonth=ifelse(Month=="april", 4, 
                             ifelse(Month=="may", 5, 
                                    ifelse(Month=="june", 6, 
                                           ifelse(Month=="july", 7, 8)))))%>%
  mutate(Count=ifelse(is.na(Count), 0, Count))
poll_divsum<-poll_div1%>%
  group_by(orchard_age, block, management, seedmix, numericmonth)%>%
  summarize(richness=sum(Count))%>%
  spread(numericmonth, richness, fill=0)%>%
  gather(numericmonth, richness, 5:9)%>%
  filter(seedmix!="megamix")%>%
  mutate(rem=paste(management, numericmonth, sep="_"))%>%
  filter(!rem%in%c("flailed_7", "flailed_8", "scraped_7", "scraped_8")) #these will be zero, remove them. 
poll_divsum$seedmix<-factor(poll_divsum$seedmix, levels=c("annuals", "industry", "perennials", "control"))
poll_divsum$orchard_age<-factor(poll_divsum$orchard_age, levels=c(15, 60, 40))


### POLLINATOR SPECIES RICHNESS (from collections)
ggplot(poll_divsum, aes(x=as.numeric(numericmonth), y=richness, color=as.factor(seedmix)))+
  geom_rect(aes(xmin=3.5, xmax=6.5, ymin=-.5, ymax=8), color="white", fill="grey90", alpha=.3)+
  geom_jitter(aes(color=seedmix), width=.2, alpha=.5, size=.7)+
  scale_color_manual(values=c("#F8766D", "#00BA38", "#619CFF", "orange"))+
  ylab("Insect genus richness per plot")+
  stat_summary(geom="line", aes(color=seedmix), fun="mean", size=1)+
  stat_summary(geom="point", aes(color=seedmix), fun="mean", size=1.5)+
  facet_wrap(~orchard_age) 

#observed pollinators
op<-read_sheet("https://docs.google.com/spreadsheets/d/1IeWQPtXPJ-MrIua0wZswSJNRnU3bvmkDKtTc5lBzEbk/edit?usp=sharing")%>%
  mutate(numericmonth=ifelse(Month=="april", 4, 
                             ifelse(Month=="may", 5, 
                                    ifelse(Month=="june", 6, 
                                           ifelse(Month=="july", 7, 8)))))%>%
  group_by(numericmonth, block, orchard_age, management, seedmix)%>%
  mutate(seedmix=ifelse(seedmix=="annual", "annuals", ifelse(seedmix=="perennial", "perennials", seedmix)))%>%
  summarize(richness=length(unique(Morphospecies)), abundance=sum(Count))
op1<-full_join(plotkey, op)
op2<-select(op1, -richness)%>%
  spread(numericmonth, abundance, fill=0)%>%
  gather(numericmonth, abundance, 5:9)%>%
  mutate(rem=paste(management, numericmonth, sep="_"))%>%
  filter(seedmix!="megamix")%>%
  filter(!rem%in%c("flailed_7", "flailed_8", "scraped_7", "scraped_8")) #these will be zero, remove them. 
op2$orchard_age<-factor(op2$orchard_age, levels=c(15, 60, 40))
op2$seedmix<-factor(op2$seedmix, levels=c("annuals", "industry", "perennials", "control"))

#pollinator abundance (observations)
ggplot(subset(op2, !is.na(seedmix)), aes(x=as.numeric(numericmonth), y=abundance+1))+
  geom_rect(aes(xmin=3.5, xmax=6.5, ymin=1, ymax=100), fill="grey90", alpha=.3)+
  geom_jitter(aes(color=seedmix), width=.2, alpha=.5, size=.7)+
  scale_color_manual(values=c("#F8766D", "#00BA38", "#619CFF", "orange"))+
  ylab("Insect visitations per plot")+
  stat_summary(geom="line", aes(color=seedmix), fun="mean", size=1)+
  stat_summary(geom="point", aes(color=seedmix), fun="mean", size=1.5)+
  facet_wrap(~orchard_age) +scale_y_continuous(trans='log10')



