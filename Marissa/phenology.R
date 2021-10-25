#phenology

##read library in tidyverse
library(tidyverse)
library(ggpubr)
library(googlesheets4)

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

#add up all of the 'flowering' plots by species, orchard, date
phenology_count<-phenology_long0%>%
  group_by(orchardage, species, date)%>%
  summarize(flowering=sum(firstflower, flowering, lastflower))%>%
  filter(date>3)
ggplot(phenology_count, aes(x=date, y=flowering))+
  geom_line(aes(color=as.factor(orchardage)))+
  facet_wrap(~species)


phenology_countsum<-phenology_count%>%
  group_by(orchardage, date)%>%
  summarize(flowering=sum(flowering))
ggplot(phenology_countsum, aes(x=date, y=flowering))+
  geom_line(aes(color=as.factor(orchardage)))


phenflor<-left_join(mutate(phenology_long0, orchardage=as.factor(orchardage)), floral)%>%
  mutate(anyflower=ifelse(firstflower==1|flowering==1|lastflower==1, 1, 0))%>%
  filter(anyflower==1)%>%
  mutate(infloresences=ifelse(flowering==1, infloresences, infloresences*.1))%>%
  select(1:5, 18)%>%
  mutate(infloresences=ifelse(is.na(infloresences),1,infloresences))
ggplot(phenflor, aes(x=date, y=infloresences))+
  geom_jitter(aes(color=as.factor(orchardage)))+
  facet_wrap(~species)


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

ggplot()+
    geom_jitter(data=phenflorsum1, aes(x=as.numeric(numericmonth), y=flor, color=as.factor(orchard_age)), width=.2, alpha=.5, size=.7)+
    ylab("floral abundance per plot")+
    geom_point(size=2, data=pfsumsum, aes(x=as.numeric(numericmonth), y=meanflor, color=as.factor(orchard_age)))+
    geom_line(size=1, data=pfsumsum, aes(x=as.numeric(numericmonth), y=meanflor, color=as.factor(orchard_age)))+
    geom_errorbar(data=pfsumsum, width=.1, aes(x=as.numeric(numericmonth), ymin=meanflor-seflor, ymax=meanflor+seflor, color=as.factor(orchard_age)))
  
  

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



ggplot(insect_phenology, 
       aes(x = numericmonth, y=Genus)) +
  geom_violin(position="dodge",aes( violinwidth=orchardtotal, fill=as.factor(orchardage)),adjust=4, scale="count")+
  geom_point(aes(size=orchardtotal, color=as.factor(orchardage)))#+
facet_grid(~Genus, scales="fixed")#+  scale_y_continuous(limits=c(0,45))  +xlab("month")+ylab("total")+
scale_x_discrete(breaks=levels(factor(insect_phenology$Month)), limits=c("april", "may", "june", "july", "august"))

ggplot(insect_phenology, 
       aes(x = numericmonth, y=orchardtotal)) +
  geom_density(position="stack", stat="identity", aes(fill=as.factor(orchardage)))+
  geom_point(aes(size=orchardtotal, color=as.factor(orchardage)))+
facet_grid(~Genus, scales="fixed")#+  scale_y_continuous(limits=c(0,45))  +xlab("month")+ylab("total")+
scale_x_discrete(breaks=levels(factor(insect_phenology$Month)), limits=c("april", "may", "june", "july", "august"))


ip2<-select(insect_phenology, 1, 2, 3, 8, 9, 10, 7, orchardage)%>%
  filter(orchardage==15)%>%
  gather(orchardage, total, 4:6)%>%
  ungroup()

ggplot(ip2, 
       aes(x = Genus, y=numericmonth)) +
  geom_violin(position="dodge",aes( violinwidth=total, fill=as.factor(orchardage)),adjust=1, scale="count")#+
geom_point(aes(size=total, color=as.factor(orchardage)))#+
facet_wrap(~Genus, scales="free")#+  scale_y_continuous(limits=c(0,45))  +xlab("month")+ylab("total")+
scale_x_discrete(breaks=levels(factor(insect_phenology$Month)), limits=c("april", "may", "june", "july", "august"))


library(ggridges)

ggplot(ip2, 
       aes(x = numericmonth, y=orchardage)) +
  geom_ridgeline(aes(height=total, fill=orchardage), scale=1)+
facet_wrap(~Genus)

#ggplot(subset(phenology_xl, species=="achmil"), aes(x = date, y=proportion, fill = stage)) +
#  geom_bar(aes(fill=stage), stat="identity") +
# # scale_fill_brewer(palette = "Blues") +
#  facet_grid(management~orchardage) 

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

#pollinator richness (genera)
ggplot()+
  geom_jitter(data=cp, aes(x=numericmonth, y=richness, color=as.factor(orchard_age)), width=.2, alpha=.5, size=.7)+
  ylab("insect genus richness per plot")+
  geom_point(size=2, data=cpsum, aes(x=numericmonth, y=meanrich, color=as.factor(orchard_age)))+
  geom_line(size=1, data=cpsum, aes(x=numericmonth, y=meanrich, color=as.factor(orchard_age)))+
  geom_errorbar(data=cpsum, width=.1, aes(x= numericmonth, ymin=meanrich-serich, ymax=meanrich+serich, color=as.factor(orchard_age)))
  

#pollinator abundance (genera)
ggplot()+
  geom_jitter(data=cp, aes(x=numericmonth, y=abundance, color=as.factor(orchard_age)), width=.2, alpha=.5, size=.7)+
  ylab("insect genus richness per plot")+
  geom_point(size=2, data=cpsum, aes(x=numericmonth, y=meanabun, color=as.factor(orchard_age)))+
  geom_line(size=1, data=cpsum, aes(x=numericmonth, y=meanabun, color=as.factor(orchard_age)))+
  geom_errorbar(data=cpsum, width=.1, aes(x= numericmonth, ymin=meanabun-seabun, ymax=meanabun+seabun, color=as.factor(orchard_age)))

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


#pollinator abundance (observations)
ggplot(op3, aes(x=numericmonth, y=abundance))+
  geom_jitter(aes(color=as.factor(orchard_age)), width=.2)+
  ylab("insect observations per plot")+
  geom_smooth(aes(color=as.factor(orchard_age)), se=F)

ggplot()+
  geom_jitter(data=op3, aes(x=numericmonth, y=abundance, color=as.factor(orchard_age)), width=.2, alpha=.5, size=.7)+
  ylab("insect observations per plot")+
  geom_point(size=2, data=opsum, aes(x=numericmonth, y=meanabun, color=as.factor(orchard_age)))+
  geom_line(size=1, data=opsum, aes(x=numericmonth, y=meanabun, color=as.factor(orchard_age)))+
  geom_errorbar(data=opsum, width=.1, aes(x= numericmonth, ymin=meanabun-seabun, ymax=meanabun+seabun, color=as.factor(orchard_age)))



#pollinator diversity (observations morphospecies)
ggplot(op3, aes(x=numericmonth, y=richness))+
  geom_jitter(aes(color=as.factor(orchard_age)), width=.2)+
  ylab("insect morphospecies richness per plot")+
  geom_smooth(aes(color=as.factor(orchard_age)), se=F)

ggplot()+
  geom_jitter(data=op3, aes(x=numericmonth, y=richness, color=as.factor(orchard_age)), width=.2, alpha=.5, size=.7)+
  ylab("insect morphospecies richness per plot")+
  geom_point(size=2, data=opsum, aes(x=numericmonth, y=meanrich, color=as.factor(orchard_age)))+
  geom_line(size=1, data=opsum, aes(x=numericmonth, y=meanrich, color=as.factor(orchard_age)))+
  geom_errorbar(data=opsum, width=.1, aes(x= numericmonth, ymin=meanrich-serich, ymax=meanrich+serich, color=as.factor(orchard_age)))


#correlations between pollinators and plants (abun/rich)
florsum<-full_join(phenflorsum1, mutate(phenrich, orchard_age=as.factor(orchard_age)))
op4<-mutate(op3, insectrich=richness, insectabun=abundance)%>%
  mutate(numericmonth=numericmonth+3)%>%
  select(-richness, -abundance)
cors<-left_join(mutate(florsum, numericmonth=as.numeric(numericmonth)), mutate(op4, orchard_age=as.factor(orchard_age)))%>%
  ungroup()

library(car)
scatterplotMatrix(~flor+richness+insectrich+insectabun, data=cors , 
                  reg.line="" , smoother="", 
                  smoother.args=list(col="grey") , cex=1.5 , 
                  pch=c(15,16,17) , 
                  main="Scatter plot with Three Cylinder Options"
)
pairs(~flor+richness+insectrich+insectabun, data=cors, bg=rainbow(3)[cors$orchard_age],col=rainbow(3)[cors$orchard_age])

library(corrplot)
corrplot(cor(select(cors, 5, 6, 7, 8)),        # Correlation matrix
         method = "number", # Correlation plot method
         type = "lower",    # Correlation plot style (also "upper" and "lower")
         diag = TRUE,      # If TRUE (default), adds the diagonal
         tl.col = "black", # Labels color
         bg = "white",     # Background color
         title = "",       # Main title
         col = NULL)       # Color palette

ggplot(cors, aes(x=flor+1, y=insectabun+1, color=orchard_age))+
  geom_jitter()+#stat_regline_equation()+
  #geom_point(aes(y=insectrich), shape=2)+
  geom_smooth(method="lm", se=F)+stat_cor()+
  scale_y_continuous(trans='log10')+scale_x_continuous(trans='log10')+
  xlab("floral abundance per plot")+ylab("insect observations per plot")

ggplot(cors, aes(x=richness+1, y=insectrich+1, color=orchard_age))+
  geom_jitter()+#stat_regline_equation()+
  #geom_point(aes(y=insectrich), shape=2)+
  geom_smooth(method="lm", se=F)+stat_cor()+
  scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  xlab("unique target species flowering per plot")+ylab("insect morphospecies richness per plot")


#ggplot(cors, aes(x=flor+1, y=richness+1, color=orchard_age))+
#  geom_jitter()+#stat_regline_equation()+
##  #geom_point(aes(y=insectrich), shape=2)+
#  geom_smooth(method="lm", se=F)+stat_cor()+
#  scale_y_continuous(trans='log10')+scale_x_continuous(trans='log10')

#ggplot(cors, aes(x=richness+1, y=insectabun+1, color=orchard_age))+
#  geom_jitter()+#stat_regline_equation()+
#  #geom_point(aes(y=insectrich), shape=2)+
#  geom_smooth(method="lm", se=F)+stat_cor()+
#  scale_y_continuous(trans='log10')+scale_x_continuous(trans='log10')

#ggplot(cors, aes(x=flor+1, y=insectrich+1, color=orchard_age))+
#  geom_jitter()+#stat_regline_equation()+
  ##geom_point(aes(y=insectrich), shape=2)+
#  geom_smooth(method="lm", se=F)+stat_cor()+
#  scale_x_continuous(trans='log10')#+scale_y_continuous(trans='log10')


#STOP HERE#### 
# phenology for pollinators
#each species, each trt the entire window of flowering, point at peak flowering, size=floral

phenology_flowering<-phenology_long0%>%
  group_by(orchardage, block, management, species, date)%>%
  mutate(allflower=sum(firstflower, lastflower, flowering))%>%
  filter(allflower!=0)%>%
  mutate(anyflower=1)%>%
  select(1:5, allflower, flowering, anyflower)%>% #allflower is sum of all types of flowering, flowering is peak, any is 1 if all >0
  mutate(orchardage=as.factor(orchardage))




phenology_flowering1<-left_join(subset(phenology_flowering, flowering!=0), floral)
group_by(orchardage, block, management, species)%>%
  summarize()
phenology_flowering2<- left_join(phenology_flowering1, floral)

ggplot(phenology_flowering, aes(date, species)) +geom_point(color="grey80")+ 
  geom_jitter(width=.1, data=subset(phenology_flowering1, anyflower==1),
              aes( x=date, y=species, size=infloresences)) +
  geom_line(color="grey80") +
  facet_grid(~orchardage)

ggplot(phenology_flowering, aes(date, species)) +geom_density(color="grey80")+ 
  geom_jitter(width=.1, data=subset(phenology_flowering1, anyflower==1),
              aes( x=date, y=species, size=infloresences)) +
  geom_line(color="grey80") +
  facet_grid(~orchardage)



#sum up # of plots in any stage of flowering + peak flowering
pfsum.plot<-phenology_flowering%>%
  group_by(orchardage, species, date)%>%
  summarize(any=sum(anyflower), peak=sum(flowering))


ggplot(pfsum.plot, aes(x=date, y=any))+ geom_violin() +
  geom_bar(stat="identity")+geom_line()+
  facet_grid(orchardage~species)


ggplot(subset(pfsum.plot, !species%in%c("barley", "cartum", "dancal", "fesroe", "lotpur", 
                                        "oats", "vetch", "viopra")), aes(x=date, y=any))+ 
  geom_bar(stat="identity", aes(fill=orchardage))+
  # geom_point(data=)
  facet_grid(~species)

pfsum.inf<-phenology_flowering1%>%
  group_by(orchardage, species, date)%>%
  filter(!is.na(infloresences))%>%
  mutate(date=ifelse(species=="gilcap"&orchardage==40, 6, 
                     ifelse(species=='geumac'&(orchardage==60|orchardage==15), 5, 
                            ifelse(species=="geumac"&orchardage==40, 6,
                                   ifelse(species=="epiden"&orchardage==15, 8, 
                                          ifelse(species=="epiden"&orchardage==60, 6, date))))))%>%
  summarize(meaninf=mean(infloresences, na.rm=T),
            seinf=calcSE(infloresences), 
            suminf=sum(infloresences, na.rm=T), 
            total=n())

grid.arrange(
  ggplot(subset(pfsum.plot, !species%in%c("barley", "cartum", "dancal", "fesroe", "lotpur", 
                                          "oats", "vetch", "viopra")), aes(x=date, y=any))+ 
    geom_bar(stat="identity", aes(fill=orchardage))+ylab("number of flowering plots")+
    # geom_point(data=)
    facet_grid(~species),
  ggplot(subset(pfsum.inf, !species%in%c("barley", "cartum", "dancal", "fesroe", "lotpur", 
                                         "oats", "vetch", "viopra")), aes(x=date, y=meaninf))+ 
    # geom_bar(stat="identity", aes(fill=orchardage))+
    geom_point(aes(color=orchardage))+scale_y_continuous(trans='log10')+ylab("mean infloresences/plot at peak flowering")+
    geom_errorbar(aes(ymin=meaninf-seinf, ymax=meaninf+seinf))+
    facet_grid(~species),
  nrow=2)



ggplot(subset(pfsum.plot, !species%in%c("barley", "cartum", "dancal", "fesroe", "lotpur", 
                                        "oats", "vetch", "viopra")), aes(x=date, y=peak))+ 
  geom_bar(stat="identity", aes(fill=orchardage))+
  facet_grid(~species)




