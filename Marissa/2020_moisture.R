##read library in tidyverse
library(tidyverse)
library(ggpubr)

#set the theme for all plots to black and white
theme_set(theme_classic())

#read the data from 2019, make a dataframe with only the columns we want
moisture20i<-read_csv("soil_moisture_2020.csv")
colnames(moisture20i)<-c("age", "block", "management", "seedmix", "407", "424", "501", "510", "517")
moisture20<-moisture20i%>%
  gather("date", "moisture", 5:9)%>%
  filter(!is.na(moisture), moisture!="-")%>%
  separate(moisture, into=c("moisture", "delete"), sep="/")%>%
  select(-delete)%>%
  mutate(moisture=as.numeric(moisture), date=as.integer(date))%>%
  filter(!is.na(age))%>%
  mutate(week=ifelse(date==407, 1, ifelse(date==424, 3, ifelse(date==501, 4, ifelse(date==510, 5, 6)))))%>%
  mutate(moisture=ifelse(week==3, moisture*.77-3.5, moisture))%>%
  mutate(management=ifelse(management=="None", 'unmanaged', tolower(management)), seedmix=tolower(seedmix))%>%
  mutate(seedmix=ifelse(seedmix=='other'&management=="unmanaged", 'megamix', ifelse(seedmix=='other', 'industry', seedmix)))

#moisture20$management<-factor(moisture20$management, levels=c("None", 
                     #                                         "Flailed", 
                     #                                         "Scraped"))
#moisture20$seedmix<-factor(moisture20$seedmix, levels=c("Control", "Annuals", "Perennials", "Other"))

#function to calculate standard error
calcSE<-function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}

ggplot(data=subset(moisture20, seedmix=="control"), aes(x=week, y=moisture, color=management)) + 
  geom_point()+
  geom_line(aes(group=interaction(block,management)))+
  facet_wrap(~age)
  labs(x="day", y="Soil % Water by Volume")+
  #scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                     name="Orchard Floor \nManagement",
                     breaks=c("None", "Flailed", "Scraped"),
                     #labels=c("No Management", "Summer Flailing", "Summer Flailing \nand Scraping"))

  ggplot(data=subset(moisture20), aes(x=week, y=moisture, color=seedmix)) + 
    geom_point()+
    geom_line(aes(group=interaction(block,seedmix,management)))+
    facet_wrap(~age)
  labs(x="day", y="Soil % Water by Volume")+

#get means and standard error to simplify weekly plot
m20_agg<-moisture20%>%
  group_by( age, week ,seedmix)%>%
  summarize(mean_moisture=mean(moisture))

#seedmixes
ggplot(data=subset(m20_agg), aes(x=week, y=mean_moisture, color=seedmix)) + facet_grid(~age)+
  geom_point()+
  geom_line()+
 # geom_errorbar(aes(ymin=mean_moisture-se_moisture, ymax=mean_moisture+se_moisture), width=.2)+
 labs(x="week", y="Soil % Water by Volume")
  #scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"), )+scale_x_continuous(breaks=c(4, 5, 6))
               
################################################################################################################
##### 2020 moisture~age*management stats
TukeyHSD(aov(moisture~management+as.factor(week), data = subset(moisture20, age==15&week>3)))
TukeyHSD(aov(moisture~management+as.factor(week), data = subset(moisture20, age==40&week>3)))
TukeyHSD(aov(moisture~management+as.factor(week), data = subset(moisture20, age==60&week>3)))
# ALWAYS SIGNIFICANT, ALWAYS unmanaged>flailed>scraped (when considered as main effects)
TukeyHSD(aov(moisture~management+as.factor(age), data = subset(moisture20, week>3))) 
# treatments no sd, but first weeks 15>60>40 moisture (more open=more moisture)

TukeyHSD(aov(moisture_loss~management, data = subset(mmloss1, age=="40 years old")) ) 
# not sig
################################################################################################################

m20_agg$seedmix=factor(m20_agg$seedmix, levels=c("annuals", "perennials", "megamix", "industry", "control"))

#seedmixes compared (PUBLICATION).. add in amount of rain in each week. 
ggplot(data=subset(m20_agg, seedmix!="megamix"), aes(x=week, y=mean_moisture, color=seedmix)) +
  facet_wrap(~age, scales="free")+  scale_y_continuous(limits=c(5,47))+ theme(axis.line=element_line())+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=mean_moisture-se_moisture, ymax=mean_moisture+se_moisture), width=.2)+
  labs(x="week", y="Soil % Water by Volume")+theme(axis.text.x=element_text(color = "black", angle=30, vjust=.8, hjust=0.8))+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "darkred", "black", "grey30", "grey50", "grey70"), )+scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6), labels=c("april 7","", "april 24", "may 1", "may 10", "may 17"))

moisture20_consolidated<-moisture20%>%
  mutate(management=ifelse(management=="Flailed"|management=="Scraped", "Managed", "Unmanaged"))
################################################################################################################
##### 2020 moisture~seedmix*age stats
# in managed plots, how does native compare with OSU (surprisingly well!)
TukeyHSD(aov(moisture~seedmix+as.factor(week), data = subset(moisture20_consolidated, age==15&week>3&management=="Managed")))
TukeyHSD(aov(moisture~seedmix+as.factor(week), data = subset(moisture20_consolidated, age==40&week>3&management=="Managed")))
TukeyHSD(aov(moisture~seedmix+as.factor(week), data = subset(moisture20_consolidated, age==60&week>3&management=="Managed")))

#in unmanaged plots, does megamix help? - not in 15 or 60, mega better than both A/P in 40 (which are indistinguishable), 
TukeyHSD(aov(moisture~seedmix+as.factor(week), data = subset(moisture20_consolidated, age==15&week>3&management=="Unmanaged")))
TukeyHSD(aov(moisture~seedmix+as.factor(week), data = subset(moisture20_consolidated, age==40&week>3&management=="Unmanaged")))
TukeyHSD(aov(moisture~seedmix+as.factor(week), data = subset(moisture20_consolidated, age==60&week>3&management=="Unmanaged")))

################################################################################################################


source('canopy.R')

join20<-moisture20%>%
  mutate(orchard_age=age)%>%
  select(-age)
moisture.canopy<-left_join(join20, canopy)

ggplot(moisture.canopy, aes(x=canopy, y=moisture)) +
 # geom_point(aes(color=seedmix)) +
  geom_smooth(aes(color=seedmix), method='lm', se=F)+
  facet_grid(~week)

## cover * moisture 2020
cov.moisture<-left_join(select(moisture.canopy,-year), subset(bare, year==2020))

ggplot(subset(cov.moisture, species=='weeds'), aes(x=cov, y=moisture)) +
  geom_point(aes(color=as.factor(seedmix))) +
  geom_smooth(aes(color=as.factor(seedmix)), method='lm', se=F) +
  #stat_smooth(aes(color=seedmix),method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
  facet_grid(~date)+  scale_linetype_manual(values=c("solid", "longdash", "dashed", "dotdash", "dotted")) #+

cov.moisture.target<-left_join(select(moisture.canopy,-year), subset(mix_cov, year==2020))

#different, increase cover = increase soil moisture
ggplot(subset(cov.moisture.target), aes(x=cov, y=moisture)) +
  geom_point(aes(color=as.factor(seedmix))) +
  geom_smooth(aes(color=as.factor(seedmix)), method='lm', se=F) +
  #stat_smooth(aes(color=seedmix),method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
  facet_grid(~date)+  scale_linetype_manual(values=c("solid", "longdash", "dashed", "dotdash", "dotted")) #+

# 2020 was wet, 2021 is dry
#in both years weeds decrease moisture, in 2021 total cover decreases moisture 2020 no effect. 
#In 2021 natives decrease moisture, in 2020 they increase
