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
  mutate(week=ifelse(date==407, 1, ifelse(date==424, 3, ifelse(date==501, 4, ifelse(date==510, 5, 6)))))

moisture20$management<-factor(moisture20$management, levels=c("None", 
                                                              "Flailed", 
                                                              "Scraped"))
moisture20$seedmix<-factor(moisture20$seedmix, levels=c("Control", "Annuals", "Perennials", "Other"))

#function to calculate standard error
calcSE<-function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}

ggplot(data=subset(moisture20, seedmix=="Control"), aes(x=week, y=moisture, color=management)) + 
  geom_point()+
  geom_line(aes(group=interaction(block, management)))+
  facet_wrap(~age)+
  labs(x="day", y="Soil % Water by Volume")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                     name="Orchard Floor \nManagement",
                     breaks=c("None", "Flailed", "Scraped"),
                     labels=c("No Management", "Summer Flailing", "Summer Flailing \nand Scraping"))


#get means and standard error to simplify weekly plot
m20_agg<-moisture20%>%
  group_by(management, age, week ,seedmix)%>%
  summarize(mean_moisture=mean(moisture),
            se_moisture=calcSE(moisture))

#controls by management
ggplot(data=subset(m20_agg, seedmix=="Control"&week>3), aes(x=week, y=mean_moisture, color=management)) + facet_grid(~age)+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=mean_moisture-se_moisture, ymax=mean_moisture+se_moisture), width=.2)+
 labs(x="week", y="Soil % Water by Volume")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"), )+scale_x_continuous(breaks=c(4, 5, 6))
               
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


#seedmixes compared
ggplot(data=subset(m20_agg, week>3), aes(x=week, y=mean_moisture, color=seedmix)) + facet_grid(management~age)+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=mean_moisture-se_moisture, ymax=mean_moisture+se_moisture), width=.2)+
  labs(x="week", y="Soil % Water by Volume")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "darkred"), )+scale_x_continuous(breaks=c(4, 5, 6))

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


