##read library in tidyverse
library(tidyverse)
library(ggpubr)

#set the theme for all plots to black and white
theme_set(theme_classic())

#read the data from 2019, make a dataframe with only the columns we want
moisture19<-read_csv("soil_moisture_2019.csv")%>%
  select(1, 2, 3, 4, 6, 15)

moisture19$management<-factor(moisture19$management, levels=c("unmanaged", 
                                          "flailed", 
                                          "scraped"))
#function to calculate standard error
calcSE<-function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}

colnames(moisture19)<-c("farm", "management", "block", "age", "week", "prop_moisture_vol")

moisture_man<-moisture19%>%
  filter(!is.na(prop_moisture_vol))%>%
  mutate(percent_moisture_vol=prop_moisture_vol*100)%>%
  mutate(week=as.factor(week))%>%
  mutate(age=ifelse(age==15, "15 years old", ifelse(age==40, "40 years old", "60 years old")))

#all the data points week by week connected by lines
ggplot(data=moisture_man, aes(x=week, y=percent_moisture_vol, color=management)) + 
  geom_point()+
  geom_line(aes(group=interaction(block, management)))+
  facet_wrap(~age)+
  labs(x="Week", y="Soil % Water by Volume")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                    name="Orchard Floor \nManagement",
                    breaks=c("unmanaged", "flailed", "scraped"),
                    labels=c("No Management", "Summer Flailing", "Summer Flailing \nand Scraping"))

#get means and standard error to simplify weekly plot
mm_agg<-moisture_man%>%
  group_by(management, age, week)%>%
  summarize(mean_moisture=mean(percent_moisture_vol),
            se_moisture=calcSE(percent_moisture_vol))%>%
  mutate(week=as.integer(week))

#replot weekly data with mean and standard error
ggplot(data=mm_agg, aes(x=week, y=mean_moisture, color=management)) + 
  geom_point()+
  geom_errorbar(aes(ymin=mean_moisture-se_moisture, ymax=mean_moisture+se_moisture), width=.2)+
  geom_line()+
  facet_wrap(~age)+ labs(x="Week", y="Soil % Water by Volume")+scale_x_continuous(breaks=c(1, 2, 3))+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                     name="Orchard Floor \nManagement",
                     breaks=c("unmanaged", "flailed", "scraped"),
                     labels=c("No Management", "Summer Flailing", "Summer Flailing \nand Scraping"))



mm_loss<-moisture_man%>%
  select(-6)%>%
  spread(week, percent_moisture_vol)
colnames(mm_loss)<-c("farm", "management", "block", "age", "week1", "week2", "week3")

mmloss1<-mm_loss%>%
  mutate(loss1=week2-week1, loss2=week3-week2)%>%
  select(-(5:7))%>%
  gather("week", "moisture_loss", loss1, loss2)%>%
  mutate(moisture_loss=abs(moisture_loss))%>%
  filter(!is.na(moisture_loss))%>%
  filter(week!="loss2")

################################################################################################################
##### 2019 moisture~age*management stats
TukeyHSD(aov(percent_moisture_vol~management+week, data = subset(moisture_man, age=="15 years old"&as.numeric(week)<3)))
#unmanaged is higher than both others in 15
TukeyHSD(aov(percent_moisture_vol~management, data = subset(moisture_man, age=="15 years old"&as.numeric(week)==3)))
#unmanaged is higher than both others in 15
TukeyHSD(aov(percent_moisture_vol~management+week, data = subset(moisture_man, age=="40 years old"&as.numeric(week)<3))) 
# yes
TukeyHSD(aov(percent_moisture_vol~management+week, data = subset(moisture_man, age=="60 years old"&as.numeric(week)<3))) 
# no sd
TukeyHSD(aov(percent_moisture_vol~management+age, data = subset(moisture_man, as.numeric(week)<3))) 
# treatments no sd, but first weeks 15>60>40 moisture (more open=more moisture)
TukeyHSD(aov(moisture_loss~management+age, data = mmloss1)) 
#moisture losses 60>40>15 (descending by age sig, starting level and management dont matter)
TukeyHSD(aov(moisture_loss~management, data = subset(mmloss1, age=="40 years old")) ) 
# not sig
################################################################################################################

ggplot(mmloss1, aes(management, moisture_loss, fill=management))+
  geom_boxplot()+
  facet_wrap(~age)+theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())+
  xlab("")+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                     name="Orchard Floor \nManagement",
                     breaks=c("unmanaged", "flailed", "scraped"),
                     labels=c("No Management", "Summer Flailing", "Summer Flailing \nand Scraping"))+
  ylab("One Week Soil Moisture Loss (% Volume)")


          
