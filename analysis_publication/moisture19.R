
library(tidyverse)

#read the data from 2019, make a dataframe with only the columns we want
moisture19<-read_csv("soil_moisture_2019.csv")%>%
  select(1, 2, 3, 4, 6, 15)

colnames(moisture19)<-c("farm", "management", "block", "age", "week", "prop_moisture_vol")

moisture_man<-moisture19%>%
  filter(!is.na(prop_moisture_vol))%>%
  mutate(percent_moisture_vol=prop_moisture_vol*100)

ggplot(data=moisture_man, aes(x=week, y=percent_moisture_vol, color=management)) + 
  geom_point()+
  geom_line(aes(group=interaction(block, management)))+
  facet_wrap(~age)

ggplot(data=moisture_man, aes(x=week, y=percent_moisture_vol, color=management)) + 
  geom_boxplot(aes(group=interaction(week, management)))+
 # geom_line(aes(group=interaction(block, management)))+
  facet_wrap(~age)

calcSE<-function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}

mm_agg<-moisture_man%>%
  group_by(management, age, week)%>%
  summarize(mean_moisture=mean(percent_moisture_vol),
            se_moisture=calcSE(percent_moisture_vol))

ggplot(data=mm_agg, aes(x=week, y=mean_moisture, color=management)) + 
  geom_point()+
  geom_errorbar(aes(ymin=mean_moisture-se_moisture, ymax=mean_moisture+se_moisture), width=.2)+
  geom_line()+
  facet_wrap(~age)

mm_loss<-moisture_man%>%
  select(-6)%>%
  spread(week, percent_moisture_vol)
colnames(mm_loss)<-c("farm", "management", "block", "age", "week1", "week2", "week3")

mmloss1<-mm_loss%>%
  mutate(loss1=week2-week1, loss2=week3-week2)%>%
  select(-(5:7))%>%
  gather("week", "moisture_loss", loss1, loss2)%>%
  mutate(moisture_loss=abs(moisture_loss))%>%
  filter(!is.na(moisture_loss))

ggplot(mmloss1, aes(management, moisture_loss))+
  geom_boxplot()+
  facet_wrap(~age)



