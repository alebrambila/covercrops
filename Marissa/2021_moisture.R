##read library in tidyverse
library(tidyverse)
library(ggpubr)
library(googlesheets4)

#set the theme for all plots to black and white
theme_set(theme_classic())
gs4_deauth()
gs_ls()

source('2020_moisture.R')

moisture21 <- read_sheet("https://docs.google.com/spreadsheets/d/1c_9D5NN0DIl5RYNbJPM59b1WNA_UZtrPglRGyQGXOWA/edit?usp=sharing")
colnames(moisture21)<-c("orchardage", "block", "management", "seeding", "nail", "probe", "date", "notes")

moisture21_1<-moisture21%>%
  separate(nail, into=c("nail", "nail_p"), sep="/")%>%
  separate(probe, into=c("probe", "probe_p"), sep="/")%>%
  dplyr::select(1, 2, 3, 4, 5, 6, 7, 8, 9)%>%
  mutate(nail=as.numeric(nail), probe=as.numeric(probe), nail_p=as.numeric(nail_p), probe_p=as.numeric(probe_p))%>%
  mutate(calibration=ifelse(is.na(nail), probe, (nail*.81)-5.4))%>%
  mutate(management=tolower(management), seedmix=tolower(seeding))
  


moisture21_calibration<-moisture21_1%>%
  filter(!is.na(probe), !is.na(nail))


ggplot(moisture21_calibration, aes(x=nail, y=probe))+ geom_point(aes(color=date))+geom_smooth(method="lm")+
  geom_abline(intercept = 0, slope = 1)+#xlim(10, 63)+ylim(10, 63)+
  xlab("%VWC (Probe)")+ylab("%VWC (Nail)")+ theme_classic()+ stat_regline_equation()



#function to calculate standard error
calcSE<-function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}

moisture21cal<-moisture21_1%>%
  mutate(calibration=ifelse(is.na(nail), probe, (nail*.81)-5.4))%>% #adjust this to calibration line above
  mutate(month=as.numeric(ifelse(date=="03.24.21", 3, ifelse(date=="04.24.21", 4, ifelse(date=="05.14.21", 5,  ifelse(date=="07.31.21", 7,  8))))))%>%
  group_by(orchardage, seeding, month)%>%
  summarize(meancal=mean(calibration, na.rm=T), secal=calcSE(calibration))

moisture21cal$seeding=factor(moisture21cal$seeding, levels=c("Annuals", "Perennials", "Megamix", "Industry", "Control", "true control", "north", "south"))

ggplot(subset(moisture21cal, !seeding%in% c("true control", "north", "south")), aes(x=month, y=meancal)) +
  geom_point(aes(color=seeding)) +
  geom_line(aes(color=seeding))+
  geom_errorbar(aes(color=seeding, ymin=meancal-secal, ymax=meancal+secal), width=.2)+
  facet_grid(~orchardage)+
  labs(x="meek", y="Soil % Water by Volume")+theme(axis.text.x=element_text(color = "black", angle=30, vjust=.8, hjust=0.8))+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "darkred", "black", "grey30", "grey50", "grey70"), )+scale_x_continuous(breaks=c(3, 4, 5, 6, 7, 8), labels=c("03.2021","04.2021", "05.2021", "06.2021", "07.2021", "08.2021"))


moisture21diff<-moisture21_1%>%
  mutate(calibration=ifelse(is.na(nail), probe, (nail*.77)-3.5))%>%
  mutate(month=as.numeric(ifelse(date=="03.24.21", 3, ifelse(date=="04.24.21", 4, ifelse(date=="05.14.21", 5,  ifelse(date=="07.31.21", 7,  8))))))%>%
  dplyr::select(1, 2, 3, 4, 10, 11, 12)%>%
  group_by(orchardage, block, management, seeding, seedmix, month)%>%
  summarize(calibration=mean(calibration))%>% #hack fix
  ungroup()%>%
  spread(month, calibration)%>%  #fix: Keys are shared for 2 rows: 673, 913
  mutate(`5`=`5`/`3`, `4`=`4`/`3`, `3`=1)%>%
  gather("month", "moisture.retained", `3`, `4`, `5`)%>%
  group_by(orchardage, seeding, month)%>%
    summarize(mean.retained=mean(moisture.retained), se.retained=calcSE(moisture.retained))

ggplot(moisture21diff, aes(x=as.integer(month), y=mean.retained)) +
  geom_point(aes(color=seeding)) +
  geom_line(aes(color=seeding))+
  geom_errorbar(aes(color=seeding, ymin=mean.retained-se.retained, ymax=mean.retained+se.retained), width=.2)+
  facet_grid(~orchardage)

  #not working
  




moisture20$management<-factor(moisture20$management, levels=c("None", 
                                                              "Flailed", 
                                                              "Scraped"))
moisture20$seedmix<-factor(moisture20$seedmix, levels=c("Control", "Annuals", "Perennials", "Other"))


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

source('canopy.r')
join21<-moisture21_1%>%
  mutate(orchard_age=as.numeric(orchardage))%>%
  select(-orchardage)%>%
  select(orchard_age, block, management, seedmix, calibration, date)

moisture21.canopy<-left_join(join21, canopy, by=c('orchard_age', 'block', 'management', 'seedmix'))  


#most promising figure for 21 so far: moisture by canopy how it changes over time/seedmix
#shows declining moisture, but variable by seedmix
#adjust to matric potential with texture
#get time on the y axis - months, and the point represents when that plot reached critical low moisture for trees (given seedmix and canopy)
ggplot(moisture21.canopy, aes(x=canopy, y=calibration)) +
 # geom_point(aes(color=as.factor(seedmix))) +
 geom_smooth(aes(color=date, linetype=seedmix), method='lm', se=F) +
  scale_linetype_manual(values=c("solid", "twodash", "dashed", "dotdash", "dotted")) #+
  facet_grid(~date)

  
  ggplot(moisture21.canopy, aes(x=canopy, y=calibration)) +
    # geom_point(aes(color=as.factor(seedmix))) +
    geom_smooth(aes(color=seedmix), method='lm', se=F) +
    #stat_smooth(aes(color=seedmix),method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
    facet_grid(~date)+  scale_linetype_manual(values=c("solid", "longdash", "dashed", "dotdash", "dotted")) #+

## cover * moisture 2021
cov.moisture21<-left_join(select(moisture21.canopy,-year), subset(bare, year==2021))

ggplot(subset(cov.moisture21, species=='weeds'), aes(x=cov, y=calibration)) +
  geom_point(aes(color=as.factor(seedmix))) +
  geom_smooth(aes(color=as.factor(seedmix)), method='lm', se=F) +
  #stat_smooth(aes(color=seedmix),method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
  facet_grid(~date)+  scale_linetype_manual(values=c("solid", "longdash", "dashed", "dotdash", "dotted")) #+

cov.moisture21target<-left_join(select(moisture21.canopy,-year), subset(mix_cov, year==2021))

#same, increase cover = decrease soil moisture
ggplot(subset(cov.moisture21target), aes(x=cov, y=calibration)) +
  geom_point(aes(color=as.factor(seedmix))) +
  geom_smooth(aes(color=as.factor(seedmix)), method='lm', se=F) +
  #stat_smooth(aes(color=seedmix),method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
  facet_grid(~date)+  scale_linetype_manual(values=c("solid", "longdash", "dashed", "dotdash", "dotted")) #+



  