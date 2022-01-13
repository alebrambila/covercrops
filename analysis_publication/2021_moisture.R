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
  mutate(month=as.numeric(ifelse(date=="03.24.21", 3, ifelse(date=="04.24.21", 4, ifelse(date=="05.14.21", 5,  ifelse(date=="07.31.21", 6,  8))))))%>%
  group_by(orchardage, seeding, month)%>%
  summarize(meancal=mean(calibration, na.rm=T), secal=calcSE(calibration))

moisture21cal$seeding=factor(moisture21cal$seeding, levels=c("Annuals", "Perennials", "Megamix", "Industry", "Control", "true control", "north", "south"))


#Figure 3: Moistures in 2021.. RAW
ggplot(subset(moisture21cal, !seeding%in% c("true control", "north", "south", "Megamix")), aes(x=month, y=meancal)) +
  geom_point(aes(color=seeding)) +
  geom_line(aes(color=seeding))+
  geom_errorbar(aes(color=seeding, ymin=meancal-secal, ymax=meancal+secal), width=.2)+
  facet_wrap(~orchardage, scales="free")+  scale_y_continuous(limits=c(5,47))+ theme(axis.line=element_line())+
  labs(x="meek", y="Soil % Water by Volume")+theme(axis.text.x=element_text(color = "black", angle=30, vjust=.8, hjust=0.8))+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "darkred", "black", "grey30", "grey50", "grey70"), )+scale_x_continuous(breaks=c(3, 4, 5, 6, 7, 8), labels=c("march","april", "may", "june", "july", "august"))


# moisture in 2021 - canopy bins: test.

moisture21bin<-left_join(mutate(moisture21_1, orchard_age=orchardage, year=2021), canopy)%>%
  mutate(calibration=ifelse(is.na(nail), probe, (nail*.81)-5.4))%>% #adjust this to calibration line above
  mutate(month=as.numeric(ifelse(date=="03.24.21", 3, ifelse(date=="04.24.21", 4, ifelse(date=="05.14.21", 5,  ifelse(date=="07.31.21", 6,  8))))))%>%
  filter(!is.na(canopy))%>%
  mutate(bin10=ifelse(canopy<11, 0, ifelse(canopy<21&canopy>10, 10, ifelse(canopy<31&canopy>20, 20, ifelse(canopy<41&canopy>30, 30, ifelse(canopy<51&canopy>40, 40, ifelse(canopy<61&canopy>50, 50, ifelse(canopy<71&canopy>60, 60, ifelse(canopy<81&canopy>70, 70, ifelse(canopy<91&canopy>80, 80, 90))))))))))%>%
 mutate(bin20=ifelse(canopy<21, "0-20", ifelse(canopy<41&canopy>20, "20-40", ifelse(canopy<61&canopy>40, "40-60", ifelse(canopy<81&canopy>60, "60-80", "80-100")))))%>%
  mutate(bin25=ifelse(canopy<26, "0-25", ifelse(canopy<51&canopy>25, "26-50", ifelse(canopy<76&canopy>51, "51-75", "75-100"))))%>%
  mutate(evenbin=ifelse(canopy<21, "0-20", ifelse(canopy<38&canopy>20, "20-37", ifelse(canopy<45&canopy>37, "38-44", "45-100"))))


binall_2021<-moisture21bin%>%
  filter(orchard_age!=15)%>%
  group_by(seeding, month)%>%
  summarize(meancal=mean(calibration), secal=calcSE(calibration), n=n())


bin10<-moisture21bin%>%
  filter(orchardage!=15)%>%
  group_by(bin10, seeding, month)%>%
  summarize(meancal=mean(calibration, na.rm=T), secal=calcSE(calibration), n=n())

bin20<-moisture21bin%>%
  filter(orchardage!=15)%>%
  group_by(bin20, seeding, month)%>%
  summarize(meancal=mean(calibration, na.rm=T), secal=calcSE(calibration), n=n())

bin25<-moisture21bin%>%
  filter(orchardage!=15)%>%
  group_by(bin25, seeding, month)%>%
  summarize(meancal=mean(calibration, na.rm=T), secal=calcSE(calibration), n=n())


evenbin<-moisture21bin%>%
  filter(orchardage!=15)%>%
  group_by(evenbin, seeding, month)%>%
  summarize(meancal=mean(calibration, na.rm=T), secal=calcSE(calibration), n=n())

#2021 alternate "binned" excluding 15 year old. 
ggplot(subset(bin25, !seeding%in% c("true control", "north", "south", "Megamix")), aes(x=month, y=meancal)) +
  geom_jitter(data=subset(moisture21bin, orchardage!=15), aes(y=calibration, color=seeding), size=.75, width=.2, alpha=.5)+
  geom_point(aes(color=seeding)) +
  geom_line(aes(color=seeding))+
  geom_errorbar(aes(color=seeding, ymin=meancal-secal, ymax=meancal+secal), width=.2)+
  facet_wrap(~bin25, scales="free")+  scale_y_continuous(limits=c(5,47))+ theme(axis.line=element_line())+
  labs(x="meek", y="Soil % Water by Volume")+theme(axis.text.x=element_text(color = "black", angle=30, vjust=.8, hjust=0.8))+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "darkred", "black", "grey30", "grey50", "grey70"), )+
  scale_x_continuous(breaks=c(3, 4, 5, 6, 7, 8), labels=c("march","april", "may", "june", "july", "august"))



#isoclines test month
grid.arrange(
ggplot(subset(moisture21bin, !seeding%in% c("true control", "north", "south", "Megamix")&orchardage!=15), aes(x=canopy, y=calibration)) +
  geom_point(aes(color=as.factor(month))) +
  geom_smooth(aes(color=as.factor(month)), method="lm", se=F)+
#geom_errorbar(aes(color=seeding, ymin=meancal-secal, ymax=meancal+secal), width=.2)+
  facet_wrap(~seeding, scales="free")+  scale_y_continuous(limits=c(5,47))+ theme(axis.line=element_line())+
  labs(x="meek", y="Soil % Water by Volume")+theme(axis.text.x=element_text(color = "black", angle=30, vjust=.8, hjust=0.8))+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "darkred", "black", "grey30", "grey50", "grey70"), ),
ggplot(subset(moisture20bin, !seedmix%in% c("megamix")&age!=15), aes(x=canopy, y=moisture)) +
  geom_point(aes(color=as.factor(week))) +
  geom_smooth(aes(color=as.factor(week)), method="lm", se=F)+
  #geom_errorbar(aes(color=seeding, ymin=meancal-secal, ymax=meancal+secal), width=.2)+
  facet_wrap(~seedmix, scales="free")+  scale_y_continuous(limits=c(5,47))+ theme(axis.line=element_line())+
  labs(x="meek", y="Soil % Water by Volume")+theme(axis.text.x=element_text(color = "black", angle=30, vjust=.8, hjust=0.8))+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "darkred", "black", "grey30", "grey50", "grey70"), ),
nrow=2)


#isoclines test mix
grid.arrange(
  ggplot(subset(moisture21bin, !seeding%in% c("true control", "north", "south", "Megamix")&orchardage!=15), aes(x=canopy, y=calibration)) +
    geom_point(aes(color=as.factor(seeding))) +
    geom_smooth(aes(color=as.factor(seeding)), method="lm", se=F)+
    #geom_errorbar(aes(color=seeding, ymin=meancal-secal, ymax=meancal+secal), width=.2)+
    facet_wrap(~as.factor(month), scales="free")+  scale_y_continuous(limits=c(5,47))+ theme(axis.line=element_line())+
    labs(x="meek", y="Soil % Water by Volume")+theme(axis.text.x=element_text(color = "black", angle=30, vjust=.8, hjust=0.8))+
    scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "darkred", "black", "grey30", "grey50", "grey70"), ),
  ggplot(subset(moisture20bin, !seedmix%in% c("megamix")&age!=15), aes(x=canopy, y=moisture)) +
    geom_point(aes(color=as.factor(seedmix))) +
    geom_smooth(aes(color=as.factor(seedmix)), method="lm", se=F)+
    #geom_errorbar(aes(color=seeding, ymin=meancal-secal, ymax=meancal+secal), width=.2)+
    facet_wrap(~week, scales="free")+  scale_y_continuous(limits=c(5,47))+ theme(axis.line=element_line())+
    labs(x="meek", y="Soil % Water by Volume")+theme(axis.text.x=element_text(color = "black", angle=30, vjust=.8, hjust=0.8))+
    scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "darkred", "black", "grey30", "grey50", "grey70"), ),
  nrow=2)


#2020 (copied from other script)
ggplot(data=subset(m20_agg, seedmix!="megamix"), aes(x=week, y=mean_moisture, color=seedmix)) +
  facet_wrap(~age, scales="free")+  scale_y_continuous(limits=c(5,47))+ theme(axis.line=element_line())+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=mean_moisture-se_moisture, ymax=mean_moisture+se_moisture), width=.2)+
  labs(x="week", y="Soil % Water by Volume")+theme(axis.text.x=element_text(color = "black", angle=30, vjust=.8, hjust=0.8))+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "darkred", "black", "grey30", "grey50", "grey70"), )+scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6), labels=c("april 7","", "april 24", "may 1", "may 10", "may 17"))

moisture20bin<-left_join(mutate(moisture20, orchard_age=age, year=2020), canopy)%>%
 # mutate(calibration=ifelse(is.na(nail), probe, (nail*.81)-5.4))%>% #notcalibrated
  filter(!is.na(canopy))%>%
  mutate(bin10=ifelse(canopy<11, 0, ifelse(canopy<21&canopy>10, 10, ifelse(canopy<31&canopy>20, 20, ifelse(canopy<41&canopy>30, 30, ifelse(canopy<51&canopy>40, 40, ifelse(canopy<61&canopy>50, 50, ifelse(canopy<71&canopy>60, 60, ifelse(canopy<81&canopy>70, 70, ifelse(canopy<91&canopy>80, 80, 90))))))))))%>%
  mutate(bin20=ifelse(canopy<21, 0, ifelse(canopy<41&canopy>20, 20, ifelse(canopy<61&canopy>40, 40, ifelse(canopy<81&canopy>60, 60, 80)))))%>%
  mutate(bin25=ifelse(canopy<26, "0-25", ifelse(canopy<51&canopy>25, "26-50", ifelse(canopy<76&canopy>51, "51-75", "75-100"))))

binall_2020<-moisture20bin%>%
  filter(orchard_age!=15)%>%
  group_by(seedmix, week)%>%
  summarize(meancal=mean(moisture, na.rm=T), secal=calcSE(moisture), n=n())

bin10_2020<-moisture20bin%>%
  filter(orchard_age!=15)%>%
  group_by(bin10, seedmix, week)%>%
  summarize(meancal=mean(moisture, na.rm=T), secal=calcSE(moisture), n=n())

bin20_2020<-moisture20bin%>%
  filter(orchard_age!=15)%>%
  group_by(bin20, seedmix, week)%>%
  summarize(meancal=mean(moisture, na.rm=T), secal=calcSE(moisture), n=n())

bin25_2020<-moisture20bin%>%
  filter(orchard_age!=15)%>%
  group_by(bin25, seedmix, week)%>%
  summarize(meancal=mean(moisture, na.rm=T), secal=calcSE(moisture), n=n())
#evenbin<-moisture21bin%>%
#  filter(orchardage!=15)%>%
##  group_by(evenbin, seeding, month)%>%
 # summarize(meancal=mean(calibration, na.rm=T), secal=calcSE(calibration), n=n())

#2020 alternate "binned" excluding 15 year old. 
ggplot(subset(bin25_2020, !seedmix%in% c("megamix")), aes(x=week, y=meancal)) +
  geom_jitter(data=subset(moisture20bin, orchard_age!=15), aes(y=moisture, color=seedmix), size=.75, width=.2, alpha=.5)+
  geom_point(aes(color=seedmix)) +
  geom_line(aes(color=seedmix))+
  geom_errorbar(aes(color=seedmix, ymin=meancal-secal, ymax=meancal+secal), width=.2)+
  facet_wrap(~bin25, scales="free")+  scale_y_continuous(limits=c(5,47))+ theme(axis.line=element_line())+
  labs(x="meek", y="Soil % Water by Volume")+theme(axis.text.x=element_text(color = "black", angle=30, vjust=.8, hjust=0.8))+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "darkred", "black", "grey30", "grey50", "grey70"), )+
scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6), labels=c("april 7","", "april 24", "may 1", "may 10", "may 17"))

#2020 and 2021 NOT BINNED
ggplot(subset(binall_2020, seedmix!="megamix"), aes(x=week, y=meancal)) +
  geom_jitter(data=subset(moisture20bin, orchard_age!=15&seedmix!="megamix"), aes(y=moisture, color=seedmix), size=.75, width=.2, alpha=.5)+
  geom_point(aes(color=seedmix)) +
  geom_line(aes(color=seedmix))+
  geom_errorbar(aes(color=seedmix, ymin=meancal-secal, ymax=meancal+secal), width=.2)+
 scale_y_continuous(limits=c(5,47))+ theme(axis.line=element_line())+
  labs(x="week", y="Soil % Water by Volume")+theme(axis.text.x=element_text(color = "black", angle=30, vjust=.8, hjust=0.8))+
  scale_color_manual(values=c("#999999", "darkred", "#56B4E9", "#E69F00"), )+
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6), labels=c("april 7","", "april 24", "may 1", "may 10", "may 17"))
ggplot(subset(binall_2021, !seeding%in% c("true control", "north", "south", "Megamix")), aes(x=month, y=meancal)) +
  geom_jitter(data=subset(moisture21bin, orchardage!=15), aes(y=calibration, color=seeding), size=.75, width=.2, alpha=.5)+
  geom_point(aes(color=seeding)) +
  geom_line(aes(color=seeding))+
  geom_errorbar(aes(color=seeding, ymin=meancal-secal, ymax=meancal+secal), width=.2)+
scale_y_continuous(limits=c(5,47))+ theme(axis.line=element_line())+
  labs(x="month", y="Soil % Water by Volume")+theme(axis.text.x=element_text(color = "black", angle=30, vjust=.8, hjust=0.8))+
  scale_color_manual(values=c("grey70", "darkred","#56B4E9","black","#E69F00"), )+
  scale_x_continuous(breaks=c(3, 4, 5, 6, 7, 8), labels=c("march","april", "may", "june", "july", "august"))



m20_agg<-moisture20%>%
  group_by( age, week ,seedmix)%>%
  summarize(mean_moisture=mean(moisture),
            se_moisture=calcSE(moisture))

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
  #facet_wrap(~age)+
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



  