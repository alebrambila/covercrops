#cover
# will include 2020 peak, 2021 winter, 2021 peak, 
# plot total, weeds, targetspp as a fn of canopy cover in y1
# persistence: 2021/2020 (fn of treatment, orchard)
library(tidyverse)
library(gridExtra)
library(googlesheets4)
library(ggpubr)

theme_set(theme_classic())

source('canopy.R')
select<-dplyr::select

# 2020
cover<-read_csv("community_cover.csv")%>%
  group_by(orchard_age, block, management, seedmix)%>%
 # mutate(industry=sum(barley, oats, vetch, clover))%>%
 # select(-barley, -oats, -vetch, -clover)%>%
  ungroup()%>%
  gather("species", "cov", 5:26)%>%
  filter(!is.na(cov))%>%
  select(-notes)%>%
  mutate(year=2020)%>%
  select(-all_annuals, -all_perennials)

cover21<-read_csv("community_cover21.csv")%>%
  group_by(orchard_age, block, management, seedmix)%>%
#  mutate(industry=sum(barley, oats, vetch, clover))%>%
#  select(-barley, -oats, -vetch, -clover)%>%
  ungroup()%>%
  gather("species", "cov", 5:29, 27)%>%
  filter(!is.na(cov))%>%
  select(-notes)%>%
  mutate(year=2021)

cover<-rbind(cover, cover21)%>%
  mutate(cov=as.numeric(cov))%>%
  filter(!is.na(cov))
  
spkey<-select(filter(cover, year==2020), species, seedmix)%>%
  unique()%>%
  filter(seedmix!="megamix")%>%
  filter(!species %in% c("weeds", "bare"))
#cov$seedmix<-factor(cov$seedmix, levels=c("control", "industry", "perennials", "annuals", "megamix"))

bare<-cover%>%
  filter(species=="bare"|species=="weeds")

# total veg cover (categorical)
#ggplot(subset(bare, bare$species=="bare"), aes(x=seedmix, y=100-as.numeric(cov)))+
#  geom_boxplot(aes(fill=seedmix))+
#  facet_grid(orchard_age~year, scales="free")+ylab("Total Vegetation % Cover")+xlab("")

#aggregate annuals, perennials, industry, megamix
mix_cov<-cover%>%
  group_by(orchard_age, year, block, management, seedmix)%>%
  filter(species!="bare", species!="weeds")%>%
  summarize(cov=sum(cov))
mix_cov$seedmix<-factor(mix_cov$seedmix, levels=c("annuals", "perennials", "megamix", "industry"))

#bare ground
bare.canopy<-left_join(bare, canopy) %>%
  group_by(orchard_age, block, year)%>%
  mutate(blockmeancov=mean(canopy, na.rm=T))%>%
  mutate(canopy=ifelse(is.na(canopy), blockmeancov, canopy))


#aggregated target spp
mix.canopy<-left_join(mix_cov, canopy)

bare.canopy$seedmix<-factor(bare.canopy$seedmix, levels=c("annuals", "perennials", "industry", "megamix", "control"))
mix.canopy$seedmix<-factor(mix.canopy$seedmix, levels=c("annuals", "perennials", "megamix", "industry", "control"))

### add winter cover
wintercover <- read_sheet("https://docs.google.com/spreadsheets/d/1fmXtfIw78BLSAW71C3ji5LhPrUG04aeuFJBrEdWbJvA/edit?usp=sharing")
winter.canopy<-as.tibble(left_join(wintercover, canopy))
winter.canopy$seedmix<-factor(winter.canopy$seedmix, levels=c("annuals", "perennials", "megamix", "industry", "control"))

#switch to boxplots, across canopy cover (or low, int, hi) - management
mix.canopy.cg<-mix.canopy%>%#cg for cover groups
  mutate(cg=ifelse(canopy<40, "low", "hi"))
ggplot(subset(mix.canopy.cg, seedmix=="annuals"|seedmix=="perennials"|seedmix=="industry"), aes(x=as.factor(year), y=cov))+
  geom_boxplot(aes(fill=as.factor(management)))+  #geom_jitter( aes(fill=management, color=management), alpha=.5, size=.5) +
  # stat_smooth(aes(linetype=as.factor(year), color=as.factor(management)), se=F, size = 1,  method = "gam", formula = y ~ s(x))+
  scale_fill_manual(values=c("black", "grey40", "grey70"))+
  ylab("Target % Cover")+xlab("")+facet_grid(~seedmix)#+xlab("Canopy % Cover")+ 
  scale_x_continuous(limits=c(0,95)) + scale_y_continuous(limits=c(0,250))+ theme(axis.line=element_line())
library(multcomp)
  library(nlme)
lme(data=subset(mix.canopy, !is.na(seedmix)), cov~as.factor(management)+seedmix, random=~1|year)
TukeyHSD(aov(data=subset(mix.canopy, seedmix=="annuals"|seedmix=="perennials"|seedmix=="industry"), cov~as.factor(management)*seedmix*as.factor(year)))
  test<-subset(mix.canopy, seedmix=="annuals"|seedmix=="perennials"|seedmix=="industry")%>%
    mutate(trt=as.factor(paste(management, seedmix, year, sep="_")))
  TukeyHSD(aov(data=test, cov~trt))
mod<-  lme(data=test, cov~trt)
  cld(glht(mod, mcp(trt="Tukey")))
 

# FIGURE2: bare and weeds 
### Look at differences between treatment and unseeded control with regards to how they impact bare ground in spring, winter and weeds
  
#spring
bare.canopy.rel<-bare.canopy%>%
    group_by(year, orchard_age, block, management)%>%
    mutate(canopy=mean(canopy))%>%
   select(-blockmeancov)%>%
    spread(seedmix, cov)%>%
    select(-megamix)%>%
    mutate(annualsdiff=(annuals)-(control))%>%
    mutate(perennialsdiff=(perennials)-(control))%>%
    mutate(industrydiff=(industry)-(control))%>%
    select(-(7:11))%>%
    gather(seedmix, cov, annualsdiff, perennialsdiff, industrydiff)
 
#winterbare
 winter.canopy.rel<-winter.canopy%>%
   select(-litter, -bare)%>%
   group_by(year, orchard_age, block, management)%>%
   mutate(canopy=mean(canopy))%>%
   spread(seedmix, veg)%>%
   select(-megamix)%>%
   # mutate(annualslrr=log((100-annuals+1)/(100-control+1)))%>%
   mutate(annualsdiff=(100-annuals)-(100-control))%>%
   mutate(perennialsdiff=(100-perennials)-(100-control))%>%
   mutate(industrydiff=(100-industry)-(100-control))%>%
   select(-(6:9))%>%
   gather(seedmix, bare, annualsdiff, perennialsdiff, industrydiff)
 winter.canopy.rel$seedmix<-factor(winter.canopy.rel$seedmix, levels=c("annualsdiff", "perennialsdiff", "industrydiff"))
 bare.canopy.rel$seedmix<-factor(bare.canopy.rel$seedmix, levels=c("annualsdiff", "perennialsdiff", "industrydiff"))
 
 
#spring bare
ggplot(subset(bare.canopy.rel, !is.na(seedmix)&species=="bare"&management!="orchard"),aes(x=canopy, y=cov))+
  #geom_point(aes(shape=as.factor(year), color=as.factor(management)), size=1) + 
  #stat_smooth(aes(linetype=as.factor(year), color=as.factor(management)),method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
  geom_point(aes(shape=as.factor(year)), size=1)+
   geom_smooth(aes(linetype=as.factor(year)), se=F, method = "lm", formula = y ~ x, size = 1, color="black")+
 # geom_smooth(data=subset(bare.canopy,seedmix!="megamix"&!is.na(seedmix)), aes(color=management, linetype=as.factor(year)), se=F, size = 1,  method = "gam", formula = y ~ s(x),)+
  # stat_smooth(aes(linetype=as.factor(year), color=as.factor(management)), se=F, size = 1,  method = "gam", formula = y ~ s(x))+
 scale_color_manual(values=c("black", "grey40", "grey70"))+
  ylab("Difference in % bare ground spring")+xlab("")+facet_wrap(~seedmix, scales="free")+xlab("% Canopy cover")+ 
  scale_x_continuous(limits=c(0,100)) + scale_y_continuous(limits=c(-100,60))+ theme(axis.line=element_line())+geom_hline(aes(yintercept=0))

#weeds
ggplot(subset(bare.canopy.rel, !is.na(seedmix)&species=="weeds"&management!="orchard"),aes(x=canopy, y=cov))+
  #geom_point(aes(shape=as.factor(year), color=as.factor(management)), size=1) + 
  #stat_smooth(aes(linetype=as.factor(year), color=as.factor(management)),method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
  geom_point(aes(shape=as.factor(year)), size=1)+
  geom_smooth(aes(linetype=as.factor(year)), se=F, method = "lm", formula = y ~ x, size = 1, color="black")+
  # geom_smooth(data=subset(bare.canopy,seedmix!="megamix"&!is.na(seedmix)), aes(color=management, linetype=as.factor(year)), se=F, size = 1,  method = "gam", formula = y ~ s(x),)+
  # stat_smooth(aes(linetype=as.factor(year), color=as.factor(management)), se=F, size = 1,  method = "gam", formula = y ~ s(x))+
  scale_color_manual(values=c("black", "grey40", "grey70"))+
  ylab("Difference in % cover weeds spring")+xlab("")+facet_wrap(~seedmix, scales="free")+xlab("% Canopy cover")+ 
  scale_x_continuous(limits=c(0,100)) + scale_y_continuous(limits=c(-100,60))+ theme(axis.line=element_line())+geom_hline(aes(yintercept=0))

#winter
ggplot(subset(winter.canopy.rel, !is.na(seedmix)),aes(x=canopy, y=bare))+
  #geom_point(aes(shape=as.factor(year), color=as.factor(management)), size=1) + 
  #stat_smooth(aes(linetype=as.factor(year), color=as.factor(management)),method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
  geom_point(aes(shape=as.factor(year)))+
  geom_smooth(aes(linetype=as.factor(year)),size = 1, method="lm", formula=y~(x), se=F, color="black")+
  # geom_smooth(data=subset(bare.canopy,seedmix!="megamix"&!is.na(seedmix)), aes(color=management, linetype=as.factor(year)), se=F, size = 1,  method = "gam", formula = y ~ s(x),)+
  # stat_smooth(aes(linetype=as.factor(year), color=as.factor(management)), se=F, size = 1,  method = "gam", formula = y ~ s(x))+
  scale_color_manual(values=c("black", "grey40", "grey70"))+
  ylab("Difference in % cover bare ground winter")+xlab("")+facet_wrap(~seedmix, scales="free")+xlab("% Canopy cover") +
  scale_x_continuous(limits=c(0,100)) + scale_y_continuous(limits=c(-100,60))+ theme(axis.line=element_line())+geom_hline(aes(yintercept=0))





#w/mech as facet (not very interesting)
#ggplot(subset(mix.canopy), aes(x=canopy, y=cov))+
#  geom_point(aes(color=management, shape=as.factor(orchard_age))) + 
##  stat_smooth(aes(color=management), method = "lm", se=F, formula = y ~ x, size = 1)+
#  ylab("Target % Cover")+xlab("")+facet_grid(seedmix~year)

#individual spp 
# need to group these, now ignoring what zone they're in to get their total cover (expansion)
veg.canopy<-left_join(cover, canopy) %>% 
  filter(species!="weeds"&species!="bare"&seedmix!="megamix")%>%
  group_by(orchard_age, block, management)%>%
  mutate(canopy=mean(canopy))%>%
  group_by(orchard_age, block, management, species, year, canopy)%>%
  summarize(plotcover=sum(cov))%>% #sum so if it goes outside of its plot its >100%, also include megamix
  spread(species, plotcover, fill=0)%>%
  gather("species", "plotcover", 6:25)%>%
  mutate(delete=paste(species, management, sep="_"))%>%
  filter(delete!="barley_unmanaged"&delete!="oats_unmanaged"&delete!="clover_unmanaged"&delete!="vetch_unmanaged")%>%
  select(-delete)

veg.canopy$species<-factor(veg.canopy$species, levels=c("amsinckia", "clarkia", "collomia", "epilobium", "gilia", "plectritis", "achillea", "eriophyllum", "geum", "lomatium", "potentilla", "prunella", "barley", "oats", "clover", "vetch","agoseris", "lotus", "viola"))
veg.canopy$management<-factor(veg.canopy$management, levels=c("unmanaged", "flailed", "scraped"))


# Publication supplemental figure 1
#grid.arrange(
#  ggplot(subset(veg.canopy, species%in%c("amsinckia", "clarkia", "collomia", "epilobium", "gilia", "plectritis", "achillea", "eriophyllum")), aes(x=canopy, y=plotcover))+ ## CHECK OUT WHAT IS GOING ON WITH INDUSTRY
#    geom_point(aes(color=management, shape=as.factor(orchard_age))) + 
#    stat_smooth(aes(color=management), method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
#    facet_grid(year~species)+ geom_hline(yintercept = 0)+ ylim(-5, 150)+
#    scale_color_manual(values=c("black", "grey40", "grey70"))+
#    ylab("Target % Cover")+xlab(""), 
#  ggplot(subset(veg.canopy, species%in%c("geum", "lomatium", "potentilla", "prunella", "barley", "oats", "clover")), aes(x=canopy, y=plotcover))+ ## CHECK OUT WHAT IS GOING ON WITH INDUSTRY
#    geom_point(aes(color=management, shape=as.factor(orchard_age))) + 
#    stat_smooth(aes(color=management), method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
#    facet_grid(year~species)+geom_hline(yintercept = 0)+ylim(-5, 150)+
#    scale_color_manual(values=c("black", "grey40", "grey70"))+
#    ylab("Target % Cover")+xlab("Canopy Cover"), 
#  nrow=2
#)


#annuals

grid.arrange(
  ggplot(subset(veg.canopy, species%in%c("amsinckia", "clarkia", "collomia", "lotus")), aes(x=canopy, y=plotcover))+ ## CHECK OUT WHAT IS GOING ON WITH INDUSTRY
    geom_point(aes(shape=as.factor(year))) + 
    #stat_smooth(data=as.data.frame(subset(veg.canopy, 
    #                                      (species=="lotus"&year==2020) )), 
    #            aes(linetype=as.factor(year)), method = "lm", se=F, formula = y ~ x, size = 1, color="grey60")+ #all of the nonsigs, hashed out for now. no line if it's not signficant
    stat_smooth(data=as.data.frame(subset(veg.canopy, 
                                          (species=="clarkia"&year==2021)|
                                            (species=="lotus"&year==2021) ))
                , aes(linetype=as.factor(year)), method = "lm", se=F, formula = y ~ x, size = 1, color="black")+ #all of the linsigs
    stat_smooth(data=as.data.frame(subset(veg.canopy, 
                                          species=="amsinckia"| species=="collomia"|
                                            (species=="clarkia"&year==2020) )), 
                aes(linetype=as.factor(year)), method = "lm", se=F, formula = y ~ poly(x, 2), size = 1, color="black")+ #all of the polysigs
    #linear
    stat_regline_equation(data=as.data.frame(subset(veg.canopy, 
                                                    (species=="clarkia"&year==2021)|
                                                      (species=="lotus") ))
                          , aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), group=year), formula = y ~ x)+ 
    #poly
    stat_regline_equation(data=as.data.frame(subset(veg.canopy, 
                                                    species=="amsinckia"| species=="collomia"|
                                                      (species=="clarkia"&year==2020) )),  aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), group=year), formula = y ~ poly(x, 2))+ 
    facet_grid(~species)+geom_hline(yintercept = 0)+ylim(-5, 150)+
    scale_color_manual(values=c("black", "grey40", "grey70"))+
    ylab("Target % Cover")+xlab(""),
  ggplot(subset(veg.canopy, species%in%c("epilobium", "gilia", "plectritis", "agoseris")), aes(x=canopy, y=plotcover))+ ## CHECK OUT WHAT IS GOING ON WITH INDUSTRY
    geom_point(aes(shape=as.factor(year))) + 
    #poly
    stat_smooth(data=as.data.frame(subset(veg.canopy, 
                                                    species=="plectritis")),aes(linetype=as.factor(year)), method = "lm", se=F, formula = y ~ poly(x, 2), size = 1, color="black")+ 
    #lin
    stat_smooth(data=as.data.frame(subset(veg.canopy, (species=="epilobium")|(species=="gilia"))), aes(linetype=as.factor(year)), method = "lm", se=F, formula = y ~ x, size = 1, color="black")+
   
     facet_grid(~species)+geom_hline(yintercept = 0)+ylim(-5, 150)+
    stat_regline_equation(data=as.data.frame(subset(veg.canopy, 
                                                    species=="plectritis")),aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), group=year), formula = y ~poly( x, 2))+
    stat_regline_equation(data=as.data.frame(subset(veg.canopy, (species=="epilobium")|(species=="gilia"))), aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), group=year), formula = y ~ x)+
      scale_color_manual(values=c("black", "grey40", "grey70"))+
    ylab("Target % cover")+xlab("% Canopy cover"), nrow=2)

#annuals
summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("amsinckia")&year==2020&!is.na(canopy))))) #poly
summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("amsinckia")&year==2021&!is.na(canopy))))) #poly
summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("clarkia")&year==2020&!is.na(canopy))))) #poly
summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("clarkia")&year==2021&!is.na(canopy))))) #lin
summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("collomia")&year==2020&!is.na(canopy))))) #poly
summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("collomia")&year==2021&!is.na(canopy))))) # poly
summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("lotus")&year==2020&!is.na(canopy)))))# nothing
summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("lotus")&year==2021&!is.na(canopy))))) #lin

summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("epilobium")&year==2021&!is.na(canopy))))) #lin
summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("epilobium")&year==2021&!is.na(canopy))))) #lin
summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("gilia")&year==2021&!is.na(canopy))))) #lin
summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("gilia")&year==2021&!is.na(canopy))))) #lin
summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("plectritis")&year==2021&!is.na(canopy))))) #poly
summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("plectritis")&year==2021&!is.na(canopy))))) #poly

  
  
  #perennials
grid.arrange(
  ggplot(subset(veg.canopy, species%in%c("achillea","agoseris", "eriophyllum", "geum")), aes(x=canopy, y=plotcover))+ ## CHECK OUT WHAT IS GOING ON WITH INDUSTRY
    geom_point(aes(shape=as.factor(year))) + 
 #   stat_smooth(data=as.data.frame(subset(veg.canopy, 
#                                          (species=="achillea"&year==2020)|
#                                            (species=="agoseris"&year==2021))), 
#                aes(linetype=as.factor(year)), method = "lm", se=F, formula = y ~ x, size = 1, color="gray60")+ #all of the nonsigs
   stat_smooth(data=as.data.frame(subset(veg.canopy, 
                                         (species=="achillea"&year==2021)|
                                           (species=="eriophyllum"&year==2021) ))
               , aes(linetype=as.factor(year)), method = "lm", se=F, formula = y ~ x, size = 1, color="black")+ #all of the linsigs
  stat_smooth(data=as.data.frame(subset(veg.canopy, 
                                           species=="geum"|
                                          (species=="agoseris"&year==2020)|
                                          (species=="eriophyllum"&year==2020) )), 
              aes(linetype=as.factor(year)), method = "lm", se=F, formula = y ~ poly(x, 2), size = 1, color="black")+ #all of the polysigs
#linear
  stat_regline_equation(data=as.data.frame(subset(veg.canopy, 
                                                  (species=="achillea"&year==2020)|(species=="achillea"&year==2021)|
                                                    (species=="eriophyllum"&year==2021)|
                                                    (species=="agoseris"&year==2021))), aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), group=year), formula = y ~ x)+ 
 #poly
  stat_regline_equation(data=as.data.frame(subset(veg.canopy, 
                                                  species=="geum"|
                                                    (species=="agoseris"&year==2020)|
                                                    (species=="eriophyllum"&year==2020))), aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), group=year), formula = y ~ poly(x, 2))+ 
    facet_grid(~species)+geom_hline(yintercept = 0)+ylim(-5, 150)+
    scale_color_manual(values=c("black", "grey40", "grey70"))+
    ylab("Target % Cover")+xlab(""),
  ggplot(subset(veg.canopy, species%in%c("lomatium", "potentilla", "prunella", "viola")), aes(x=canopy, y=plotcover))+ ## CHECK OUT WHAT IS GOING ON WITH INDUSTRY
    geom_point(aes(shape=as.factor(year))) + 
  #non
 # stat_smooth(data=as.data.frame(subset(veg.canopy, (species=="potentilla")|
#                                          (species=="prunella"&year==2020)|
#                                          (species=="viola"&year==2021))), aes(linetype=as.factor(year)), method = "lm", se=F, formula = y ~ x, size = 1, color="gray60")+ #all of the nonsigs (drop for now)
 #lin
  stat_smooth(data=as.data.frame(subset(veg.canopy, (species=="lomatium")|
                                          (species=="prunella"&year==2021)|
                                          (species=="viola"&year==2020))), aes(linetype=as.factor(year)), method = "lm", se=F, formula = y ~ x, size = 1, color="black")+
    facet_grid(~species)+geom_hline(yintercept = 0)+ylim(-5, 150)+
    stat_regline_equation(aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), group=year), formula = y ~ x)+
    scale_color_manual(values=c("black", "grey40", "grey70"))+
    ylab("Target % Cover")+xlab("Canopy Cover"), nrow=2)

#perennials
#summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("achillea")&year==2020&!is.na(canopy))))) #nothing 
#summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("achillea")&year==2021&!is.na(canopy))))) #linear 
#summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("eriophyllum")&year==2020&!is.na(canopy))))) #poly 
#summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("eriophyllum")&year==2021&!is.na(canopy))))) #linear 
#summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("geum")&year==2020&!is.na(canopy))))) #poly
#summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("geum")&year==2021&!is.na(canopy))))) # poly
#summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("agoseris")&year==2020&!is.na(canopy)))))# poly
#summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("agoseris")&year==2021&!is.na(canopy))))) #nothing
#summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("lomatium")&year==2020&!is.na(canopy)))))# linear
#summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("lomatium")&year==2021&!is.na(canopy))))) #linear
#summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("potentilla")&year==2020&!is.na(canopy)))))# nothing
#summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("potentilla")&year==2021&!is.na(canopy))))) #nothing
#summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("prunella")&year==2020&!is.na(canopy)))))# nothing
#summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("prunella")&year==2021&!is.na(canopy))))) #linear
#summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("viola")&year==2020&!is.na(canopy)))))# linear
#summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("viola")&year==2021&!is.na(canopy))))) #nothing

  #industry
grid.arrange(
  ggplot(subset(veg.canopy, species%in%c("barley", "oats")), aes(x=canopy, y=plotcover))+ ## CHECK OUT WHAT IS GOING ON WITH INDUSTRY
    geom_point(aes( shape=as.factor(year))) + 
    stat_smooth(data=subset(veg.canopy, species%in%c("barley")&year==2020), aes(linetype=as.factor(year)), method = "lm", se=F, formula = y ~ x, size = 1, color="gray60")+
    stat_smooth(data=subset(veg.canopy, species%in%c("barley")&year==2021), aes(linetype=as.factor(year)), method = "lm", se=F, formula = y ~ x, size = 1, color="black")+
    stat_smooth(data=subset(veg.canopy, species%in%c("oats")), aes(linetype=as.factor(year)), method = "lm", se=F, formula = y ~ x, size = 1, color="gray60")+
    facet_grid(~species)+geom_hline(yintercept = 0)+ylim(-5, 150)+
    scale_color_manual(values=c("black", "grey40"))+
    stat_regline_equation(data=subset(veg.canopy, species%in%c("oats")), aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), group=year), formula = y ~ x)+ #barley linear, oats poly
    stat_regline_equation(data=subset(veg.canopy, species%in%c("barley")), aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), group=year), formula = y ~ x)+
    ylab("Target % Cover")+xlab(""),
  ggplot(subset(veg.canopy, species%in%c("clover", "vetch")), aes(x=canopy, y=plotcover))+ ## CHECK OUT WHAT IS GOING ON WITH INDUSTRY
    geom_point(aes( shape=as.factor(year))) + 
    stat_smooth(data=subset(veg.canopy, species%in%c("vetch")&year==2020), aes(linetype=as.factor(year)), method = "lm", se=F, formula = y ~ poly(x, 2), size = 1, color="black")+
    stat_smooth(data=subset(veg.canopy, species=="clover"|(species%in%c("vetch")&year==2021)), aes(linetype=as.factor(year)), method = "lm", se=F, formula = y ~ x, size = 1, color="black")+
    stat_smooth(data=subset(veg.canopy, species=="vetch"&year==2021), aes(linetype=as.factor(year)), method = "lm", se=F, formula = y ~ x, size = 1, color="gray60")+
    stat_smooth(data=subset(veg.canopy, species=="clover"&year==2020), aes(linetype=as.factor(year)), method = "lm", se=F, formula = y ~ x, size = 1, color="gray60")+
    stat_regline_equation(data=subset(veg.canopy, species%in%c("vetch")&year==2020), aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), group=year), formula = y ~ poly(x, 2))+ #clover lin, vetch 21 poly, 20 lin
    stat_regline_equation(data=subset(veg.canopy, species=="clover"|(species%in%c("vetch")&year==2021)), aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), group=year), formula = y ~ x)+
    facet_grid(~species)+geom_hline(yintercept = 0)+ylim(-5, 150)+
    scale_color_manual(values=c("black", "grey40"))+
    ylab("Target % Cover")+xlab("Canopy Cover"), nrow=2)
#I CHOSE POLY ONLY WHEN R2 INCREASED BY .05 or GREATER, OTHERWISE, LINEAR <-no checked for significance of linear and polynomial terms
#describe this in text as x species had polynomial relationship in at least one year, with 

#industry
summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("barley")&year==2020&!is.na(canopy))))) #nothing significant
summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("barley")&year==2021&!is.na(canopy))))) #linear p=.004
summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("clover")&year==2020&!is.na(canopy))))) #nothing significant
summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("clover")&year==2021&!is.na(canopy))))) #lin p=3.13 -05
summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("oats")&year==2020&!is.na(canopy))))) #nothing
summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("oats")&year==2021&!is.na(canopy))))) # nothing
summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("vetch")&year==2020&!is.na(canopy)))))# no lin, poly=.03
summary(lm(plotcover~poly(canopy,2),data=as.data.frame(subset(veg.canopy, species==("vetch")&year==2021&!is.na(canopy))))) #nothing



#try a version that maxxes out at 100% (or does not add in % that spread - simplify).
# reorder spp by descending average cover.

