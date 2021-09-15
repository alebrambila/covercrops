#cover
# will include 2020 peak, 2021 winter, 2021 peak, 
# plot total, weeds, targetspp as a fn of canopy cover in y1
# persistence: 2021/2020 (fn of treatment, orchard)
library(tidyverse)
library(gridExtra)
library(googlesheets4)

theme_set(theme_classic())

source('canopy.R')

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

ggplot(mix_cov, aes(x=seedmix, y=cov))+
  geom_boxplot(aes(fill=seedmix))+
  facet_grid(orchard_age*management~year, scales="free")+ylab("% Cover Target")


### Redo with continuous cover

#bare ground
bare.canopy<-left_join(bare, canopy) %>%
  group_by(orchard_age, block, year)%>%
  mutate(blockmeancov=mean(canopy, na.rm=T))%>%
  mutate(canopy=ifelse(is.na(canopy), blockmeancov, canopy))



ggplot(subset(bare.canopy, bare$species=="bare"&management!="orchard"), aes(x=canopy, y=cov))+
  geom_point(aes(color=seedmix, shape=as.factor(orchard_age))) + 
  stat_smooth(aes(color=seedmix), se=F, method = "gam", formula = y ~ s(x), size = 1)+
  ylab("Total May Bare Ground % Cover")+xlab("")+facet_grid(~year)

#weeds
ggplot(subset(bare.canopy, bare$species=="weeds"&management!="orchard"), aes(x=canopy, y=(cov)))+
  geom_point(aes(color=seedmix, shape=as.factor(orchard_age))) + 
  stat_smooth(aes(color=seedmix), se=F, method = "lm", formula = y ~ poly(x, 2), size = 1)+
  ylab("Weeds % Cover")+xlab("")+facet_grid(~year)

#aggregated target spp
mix.canopy<-left_join(mix_cov, canopy)

ggplot(subset(mix.canopy), aes(x=canopy, y=cov))+
  geom_point(aes(color=seedmix, shape=as.factor(orchard_age))) + 
  stat_smooth(aes(color=seedmix),method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
  ylab("Target % Cover")+xlab("")+facet_grid(year~management)

#no management
ggplot(subset(mix.canopy), aes(x=canopy, y=cov))+
  geom_point(aes(color=seedmix, shape=as.factor(orchard_age))) + 
  stat_smooth(aes(color=seedmix),method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
  ylab("Target % Cover")+xlab("")+facet_grid(~year)


bare.canopy$seedmix<-factor(bare.canopy$seedmix, levels=c("annuals", "perennials", "industry", "megamix", "control"))
mix.canopy$seedmix<-factor(mix.canopy$seedmix, levels=c("annuals", "perennials", "megamix", "industry", "control"))

### add winter cover
wintercover <- read_sheet("https://docs.google.com/spreadsheets/d/1fmXtfIw78BLSAW71C3ji5LhPrUG04aeuFJBrEdWbJvA/edit?usp=sharing")
winter.canopy<-as.tibble(left_join(wintercover, canopy))
winter.canopy$seedmix<-factor(winter.canopy$seedmix, levels=c("annuals", "perennials", "megamix", "industry", "control"))

ggplot(winter.canopy, aes(x=canopy, y=veg))+
  geom_point(aes(color=seedmix, shape=as.factor(orchard_age))) +    
 scale_color_manual(values=c("darkgreen", "darkorange", "dodgerblue", "tan", "black"))+
  stat_smooth(aes(color=seedmix), se=F, method = "lm", formula = y ~ poly(x, 2), size = 1)+
  ylab("December Vegetative % Cover")+xlab("")+facet_grid(~year)


# Publication figure 1
grid.arrange(
  ggplot(subset(mix.canopy), aes(x=canopy, y=cov))+
    geom_point(aes(color=seedmix, shape=as.factor(orchard_age))) + 
    stat_smooth(aes(color=seedmix),method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
    scale_color_manual(values=c("darkgreen", "darkorange", "dodgerblue", "tan", "black"))+
    ylab("Target % Cover")+xlab("")+facet_grid(~year),
  ggplot(subset(bare.canopy, bare$species=="weeds"&management!="orchard"), aes(x=canopy, y=(cov)))+
    geom_point(aes(color=seedmix, shape=as.factor(orchard_age))) + 
    scale_color_manual(values=c("darkgreen", "darkorange", "dodgerblue", "tan", "black"))+
    stat_smooth(aes(color=seedmix), se=F, method = "lm", formula = y ~ poly(x, 2), size = 1)+
    ylab("Weeds % Cover")+xlab("")+facet_grid(~year),
  ggplot(subset(bare.canopy, bare$species=="bare"&management!="orchard"), aes(x=canopy, y=cov))+
    geom_point(aes(color=seedmix, shape=as.factor(orchard_age))) +    
    scale_color_manual(values=c("darkgreen", "darkorange", "dodgerblue", "tan", "black"))+
    stat_smooth(aes(color=seedmix), se=F, method = "lm", formula = y ~ poly(x, 2), size = 1)+
    ylab("Total May Bare Ground % Cover")+xlab("")+facet_grid(~year),
  ggplot(winter.canopy, aes(x=canopy, y=veg))+
    geom_point(aes(color=seedmix, shape=as.factor(orchard_age))) +    
    scale_color_manual(values=c("darkgreen", "darkorange", "dodgerblue", "tan", "black"))+
    stat_smooth(aes(color=seedmix), se=F, method = "lm", formula = y ~ poly(x, 2), size = 1)+
    ylab("December Vegetative % Cover")+xlab("")+facet_grid(~year),
  nrow=4)
  

# edits from lauren; switch contrast, main panels for larger figure 1
ggplot(subset(mix.canopy, seedmix=="annuals"|seedmix=="perennials"|seedmix=="industry"), aes(x=canopy, y=cov))+
  geom_point(aes(shape=as.factor(year)), size=1, color="darkgreen") + 
  #stat_smooth(aes(linetype=as.factor(year)),method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
  stat_smooth(aes(linetype=as.factor(year)), se=F, size = 1,  method = "gam", formula = y ~ s(x), color="darkgreen")+
  scale_color_manual(values=c("darkorange", "dodgerblue"))+
  geom_point(data=subset(bare.canopy,seedmix!="megamix"&!is.na(seedmix)), aes(color=species, shape=as.factor(year)), size=1)+
 # geom_smooth(data=subset(bare.canopy,seedmix!="megamix"&!is.na(seedmix)), aes(color=species,linetype=as.factor(year)), se=F, method = "lm", formula = y ~ poly(x, 2), size = 1)+
  geom_smooth(data=subset(bare.canopy,seedmix!="megamix"&!is.na(seedmix)), aes(color=species,linetype=as.factor(year)), se=F, size = 1,  method = "gam", formula = y ~ s(x),)+
  ylab("Target % Cover")+xlab("")+facet_wrap(~seedmix)+xlab("Canopy % Cover")+ 
  scale_x_continuous(limits=c(0,95)) + scale_y_continuous(limits=c(0,250))+ theme(axis.line=element_line())





#mechanical rather than bare/weeds (to parallel invidvidual species better)
ggplot(subset(mix.canopy, seedmix=="annuals"|seedmix=="perennials"|seedmix=="industry"), aes(x=canopy, y=cov))+
  geom_point(aes(shape=as.factor(year), color=as.factor(management)), size=1) + 
  stat_smooth(aes(linetype=as.factor(year), color=as.factor(management)),method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
 # stat_smooth(aes(linetype=as.factor(year), color=as.factor(management)), se=F, size = 1,  method = "gam", formula = y ~ s(x))+
  scale_color_manual(values=c("darkgreen", "darkorange", "dodgerblue"))+
  ylab("Target % Cover")+xlab("")+facet_wrap(~seedmix, scales="free")+xlab("Canopy % Cover")+ 
  scale_x_continuous(limits=c(0,95)) + scale_y_continuous(limits=c(0,250))+ theme(axis.line=element_line())

#greyscale
ggplot(subset(mix.canopy, seedmix=="annuals"|seedmix=="perennials"|seedmix=="industry"), aes(x=canopy, y=cov))+
  geom_point(aes(shape=as.factor(year), color=as.factor(management)), size=1) + 
  stat_smooth(aes(linetype=as.factor(year), color=as.factor(management)),method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
  # stat_smooth(aes(linetype=as.factor(year), color=as.factor(management)), se=F, size = 1,  method = "gam", formula = y ~ s(x))+
  scale_color_manual(values=c("black", "grey40", "grey70"))+
  ylab("Target % Cover")+xlab("")+facet_wrap(~seedmix, scales="free")+xlab("Canopy % Cover")+ 
  scale_x_continuous(limits=c(0,95)) + scale_y_continuous(limits=c(0,250))+ theme(axis.line=element_line())


# FIGURE2: bare and weeds

#springbare
ggplot(subset(bare.canopy, !is.na(seedmix)&species=="bare"),aes(x=canopy, y=cov))+
  #geom_point(aes(shape=as.factor(year), color=as.factor(management)), size=1) + 
  #stat_smooth(aes(linetype=as.factor(year), color=as.factor(management)),method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
  geom_point(aes(color=management, shape=as.factor(year)), size=1)+
   geom_smooth(aes(color=management,linetype=as.factor(year)), se=F, method = "lm", formula = y ~ poly(x, 2), size = 1)+
 # geom_smooth(data=subset(bare.canopy,seedmix!="megamix"&!is.na(seedmix)), aes(color=management, linetype=as.factor(year)), se=F, size = 1,  method = "gam", formula = y ~ s(x),)+
  # stat_smooth(aes(linetype=as.factor(year), color=as.factor(management)), se=F, size = 1,  method = "gam", formula = y ~ s(x))+
 scale_color_manual(values=c("black", "grey40", "grey70"))+
  ylab("% Cover")+xlab("")+facet_wrap(~seedmix, scales="free")+xlab("Canopy % Cover")+ 
  scale_x_continuous(limits=c(0,100)) + scale_y_continuous(limits=c(0,101))+ theme(axis.line=element_line())

#weeds
ggplot(subset(bare.canopy, !is.na(seedmix)&species=="weeds"),aes(x=canopy, y=cov))+
  #geom_point(aes(shape=as.factor(year), color=as.factor(management)), size=1) + 
  #stat_smooth(aes(linetype=as.factor(year), color=as.factor(management)),method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
  geom_point(aes(color=management, shape=as.factor(year)), size=1)+
  geom_smooth(aes(color=management,linetype=as.factor(year)), se=F, method = "lm", formula = y ~ poly(x, 2), size = 1)+
  # geom_smooth(data=subset(bare.canopy,seedmix!="megamix"&!is.na(seedmix)), aes(color=management, linetype=as.factor(year)), se=F, size = 1,  method = "gam", formula = y ~ s(x),)+
  # stat_smooth(aes(linetype=as.factor(year), color=as.factor(management)), se=F, size = 1,  method = "gam", formula = y ~ s(x))+
  scale_color_manual(values=c("black", "grey40", "grey70"))+
  ylab("% Cover")+xlab("")+facet_wrap(~seedmix, scales="free")+xlab("Canopy % Cover")+ 
  scale_x_continuous(limits=c(0,100)) + scale_y_continuous(limits=c(0,101))+ theme(axis.line=element_line())

#winterbare
ggplot(subset(winter.canopy, !is.na(seedmix)),aes(x=canopy, y=100-veg))+
  #geom_point(aes(shape=as.factor(year), color=as.factor(management)), size=1) + 
  #stat_smooth(aes(linetype=as.factor(year), color=as.factor(management)),method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
  geom_point(aes(color=management, shape=as.factor(year)), size=1)+
  geom_smooth(aes(color=management,linetype=as.factor(year)), se=F, method = "lm", formula = y ~ poly(x, 2), size = 1)+
  # geom_smooth(data=subset(bare.canopy,seedmix!="megamix"&!is.na(seedmix)), aes(color=management, linetype=as.factor(year)), se=F, size = 1,  method = "gam", formula = y ~ s(x),)+
  # stat_smooth(aes(linetype=as.factor(year), color=as.factor(management)), se=F, size = 1,  method = "gam", formula = y ~ s(x))+
  scale_color_manual(values=c("black", "grey40", "grey70"))+
  ylab("% Cover")+xlab("")+facet_wrap(~seedmix, scales="free")+xlab("Canopy % Cover")+ 
  scale_x_continuous(limits=c(0,100)) + scale_y_continuous(limits=c(0,101))+ theme(axis.line=element_line())





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

ggplot(subset(veg.canopy, !species%in%c("agoseris", "lotus", "viola")), aes(x=canopy, y=plotcover))+ ## CHECK OUT WHAT IS GOING ON WITH INDUSTRY
  geom_point(aes(color=management, shape=as.factor(orchard_age))) + 
  stat_smooth(aes(color=management), method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
  facet_grid(year~species)+
  scale_color_manual(values=c("black", "grey40", "grey70"))+
  ylab("Target % Cover")+xlab("Canopy Cover")# achillea over 100%
  

# Publication supplemental figure 1
grid.arrange(
  ggplot(subset(veg.canopy, species%in%c("amsinckia", "clarkia", "collomia", "epilobium", "gilia", "plectritis", "achillea", "eriophyllum")), aes(x=canopy, y=plotcover))+ ## CHECK OUT WHAT IS GOING ON WITH INDUSTRY
    geom_point(aes(color=management, shape=as.factor(orchard_age))) + 
    stat_smooth(aes(color=management), method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
    facet_grid(year~species)+ geom_hline(yintercept = 0)+ ylim(-5, 150)+
    scale_color_manual(values=c("black", "grey40", "grey70"))+
    ylab("Target % Cover")+xlab(""), 
  ggplot(subset(veg.canopy, species%in%c("geum", "lomatium", "potentilla", "prunella", "barley", "oats", "clover")), aes(x=canopy, y=plotcover))+ ## CHECK OUT WHAT IS GOING ON WITH INDUSTRY
    geom_point(aes(color=management, shape=as.factor(orchard_age))) + 
    stat_smooth(aes(color=management), method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
    facet_grid(year~species)+geom_hline(yintercept = 0)+ylim(-5, 150)+
    scale_color_manual(values=c("black", "grey40", "grey70"))+
    ylab("Target % Cover")+xlab("Canopy Cover"), 
  nrow=2
)


#annuals
grid.arrange(
  ggplot(subset(veg.canopy, species%in%c("amsinckia", "clarkia", "collomia")), aes(x=canopy, y=plotcover))+ ## CHECK OUT WHAT IS GOING ON WITH INDUSTRY
    geom_point(aes(color=management, shape=as.factor(year))) + 
    stat_smooth(aes(color=management, linetype=as.factor(year)), method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
    facet_grid(~species)+ geom_hline(yintercept = 0)+ ylim(-5, 150)+
    scale_color_manual(values=c("black", "grey40", "grey70"))+
    ylab("Target % Cover")+xlab(""),
  ggplot(subset(veg.canopy, species%in%c( "epilobium", "gilia", "plectritis")), aes(x=canopy, y=plotcover))+ ## CHECK OUT WHAT IS GOING ON WITH INDUSTRY
    geom_point(aes(color=management, shape=as.factor(year))) + 
    stat_smooth(aes(color=management, linetype=as.factor(year)), method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
    facet_grid(~species)+ geom_hline(yintercept = 0)+ ylim(-5, 150)+
    scale_color_manual(values=c("black", "grey40", "grey70"))+
    ylab("Target % Cover")+xlab("Canopy Cover"),
  nrow=2)
  
  
  #perennials
grid.arrange(
  ggplot(subset(veg.canopy, species%in%c("achillea", "eriophyllum", "geum")), aes(x=canopy, y=plotcover))+ ## CHECK OUT WHAT IS GOING ON WITH INDUSTRY
    geom_point(aes(color=management, shape=as.factor(year))) + 
    stat_smooth(aes(color=management, linetype=as.factor(year)), method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
    facet_grid(~species)+geom_hline(yintercept = 0)+ylim(-5, 150)+
    scale_color_manual(values=c("black", "grey40", "grey70"))+
    ylab("Target % Cover")+xlab(""),
  ggplot(subset(veg.canopy, species%in%c("lomatium", "potentilla", "prunella")), aes(x=canopy, y=plotcover))+ ## CHECK OUT WHAT IS GOING ON WITH INDUSTRY
    geom_point(aes(color=management, shape=as.factor(year))) + 
    stat_smooth(aes(color=management, linetype=as.factor(year)), method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
    facet_grid(~species)+geom_hline(yintercept = 0)+ylim(-5, 150)+
    scale_color_manual(values=c("black", "grey40", "grey70"))+
    ylab("Target % Cover")+xlab("Canopy Cover"), nrow=2)

  #industry
grid.arrange(
  ggplot(subset(veg.canopy, species%in%c("barley", "oats")), aes(x=canopy, y=plotcover))+ ## CHECK OUT WHAT IS GOING ON WITH INDUSTRY
    geom_point(aes(color=management, shape=as.factor(year))) + 
    stat_smooth(aes(color=management, linetype=as.factor(year)), method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
    facet_grid(~species)+geom_hline(yintercept = 0)+ylim(-5, 150)+
    scale_color_manual(values=c("black", "grey40"))+
    ylab("Target % Cover")+xlab(""),
  ggplot(subset(veg.canopy, species%in%c("clover", "vetch")), aes(x=canopy, y=plotcover))+ ## CHECK OUT WHAT IS GOING ON WITH INDUSTRY
    geom_point(aes(color=management, shape=as.factor(year))) + 
    stat_smooth(aes(color=management, linetype=as.factor(year)), method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
    facet_grid(~species)+geom_hline(yintercept = 0)+ylim(-5, 150)+
    scale_color_manual(values=c("black", "grey40"))+
    ylab("Target % Cover")+xlab("Canopy Cover"), nrow=2)



#try a version that maxxes out at 100% (or does not add in % that spread - simplify).
# reorder spp by descending average cover.

