#cover
# will include 2020 peak, 2021 winter, 2021 peak, 
# plot total, weeds, targetspp as a fn of canopy cover in y1
# persistence: 2021/2020 (fn of treatment, orchard)

source('canopy.R')

# 2020
cover<-read_csv("community_cover.csv")%>%
  group_by(orchard_age, block, management, seedmix)%>%
  mutate(industry=sum(barley, oats, vetch, clover))%>%
  select(-barley, -oats, -vetch, -clover)%>%
  ungroup()%>%
  gather("species", "cov", 5:24, 26)%>%
  filter(!is.na(cov))%>%
  select(-notes)%>%
  mutate(year=2020)

cover21<-read_csv("community_cover21.csv")%>%
  group_by(orchard_age, block, management, seedmix)%>%
  mutate(industry=sum(barley, oats, vetch, clover))%>%
  select(-barley, -oats, -vetch, -clover)%>%
  ungroup()%>%
  gather("species", "cov", 5:25, 27)%>%
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
ggplot(subset(bare, bare$species=="bare"), aes(x=seedmix, y=100-as.numeric(cov)))+
  geom_boxplot(aes(fill=seedmix))+
  facet_grid(orchard_age~year, scales="free")+ylab("Total Vegetation % Cover")+xlab("")

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
bare.canopy<-left_join(bare, dplyr::select(canopy, -year)) ## FIX THIS WHEN WE GET THE NEW DATA CANOPY

ggplot(subset(bare.canopy, bare$species=="bare"), aes(x=canopy, y=cov))+
  geom_point(aes(color=seedmix, shape=as.factor(orchard_age))) + 
  stat_smooth(aes(color=seedmix), se=T, method = "gam", formula = y ~ s(x), size = 1)+
  ylab("Total May Bare Ground % Cover")+xlab("")+facet_grid(~year)

#weeds
ggplot(subset(bare.canopy, bare$species=="weeds"), aes(x=canopy, y=(cov)))+
  geom_point(aes(color=seedmix, shape=as.factor(orchard_age))) + 
  stat_smooth(aes(color=seedmix), se=T, method = "gam", formula = y ~ s(x), size = 1)+
  ylab("Weeds % Cover")+xlab("")+facet_grid(~year)

#aggregated target spp
mix.canopy<-left_join(mix_cov, dplyr::select(canopy, -year)) ## FIX THIS WHEN WE GET THE NEW DATA CANOPY

ggplot(subset(mix.canopy), aes(x=canopy, y=cov))+
  geom_point(aes(color=seedmix, shape=as.factor(orchard_age))) + 
  stat_smooth(aes(color=seedmix),method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
  ylab("Target % Cover")+xlab("")+facet_grid(~year)

#w/mech
ggplot(subset(mix.canopy), aes(x=canopy, y=cov))+
  geom_point(aes(color=management, shape=as.factor(orchard_age))) + 
  stat_smooth(aes(color=management), method = "lm", se=F, formula = y ~ x, size = 1)+
  ylab("Target % Cover")+xlab("")+facet_grid(seedmix~year)

#individual spp 
# need to group these, now ignoring what zone they're in to get their total cover (expansion)
veg.canopy<-left_join(cover, dplyr::select(canopy, -year)) %>% ## FIX THIS WHEN WE GET THE NEW DATA CANOPY
  filter(species!="weeds"&species!="bare"&seedmix!="megamix")%>%
  group_by(orchard_age, block, management)%>%
  mutate(canopy=mean(canopy))%>%
  group_by(orchard_age, block, management, species, year, canopy)%>%
  summarize(plotcover=sum(cov))%>%
  spread(species, plotcover, fill=0)%>%
  gather("species", "plotcover", 6:22)

ggplot(veg.canopy, aes(x=canopy, y=plotcover))+ ## CHECK OUT WHAT IS GOING ON WITH INDUSTRY
  geom_point(aes(color=management, shape=as.factor(orchard_age))) + 
  stat_smooth(aes(color=management), method = "lm", se=F, formula = y ~ poly(x, 2), size = 1)+
  facet_grid(year~species)+
  ylab("Target % Cover")+xlab("")
  


