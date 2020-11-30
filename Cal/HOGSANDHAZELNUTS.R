##Hogs and Hazelnuts##
##Calvin Penkauskas, Winter 2020##


##General Data Frame##

library(tidyverse)
library(gridExtra)
library(egg)
library(ggpubr)
library(multcomp)
library(cowplot)
library(grid)
library(nlme)

# Set a theme
theme_set(theme_cowplot() + theme(strip.background = element_blank(), 
          text = element_text(size = 18), 
          axis.text = element_text(size = 16),
          strip.text = element_text(size = 18)))

ACORN_DATA <- read_csv("ACORN_DATA.csv")

FBW_DATA <- read_csv("FBW_DATA.csv")

calcSE <- function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))}

##Infested acorn change due to grazing##

infestedbaseline <- ACORN_DATA %>%
  filter(time == "Before") %>%
  mutate( id = paste(oak, plot, sep = "")) %>%
  spread(type, count) %>%
  mutate(total = infested + other) %>%
  mutate(ratio = infested / total) %>%
  group_by(year,Treatment) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(ratio = ifelse(is.finite(ratio), ratio, 100)) %>%
  summarise(meanratio = mean(ratio), se1 = calcSE(ratio))%>%
  ungroup()

infestedtotal <- ACORN_DATA %>%
  mutate( id = paste(oak, plot, sep = "")) %>%
  spread(type, count) %>%
  dplyr::select(-other) %>%
  group_by(year,Treatment, time) %>%
  mutate(infested=infested*4)%>%
  summarise(meaninfested = mean(infested), se1 = calcSE(infested))%>%
  ungroup()
infestedtotal$time <- factor(infestedtotal$time, levels = c("Before", "After"))

infestedratio <- ACORN_DATA %>%
  filter(type == "infested") %>%
  mutate( id = paste(oak, plot, sep = "")) %>%
  spread(time, count) %>%
  mutate(ratio2 = ((100* (After/ Before))-100)) %>%
  group_by(Treatment) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  ungroup() %>%
  mutate(ratio2 = ifelse(is.finite(ratio2), ratio2, 100))

Change <- infestedratio %>%
  filter(Treatment == "Grazed")

summary(Change)

infestedbaseline1 <- ACORN_DATA %>%
  filter(time == "Before") %>%
  mutate( id = paste(oak, plot, sep = "")) %>%
  spread(type, count) %>%
  mutate(total = infested + other) %>%
  mutate(ratio = infested / total)%>%
  mutate(alltrt=as.factor(paste(Treatment, year)))

infestedbaseline2<-infestedbaseline1%>%
  group_by(year,Treatment) %>%
  filter(!is.na(ratio))%>%
  summarise(meanratio = mean(ratio), se1 = calcSE(ratio))%>%
  ungroup()

#################### - FIGURE 1 - Hazelnut and Oak Populations ####################

## Averaged across years to show that they are present in oaks and hazelnuts generally, 
## but only emerging in oaks.  This sets up the problem, oak emergence infecting hazelnuts

## currently averaged across all years and tratments to show what ever happened.

baselines<-FBW_DATA %>%
  filter(!year==2020)
baselines$type <- factor(baselines$type, levels = c("Emergence", "Abundance"))


oak_baselines <- baselines %>%
  filter(Habitat == "Oak")

hazel_baselines <- baselines %>%
  filter(Habitat == "Hazelnut", type == "Abundance")

### Figure 1 Reformated###

f1v2 <- ggplot(baselines, aes(y=count, x=Habitat)) +
  geom_boxplot(size=.75, outlier.size = 2)+
  facet_wrap(~type, scale = "free")+
  labs(y = "Number of filbertworms per trap") + panel_border() +
  theme(legend.position="none")

pdf("fig1_baseline-distribution.pdf", width = 8, height = 5)

f1v2

# add panel labels
grid.text(c("(a)", "(b)"),x = c(.10,.58),
          y = c(.88,.88),
          gp=gpar(fontsize=18)) 
dev.off()

##Acorn totals

acorn_total <- ACORN_DATA%>%
  filter(time == "Before") %>%
  mutate( id = paste(oak, plot, sep = "")) %>%
  spread(type, count) %>%
  mutate(total = infested + other) %>%
  mutate(alltrt=as.factor(paste(Treatment, year)))

acorn_summed <- acorn_total %>%
  group_by(year, Treatment) %>%
  summarize(meanacorn = mean(total), seacorn = calcSE(total))
  
ggplot(acorn_summed, aes(x = year, y = meanacorn, linetype=Treatment)) + geom_point(size=2) + geom_line(size=.75) +
  geom_errorbar(aes(ymin = meanacorn - seacorn, ymax = meanacorn + seacorn), size=.75, width = .5) +
  labs(x = "Year", y = expression(paste("Acorns per m"^2))) + theme(legend.position = "none") +
  scale_x_continuous(breaks=c(2018, 2019, 2020), limits=c(2017.75, 2020.25) ) +   panel_border() 

ggsave("SupFig-acornsovertime.pdf", width = 5, height = 4)


#################### - THREE OPTIONS FOR FIGURE 2 - Effectiveness of Grazing ####################

### FIGURE 2.(v1) Proportion of infested acorns before grazing each year.
# This figure shows how there was initially a higher proportion of infested acorns in the grazed plot than the paired control plot
# but the following year, the pre-grazed infestation rate was lowered to be indistinguishable from the paired control plot
f2a <- ggplot(data = infestedbaseline2, aes(x = year, y = meanratio, linetype = Treatment)) +
  geom_line(size=.75) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (meanratio - se1), ymax = (meanratio + se1)), width= 0.5, size=.75) +
  labs( x = "", y = "Proportion of Infested Acorns")+
  scale_x_continuous(breaks=c(2018, 2019, 2020), limits=c(2017.75, 2020.25)) +
  theme(legend.position = "none")

plot(f2a)

### FIGURE 2. (v2) Number of infested acorns before and after grazing each year
# The only change is now we see the infested acorn total, rather than proportion infested
f2b <- ggplot(data = infestedtotal, aes(x = time, y = meaninfested, linetype=Treatment, group=Treatment)) +
  stat_summary(fun.y=sum, geom="line", lwd=.75)+
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (meaninfested - se1), ymax = (meaninfested + se1)), width= 0.5, 
  size=.75, show.legend = FALSE) +
  labs(
       x = "Time relative to grazing",
       y = "# of infested acorns / m^2")+
  facet_wrap(~year)+
  theme(legend.position="none")

plot(f2b)

f2v3 <- ggplot(data = subset(infestedtotal, year != 2020), aes(x = time, y = meaninfested, linetype=Treatment, group=Treatment)) +
  geom_line(size = .75) + 
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (meaninfested - se1), ymax = (meaninfested + se1)), width= 0.5, show.legend = FALSE) +
  labs(
    x = "Time relative to grazing",
    y = expression(paste("Number of infested acorns per m"^2))) +
  facet_wrap(~year)+
  theme(legend.position="none", legend.title = element_blank(), plot.caption = element_text(hjust = .5)) +
  panel_border() 

pdf("fig2_grazingeffectonacorns.pdf", width = 8, height = 5)

f2v3

# add panel labels
grid.text(c("(a)", "(b)"),x = c(.13,.58),
          y = c(.88,.88),
          gp=gpar(fontsize=18)) 
dev.off()

ggarrange(f2b, f2a, nrow=2, ncol=1, common.legend = T, legend=F)

############
##F2 Stats##
############

infested_ratio.aov <- aov(ratio2 ~ Treatment + year + Treatment:year, data = infestedratio)
summary(infested_ratio.aov)

infested_baseline.aov <- aov(ratio ~ Treatment + year + Treatment:year, data = infestedbaseline1)
abaov<-aov(ratio~alltrt, infestedbaseline1)
summary(glht(abaov, linfct=mcp(alltrt="Tukey")))

summary(infested_baseline.aov)

summary(abaov)

#################### - FIGURE 3 - Grazing effects on Emergence and Abundance (in Oaks)  ####################

oakemergence <- FBW_DATA %>%
  filter(type == "Emergence") %>%
  filter(Habitat == "Oak") %>%
  group_by(year, Treatment) %>%
  summarise(mean1 = mean(count), se3 = calcSE(count)) %>%
  ungroup() 
oakabundance <- filter(FBW_DATA, !is.na(count)) %>%
  filter(type == "Abundance") %>%
  filter(Habitat == "Oak") %>%
  group_by(year, Treatment) %>%
  summarise(mean2 = mean(count), se4 = calcSE(count)) %>%
  ungroup() 

### FIGURE 3.(v1) Proportion of infested acorns before grazing each year.
# This figure shows how there was initially a higher proportion of infested acorns in the grazed plot than the paired control plot
# but the following year, the pre-grazed infestation rate was lowered to be indist

emergeabund <- FBW_DATA %>%
  filter(Habitat == "Oak") %>%
  group_by(year, Treatment, type) %>%
  summarise(mean1 = mean(count), se3 = calcSE(count)) %>%
  ungroup() %>%
  mutate(type = factor(type, levels = c("Emergence", "Abundance")))


fig3v2 <- ggplot(emergeabund, aes(x=year, y = mean1, linetype = Treatment)) + geom_line(size = .75) + geom_point(size=2)+
  geom_errorbar(aes(ymin = (mean1 - se3), ymax = (mean1 + se3)), size=.75, width= 0.5) +
  labs( x = "Year",
    y = "Number of filbertworms per trap") +
  facet_wrap(~type, scales = "free") + theme(legend.position = "none") +
  scale_x_continuous(breaks=c(2018, 2019, 2020), limits=c(2017.75, 2020.25)) + panel_border() 

pdf("fig3_treatment-effects.pdf", width = 8, height = 5)

fig3v2

# add panel labels
grid.text(c("(a)", "(b)"),x = c(.12,.59),
          y = c(.88,.88),
          gp=gpar(fontsize=18)) 
dev.off()

## Figure 3 Stats: 

oakabundance1<-oakabundance%>%
  mutate(alltrt=paste(year, Treatment))%>%
  mutate(alltrt=as.factor(alltrt))

oak_abundance.aov <- aov(count ~ Treatment + year + Treatment:year, data = oakabundance1)
summary(oak_abundance.aov)

oakabaov<-aov(count~alltrt, oakabundance1)
summary(glht(oakabaov, linfct=mcp(alltrt="Tukey")))

ggplot(oakabundance1, aes(x=alltrt, y=count)) + geom_boxplot()


oakemergence.aov <- aov(count ~ Treatment + year + Treatment:year, data = oakemergence1)
summary(oakemergence.aov)

oakemergence1<-oakemergence1%>%
  mutate(alltrt=paste(year, Treatment))%>%
  mutate(alltrt=as.factor(alltrt))

oakemaov<-aov(count~alltrt, oakemergence1)
summary(glht(oakemaov, linfct=mcp(alltrt="Tukey")))

ggplot(oakabundance1, aes(x=alltrt, y=count)) + geom_boxplot()

### VEGVEG ###

veg<-read_csv("vegcov.csv")%>%
  dplyr::select(1, 3, 4,5, 6, 7 )%>%
  group_by(timing, pig, transect, meter, vegtype)%>%
  summarize(cover=sum(coverest))
veg$timing <- factor(veg$timing, levels=c("B", "A", "2020"))

veg0<-veg%>%
  group_by(pig, timing, vegtype)%>%
  summarize(meancover=mean(cover), secover=calcSE(cover))%>%
  group_by(timing, vegtype)%>%
  mutate(secover=mean(secover))%>%
  spread(pig, meancover, fill=0)%>%
  mutate(diff=P-NP)

veg1<-veg%>%
  group_by(timing, vegtype, pig)%>%
  summarize(meancover=mean(cover), secover=calcSE(cover))%>%
  ungroup()%>%
  mutate(vegtype=ifelse(vegtype=="ground substrate", "bare ground", ifelse(vegtype=="invasive vine", "introduced shrub", vegtype)))%>%
  filter(timing!="A")%>%
  mutate(timing=as.integer(ifelse(timing=="B", 2018, 2020)))


fig4 <- ggplot(subset(veg1, vegtype!="distrubed"&vegtype!="moss"&vegtype!="tree"&vegtype!="wood"&timing!="A"&vegtype!="native vine"), aes(timing, meancover, linetype = pig)) +
  stat_summary(aes(timing, meancover, group=pig, linetype=pig), 
               fun.y=median, geom="line", show.legend = FALSE, size=.75) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (meancover-secover), ymax = (meancover+secover),
                    linetype = pig), size=.75, width= 0.5, show.legend = FALSE)+facet_wrap(~vegtype)+xlab("Year")+ylab("Percent Cover")+
  scale_x_continuous(breaks=c(2018, 2020), limits=c(2017.75, 2020.25) )+
  theme(legend.position = "none") + panel_border() 

pdf("fig4_vegetation.pdf", width = 8, height = 8)

fig4

# add panel labels
grid.text(c("(a)", "(b)", "(c)", "(d)"),x = c(.13,.58, .13,.58),
          y = c(.92,.92, .46, .46),
          gp=gpar(fontsize=18)) 
dev.off()

vegaov<-aov(cover~timing*pig, subset(veg, vegtype!="distrubed"&vegtype!="moss"&vegtype!="tree"&vegtype!="wood"&timing!="A"&vegtype!="native vine"))
summary(glht(vegaov, linfct=mcp(alltrt="Tukey")))

veglme<-lme(cover~timing*pig, random=~1|meter/transect, subset(veg, vegtype!="distrubed"&vegtype!="moss"&vegtype!="tree"&vegtype!="wood"&timing!="A"&vegtype!="native vine"))
summary(veglme)

vegintshrub<-lme(cover~timing*pig, random=~1|meter/transect, subset(veg, vegtype=="invasive vine"&timing!="A"))
summary(vegintshrub)
## overall difference, but no interaction effect

vegbg<-lme(cover~timing*pig, random=~1|meter/transect, subset(veg, vegtype=="ground substrate"&timing!="A"))
summary(vegbg)
## same

vegherb<-lme(cover~timing*pig, random=~1|meter/transect, subset(veg, vegtype=="herbaceous/litter"&timing!="A"))
summary(vegherb)
##only timing

vegns<-lme(cover~timing*pig, random=~1|meter/transect, subset(veg, vegtype=="native shrub"&timing!="A"))
summary(vegns)
## neither, only timing