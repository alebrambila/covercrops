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

# Set a theme
# theme_set(theme_classic())
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

#################### - FIGURE 1 - Hazelnut and Oak Populations ####################

## Averaged across years to show that they are present in oaks and hazelnuts generally, 
## but only emerging in oaks.  This sets up the problem, oak emergence infecting hazelnuts

## currently averaged across all years and tratments to show what ever happened.

baselines<-FBW_DATA
baselines$type <- factor(baselines$type, levels = c("Emergence", "Abundance"))

### Figure 1 ###
f1<-ggplot(baselines, aes(y=count, x=Habitat)) +
  geom_boxplot(aes(fill=Habitat))+
  facet_wrap(~type)+
  labs(y = "Average # of FBW") +
  scale_fill_manual(values=c('#03b6fc','#9a00bd')) +
  theme(legend.position="top", legend.title = element_blank(), plot.caption = element_text(hjust = .5))

plot(f1)

## fig 1 reformatted
f1v2 <- ggplot(baselines, aes(y=count, x=Habitat)) +
  geom_boxplot(fill = "lightgrey")+
  facet_wrap(~type, scale = "free")+
  labs(y = "Number of filbertworms per trap") + panel_border() 


pdf("fig1_baseline-distribution.pdf", width = 8, height = 5)

f1v2

# add panel labels
grid.text(c("(a)", "(b)"),x = c(.12,.58),
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

ggplot(acorn_total, aes(x= Treatment, y = total)) +
  geom_boxplot(aes(fill=Treatment)) +
  facet_wrap(~year)+
  scale_fill_manual(values=c('darkorange','#9a00bd'))


#################### - THREE OPTIONS FOR FIGURE 2 - Effectiveness of Grazing ####################

### FIGURE 2.(v1) Proportion of infested acorns before grazing each year.
# This figure shows how there was initially a higher proportion of infested acorns in the grazed plot than the paired control plot
# but the following year, the pre-grazed infestation rate was lowered to be indistinguishable from the paired control plot
f2v1 <- ggplot(data = infestedbaseline, aes(x = year), show.legend = FALSE) +
  geom_line(aes(y = meanratio, color = Treatment), show.legend = FALSE) +
  geom_point(aes( y = meanratio, color = Treatment), show.legend = FALSE) +
  geom_errorbar(aes(ymin = (meanratio - se1), ymax = (meanratio + se1),
                    color = Treatment), width= 0.5, show.legend = FALSE) +
  labs( x = "",
        y = "Proportion of Infested Acorns",
        colour = "Treatment")+
  scale_color_manual(values=c('darkorange','#9a00bd'))+
  scale_x_continuous(breaks=c(2018, 2019, 2020), limits=c(2017.75, 2020.25) ) +
  theme(legend.position="right", legend.title = element_blank(), plot.caption = element_text(hjust = .5))+
  annotate("text", x=2018.66, y= .5, label="* P=0.065", color='#9000bf') 

plot(f2v1)

### FIGURE 2. (v2) Number of infested acorns before and after grazing each year
# The only change is now we see the infested acorn total, rather than proportion infested
f2v2 <- ggplot(data = infestedtotal, aes(x = time, group=Treatment)) +
  stat_summary(fun.y=sum, aes(y=meaninfested, color=Treatment), geom="line")+
  geom_point(aes( y = meaninfested, color = Treatment), show.legend = FALSE) +
  geom_errorbar(aes(ymin = (meaninfested - se1), ymax = (meaninfested + se1),
                    color = Treatment), width= 0.5, show.legend = FALSE) +
  labs(caption = "(p= 0.046*)",
       x = "Time relative to grazing",
       y = "# of infested acorns / m^2",
       colour = "Treatment")+
  facet_wrap(~year)+
  scale_color_manual(values=c('darkorange','#9a00bd'))+
  theme(legend.position="top", legend.title = element_blank(), plot.caption = element_text(hjust = .5))

plot(f2v2)

### FIGURE 2 (v3)
# This third option shows relative percent change in the number of infested nuts in grazed vs. ungrazed plots, 
#  averaged across 2018 and 2019
#f2v3 <- ggplot(data = infestedratio, aes(x = Treatment)) +
#  geom_boxplot(mapping = aes( y = ratio2, color = Treatment), show.legend = FALSE) +
#  labs(
#    x = "",
#    y = "% Change in infested nuts before and after grazing",
#    colour = "Treatment")+
#  scale_color_manual(values=c('#cb84e3','#9000bf')) +
#  theme(legend.position="top", legend.title = element_blank(), plot.caption = element_text(hjust = .5))+
#  annotate("text", x=1.5, y= 200, label="p=1.04x10^-5") 

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


fig3v2 <- ggplot(emergeabund, aes(x=year, y = mean1, color = Treatment)) + geom_line(lwd = .6) + geom_point(size = 2)+
  geom_errorbar(aes(ymin = (mean1 - se3), ymax = (mean1 + se3)), width= 0.2) +
  labs( x = "Year",
    y = "Number of filbertworms per trap") +
  facet_wrap(~type, scales = "free") + theme(legend.position = "none") +
  scale_color_manual(values=c('grey80','grey30')) +
  scale_x_continuous(breaks=c(2018, 2019, 2020), limits=c(2017.75, 2020.25)) + panel_border() 

pdf("fig3_treatment-effects.pdf", width = 8, height = 5)

fig3v2

# add panel labels
grid.text(c("(a)", "(b)"),x = c(.13,.6),
          y = c(.88,.88),
          gp=gpar(fontsize=18)) 
dev.off()


# OLD FIGURE CODE
# f3a <- ggplot(data = oakemergence, aes(x = year), show.legend = FALSE) +
#   geom_line(aes(y = mean1,  color = Treatment), show.legend = FALSE) +
#   geom_point(aes( y = mean1, color = Treatment), show.legend = FALSE) +
#   geom_errorbar(aes(ymin = (mean1 - se3), ymax = (mean1 + se3),
#                     color = Treatment), width= 0.2, show.legend = FALSE) +
#   labs( x = "Emergence",
#     y = "Number of filbertworms per trap",
#     colour = "Treatment") +
#   scale_color_manual(values=c('grey80','grey30')) +
#   scale_x_continuous(breaks=c(2018, 2019, 2020), limits=c(2017.75, 2020.25) ) #+
# #  annotate("text", x=2018.3, y= 1.2, label="** P=0.004", color='darkorange') +
# #  annotate("text", x=2018.7, y= .7, label=" ** P<0.001") +
# #  annotate("text", x=2019.7, y= 1.2, label=" ** P<0.001", color='darkorange')
# 
# f3b <- ggplot(data = oakabundance, aes(x = year)) +
#   geom_line(aes(y = mean2,  color = Treatment), show.legend = FALSE) +
#   geom_point(aes( y = mean2, color = Treatment), show.legend = FALSE) +
#   geom_errorbar(aes(ymin = (mean2 - se4), ymax = (mean2 + se4),
#                     color = Treatment, width= 0.2), show.legend = FALSE) +
#   labs(y="",
#        x = "Abundance",
#        colour = "Treatment")+
#   scale_color_manual(values=c('grey80','grey30')) +
#   scale_x_continuous(breaks=c(2018, 2019, 2020), limits=c(2017.75, 2020.25) )
# 
# ## Figure 3 Panel arrange##
# ggarrange(f3a, f3b, common.legend = T)
# 
# oakemergence1 <- FBW_DATA %>%
#   filter(type == "Emergence") %>%
#   filter(Habitat == "Oak") 
# 
# oakabundance1 <- FBW_DATA %>%
#   filter(type == "Abundance") %>%
#   filter(Habitat == "Oak")
# 
# ggplot(data = oakemergence1, aes(x = Treatment)) +
#   geom_boxplot(mapping = aes( y = count, color = Treatment))+
#   facet_wrap(~ year, nrow = 1)




## Figure 3 Stats: 

oak_abundance.aov <- aov(count ~ Treatment + year + Treatment:year, data = oakabundance1)
summary(oak_abundance.aov)

oakabundance1<-oakabundance1%>%
  mutate(alltrt=paste(year, Treatment))%>%
  mutate(alltrt=as.factor(alltrt))

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


## Potentially Appendix material: 
##FBW Abundance in Hazelnuts Before and After Treatment##

#hazelnutabundance <- FBW_DATA %>%
#  filter(type == "Abundance") %>%
#  filter(Habitat == "Hazelnut") %>%
#  group_by(year, Treatment) %>%
#  summarise(mean3 = mean(count), se5 = calcSE(count)) %>%
#  ungroup()

#hazel_abund_Line <- ggplot(hazelnutabundance, aes(x = year, y = mean3)) + 
#  geom_line(aes(y = mean3,  color = Treatment)) +
#  geom_point(aes( y = mean3, color = Treatment), show.legend = FALSE) +
#  geom_errorbar(aes(ymin = (mean3 - se5), ymax = (mean3 + se5),
#  color = Treatment, width= 0.5), show.legend = FALSE) +
#  labs(caption = "(p=0.21)",
#       x = "Abundance",
#       y = "Average # of FBW",
#       colour = "Treatment")+
#  scale_color_manual(values=c('#94d0ff','#0076d1')) +
#  scale_x_continuous(breaks=c(2018, 2019, 2020), limits=c(2017.75, 2020.25) ) +
#  theme(legend.position="top", legend.title = element_blank(), plot.caption = element_text(hjust = .5))

#plot (hazel_abund_Line)

#hazelnutabundance1 <- FBW_DATA %>%
#  filter(type == "Abundance") %>%
#  filter(Habitat == "Hazelnut")

#ggplot(data = hazelnutabundance1, aes(x = Treatment)) +
#  geom_boxplot(mapping = aes( y = count, color = Treatment))+
#  facet_wrap(~ year, nrow = 1)

#hazelnut_abundance.aov <- aov(count ~ Treatment + year + Treatment:year, data = hazelnutabundance1)

#summary(hazelnut_abundance.aov)


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



#ggplot(subset(veg, vegtype!="distrubed"&vegtype!="moss"&vegtype!="tree"&vegtype!="wood"), aes(timing, cover)) +
#  geom_boxplot(aes(fill=pig))+
#  stat_summary(aes(timing, cover, group=pig, color=pig), fun.y=median, geom="line")+
#  facet_wrap(~vegtype, scales="free")

ggplot(subset(veg1, vegtype!="distrubed"&vegtype!="moss"&vegtype!="tree"&vegtype!="wood"&timing!="A"&vegtype!="native vine"), aes(timing, meancover)) +
  #geom_boxplot(aes(fill=pig))+
  stat_summary(aes(timing, meancover, group=pig, color=pig), fun.y=median, geom="line", show.legend = FALSE)+
  geom_errorbar(aes(ymin = (meancover-secover), ymax = (meancover+secover),
                    color = pig), width= 0.05, show.legend = FALSE)+facet_wrap(~vegtype)+xlab("Year")+ylab("Percent Cover")+
  scale_x_continuous(breaks=c(2018, 2020), limits=c(2017.75, 2020.25) )+
  scale_color_manual(values=c('darkorange','#9a00bd'))

vegaov<-aov(cover~timing*pig, subset(veg, vegtype!="distrubed"&vegtype!="moss"&vegtype!="tree"&vegtype!="wood"&timing!="A"&vegtype!="native vine"))
summary(glht(vegaov, linfct=mcp(alltrt="Tukey")))

library(nlme)
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


#ggplot(subset(veg0, vegtype!="distrubed"&vegtype!="moss"&vegtype!="tree"&vegtype!="wood"), aes(timing, diff)) +
#  #geom_boxplot(aes(fill=pig))+
#  stat_summary(aes(timing, diff, group=vegtype, color=vegtype), fun.y=median, geom="line")+
#  geom_errorbar(aes(ymin = (diff-secover), ymax = (diff+secover),
#                        color = vegtype), width= 0.05, show.legend = FALSE)+ylab("grazing impact on % cover")+
#  geom_hline(yintercept=0)


