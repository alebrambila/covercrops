##Hogs and Hazelnuts##
##Calvin Penkauskas, Winter 2020##

##General Data Frame##

library(tidyverse)
library(gridExtra)
library(egg)
library(ggpubr)
library(multcomp)
theme_set(theme_classic())

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



#infestedbaseline1 <- ACORN_DATA %>%
#  filter(time == "Before") %>%
#  mutate( id = paste(oak, plot, sep = "")) %>%
#  spread(type, count) %>%
#  mutate(total = infested + other) %>%
#  mutate(ratio = infested / total)%>%
#  mutate(alltrt=as.factor(paste(Treatment, year)))
#
#ib2<-subset(infestedbaseline1, Treatment=="Grazed")
#summary(aov(ratio~year, ib2))
#
#ib3<-ib2%>%
#  mutate(year=ifelse(year==2018, "a2018a", "a2019a"))%>%
#  dplyr::select(-alltrt, -total, -infested, -other)%>%
#  spread(year, ratio)%>%
#  group_by(id)%>%
#  mutate(diff=a2019a-a2018a)
#
#ggplot(ib3, aes(y=diff)) +geom_boxplot(aes(x=Treatment))+geom_point(aes(x=Treatment))


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
  labs(y = "Number of filbertworm per trap") +
  scale_fill_manual(values=c('#cb84e3','#9000bf')) +
  theme(legend.position="top", legend.title = element_blank(), plot.caption = element_text(hjust = .5))


#################### - THREE OPTIONS FOR FIGURE 2 - Effectiveness of Grazing ####################

### FIGURE 2.(v1) Proportion of infested acorns before grazing each year.
      # This figure shows how there was initially a higher proportion of infested acorns in the grazed plot than the paired control plot
      # but the following year, the pre-grazed infestation rate was lowered to be indistinguishable from the paired control plot
f2v1 <- ggplot(data = infestedbaseline, aes(x = year)) +
  geom_line(aes(y = meanratio, color = Treatment)) +
  geom_point(aes( y = meanratio, color = Treatment), show.legend = FALSE) +
  geom_errorbar(aes(ymin = (meanratio - se1), ymax = (meanratio + se1),
  color = Treatment), width= 0.5, show.legend = FALSE) +
  labs(caption = "(p= 0.046*)",
       x = "Baseline",
       y = "Proportion of Infested Acorns",
       colour = "Treatment")+
      scale_color_manual(values=c('#cb84e3','#9000bf'))+
      scale_x_continuous(breaks=c(2018, 2019), limits=c(2017.75, 2019.25) ) +
  theme(legend.position="top", legend.title = element_blank(), plot.caption = element_text(hjust = .5))+
  annotate("text", x=2018, y= .65, label="*")+annotate("text", x=2018.5, y= .44, label="*", color='#9000bf') 

### FIGURE 2. (v2) Number of infested acorns before and after grazing each year
      # The only change is now we see the infested acorn total, rather than proportion infested
f2v2 <- ggplot(data = infestedtotal, aes(x = time, group=Treatment)) +
  stat_summary(fun.y=sum, aes(y=meaninfested, color=Treatment), geom="line")+
  geom_point(aes( y = meaninfested, color = Treatment), show.legend = FALSE) +
  geom_errorbar(aes(ymin = (meaninfested - se1), ymax = (meaninfested + se1),
                    color = Treatment), width= 0.5, show.legend = FALSE) +
  labs(caption = "(p= 0.046*)",
       x = "Time relative to grazing",
       y = "Number of infested acorns / m2",
       colour = "Treatment")+
  facet_wrap(~year)+
 scale_color_manual(values=c('#cb84e3','#9000bf'))+
  theme(legend.position="top", legend.title = element_blank(), plot.caption = element_text(hjust = .5))+
  annotate("text", x=1.5, y= 17, label="*", color='#9000bf') 

### FIGURE 2 (v3)
# This third option shows relative percent change in the number of infested nuts in grazed vs. ungrazed plots, 
#  averaged across 2018 and 2019
f2v3 <- ggplot(data = infestedratio, aes(x = Treatment)) +
  geom_boxplot(mapping = aes( y = ratio2, color = Treatment), show.legend = FALSE) +
  labs(
    x = "",
    y = "% Change in infested nuts before and after grazing",
    colour = "Treatment")+
  scale_color_manual(values=c('#cb84e3','#9000bf')) +
  theme(legend.position="top", legend.title = element_blank(), plot.caption = element_text(hjust = .5))+
  annotate("text", x=1.5, y= 200, label="p=1.04x10^-5") 

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
oakabundance <- FBW_DATA %>%
  filter(type == "Abundance") %>%
  filter(Habitat == "Oak") %>%
  group_by(year, Treatment) %>%
  summarise(mean2 = mean(count), se4 = calcSE(count)) %>%
  ungroup() 

### FIGURE 3.(v1) Proportion of infested acorns before grazing each year.
# This figure shows how there was initially a higher proportion of infested acorns in the grazed plot than the paired control plot
# but the following year, the pre-grazed infestation rate was lowered to be indist


f3a <- ggplot(data = oakemergence, aes(x = year), show.legend = FALSE) +
  geom_line(aes(y = mean1,  color = Treatment), show.legend = FALSE) +
  geom_point(aes( y = mean1, color = Treatment), show.legend = FALSE) +
  geom_errorbar(aes(ymin = (mean1 - se3), ymax = (mean1 + se3),
                    color = Treatment), width= 0.5, show.legend = FALSE) +
  labs(caption = "(p= 0.026*)",
       x = "Emergence",
       y = "Number of filbertworm per trap",
       colour = "Treatment") +
  scale_color_manual(values=c('#cb84e3','#9000bf')) +
  scale_x_continuous(breaks=c(2018, 2019), limits=c(2017.75, 2019.25) )
  
f3b <- ggplot(data = oakabundance, aes(x = year)) +
  geom_line(aes(y = mean2,  color = Treatment), show.legend = FALSE) +
  geom_point(aes( y = mean2, color = Treatment), show.legend = FALSE) +
  geom_errorbar(aes(ymin = (mean2 - se4), ymax = (mean2 + se4),
                    color = Treatment, width= 0.5), show.legend = FALSE) +
  labs(y="",
       x = "Abundance",
       colour = "Treatment")+
  scale_color_manual(values=c('#cb84e3','#9000bf')) +
  scale_x_continuous(breaks=c(2018, 2019), limits=c(2017.75, 2019.25) )


#oakemergence1 <- FBW_DATA %>%
#  filter(type == "Emergence") %>%
#  filter(Habitat == "Oak") 

#oakabundance1 <- FBW_DATA %>%
#  filter(type == "Abundance") %>%
#  filter(Habitat == "Oak")
  
#ggplot(data = oakabundance1, aes(x = Treatment)) +
#  geom_boxplot(mapping = aes( y = count, color = Treatment))+
#  facet_wrap(~ year, nrow = 1)

## Figure 3 Panel arrange##
ggarrange(f3a, f3b, common.legend = T)


## Figure 3 Stats: 

oak_abundance.aov <- aov(count ~ Treatment + year + Treatment:year, data = oakabundance1)
summary(oak_abundance.aov)

oakemergence.aov <- aov(count ~ Treatment + year + Treatment:year, data = oakemergence1)
summary(oakemergence.aov)


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
#  scale_x_continuous(breaks=c(2018, 2019), limits=c(2017.75, 2019.25) ) +
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


