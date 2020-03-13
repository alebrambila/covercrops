##Hogs and Hazelnuts##
##Calvin Penkauskas, Winter 2020##

##General Data Frame##

library(tidyverse)

ACORN_DATA <- read_csv("ACORN_DATA.csv")

FBW_DATA <- read_csv("FBW_DATA.csv")

calcSE <- function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))}

##Ratio of Infested Acorns Before and After Woodland Hog Grazing##

infestedbaseline <- ACORN_DATA %>%
  filter(time == "before") %>%
  mutate( id = paste(oak, plot, sep = "")) %>%
  spread(type, count) %>%
  mutate(total = infested + other) %>%
  mutate(ratio = infested / total) %>%
  group_by(year,treatment) %>%
  summarise(meanratio = mean(ratio), se1 = calcSE(ratio))%>%
  ungroup()#%>%
  #mutate(year = as.factor(year))

ggplot(data = infestedbaseline, aes(x = year)) +
  geom_line(aes(y = meanratio, color = treatment)) +
  geom_point(aes( y = meanratio, color = treatment)) +
  geom_errorbar(aes(ymin = (meanratio - se1), ymax = (meanratio + se1),
  color = treatment))+
  labs(title = "Hog Grazing Effect on Infested Acorns",
       subtitle = "(2018-19)",
       caption = "Figure 6. The ratio of infested to healthy acorns in the oak woodland
       in response to hog-grazing grazing (p= 0.046*)",
       x = "Year",
       y = "# Infested Acorns / Total # of Acorns",
       colour = "Treatment")+
  theme_classic()

infestedbaseline1 <- ACORN_DATA %>%
  filter(time == "before") %>%
  mutate( id = paste(oak, plot, sep = "")) %>%
  spread(type, count) %>%
  mutate(total = infested + other) %>%
  mutate(ratio = infested / total)

infested_baseline.aov <- aov(ratio ~ treatment + year + treatment:year, data = infestedbaseline1)

summary(infested_baseline.aov)

##Percent Change in Infested Acorns Between Treatments##

infestedratio <- ACORN_DATA %>%
  filter(type == "infested") %>%
  mutate( id = paste(oak, plot, sep = "")) %>%
  spread(time, count) %>%
  mutate(ratio2 = ((100* (after/ before))-100)) %>%
  group_by(treatment) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  #summarise(meanratio2 = mean(ratio2), se2 = calcSE(ratio2))%>%
  ungroup() %>%
  mutate(ratio2 = ifelse(is.finite(ratio2), ratio2, 100))

ggplot(data = infestedratio, aes(x = treatment)) +
  geom_boxplot(mapping = aes( y = ratio2, color = treatment)) #+
  #geom_point(aes( y = meanratio2, color = treatment)) +
  #geom_errorbar(aes(ymin = (meanratio2 - se2), ymax = (meanratio2 + se2),
  #color = treatment))

ggplot(data = infestedratio, aes(x = treatment)) +
  geom_boxplot(mapping = aes( y = ratio2, color = treatment))+
  facet_wrap(~ year, nrow = 1)

infested_ratio.aov <- aov(ratio2 ~ treatment + year + treatment:year, data = infestedratio)

summary(infested_ratio.aov)

##FBW Emergence in Oak Woodland Before and After Treatment##

oakemergence <- FBW_DATA %>%
  filter(type == "emergence") %>%
  filter(habitat == "oak") %>%
  group_by(year, treatment) %>%
  summarise(mean1 = mean(count), se3 = calcSE(count)) %>%
  ungroup() #%>%
  #mutate(year = as.factor(year))
  

ggplot(data = oakemergence, aes(x = year)) +
  geom_line(aes(y = mean1,  color = treatment)) +
  #geom_boxplot(aes(y = mean1, color = treatment)) #+
  geom_point(aes( y = mean1, color = treatment)) +
  geom_errorbar(aes(ymin = (mean1 - se3), ymax = (mean1 + se3),
                    color = treatment))

oakemergence1 <- FBW_DATA %>%
  filter(type == "emergence") %>%
  filter(habitat == "oak") #%>%
  #group_by(year, treatment) %>%
  #summarise(mean1 = mean(count), se3 = calcSE(count)) %>%
  #ungroup() #%>%
#mutate(year = as.factor(year))

oakemergence.aov <- aov(count ~ treatment + year + treatment:year, data = oakemergence1)

summary(oakemergence.aov)

    
##FBW Abundance in Oak Woodland Before and After Treatment##
  
oakabundance <- FBW_DATA %>%
    filter(type == "abundance") %>%
    filter(habitat == "oak") %>%
    group_by(year, treatment) %>%
    summarise(mean2 = mean(count), se4 = calcSE(count)) %>%
    ungroup() #%>%
    #mutate(year = as.factor(year))
  
ggplot(data = oakabundance, aes(x = year)) +
  geom_line(aes(y = mean2,  color = treatment)) +
  #geom_boxplot(aes(y= mean2, color = treatment)) +
  geom_point(aes( y = mean2, color = treatment)) +
  geom_errorbar(aes(ymin = (mean2 - se4), ymax = (mean2 + se4),
                    color = treatment))

oakabundance1 <- FBW_DATA %>%
  filter(type == "abundance") %>%
  filter(habitat == "oak")
  
ggplot(data = oakabundance1, aes(x = treatment)) +
  geom_boxplot(mapping = aes( y = count, color = treatment))+
  facet_wrap(~ year, nrow = 1)

oak_abundance.aov <- aov(count ~ treatment + year + treatment:year, data = oakabundance1)

summary(oak_abundance.aov)

##FBW Abundance in Hazelnuts Before and After Treatment##
  
hazelnutabundance <- FBW_DATA %>%
  filter(type == "abundance") %>%
  filter(habitat == "hazelnut") %>%
  group_by(year, treatment) %>%
  summarise(mean3 = mean(count), se5 = calcSE(count)) %>%
  ungroup() #%>%
  #mutate(year = as.factor(year))

ggplot(hazelnutabundance, aes(x = year, y = mean3)) + 
  geom_boxplot() +
  #geom_line(aes(y = mean3,  color = treatment)) +
  #geom_point(aes( y = mean3, color = treatment)) +
  #geom_errorbar(aes(ymin = (mean3 - se5), ymax = (mean3 + se5),
  #color = treatment)) #+
  facet_wrap(~ treatment, nrow = 1)

hazelnutabundance1 <- FBW_DATA %>%
  filter(type == "abundance") %>%
  filter(habitat == "hazelnut")

ggplot(data = hazelnutabundance1, aes(x = treatment)) +
  geom_boxplot(mapping = aes( y = count, color = treatment))+
  facet_wrap(~ year, nrow = 1)

hazelnut_abundance.aov <- aov(count ~ treatment + year + treatment:year, data = hazelnutabundance1)

summary(hazelnut_abundance.aov)

##ANOVA Ratio of Infested/Other Acorns##

ACORN_ANALYSIS1 <- read_csv("ACORN_DATA.csv") %>%
  #filter(time == "before") %>%
  mutate( id = paste(oak, plot, sep = "")) %>%
  spread(type, count) %>%
  mutate(total = infested + other) %>%
  mutate(ratio = infested / total) %>%
  mutate_all(~replace(., is.na(.), 0)) 
  
ggplot(data = ACORN_ANALYSIS1, aes(x = treatment)) +
  geom_boxplot(mapping = aes( y = ratio, color = treatment))+
  facet_wrap(~ year, nrow = 1)

infested_ratio2.aov <- aov(ratio ~ treatment + year + treatment:year, data = ACORN_ANALYSIS1)

#summarySE(infested_ratio.aov, measurevar="len", groupvars=c("supp","dose"))

summary(infested_ratio2.aov)

ggplot(infested_ratio.aov, aes(x=treatment, y=ratio, colour=supp, group=supp)) + 
  geom_errorbar(aes(ymin=len-ci, ymax=len+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3)

plot(infested_ratio.aov)

ggplot(infested_ratio.aov, aes(x = am, y = hp)) +
  geom_point() +
  geom_bar(data = gd, stat = "identity", alpha = .3)

ggplot(data = infested_ratio.aov, aes(x = treatment)) +
  geom_bar()
  #geom_line(aes(y = meanratio, color = treatment)) #+
  #geom_point(aes( y = meanratio, color = treatment)) +
  #geom_errorbar(aes(ymin = (meanratio - se1), ymax = (meanratio + se1),
   #                 color = treatment))

ACORN_ANALYSIS2 <- read_csv("ACORN_DATA.csv") %>%
  #filter(time == "before") %>%
  mutate( id = paste(oak, plot, sep = "")) %>%
  spread(type, count)

acorn1.aov <- aov(other ~ treatment+time, data = ACORN_ANALYSIS2)

summary(acorn1.aov)
