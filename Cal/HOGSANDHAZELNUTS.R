##Hogs and Hazelnuts##
##Calvin Penkauskas, Winter 2020##

##General Data Frame##

library(tidyverse)
library(gridExtra)
library(egg)
theme_set(theme_classic())

ACORN_DATA <- read_csv("ACORN_DATA.csv")

FBW_DATA <- read_csv("FBW_DATA.csv")


calcSE <- function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))}

##Ratio of Infested Acorns Before and After Woodland Hog Grazing##

infestedbaseline <- ACORN_DATA %>%
  filter(time == "Before") %>%
  mutate( id = paste(oak, plot, sep = "")) %>%
  spread(type, count) %>%
  mutate(total = infested + other) %>%
  mutate(ratio = infested / total) %>%
  group_by(year,Treatment) %>%
  summarise(meanratio = mean(ratio), se1 = calcSE(ratio))%>%
  ungroup()

Proportion_Line <- ggplot(data = infestedbaseline, aes(x = year)) +
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
  theme(legend.position="top", legend.title = element_blank(), plot.caption = element_text(hjust = .5))

infestedbaseline1 <- ACORN_DATA %>%
  filter(time == "Before") %>%
  mutate( id = paste(oak, plot, sep = "")) %>%
  spread(type, count) %>%
  mutate(total = infested + other) %>%
  mutate(ratio = infested / total)

infested_baseline.aov <- aov(ratio ~ Treatment + year + Treatment:year, data = infestedbaseline1)

summary(infested_baseline.aov)

##Percent Change in Infested Acorns Between Treatments##

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

Change_Box <- ggplot(data = infestedratio, aes(x = Treatment)) +
  geom_boxplot(mapping = aes( y = ratio2, color = Treatment), show.legend = FALSE) +
  labs(caption = "(p= 1.04x10-5***)",
       x = "Treatment",
       y = "% Change",
       colour = "Treatment")+
      scale_color_manual(values=c('#cb84e3','#9000bf')) +
      theme(legend.position="top", legend.title = element_blank(), plot.caption = element_text(hjust = .5))

ggplot(data = infestedratio, aes(x = Treatment)) +
  geom_boxplot(mapping = aes( y = ratio2, color = Treatment))+
  facet_wrap(~ year, nrow = 1)

infested_ratio.aov <- aov(ratio2 ~ Treatment + year + Treatment:year, data = infestedratio)

summary(infested_ratio.aov)

##FBW Emergence in Oak Woodland Before and After Treatment##

oakemergence <- FBW_DATA %>%
  filter(type == "Emergence") %>%
  filter(Habitat == "Oak") %>%
  group_by(year, Treatment) %>%
  summarise(mean1 = mean(count), se3 = calcSE(count)) %>%
  ungroup() 

Oak_Emerge_Line <- ggplot(data = oakemergence, aes(x = year), show.legend = FALSE) +
  geom_line(aes(y = mean1,  color = Treatment), show.legend = FALSE) +
  geom_point(aes( y = mean1, color = Treatment), show.legend = FALSE) +
  geom_errorbar(aes(ymin = (mean1 - se3), ymax = (mean1 + se3),
                    color = Treatment), width= 0.5, show.legend = FALSE) +
  labs(caption = "(p= 0.026*)",
       x = "Emergence",
       y = "Average # of FBW",
       colour = "Treatment") +
  scale_color_manual(values=c('#cb84e3','#9000bf')) +
  scale_x_continuous(breaks=c(2018, 2019), limits=c(2017.75, 2019.25) ) +
  theme(legend.position="top", legend.title = element_blank(), plot.caption = element_text(hjust = .5))

oakemergence1 <- FBW_DATA %>%
  filter(type == "Emergence") %>%
  filter(Habitat == "Oak") 

oakemergence.aov <- aov(count ~ Treatment + year + Treatment:year, data = oakemergence1)

summary(oakemergence.aov)

    
##FBW Abundance in Oak Woodland Before and After Treatment##
  
oakabundance <- FBW_DATA %>%
    filter(type == "Abundance") %>%
    filter(Habitat == "Oak") %>%
    group_by(year, Treatment) %>%
    summarise(mean2 = mean(count), se4 = calcSE(count)) %>%
    ungroup() 
  
Oak_abund_Line <- ggplot(data = oakabundance, aes(x = year)) +
  geom_line(aes(y = mean2,  color = Treatment), show.legend = FALSE) +
  geom_point(aes( y = mean2, color = Treatment), show.legend = FALSE) +
  geom_errorbar(aes(ymin = (mean2 - se4), ymax = (mean2 + se4),
                    color = Treatment, width= 0.5), show.legend = FALSE) +
  labs(caption = "(p=0.54)",
       x = "Abundance",
       y = "Average # of FBW",
       colour = "Treatment")+
  scale_color_manual(values=c('#cb84e3','#9000bf')) +
  scale_x_continuous(breaks=c(2018, 2019), limits=c(2017.75, 2019.25) ) +
  theme(legend.position="top", legend.title = element_blank(), plot.caption = element_text(hjust = 0.5))

oakabundance1 <- FBW_DATA %>%
  filter(type == "Abundance") %>%
  filter(Habitat == "Oak")
  
ggplot(data = oakabundance1, aes(x = Treatment)) +
  geom_boxplot(mapping = aes( y = count, color = Treatment))+
  facet_wrap(~ year, nrow = 1)

oak_abundance.aov <- aov(count ~ Treatment + year + Treatment:year, data = oakabundance1)

summary(oak_abundance.aov)

##FBW Abundance in Hazelnuts Before and After Treatment##
  
hazelnutabundance <- FBW_DATA %>%
  filter(type == "Abundance") %>%
  filter(Habitat == "Hazelnut") %>%
  group_by(year, Treatment) %>%
  summarise(mean3 = mean(count), se5 = calcSE(count)) %>%
  ungroup()

hazel_abund_Line <- ggplot(hazelnutabundance, aes(x = year, y = mean3)) + 
  geom_line(aes(y = mean3,  color = Treatment)) +
  geom_point(aes( y = mean3, color = Treatment), show.legend = FALSE) +
  geom_errorbar(aes(ymin = (mean3 - se5), ymax = (mean3 + se5),
  color = Treatment, width= 0.5), show.legend = FALSE) +
  labs(caption = "(p=0.21)",
       x = "Abundance",
       y = "Average # of FBW",
       colour = "Treatment")+
  scale_color_manual(values=c('#94d0ff','#0076d1')) +
  scale_x_continuous(breaks=c(2018, 2019), limits=c(2017.75, 2019.25) ) +
  theme(legend.position="top", legend.title = element_blank(), plot.caption = element_text(hjust = .5))

plot (hazel_abund_Line)

hazelnutabundance1 <- FBW_DATA %>%
  filter(type == "Abundance") %>%
  filter(Habitat == "Hazelnut")

ggplot(data = hazelnutabundance1, aes(x = Treatment)) +
  geom_boxplot(mapping = aes( y = count, color = Treatment))+
  facet_wrap(~ year, nrow = 1)

hazelnut_abundance.aov <- aov(count ~ Treatment + year + Treatment:year, data = hazelnutabundance1)

summary(hazelnut_abundance.aov)

##Baseline FBW##


Baseline <- ggplot(data = FBW_DATA, aes(x = Habitat)) +
  geom_boxplot(mapping = aes( y = count, color = Habitat), show.legend = FALSE) +
  labs(
    x = NULL,
    y = "# of FBW per trap",
    colour = "habitat")+
  scale_color_manual(values=c('#0076d1','#9000bf')) +
  theme(legend.position="top", legend.title = element_blank(), plot.caption = element_text(hjust = .5)) +
  facet_wrap(vars(type))


plot (Baseline)

##Panels##
ggarrange(Proportion_Line, Oak_Emerge_Line, Oak_abund_Line, hazel_abund_Line,  ncol = 2, nrow = 2)

