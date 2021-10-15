library(tidyverse)
library(dplyr)
# canopy cover
canopy<-read_csv("canopy_cover.csv")%>%
  #weighted adjustment for blocks 4-6 of 15 that are shaded by xmas trees
  mutate(canopy=ifelse(orchard_age==15&block>3&seedmix!='annuals'&seedmix!='perennials', canopy*.4+90*.6, canopy))%>%
  mutate(canopy=ifelse(orchard_age==15&block<3&seedmix!='annuals'&seedmix!='perennials', canopy*.4+60*.6, canopy))



ggplot(canopy, aes(x=orchard_age, y=canopy))+geom_boxplot(aes(group=orchard_age))+geom_jitter(aes(group=orchard_age))+facet_wrap(~year)
  

plotkey<-canopy%>%
  group_by(orchard_age, block, management, seedmix)%>%
  summarize()
#need to add in block 6 for 2020, and values for 2021
# adjustment for lower half of 15 year old that should have a higher canopy due to xmas trees