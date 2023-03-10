library(raster)
library(rgdal)
library(prism)
library(tidyverse)

# set prism directory
prism_set_dl_dir("/Users/alejandro/Documents/Repositories/prism/downloads")

# download prism data (normals)
get_prism_normals(type="tmean", resolution = "4km", mon = 1:12, keepZip = FALSE) # mean temp
get_prism_normals(type="tmax", resolution = "4km", mon = 1:12, keepZip = FALSE)
get_prism_normals(type="tmin", resolution = "4km", mon = 1:12, keepZip = FALSE)
get_prism_normals(type="tdmean", resolution = "4km", mon = 1:12, keepZip = FALSE)
get_prism_normals(type="ppt", resolution = "4km", mon = 1:12, keepZip = FALSE)
get_prism_normals(type="vpdmin", resolution = "4km", mon = 1:12, keepZip = FALSE)
get_prism_normals(type="vpdmax", resolution = "4km", mon = 1:12, keepZip = FALSE)

get_prism_annual(type="ppt", years=2019)
get_prism_annual(type="tmean", years=c(2000:2020))




get_prism_monthlys(type="ppt", years=2021, mon=2:8)
get_prism_monthlys(type="tmean", years=2021, mon=2:8)

get_prism_dailys(type="ppt", minDate="2020-04-01", maxDate="2020-05-31")
get_prism_dailys(type="tmean", minDate="2020-04-01", maxDate="2020-05-31")
get_prism_dailys(type="ppt", minDate="2021-04-01", maxDate="2021-05-17")


#what do you got?
prism_archive_ls()
pd_get_name(prism_archive_ls())

# visualize it tool (tell it data, month, etc. )
smean <- prism_archive_subset(
  "tmean", "annual", year = 2001
)
pd_image(smean)



# associate locations with long, lat
# to do: create csv of location names with longlat, run a for loop to create a named object for each location

places<-as.tibble(c("101_223", "323_2", "113,3", "433_24", "235_1", "223_6"))%>%
  mutate(value2=paste(value, "name", sep="."))

places<-read_csv("places.csv")

pv <- strsplit(as.character(places$value), "_")
names(pv) <- places$value2
list2env(pv, envir = .GlobalEnv)

# or keep as pv, use that list as a run through "for loop" to slice and extract data for each place. 


pvd<-c(long, lat)

n<-c(-122.958918, 46.896415)
c<-c(-123.182171, 44.02615)
s<-c(-123.642278, 41.27811)

fieldsite<-c(-122.8845, 44.0072)
marissa<-c(-123.0785, 45.0785)

to_slice <- prism_archive_subset("tmean", "annual normals" , resolution = "4km")
test=pd_plot_slice(to_slice, fieldsite)

get_prism_normals(type="tmean", resolution = "4km", annual=T, keepZip = FALSE)


# prism_archive_subset() will return prism data that matches the specified 
# variable, time step, years, months, days, etc.


#get 2020 dailies
to_slice <- prism_archive_subset(type="ppt", temp_period="daily", minDate="2020-04-01", maxDate="2020-05-31")
ppt2020a <- pd_plot_slice(to_slice, marissa)
ppt2020<-as.tibble(ppt2020a[["data"]])%>%
  mutate(type="ppt")%>%
  separate(date, into = c("year", "month", "day"), sep="-")%>%
  mutate(day=as.numeric(day))%>%
  mutate(period=ifelse(day<8, 1, ifelse(day>7&day<14, 2, ifelse(day>13&day<21, 3, 4))))%>%
  mutate(period=ifelse(month=="05", period+4, period))%>%
  group_by(year, month, week, type, period)%>%
  summarize(data=sum(data))

to_slice <- prism_archive_subset(type="tmean", temp_period="daily", minDate="2020-04-01", maxDate="2020-05-31")
temp2020a <- pd_plot_slice(to_slice, marissa)
temp2020<-as.tibble(temp2020a[["data"]])%>%
  mutate(type="temp")%>%
  separate(date, into = c("year", "month", "day"), sep="-")%>%
  mutate(day=as.numeric(day))%>%
  mutate(period=ifelse(day<8, 1, ifelse(day>7&day<14, 2, ifelse(day>13&day<21, 3, 4))))%>%
  mutate(period=ifelse(month=="05", period+4, period))

#get 2021 monthlies
to_slice <- prism_archive_subset("ppt", "monthly", years = 2021,  mon=2:8)
ppt2021a <- pd_plot_slice(to_slice, marissa)
ppt2021<-as.tibble(ppt2021a[["data"]])%>%
  mutate(type="ppt")%>%
  separate(date, into = c("year", "month", "day"), sep="-")%>%
  mutate(period=as.numeric(month))%>%
  select(year, month, type, period, data)%>%
  mutate(data=data/4)%>%
  mutate(period=period-2)

climate<-rbind(ppt2020, ppt2021)
ggplot(climate, aes(x=period, data)) + geom_bar(stat="identity")+
  facet_grid(~year)+xlab("") + ylab("Mean weekly ppt")+
  xlim(.5, 6.5)

ggplot(subset(climate, type=="tmean"), aes(x=year, y=data ))  +
  geom_bar(data = subset(climate,type=="ppt"), aes(x = year, y = data/100), stat = "identity", fill = "lightgrey") +
  geom_point()  + geom_line(aes(group="type"))+
  scale_y_continuous(sec.axis = sec_axis(~.*100, name = "Annual Precipitation (mm)"))+ylab("Mean annual temperature C")+
  geom_hline(yintercept=11.7382, linetype=2)+   
  geom_hline(yintercept=11.01667, linetype=2, color="red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


to_slice<-prism_archive_subset("tmean", 'monthly normals', temp_period = "monthly")


n1p <- pd_plot_slice(to_slice, n)
c1p <- pd_plot_slice(to_slice, c)
s1p <- pd_plot_slice(to_slice, s)

min(c1p[["data"]][["data"]][1:12])
min(c1p[["data"]][["data"]][13:24])



pvd_tmean_monthly_normals<- as_tibble(p[["data"]][["data"]])
pvd_tmean_monthly_normals<-rownames_to_column(pvd_tmean_monthly_normals, var = "month")

ggplot(pvd_tmean_monthly_normals, aes(as.integer(month), value)) +geom_point() 


to_slice <- prism_archive_subset("tdmean", "monthly normals", mon = 1:12, resolution = "4km")
p <- pd_plot_slice(to_slice, pvd)


pvd_dew_monthly_normals<- as_tibble(p[["data"]][["data"]])
pvd_dew_monthly_normals<-rownames_to_column(pvd_dew_monthly_normals, var = "month")

ggplot(pvd_dew_monthly_normals, aes(as.integer(month), value)) +geom_point() 



get_pris


to_slice <- prism_archive_subset(type="ppt", temp_period="daily", minDate="2020-04-01", maxDate="2020-05-17")
ppta2020 <- pd_plot_slice(to_slice, marissa)
ppt2020<-as.tibble(ppta2020[["data"]])%>%
  summarize(data=sum(data))
ppt2020

to_slice <- prism_archive_subset(type="ppt", temp_period="daily", minDate="2021-04-01", maxDate="2021-05-17")
ppta2021 <- pd_plot_slice(to_slice, marissa)
ppt2021<-as.tibble(ppta2021[["data"]])%>%
  summarize(data=sum(data))
ppt2021

