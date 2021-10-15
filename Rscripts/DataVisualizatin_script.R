### Visualize time series and explore data limitations based on time gaps
## Created/Adapted by: Betsy Summers

# Load libraries
library(dplyr)
library(zoo)
library(DataCombine)
library(lubridate)
library(tidyverse)
library(cowplot)
library(visdat)
library(naniar)

## Load data ----
site_daily <- read_csv('data/Bern_site_data_example.csv') 
#str(site_daily)

## rename a couple of variables
site_daily <- rename(site_daily, "GPP" = "GPP.final.pos", 
                     "PAR" = "light_DLI") 

## Create a column for season category based on months that I will latter fitler for the more interesting time periods (spring and summer) for analysis 
site_daily <- site_daily %>%
  mutate(season = case_when(Month %in% c("Mar", "Apr", "May", "Jun") ~'Spring',
                            Month %in% c("Jul", "Aug", "Sep") ~'Summer',
                            Month %in% c("Oct","Nov") ~'Fall',
                            Month %in% c("Dec","Jan","Feb") ~'Winter')
  )

site_daily$season <- factor(site_daily$season, levels = c("Spring","Summer","Fall","Winter"))
site_daily$Year <- as.factor(site_daily$Year)
#str(site_daily)

## Select variables that are used for data visualization and analyses
site_daily <- site_daily %>%
  dplyr::select(date, JulianDay, Month, season, ENSO.cat,Year, GPP, ER.final, PAR, turbidity.cap, Q.cms, tempwater, DO.obs) 

## Look at the site_daily data by year ----
P.site.gpp <- ggplot(site_daily, aes(JulianDay, GPP, color = Year)) + 
  geom_line() + 
  facet_wrap(~Year) + theme_bw() + 
  ylab(bquote('GPP ('*g ~O[2]~ m^-2~d^-1*')')) 
P.site.gpp

P.site.ER <-ggplot(site_daily, aes(JulianDay, ER.final, color = Year)) + 
  geom_line() + 
  facet_wrap(~Year) + theme_bw() + 
  ylab(bquote('ER ('*g ~O[2]~ m^-2~d^-1*')')) 
#P.site.ER

P.site.turb <-ggplot(site_daily, aes(JulianDay, turbidity.cap, color = Year)) + 
  geom_line() + 
  facet_wrap(~Year) + theme_bw() + 
  ylab(bquote('Turbidity (NTU)')) 
#P.site.turb

P.site.Q <-ggplot(site_daily, aes(JulianDay, Q.cms, color = Year)) + 
  geom_line() + 
  facet_wrap(~Year) + theme_bw() + 
  ylab(bquote('Q (cms)')) 
#P.site.Q


### one plot for time series ----
ts.gpp <- site_daily %>%
  mutate(date = as.Date(date, format='%m/%d/%Y')) %>%
  ggplot(aes(date, GPP)) + geom_point() + theme_bw() +
  scale_x_date(limits = as.Date(c("2008-01-01","2010-12-31"))) +
  ylab(bquote('GPP ('*g ~O[2]~ m^-2~d^-1*')')) +
  theme(axis.title.x = element_blank()) 

ts.er <- site_daily %>%
  mutate(date = as.Date(date, format='%m/%d/%Y')) %>%
  ggplot(aes(date, ER.final)) + geom_point() + theme_bw() +
  scale_x_date(limits = as.Date(c("2008-01-01","2010-12-31"))) + 
ylab(bquote('ER ('*g ~O[2]~ m^-2~d^-1*')')) 

ts.turb <- site_daily %>%
  mutate(date = as.Date(date, format='%m/%d/%Y')) %>%
  ggplot(aes(date, turbidity.cap)) + geom_point() + theme_bw() +
  scale_x_date(limits = as.Date(c("2008-01-01","2010-12-31"))) + 
  ylab(bquote('Turbidity (NTU)')) +
  theme(axis.title.x = element_blank()) 

ts.Q <- site_daily %>%
  mutate(date = as.Date(date, format='%m/%d/%Y')) %>%
  ggplot(aes(date, Q.cms)) + geom_point() + theme_bw() +
  scale_x_date(limits = as.Date(c("2008-01-01","2010-12-31"))) + 
  ylab(bquote('Q ('*m^3/s*')')) +
  theme(axis.title.x = element_blank())

plot_grid(ts.gpp, ts.turb, ts.Q, ncol = 1, align = 'v', axis = 'l')

## Look at completeness of time series record ----
### Subset site_daily data to focus on seasonal time periods. Here, I am interested in snowmelt (March through June) and monsoon (Jul - Sep) time period  ----
## Option of filter years that have known large time gaps. Here, I'm using a truncated dataset for example and don't need to filter year. 

site_daily <- site_daily %>%
 # filter(Year != 2007) %>%
 # filter(Year != 2008) %>%
#  filter(Year != 2019) %>%
  filter(season %in% c("Spring", "Summer"))  # focus on the snowmelt and monsoon timeperiods

### Look structure and data gaps for the whole time period ----
vis_miss(site_daily)
miss_var_summary(site_daily)

## some gaps are single days and could be interpolated but there are bigger problems here
# These plots are snowmelt and monsoon time periods
ggplot(site_daily, 
       aes(x = JulianDay, 
           y = GPP)) + 
  geom_miss_point() + 
  facet_wrap(~Year)

# These plots are snowmelt and monsoon time periods
ggplot(site_daily, 
       aes(x = JulianDay, 
           y = turbidity.cap)) + 
  geom_miss_point() + 
  facet_wrap(~Year)

#combinations of missingness across cases. Tease apart the missingness of GPP estimates to sensor data or "bad" Metabolizer estimes 
gg_miss_upset(site_daily)

gg_miss_fct(site_daily, Year)

GPP.p.miss <- site_daily %>%
  group_by(Year, Month) %>%
  dplyr::select(GPP) %>%
  miss_var_summary() #%>%
  #print(n=nrow(.))

GPP.p.miss$Month = factor(GPP.p.miss$Month, level = month.abb)

GPP.p.miss <- GPP.p.miss %>%
  mutate(season = case_when(Month %in% c("Mar", "Apr", "May", "Jun") ~'Spring',
                            Month %in% c("Jul", "Aug", "Sep") ~'Summer')
  )
ggplot(GPP.p.miss, 
       aes(x = Month, 
           y = pct_miss, color = season)) + 
  geom_miss_point() + 
  facet_wrap(~Year) + 
  geom_hline(yintercept = 80) # visualize criteria where pct_miss > 80 will be removed from future analyses


