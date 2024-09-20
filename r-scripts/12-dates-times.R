# working with dates and times in R - lubridate and tidyquant packages
# analyzing Climate Change, including time series analysis

remove(list=ls())
library(tidyverse)  # contains lubridate
library(tidyquant)  # for calculating moving averages

# database: (remove hashtag)
#browseURL("https://docs.google.com/spreadsheets/d/1yKedemxKYMiBd8nwzZI5GI06enIZKRUvmbbLJk10uAI/edit?gid=304223944")

# read the daily weather data from the KNMI station Lauwersoog - 1 jan 1991 - 31 sep 2024
dat<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRYSmvT7qFqBPa-XIxFIzIXpZOYlWpY-MqyMVVwh_Q1nN7pzxSGaPKlWRhPfCtomR59bkLuOgaUFRa1/pub?gid=2037414920&single=true&output=csv", show_col_types = FALSE) %>%
     mutate(date=lubridate::ymd(YYYYMMDD),   # make variable of type Date from string
            day=lubridate::day(date), 
            month=lubridate::month(date),
            year=lubridate::year(date),
            metyear_dec=ifelse(month==12,year+1,year),  # calculate meteorological year, which starts at 1 December of previous year
            metyear_nov=ifelse(month %in% c(11,12),year+1,year),  # winter for Hellman index, which starts at 1 November of previous year
            RH=ifelse(RH==-1,0,RH/10),     # RH = Daily precipitation amount (in 0.1 mm) (-1 for <0.05 mm) -> convert to mm/day
            TG=TG/10) %>% # TG = Daily mean temperature in (0.1 degrees Celsius) -> convert to 0C
     select(date,metyear_nov,metyear_dec,year,month,day,RH,TG)
head(dat,n=10)
tail(dat,n=10) # show the last 10 records of the tibble

# aggregate the dates per month
dat_month<-dat %>% 
  dplyr::group_by(year,metyear_nov,metyear_dec,month) %>%
  summarize(totrain_mo_mm=sum(RH,na=T), #monthly total rainfall in mm
            avgtemp_mo_oC=mean(TG,na.rm=T)) %>%             #monthly average temperature in oC
  mutate(mdate=ymd(paste(year,"-",month,"-","15",sep="")))  # months characterized by their 15th day to keep them in time series format
tail(dat_month,20) # note the definition of the metyear_nov and metyear_dec variables, these months "belong" to the next year

# plot monthly average temperature with 5-year running average
dat_month %>% ggplot(aes(x=mdate,y=avgtemp_mo_oC)) +
  geom_line() +
  tidyquant::geom_ma(ma_fun=SMA,n=61,col="red",linetype="solid") +
    coord_x_date(xlim=c("2000-01-01", "2024-08-31")) +
  ggtitle("Monthly temperature at Lauwersoog") +
  theme(text = element_text(size=16)) #SMA is Smoothed Moving Average (for our case number of months) and n is the number of observations to average over


# plot monthly rainfall with 1-year running average
dat_month %>% ggplot(aes(x=mdate,y=totrain_mo_mm)) +
  geom_line(linewidth=0.7) +
  tidyquant::geom_ma(ma_fun=SMA,n=13,col="red",linetype="solid",linewidth=1) +
  coord_x_date(xlim=c("2014-01-01", "2024-09-01")) +
  xlab("date") +
  ylab("total monthly rainfall") +
  ggtitle("Lauwersoog") +
  theme(text = element_text(size=20))

# additive time series decomposition of the monthly mean temperatures: what is seasonal and what is a longterm trend?
monthly_temps<-dat_month %>%
  ungroup() %>%  # otherwise the grouping variables are kept
  #  filter(year!=2024)  %>% # only use complete years
  select(avgtemp_mo_oC) 

monthly_temps_ts<-ts(monthly_temps,frequency=12,start=c(2000,1))  # make time series object, with frequency and starting month
monthly_temps_ts  #inspect time series object
monthly_temp_comp <- decompose(monthly_temps_ts)  # decompose in time series components
plot(monthly_temp_comp)   # plot the result
  
# note the trend component is the same as the 12 month running average from the previous plot
# adjust the time series for the seasonal component, and plot it

TempSeasonAdj <- monthly_temps_ts - monthly_temp_comp$seasonal
plot(TempSeasonAdj,ylab="seasonally corrected temperature (oC)")
grid(nx = NULL,  # add y gridlines
     ny = NA,
     lty = 2, col = "gray", lwd = 2)  

# note again that really cold monthly temperatures (after seasonal correction) become rare, especially since since 2010
# while really warm months (after seasonal correction) become more common from 1991 -2016, while declining a bit again since
# to make the plots and do this in tidyverse style, you may want to explore the timetk package
# https://cran.r-project.org/web/packages/timetk/timetk.pdf


# average temperature of the coldest month per meteorological year

dat_metyear_dec<-dat_month %>%  # data for the meteorological year
  dplyr::group_by(metyear_dec) %>%
  summarize(minmonth=min(avgtemp_mo_oC,na.rm=T))  #annual  minimum  average monthlytemperature in oC
tail(dat_metyear_dec,20)
dat_metyear_dec %>%
  ggplot(aes(x=metyear_dec,y=minmonth)) +
  #  geom_line(linewidth=0.7) +
  geom_smooth() +
  geom_point(size=3) +
  ggtitle("average temperature of the coldest month per meteorological year  Lauwersoog") +
  theme(text = element_text(size=12))

  # note that 2010 and 2011 were the last winters with a (on average) sub-zero month, 
# potentially relevant for cockle recruitment (crabs stay on mudflats eating spatfall)

# Hellmann index (sum of all below-zero daily average temperatures from 1 November of previous year until 31 march)
# see https://nl.wikipedia.org/wiki/Koudegetal

dat_Hellmann<-dat %>%  
  dplyr::filter(month %in% c(11,12,1,2,3),
                TG<0) %>%
  dplyr::group_by(metyear_nov) %>%
  summarize(Hellmann=sum(TG,na.rm=T))  #calculate Hellmann index
tail(dat_Hellmann,20)
dat_Hellmann %>%
  ggplot(aes(x=metyear_nov,y=Hellmann)) +
  #  geom_line(size=1) +
  geom_point(size=5) +
  geom_smooth(method="lm",linewidth=2,fill="lightblue") +
  xlab("winter (november - march)") +
  ylab("Hellmann index") +
  ggtitle("Winter cold index Lauwersoog station") +
  theme(text = element_text(size=12))

# 1997 was the last "Elfstedentocht"
# 2014 -2024 (our transect study) is characterized by only warm winters

# Warmth index
library(quantreg)
# see for calculation https://www.knmi.nl/nederland-nu/klimatologie/lijsten/warmtegetallen

dat_WarmthIndex<-dat %>%  
  dplyr::filter(month %in% 4:8,    #note: KNMI uses until oct, but this is more relevant for our transect (2nd week september)
                TG>18) %>%
  dplyr::group_by(year) %>%
  summarize(WarmthIndex=sum(TG,na.rm=T))  #calculate Hellmann index
tail(dat_WarmthIndex,20)

# Fit quantile regression models at different quantiles
fit_10 <- rq(WarmthIndex ~ year, data = dat_WarmthIndex, tau = 0.10)
fit_50 <- rq(WarmthIndex ~ year, data = dat_WarmthIndex, tau = 0.50)
fit_90 <- rq(WarmthIndex ~ year, data = dat_WarmthIndex, tau = 0.90)

# Add predictions to dataframe
dat_WarmthIndex$pred_q10 <- predict(fit_10, newdata = dat_WarmthIndex)
dat_WarmthIndex$pred_q50 <- predict(fit_50, newdata = dat_WarmthIndex)
dat_WarmthIndex$pred_q90 <- predict(fit_90, newdata = dat_WarmthIndex)


dat_WarmthIndex %>%
  ggplot(aes(x=year,y=WarmthIndex)) +
  geom_point(size=3) +
  geom_line(aes(x=year,y=pred_q10),col="blue",linewidth=1) +
  geom_line(aes(x=year,y=pred_q50),col="orange",linewidth=1) +
  geom_line(aes(x=year,y=pred_q90),col="green",linewidth=1) +
  xlab("summer (april - august)") +
  ylab("Warmth index") +
  ggtitle("Warmth index Lauwersoog station") +
  theme(text = element_text(size=12))+
  geom_smooth(method="lm",col="red")
# note the hot summer of 2018, which likely killed the cockle cohort
# summer of 2011 was cool, and that previous winter was cold 
