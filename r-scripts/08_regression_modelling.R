# Multiple regression in a glm context: calculation and plotting
# Q: How does the distribution along the gradient of Orchestia change between years?

# clear everything in memory (of R)
remove(list=ls())

library(tidyverse)


# read the macrodetritivore abundance data, call the dataset orchdat
# filter to use only years 2018,2019,2021,2022,2023
# select only the variables year,Distance_ID,replicate,Orchestia_gammarellus
# rename Orchestia_gammarellus to OrchGamm
# filter to only use replicates 1, 2 and 3 of each year
# group by year and Distance_ID
# calculate the sum of the number of Orchestia found per year and Distance_ID
# do all the above in one pipeline
orchdat<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT6bdROUwchBBIreCQQdBenu35D5Heo8bl3UgES9wrpBwax_GUM1bKSo_QXfmLq8Ml9-crCI7MmW2XH/pub?gid=615066255&single=true&output=csv") |>
  dplyr::filter(year %in% c(2018,2019,2021,2022,2023,2024), replicate<=3,Species_ID=="Orchestia_gammarellus") |>
  dplyr::group_by(year,TransectPoint_ID) |>
  dplyr::summarize(CountSum=sum(Count,na.rm=T)) 
orchdat


# read the macrotransect elevation data
elevdat<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT4C7olgh28MHskOjCIQGYlY8v5z1sxza9XaCccwITnjoafF_1Ntfyl1g7ngQt4slnQlseWT6Fk3naB/pub?gid=1550309563&single=true&output=csv") %>%
  dplyr::filter(year %in% c(2018,2019,2021,2022,2023,2024) & !is.na(TransectPoint_ID)) %>%
  dplyr::select(year,TransectPoint_ID,elevation_m)   # select  only distance_m and elevation 
elevdat

# add the elevation to the orchestia data, and filter to retain only elevation < 1.75 m (where it lives)
orchdat2<-left_join(orchdat,elevdat,by=c("year","TransectPoint_ID")) %>%
  dplyr::mutate(year=factor(year))  %>% # make year into a factor
  dplyr::filter(TransectPoint_ID>=200 & TransectPoint_ID<=1000) %>%  # restrict it to the saltmarsh and mudflat, where the species occurs 
  dplyr::filter(!(year==2022 & TransectPoint_ID==260)) # remove missing plot
orchdat2

# explore how Orchestia changes along the transect in a bar plot
orchdat2  %>% 
  ggplot(aes(x=factor(TransectPoint_ID),y=CountSum,group=year)) +
  geom_bar(stat="identity") +
  facet_grid(year~.)  #facet_grid(what_in_rows~what_in_columns), where . is nothing)

# remove 2024
orchdat3 <- orchdat2 # |> dplyr::filter(year!=2024)

# explore how elevation changes along the transect in a barplot
orchdat3 %>% filter(year==2023) %>%
  ggplot(aes(x=factor(TransectPoint_ID),y=elevation_m)) +
     geom_bar(stat = "identity") 

# explore how the flooding probability changes with elevation between 1 and 2 m elevation
floodprob<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTiJsz0xcUAhOjlTcHpT7NXhSysGMdzI6NfYm_kzpFtV99Eyea2up7dB-5a_P-jAKiwMBAXYURBTpRU/pub?gid=600890443&single=true&output=csv") %>%
  dplyr::filter(year %in% c(2018:2023))
floodprob %>%
  ggplot(aes(x=elevation_m,y=floodprob,col=factor(year))) +
  geom_line(size=1.2)
floodprob %>%
  ggplot(aes(x=elevation_m,y=floodprob*(365/2)*2,col=factor(year))) +
  geom_line(size=1.2) +
  xlim(1,2) +ylim(0,70) +
  ylab("number of floods 1 mar - 30 aug")


# plot Orchestia (y) versus elevation_m (x) in ggplot as a scatterplot, with each year as a different color
p1<- orchdat3  %>% 
  ggplot(aes(x=elevation_m,y=CountSum,color=year)) +
  geom_point(size=3) 
p1

# calculate the optimal preferred elevation by Orchestia for each year using weighted.mean function
# store the result in the graphical object p1
orchdat3 %>%
  group_by(year) %>%
  summarize(wa_elevation=weighted.mean(x=elevation_m,w=CountSum,na.rm=T))

## explore response to elevation and year as a linear model, call this m1
# first only elevation (not yet year)
orchdat3
m1<-lm(CountSum~elevation_m,data=orchdat3)
m1
summary(m1)
#add the linear model to the plot
orchdat3$pred1<-predict(m1)
orchdat3
p2<-p1+ geom_line(data=orchdat3,aes(y=pred1),col="black",size=1.2)
p2

# fitmodel  m2 by adding a quadratic term for elevation_m to check for an ecological optimum
# y=b0 + b1x1 + b2x1^2
orchdat3
m2<-lm(CountSum~elevation_m+I(elevation_m^2),data=orchdat3)
m2
summary(m2)
#add the linear model to the plot
# calculate the predicted value of m2 for every observation, add to the dataset as a variable as pred2
# add the new predicted line to the previous plot p2, store as object p3 and show it
orchdat3$pred2<-predict(m2)
orchdat3
p3<-p2+ geom_line(data=orchdat3,
                  aes(y=pred2),
                  col="black",size=1.2,linetype="dashed")
p3
# test if the new model m2   significantly explains more variation than the first model m1
anova(m1,m2)

# predict the orchestia abundance at 1.5 m elevation
m2
# y=b0+b1x+b2x^2
-509.1+713.7*1.5+-205.8*1.5^2

# add year (as a factor) to the model and fit it as model m3
# so y = b0 + b1x1 + b2x1^2 +b3x2 
# test if it is significant, and better than the previous one
m3<-lm(CountSum~elevation_m+I(elevation_m^2)+factor(year),
       data=orchdat3)
m3
summary(m3)
anova(m3)
#add the linear model to the plot
# calculate the predicted value of m2 for every observation, add to the dataset as a variable as pred2
# add the new predicted line to the previous plot p2, store as object p3 and show it
orchdat3$pred3<-predict(m3)
orchdat3
p4<-p1+ geom_line(data=orchdat3,
                  aes(y=pred3,col=factor(year)),
                  size=1.2,linetype="solid")
p4
# does this significantly explain more variation than the model without year?
anova(m2,m3)
# yes this is a better model

# include the interaction between elevation and year (as a factor) in the model 
m4<-lm(CountSum~elevation_m+I(elevation_m^2)+factor(year)+elevation_m:factor(year),
       data=orchdat2)
m4
anova(m4) # use this in case of categorical predictors
anova(m3,m4) # so adding the interaction does not make the model better
#add the linear model to the plot
# calculate the predicted value of m2 for every observation, add to the dataset as a variable as pred2
# add the new predicted line to the previous plot p2, store as object p3 and show it
orchdat3$pred4<-predict(m4)
orchdat3
p4<-p1+ geom_line(data=orchdat2,
                  aes(y=pred4,col=factor(year)),
                  size=1.2,linetype="solid")
p4
#### m3 is the preferred model in this case (outcome of model selection)

# explore the consequences of a log transformation of y values
x<-c(0,1,2,3,4,5)
y<-c(1,10,100,1000,10000,100000)
dat<-data.frame(x,y)
dat
dat %>% ggplot(aes(x=x,y=y)) +
  geom_point(shape=16,size=5) +
  geom_line(size=1.2)
dat %>% ggplot(aes(x=x,y=log10(y))) +
  geom_point(shape=16,size=3) +
  geom_line(size=1.2)
dat %>% ggplot(aes(x=x,y=log(y))) + # use log with base number e
  geom_point(shape=16,size=3) +
  geom_line(size=1.2)
# explore the consequences of the log base number (10 or e)
log(0)
log10(1)
log10(10)
log(0)
log(1)
exp(1)
log(exp(1))

### develop, test for significance and plot different models of increasing complexity 
#  using  multiple regresssion, assuming a poisson distribution (so use a generalized linear model)
# Explore  how the abundance of Orchestia depends on elevation_m and year,  their potential interaction,
# and a potential ecological optimum of Orchestia with respect to elevation_m
# show the effect of elevation but now in a generalized linear model instead of linear model, using a log link function and a poisson distribution
orchdat2
m5<-glm(CountSum~elevation_m+I(elevation_m^2),
        family=poisson(log),
        data=orchdat3)
m5
anova(m5,test="Chisq")
#add the linear model to the plot
# calculate the predicted value of m2 for every observation, add to the dataset as a variable as pred2
# add the new predicted line to the previous plot p2, store as object p3 and show it
orchdat3$pred5<-exp(predict(m5))  # backtransform the predicted values
orchdat3
p5<-p1+ geom_line(data=orchdat3,
                  aes(y=pred5),
                  col="black",size=1.2,linetype="dashed")
p5


# now test and show  the effect of both elevation , elevation squared and year
orchdat3
m6<-glm(CountSum~elevation_m+I(elevation_m^2)+factor(year),
        family=poisson(log),
        data=orchdat2)
m6
anova(m6,test="Chisq")
#add the linear model to the plot
# calculate the predicted value of m2 for every observation, add to the dataset as a variable as pred2
# add the new predicted line to the previous plot p2, store as object p3 and show it
orchdat3$pred6<-exp(predict(m6))  # backtransform the predicted values
orchdat3
p5<-p1+ geom_line(data=orchdat3,
                  aes(y=pred6,col=factor(year)),
                  size=1.2,linetype="solid")
p5
# better than the previous?
anova(m5,m6,test="Chisq")

# add the interaction to the model: elevation + elevation ^2 + year + elevation*year
# now test and show  the effect of both elevation + year
orchdat2
m7<-glm(CountSum~elevation_m+I(elevation_m^2)+factor(year)+elevation_m:factor(year),
        family=poisson(log),
        data=orchdat3)
m7
anova(m7,test="Chisq")
anova(m6,m7,test="Chisq")
#add the  model to the plot
# calculate the predicted value of m2 for every observation, add to the dataset as a variable as pred2
# add the new predicted line to the previous plot p2, store as object p3 and show it
orchdat3$pred7<-exp(predict(m7))  # backtransform the predicted values
orchdat3
p6<-p1+ geom_line(data=orchdat3,
                  aes(y=pred7,col=factor(year)),
                  size=1.2,linetype="solid")
p6




