library(descr)
library(readr)
#library(RnBeads)
library(magick)
library(dplyr)
library(magrittr)
library(grid)
library(kableExtra)
library(stringr)
library(gridExtra)
library(knitr)
library(ggplot2)
library(gtable)
library(labelled)
library(haven)
library(gmodels)
library(png)
library(tidyverse)
library(cjoint)
library(stargazer)


CJ <- read.csv(file = 'C:/Users/dl0ck/OneDrive/Summer 2021/PolAdsTransparency/politics_user_study/Experiments/Conjoint Analysis/CJ.csv', header = TRUE, stringsAsFactors = T)


CJ$globalWarming <- recode(CJ$globalWarming, '1' = "Mostly by human activity", '2' = 'Equally by human \n activity and natural causes', '3' = 'Mostly by natural causes')

CJ$regulate <- recode(CJ$regulate, '1' = "Regulate business to protect \n the environment and create jobs", '2' = '2', '3' = '3', '4' ="4", '5' = "5",'6'="6",'7'="No regulation because it will \n not work and will cost jobs")


dat <- CJ[complete.cases(CJ$globalWarming),][1:1013,]
dat1 <- CJ[complete.cases(CJ$regulate),][1:1013,]
dat2<- CJ[complete.cases(CJ$priors),][1:1013,]

dat <- dat %>% mutate(globalWarming = factor(globalWarming, levels = c("Mostly by human activity", 'Equally by human \n activity and natural causes', 'Mostly by natural causes')))

dat1 <- dat1 %>% mutate(regulate = factor(regulate, levels = c("Regulate business to protect \n the environment and create jobs", '2','3',"4", "5","6","No regulation because it will \n not work and will cost jobs")))

qplot(dat$globalWarming, geom="bar") + 
  theme(,axis.text.x=element_text(angle=15, hjust=0.5, vjust=.5))

qplot(dat1$regulate, geom="bar") + 
  theme(,axis.text.x=element_text(angle=15, hjust=0.5, vjust=.5))

qplot(dat2$priors, geom="bar") + 
  theme(,axis.text.x=element_text(angle=15, hjust=0.5, vjust=.5))
