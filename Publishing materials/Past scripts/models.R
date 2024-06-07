library(cowplot)
library(descr)
library(readr)
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
library(Hmisc)
library(sjPlot)
library(labelled)
library(haven)
library(png)
library(magicfor)    
library(magicfor)  
library(fastDummies)
library(lme4)
library(coefplot)
library(pBrackets)
library(estimatr)
library(ggtext)
library(texreg)
local <- 'C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/'

####################
#data, models 
#and functions 
#for tables and plots
######################
#customized cjoint plot func
source(paste0(local,'plot_amce.R'))#to extract cjoint data for plots, I modify their source code
tass1 <- read.csv(paste0(local,'Data/TASS 04/8557_WUSTL-Weidenbaum04_MAIN.csv'))
#regulation opinions
tass2 <- read.csv(paste0(local,'Data/TASS 05/8557_WUSTL-Weidenbaum05_Redacted.csv'))

#conjoint
cj <- read.csv(file = paste0(local,'data/Conjoint/CJ.csv'), header = TRUE,
               stringsAsFactors = T)
#real ads means
dat <- read.csv(paste0(local,'data/Real', '/data.csv'))
datb <- read.csv(paste0(local,'data/Real', '/datb.csv'))
datc <- read.csv(paste0(local,'data/Real', '/datc.csv'))
datc <- datc[,c(2:5,7,8)]

#real ads lm
pooled     <- read.csv(paste0(local,'data/Real','/pooled.csv'))
orig <- read.csv(paste0(local,'data/Real', '/original.csv'))
redo <- read.csv(paste0(local,'data/Real', '/correction.csv'))



####################
#Functions objects 
#and transformations
#for plots tables
#and models
####################

####################
#global custom theme


label.baseline = TRUE
text.size = 30
text.color = "black"
point.size = 0.5
dodge.size = 0.9
font.family=NULL
theme_bw1 <- function(base_size = text.size, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family 
  ) %+replace% 
    theme(axis.text.x = 
            element_text(size = base_size *  0.9, 
                         colour = text.color, hjust = 0.5,
                         vjust = 1),  
          axis.text.y = element_text(size = base_size, 
                                     colour = text.color,
                                     hjust = 0, vjust = 0.5,
                                     family = font.family), 
          axis.title.y = element_text(size = base_size, 
                                      angle = 180, vjust = 0.01,
                                      hjust = 0.1, 
                                      family = font.family), 
          
          plot.title = element_text(face = "bold", 
                                    family = font.family),
          axis.ticks = element_blank(), 
          
          legend.background = element_blank(), 
          
          legend.key = element_blank(),
          panel.background = element_blank(), 
          panel.border = element_blank(), 
          strip.background = element_blank(), 
          
          
          plot.background = element_blank(), complete = TRUE,
          panel.grid.minor =   element_blank(),
          panel.grid.major =  element_line(colour = "grey87",size=0.5))
}
theme_set(theme_bw1())



##############
#export tables

#small function that exports tables to .tex file}
#only texreg handles lm_robust-it doesnt do regular tables
mod_texreg <- function(output.file, ...) {
  output <- capture.output(texreg(...))
  cat(paste(output, collapse = "\n"), "\n", file=output.file, append=F)
}




###########
#regulation 
#opinon

#PID 
tass2$pid <- NA 
tass2$pid[tass2$PartyID7 == -1 ] = NA
tass2$pid[tass2$PartyID7 == 4 ] = "Moderate/Independent/None"
tass2$pid[tass2$PartyID7 < 4 & tass2$PartyID7 > 0] = "Democrat"
tass2$pid[tass2$PartyID7 > 4 & tass2$PartyID7  < 8] = "Republican"

#WEIGHTED MEANS + CIS
wtd.ci <- function(x, weights, conf.level = 0.95) {
  require(Hmisc)
  nx <- length(x)
  df <- nx - 1
  vx <- wtd.var(x, weights, normwt = TRUE) ## From Hmisc
  mx <- weighted.mean(x, weights, na.rm=T)
  stderr <- sqrt(vx/nx)
  tstat <- mx/stderr ## not mx - mu
  alpha <- 1 - conf.level
  cint <- qt(1 - alpha/2, df)
  cint <- tstat + c(-cint, cint)
  CI <- c(cint * stderr)
  c( Estimate=mx,"CI lower"=CI[1],"CI upper"=CI[2],"Std. Error"=stderr)
}


# which(colnames(tass2)=='ADS4_A')
# which(colnames(tass2)=='ADS4_T')
#ADS4_A:T -> 137:156
Ads3 <- 137:156


for(i in 1:20){
  tass2[,Ads3[i]] <- to_factor(tass2[,Ads3[i]])
  tass2[,Ads3[i]]<- recode(tass2[,Ads3[i]], "1" = "Strongly agree",
                           "2" = "Agree", "3" = "Agree only a little", 
                           "4" = "Disagree only a little", '5' = 'Disagree',
                           '6' = 'Strongly disagree',"77" = NA_character_, 
                           "98" = NA_character_, "99" = NA_character_) 
  
}


topics3 <-c("Political ads should be removed if they contain false information",
            "Political ads should be allowed, even if they contain misleading information",
            "Political ads with false content should be allowed, but labeled with a
 warning","Companies should not try to label political ads as false, even if they
 containing misleading information","No political ads should be allowed at all",
            "Political candidates should always be allowed to post ads without restriction",
            "All ads on controversial topics should be removed, regardless of source","Ads
 that encourage voting should always be permitted","Ads from news organizations
 should be exempted from regulations of political advertising","Political ads from 
 news organizations should be subject to the same regulations as ads from other sources",
            "Ads that discredit voting by mail should not be permitted on social media",
            "Political ads should include information about who paid for them","Political
 advertisers should not be required to disclose their funding sources","Microtargeting
 political ads to specific audiences based on things like age, gender, and race should
 not be permitted.","Political advertisers should be allowed to select who sees their
 ads","Ads that contain misinformation about a candidate should be removed","All ads
 about candidates should be allowed, even if they contain false content","Companies
 should provide a publicly accessible database of all political ads","Ads about
 candidates or upcoming elections should be banned, but ads about social issues (e.g.,
 guns or abortion) should still be allowed","Ads from certain sources like political
 action committees (PACs) and dark money organizations should be banned")


###
#CJ
#Define baselines
#cjoint needs baselines defined seperately for unconditional and interacted
baselines <- list()
baselines$sourceType <- "Company"
baselines$sourceOrient <- "Pro-environment"
baselines$messageStrength <- "Weak"
baselines$messageOrient <- "Pro-environment"
baselines$image <- "Landscape"
baselines$pid <- 'Republican'

bases <- list()
bases$image <- 'Landscape'
bases$messageOrient <- 'Pro-environment'
bases$messageStrength <- 'Weak'
bases$sourceOrient <- 'Pro-environment'
bases$sourceType <- 'Company'





###########################
#opinions on web regulation
#f1
############################

sub <- c(144,141,143,155,156,149,148,154,150,151,145,146)#column numbers, custom order
comp <- c()
for(i in 1:length(sub)){
  comp <- c(comp, which(Ads3==sub[i]))
}

#comparison dataset to make sure the column numbers and column topics match up to the plot
or <- data.frame(matrix(nrow=12, ncol =2))
or[,1] <- topics3[comp]
or[,2] <- as.numeric(Ads3[comp])


new <- data.frame(matrix(nrow=12, ncol=4))#storage object
colnames(new)<-c('est','upper','lower','std. err')#grabbing means and cis
for(i in 1:length(sub)){#go through each column
  #weighted cross tabs Real Ad opinions with frequency and percentages
  new[i,] <- wtd.ci(as.numeric(as.factor(tass2[,sub[i]])), weights=tass2$WEIGHT)
}


####################
#conjoint experiment
####################

################################################
#cjoint does not vibe with texreg or stargazer
#to overcome this we will make a regular lm and
#overwrite the data with cjoint data

cj$messageStrength<- factor(cj$messageStrength, 
                            levels = c('Weak',
                                       'Moderate',
                                       'Strong'))

cj$image <- factor(cj$image, levels = c('Landscape',
                                        'Desert','Oil rig',
                                        'Pipeline','Solar wind'))

cj$messageOrient <- factor(cj$messageOrient, 
                           levels = c('Pro-environment',
                                      'Pro-development'))

cj$messageStrength <- factor(cj$messageStrength,
                             levels = c('Weak',
                                        'Moderate',
                                        'Strong'))

cj$sourceOrient  <- factor(cj$sourceOrient, 
                           levels = c('Pro-environment',
                                      'Pro-development'))



cj$sourceType  <- factor(cj$sourceType, levels = c('Company',
                                                   'Candidate',
                                                   'Organization'))


########################
#unconditional
#cjoint
#f2
unconditional <- amce(dv1 ~ image  + messageOrient + messageStrength + sourceOrient + sourceType  , respondent.id= 'CaseId', cluster=TRUE,  weights ='WEIGHT', data=cj, baseline= baselines, na.ignore=T)

#lm object to be overwritten
uncon <- summary(lm(dv1 ~  image  + messageOrient + messageStrength + sourceOrient + sourceType , data=cj, weights=WEIGHT))

#overwrite lm object with cjoint data
uncon$coefficients[2:11,c('Estimate','Std. Error','Pr(>|t|)')] <- rbind(cbind(summary(unconditional)$amce[,c('Estimate')],summary(unconditional)$amce[,c('Std. Err')],summary(unconditional)$amce[,c('Pr(>|z|)')]))


#################
#message X priors
#f3
mesxprior <- amce(dv1 ~  image  + messageOrient + messageStrength + sourceOrient + sourceType   + priors + messageOrient*priors, cluster=TRUE, respondent.id="CaseId", respondent.varying= 'priors', weights ='WEIGHT', data=cj, baseline= baselines, na.ignore=T)

#lm object to be overwritten
messP <- summary(lm(dv1 ~ image  + messageOrient + messageStrength + sourceOrient + sourceType   + priors + messageOrient*priors,weights =WEIGHT, data=cj))

#overwrite lm object with cjoint data- main variables
messP$coefficients[2:11,c('Estimate','Std. Error','Pr(>|t|)')] <- rbind(cbind(summary(mesxprior)$amce[,c('Estimate')],summary(mesxprior)$amce[,c('Std. Err')],summary(mesxprior)$amce[,c('Pr(>|z|)')]))

#overwrite lm object with cjoint data- interacted variables
messP$coefficients[12:13,c('Estimate','Std. Error','Pr(>|t|)')] <- rbind(cbind(c(as.matrix(summary(mesxprior)[[3]]['Estimate']),as.matrix(summary(mesxprior)[[4]]['Estimate'])),c(as.matrix(summary(mesxprior)[[3]]['Std. Err']),as.matrix(summary(mesxprior)[[4]]['Std. Err'])),c(as.matrix(summary(mesxprior)[[3]]['Pr(>|z|)']),as.matrix(summary(mesxprior)[[4]]['Pr(>|z|)']))))

#change rownames to prevent confusion
rownames(messP$coefficients)[12] <- 'messageOrientPro-development:priorsPro-environment'


################
#source X priors
#f3
sourcexprior <- amce(dv1 ~  image  + messageOrient + messageStrength + sourceOrient + sourceType   + priors + sourceOrient*priors, cluster=TRUE, respondent.id="CaseId", weights ='WEIGHT', data=cj, baseline= baselines, respondent.varying= 'priors', na.ignore=T)

#lm object to be overwritten
sourceP <- summary(lm(dv1 ~  image  + messageOrient + messageStrength + sourceOrient + sourceType   + priors + sourceOrient*priors,weights =WEIGHT, data=cj))

#overwrite lm object with cjoint data- main variables
sourceP$coefficients[2:11,c('Estimate','Std. Error','Pr(>|t|)')] <- rbind(cbind(summary(sourcexprior)$amce[,c('Estimate')],summary(sourcexprior)$amce[,c('Std. Err')],summary(sourcexprior)$amce[,c('Pr(>|z|)')]))

#overwrite lm object with cjoint data- interacted variables
sourceP$coefficients[12:13,c('Estimate','Std. Error','Pr(>|t|)')] <- rbind(cbind(c(as.matrix(summary(sourcexprior)[[3]]['Estimate']),as.matrix(summary(sourcexprior)[[4]]['Estimate'])),c(as.matrix(summary(sourcexprior)[[3]]['Std. Err']),as.matrix(summary(sourcexprior)[[4]]['Std. Err'])),c(as.matrix(summary(sourcexprior)[[3]]['Pr(>|z|)']),as.matrix(summary(sourcexprior)[[4]]['Pr(>|z|)']))))

#change rownames to prevent confusion
rownames(sourceP$coefficients)[12] <- 'sourcePro-development:priorsPro-environment'



###############
#Interactions
#f3

#edited cj source code to extract data
plt(mesxprior,plot.display='interaction')#run code
datM <- PLOTDATA#extract output
plt(sourcexprior,plot.display='interaction')
datS <- PLOTDATA

#this grabs the interaction effects from the 2 models for this plot
#getting the cis would be more difficult so we do some tricks for that
cjIn <- rbind(
  data.frame(summary(mesxprior)$priors1amce[3:6], 'Prior' ='Prefers development', datM[3,c('upper','lower')], 'Attribute'= paste('Message orientation:','                ','\n', '(Baseline = Pro-environment)\n', 'Pro-development')),
  data.frame(summary(mesxprior)$priors2amce[3:6], 'Prior' ='Prefers environment', datM[6,c('upper','lower')], 'Attribute'= paste('Message orientation:','                ','\n', '(Baseline = Pro-environment)\n', 'Pro-development')),
  data.frame(summary(sourcexprior)$priors1amce[3:6],'Prior' ='Prefers development',datS[3,c('upper','lower')], 'Attribute'= paste('Source orientation:','                    ','\n', '(Baseline = Pro-environment)\n', 'Pro-development')),  
  data.frame(summary(sourcexprior)$priors2amce[3:6], 'Prior' ='Prefers environment',datS[6,c('upper','lower')], 'Attribute'= paste('Source orientation:','                    ','\n', '(Baseline = Pro-environment)\n', 'Pro-development')))

colnames(cjIn) <- c('est','std. err', 'z','pr','Prior attitudes', 'upper','lower', 'Attribute')


cjIn$est <- as.numeric(cjIn$est)


######################
#real advertisement
#(pooled)
######################

#interaction with orientation
pooled$orientation <- factor(pooled$orientation, levels=c( "Pro-environment","Pro-development"))
trump6 <- lm_robust(estimate ~ Trump * orientation, data =subset(pooled,pooled$experiment=='POLADSTRUMP1S4'|pooled$experiment == 'POLADSTRUMP2S4') , weights = WEIGHT)

biden6  <- lm_robust(estimate ~ Biden* orientation, data = subset(pooled,pooled$experiment=='POLADSBIDEN1S4'|pooled$experiment == 'POLADSBIDEN2S4'), weights = WEIGHT)

patagonia6  <- lm_robust(estimate ~ Patagonia* orientation, data =subset(pooled,pooled$experiment=='POLADSPATAGONIA1S4'|pooled$experiment == 'POLADSPATAGONIA2S4'), weights = WEIGHT)

exxon6 <- lm_robust(estimate ~ Exxon* orientation, data = subset(pooled,pooled$experiment=='POLADSEXXON1S4'|pooled$experiment == 'POLADSEXXON2S4'), weights = WEIGHT)

power6  <- lm_robust(estimate ~ Power* orientation, data = subset(pooled,pooled$experiment=='POLADSPOWER1S4'|pooled$experiment == 'POLADSPOWER2S4'), weights = WEIGHT)


sierra6 <- lm_robust(estimate ~Sierra* orientation, data = subset(pooled,pooled$experiment=='POLADSSIERRA1S4'|pooled$experiment == 'POLADSSIERRA2S4'), weights = WEIGHT)


colgate6 <- lm_robust(estimate ~ Colgate* orientation, data = subset(pooled,pooled$experiment=='POLADSCOLGATE1S4'|pooled$experiment == 'POLADSCOLGATE2S4'), weights = WEIGHT)


msc6 <- lm_robust(estimate ~ MSC* orientation, data = subset(pooled,pooled$experiment=='POLADSMSC1S4'|pooled$experiment == 'POLADSMSC2S4'), weights = WEIGHT)


##############
#put lms into 
#table for plot

#interaction w/ orientation 

###politicans###
###f5
#gather data into small object
polI = multiplot(trump6,biden6, intercept = F, plot = F)

#reorder for plot display
polI$Coefficient = factor(polI$Coefficient, 
                          levels= rev(c("orientationPro-development",
                                        "Biden","Trump" ,
                                        "Biden:orientationPro-development" ,
                                        "Trump:orientationPro-development")))

#remove Pro-development because it shows up in both models (looks bad in plot)
poli <- subset(polI,polI$Coefficient != 'orientationPro-development')




###organizations###
###f5
orgI = multiplot(power6,sierra6, intercept = F, plot= F)

orgI$Coefficient = factor(orgI$Coefficient, 
                          levels= rev(c("orientationPro-development",
                                        "Power","Sierra",
                                        "Power:orientationPro-development",
                                        "Sierra:orientationPro-development")))

orgi <- subset(orgI,orgI$Coefficient != 'orientationPro-development')



###companies###
###f6
comI = multiplot(exxon6,msc6,patagonia6, colgate6,intercept = F, plot = F)

comI$Coefficient = factor(comI$Coefficient, 
                          levels=rev(c(  "orientationPro-development","Exxon","MSC",
                                         'Patagonia','Colgate'
                                         , "Exxon:orientationPro-development" 
                                         ,"MSC:orientationPro-development" , 
                                         "Patagonia:orientationPro-development" 
                                         ,"Colgate:orientationPro-development" )))

comi <- subset(comI, comI$Coefficient != 'orientationPro-development')


###########
#APPENDIX
###########




#######################
# A.conjoint experiment
#######################

# _Trump_ _Biden_ etc, are dummy indicating whether a respondent saw a high or low 
# political ad in that category
#unconditional
trump3 <- lm_robust(estimate ~ Trump, data =subset(pooled,pooled$experiment=='POLADSTRUMP1S4'|pooled$experiment == 'POLADSTRUMP2S4') , weights = WEIGHT, clusters = CaseId)

biden3  <- lm_robust(estimate ~ Biden, data = subset(pooled,pooled$experiment=='POLADSBIDEN1S4'|pooled$experiment == 'POLADSBIDEN2S4'), weights = WEIGHT, clusters = CaseId)

patagonia3  <- lm_robust(estimate ~ Patagonia, data =subset(pooled,pooled$experiment=='POLADSPATAGONIA1S4'|pooled$experiment == 'POLADSPATAGONIA2S4'), weights = WEIGHT, clusters = CaseId)

exxon3 <- lm_robust(estimate ~ Exxon, data = subset(pooled,pooled$experiment=='POLADSEXXON1S4'|pooled$experiment == 'POLADSEXXON2S4'), weights = WEIGHT, clusters = CaseId)

power3  <- lm_robust(estimate ~ Power, data = subset(pooled,pooled$experiment=='POLADSPOWER1S4'|pooled$experiment == 'POLADSPOWER2S4'), weights = WEIGHT, clusters = CaseId)


sierra3 <- lm_robust(estimate ~Sierra, data = subset(pooled,pooled$experiment=='POLADSSIERRA1S4'|pooled$experiment == 'POLADSSIERRA2S4'), weights = WEIGHT, clusters = CaseId)


colgate3 <- lm_robust(estimate ~ Colgate, data = subset(pooled,pooled$experiment=='POLADSCOLGATE1S4'|pooled$experiment == 'POLADSCOLGATE2S4'), weights = WEIGHT, clusters = CaseId)


msc3 <- lm_robust(estimate ~ MSC, data = subset(pooled,pooled$experiment=='POLADSMSC1S4'|pooled$experiment == 'POLADSMSC2S4'), weights = WEIGHT, clusters = CaseId)



######################
# B.real advertisement
# (pooled)
######################

