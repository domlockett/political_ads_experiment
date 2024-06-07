library(cowplot)
library(descr)
library(readr)
library(magick)
library(dplyr)
library(ggeffects)
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

#real ads lm
pooled    <- read.csv(paste0(local,'data/Real','/pooled.csv'))



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

#small function that exports tables to .tex file [lm-robust]
mod_texreg <- function(output.file, ...) {
  output <- capture.output(texreg(...))
  cat(paste(output, collapse = "\n"), "\n", file=output.file, append=F)
}




###########
#regulation opinion

#PID recode
tass2$pid <- NA 
tass2$pid[tass2$PartyID7 == -1 ] = NA
tass2$pid[tass2$PartyID7 == 4 ] = "Moderate/Independent/None"
tass2$pid[tass2$PartyID7 < 4 & tass2$PartyID7 > 0] = "Democrat"
tass2$pid[tass2$PartyID7 > 4 & tass2$PartyID7  < 8] = "Republican"

#WEIGHTED MEANS + CIS-function to extract wtd CIs
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
# 
#ADS4_A:T -> 137:156
#opinion survey questions of interest
Ads3 <- 137:156


#recode opinion survey 
for(i in 1:20){
  tass2[,Ads3[i]] <- to_factor(tass2[,Ads3[i]])
  tass2[,Ads3[i]]<- recode(tass2[,Ads3[i]], "1" = "Strongly agree",
                           "2" = "Agree", "3" = "Agree only a little", 
                           "4" = "Disagree only a little", '5' = 'Disagree',
                           '6' = 'Strongly disagree',"77" = NA_character_, 
                           "98" = NA_character_, "99" = NA_character_) 
  
}

#opinion questions from original survey
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
#cjoint needs baselines defined separately for unconditional and interacted
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


####################
#conjoint experiment
####################

################################################
#cjoint does not vibe with texreg or stargazer
#to overcome this we will make a regular lm and
#overwrite the data with cjoint data


########################
#Fig. 1#
#unconditional
#cjoint

unconditional <- amce(dv1 ~ image  + messageOrient + messageStrength + sourceOrient + sourceType  , respondent.id= 'CaseId', cluster=TRUE,  weights ='WEIGHT', data=cj, baseline= baselines, na.ignore=T)

#lm object to be overwritten
uncon <- summary(lm(dv1 ~  image  + messageOrient + messageStrength + sourceOrient + sourceType , data=cj, weights=WEIGHT))

#overwrite lm object with cjoint data
uncon$coefficients[2:11,c('Estimate','Std. Error','Pr(>|t|)')] <- rbind(cbind(summary(unconditional)$amce[,c('Estimate')],summary(unconditional)$amce[,c('Std. Err')],summary(unconditional)$amce[,c('Pr(>|z|)')]))



###############
#Fig. 2#
#Interactions

#################
#message X priors
#Fig. 2, P1
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
#Fig. 2, P2

sourcexprior <- amce(dv1 ~  image  + messageOrient + messageStrength + sourceOrient + sourceType   + priors + sourceOrient*priors, cluster=TRUE, respondent.id="CaseId", weights ='WEIGHT', data=cj, baseline= baselines, respondent.varying= 'priors', na.ignore=T)

#lm object to be overwritten
sourceP <- summary(lm(dv1 ~  image  + messageOrient + messageStrength + sourceOrient + sourceType   + priors + sourceOrient*priors,weights =WEIGHT, data=cj))

#overwrite lm object with cjoint data- main variables
sourceP$coefficients[2:11,c('Estimate','Std. Error','Pr(>|t|)')] <- rbind(cbind(summary(sourcexprior)$amce[,c('Estimate')],summary(sourcexprior)$amce[,c('Std. Err')],summary(sourcexprior)$amce[,c('Pr(>|z|)')]))

#overwrite lm object with cjoint data- interacted variables
sourceP$coefficients[12:13,c('Estimate','Std. Error','Pr(>|t|)')] <- rbind(cbind(c(as.matrix(summary(sourcexprior)[[3]]['Estimate']),as.matrix(summary(sourcexprior)[[4]]['Estimate'])),c(as.matrix(summary(sourcexprior)[[3]]['Std. Err']),as.matrix(summary(sourcexprior)[[4]]['Std. Err'])),c(as.matrix(summary(sourcexprior)[[3]]['Pr(>|z|)']),as.matrix(summary(sourcexprior)[[4]]['Pr(>|z|)']))))

#change rownames to prevent confusion
rownames(sourceP$coefficients)[12] <- 'sourceOrientPro-development:priorsPro-environment'


####################################################
#new model w both interactions
sourcexpriorxmess <- amce(dv1 ~   image  + messageOrient + messageStrength + sourceOrient + sourceType  + priors +  messageOrient*priors + sourceOrient*priors, cluster=TRUE, respondent.id="CaseId", weights ='WEIGHT', data=cj, baseline= baselines, respondent.varying= 'priors', na.ignore=T)

sourceP2 <- summary(lm(dv1 ~ image*messageStrength,weights =WEIGHT, data=cj))

#overwrite lm object with cjoint data- main variables
sourceP2$coefficients[2:11,c('Estimate','Std. Error','Pr(>|t|)')] <- rbind(cbind(summary(sourcexpriorxmess)$amce[,c('Estimate')],summary(sourcexpriorxmess)$amce[,c('Std. Err')],summary(sourcexpriorxmess)$amce[,c('Pr(>|z|)')]))
rownames(sourceP2$coefficients)[12]  <-'messageOrientPro-development:priorsPro-environment'
rownames(sourceP2$coefficients)[13] <- 'sourceOrientPro-development:priorsPro-environment'
#change rownames to prevent confusion
rownames(sourceP2$coefficients)[14] <- 'messageOrientPro-environment:priorsPro-environment'
rownames(sourceP2$coefficients)[15] <- 'sourceOrientPro-environment:priorsPro-environment'


#overwrite lm object with cjoint data- interacted variables
sourceP2$coefficients[12:15,c('Estimate','Std. Error','Pr(>|t|)')] <- rbind(cbind(c(as.matrix(summary(sourcexpriorxmess)[[3]]['Estimate']),as.matrix(summary(sourcexpriorxmess)[[4]]['Estimate'])),c(as.matrix(summary(sourcexpriorxmess)[[3]]['Std. Err']),as.matrix(summary(sourcexpriorxmess)[[4]]['Std. Err'])),c(as.matrix(summary(sourcexpriorxmess)[[3]]['Pr(>|z|)']),as.matrix(summary(sourcexpriorxmess)[[4]]['Pr(>|z|)']))))

rownames(sourceP2$coefficients)[1:11]<-rownames(sourceP$coefficients)[1:11]



#edited cj source code to extract data
plt1(mesxprior,plot.display='interaction',colors=c("paleturquoise4","lightpink3","sienna4", 'navajowhite3'))
datM <- plotdata#extract output
plt1(sourcexprior,plot.display='interaction',colors=c("paleturquoise4","lightpink3","sienna4", 'navajowhite3'))
datS <- plotdata
plt1(sourcexpriorxmess,plot.display='interaction',colors=c("paleturquoise4","lightpink3","sienna4", 'navajowhite3'))
datMS <- plotdata


d <- rbind(datM,datS)
d$group[d$group == "<NA>"] <- NA
d$facet <- factor(d$facet,levels=unique(d$facet))
d$var <- factor(d$var, levels=c(levels(d$var)[4:6],levels(d$var)[1:3]))
d$prior[d$facet=="Conditional on\npriors = Pro-development"]<- 'Pro-development'
d$prior[d$facet==levels(d$facet)[2]]<- 'Pro-environment'
d$`Prior attitudes:` <- d$prior

group.order=c('messageOrient','sourceOrient')
if (!is.null(group.order)){
  
  n.row <- length(unique(as.character(d$var)))
  order.var <- vector("character", length = n.row)
  
  i <- 1
  while (i<n.row){
    for (j in group.order){
      order.var[i] <- unique(as.character(d$var[d$var==gsub(" ","",j)]))
      i <- i+1
      temp.d <- d
      temp.d$group <- gsub(" ","",temp.d$group)
      temp.d <- subset(temp.d, group==gsub(" ","",j))
      
      temp.var <- unique((as.character(temp.d$var)))
      order.var[i:(i+length(temp.var)-1)] <- temp.var
      i <- i+length(temp.var)
    }
  }
  order.var <- rev(order.var)
  
  order.df <- data.frame(order.var, 1:length(order.var))
  colnames(order.df) <- c("var", "order")
  
  d$var <- factor(d$var, levels=order.var)
  
  d <- merge(d, order.df, by.x="var", by.y="var", suffixes=c("",""))
  
}







################
#opinions survey
#################
#Fig. 3
#################
# 
sub <- c(144,141,143,155,156,149,148,154,150,151,145,146)#column numbers, custom order
# comp <- c()
# for(i in 1:length(sub)){
#   comp <- c(comp, which(Ads3==sub[i]))
# }
# 
# #comparison dataset to make sure the column numbers and column topics match up to the plot
# or <- data.frame(matrix(nrow=12, ncol =2))
# or[,1] <- topics3[comp]
# or[,2] <- as.numeric(Ads3[comp])


new <- data.frame(matrix(nrow=12, ncol=4))#storage object
colnames(new)<-c('est','upper','lower','std. err')#grabbing means and cis
for(i in 1:length(sub)){#go through each column
  #weighted cis Real Ad opinions with frequency and percentages
  new[i,] <- wtd.ci(as.numeric(as.factor(tass2[,sub[i]])), weights=tass2$WEIGHT)
}

#########################
#real advertisement
#(pooled from two surveys)
#########################

#pooled$estimate <- ifelse(pooled$estimate<4,0,1)

# _Trump_ _Biden_ etc dummy indicating whether respondent saw
#  high or low political ad in that experiment
# PID controls
trump8 <- lm_robust(estimate ~ Trump + pid, data =subset(pooled,pooled$experiment=='POLADSTRUMP1S4'|pooled$experiment == 'POLADSTRUMP2S4') , weights = WEIGHT )

biden8  <- lm_robust(estimate ~ Biden+ pid, data = subset(pooled,pooled$experiment=='POLADSBIDEN1S4'|pooled$experiment == 'POLADSBIDEN2S4'), weights = WEIGHT )

patagonia8  <- lm_robust(estimate ~ Patagonia+ pid, data =subset(pooled,pooled$experiment=='POLADSPATAGONIA1S4'|pooled$experiment == 'POLADSPATAGONIA2S4'), weights = WEIGHT )

exxon8 <- lm_robust(estimate ~ Exxon+ pid, data = subset(pooled,pooled$experiment=='POLADSEXXON1S4'|pooled$experiment == 'POLADSEXXON2S4'), weights = WEIGHT )

power8  <- lm_robust(estimate ~ Power+ pid, data = subset(pooled,pooled$experiment=='POLADSPOWER1S4'|pooled$experiment == 'POLADSPOWER2S4'), weights = WEIGHT )


sierra8 <- lm_robust(estimate ~Sierra+ pid, data = subset(pooled,pooled$experiment=='POLADSSIERRA1S4'|pooled$experiment == 'POLADSSIERRA2S4'), weights = WEIGHT )


colgate8 <- lm_robust(estimate ~ Colgate+ pid, data = subset(pooled,pooled$experiment=='POLADSCOLGATE1S4'|pooled$experiment == 'POLADSCOLGATE2S4'), weights = WEIGHT )


msc8 <- lm_robust(estimate ~ MSC+ pid, data = subset(pooled,pooled$experiment=='POLADSMSC1S4'|pooled$experiment == 'POLADSMSC2S4'), weights = WEIGHT )


########################
# stacking experiments#

# non-political companies dropped to better resemble conjoint

pooled$political <- NA

#experiment name (pooled$MSC) is binary indicating if resp saw high or low political ad
#for each experiment we put dv into 1 variable 'political'
for(i in 1:nrow(pooled)){
  if(pooled$experiment[i] == 'POLADSTRUMP2S4'|pooled$experiment[i] =="POLADSTRUMP1S4"){
    pooled$political[i] <- pooled$Trump[i]
  }  else if(pooled$experiment[i] == 'POLADSBIDEN1S4' | pooled$experiment[i] ==  "POLADSBIDEN2S4"){
    pooled$political[i] <- pooled$Biden[i]
  } else if(pooled$experiment[i] == 'POLADSEXXON1S4'| pooled$experiment[i] == "POLADSEXXON2S4" ){
    pooled$political[i] <- pooled$Exxon[i]
  } else if(pooled$experiment[i] == 'POLADSPATAGONIA1S4'| pooled$experiment[i] == "POLADSPATAGONIA2S4" ){
    pooled$political[i] <- pooled$Patagonia[i]
  } else if(pooled$experiment[i] == 'POLADSPOWER1S4'| pooled$experiment[i] == "POLADSPOWER2S4" ){
    pooled$political[i] <- pooled$Power[i]
  } else if(pooled$experiment[i] == 'POLADSSIERRA1S4'| pooled$experiment[i] == "POLADSSIERRA2S4" ){
    pooled$political[i] <- pooled$Sierra[i]
  } #else if(pooled$experiment[i] == 'POLADSCOLGATE1S4'| pooled$experiment[i] == "POLADSCOLGATE2S4"){
  #pooled$political[i] <- pooled$Colgate[i]
  #} else if(pooled$experiment[i] == 'POLADSMSC1S4'| pooled$experiment[i] == 'POLADSMSC2S4' ){
  # pooled$political[i] <- pooled$MSC[i]
  # }
}


ra <- pooled

#create new binary classifiers for pol, org, comp
ra$candidate <- NA
for(i in 1:nrow(ra)){
  if(ra$experiment[i] == "POLADSTRUMP1S4"|ra$experiment[i] =="POLADSTRUMP2S4"|
     ra$experiment[i] == "POLADSBIDEN1S4"|ra$experiment[i] =="POLADSBIDEN2S4"){
    ra$candidate[i] <- 1
  } else{ ra$candidate[i] <-0}
}




ra$organization <- NA
for(i in 1:nrow(ra)){
  if(ra$experiment[i] == "POLADSPOWER1S4"|ra$experiment[i] =="POLADSPOWER2S4" |
     ra$experiment[i] == "POLADSSIERRA1S4"|ra$experiment[i] =="POLADSSIERRA2S4"){
    ra$organization[i] <- 1
  } else{ ra$organization[i] <-0}
}

ra$company <- NA
for(i in 1:nrow(ra)){
  if(ra$experiment[i] == "POLADSEXXON1S4"|ra$experiment[i] =="POLADSEXXON2S4"|
     ra$experiment[i] == "POLADSPATAGONIA1S4"|ra$experiment[i] =="POLADSPATAGONIA2S4"){ 
    #|
    # ra$experiment[i] == "POLADSCOLGATE1S4"|ra$experiment[i] =="POLADSCOLGATE2S4"|
    # ra$experiment[i] == "POLADSMSC1S4"|ra$experiment[i] =="POLADSMSC2S4"){
    ra$company[i] <- 1
  } else{ ra$company[i] <-0}
  
}

ra$pol <- ifelse(ra$political== 1,'Strong','Weak')
ra$pol <- factor(ra$pol, levels = c('Weak','Strong'))


ra$sourceOrient <- NA
for(i in 1:nrow(ra)){
  if(ra$experiment[i] == "POLADSEXXON1S4" | 
     ra$experiment[i] == "POLADSEXXON2S4" |
     # ra$experiment[i] == "POLADSMSC1S4" |
     # ra$experiment[i] == "POLADSMSC2S4" |
     ra$experiment[i] == "POLADSTRUMP1S4" |
     ra$experiment[i] == "POLADSTRUMP2S4" | 
     ra$experiment[i] == "POLADSPOWER1S4" | 
     ra$experiment[i] == "POLADSPOWER2S4" ){
    ra$sourceOrient[i] <- 1
  } else{ ra$sourceOrient[i] <-0}
}
ra$pid <- factor(ra$pid, levels = c('Democrat','Republican'))
ra$orientation<- factor(ra$orientation, levels = c('Pro-environment','Pro-development'))

ra$sourceOrient <- ifelse(ra$sourceOrient== 1,'Pro-development','Pro-environment')
ra$sourceOrient<- factor(ra$sourceOrient, levels = c('Pro-environment','Pro-development'))

poolFit <- lm_robust(estimate ~ candidate+organization+ pol +sourceOrient, data = subset(ra,ra$experiment[i] !="POLADSMSC1S4" |ra$experiment[i] != "POLADSMSC2S4"), weights = WEIGHT, clusters = CaseId, fixed_effects = ~CaseId, se_type = 'stata')

interFit <- lm_robust(estimate ~ pol*candidate+pol*organization+ sourceOrient, data = ra, weights = WEIGHT, clusters = CaseId, fixed_effects = ~CaseId, se_type = 'stata')

partyFit <- lm_robust(estimate ~ sourceOrient*pid+candidate+organization+ pol, data = ra, weights = WEIGHT, clusters = CaseId, se_type = 'stata')

