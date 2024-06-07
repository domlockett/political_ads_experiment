
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

#regulation opinions
tass2 <- read.csv(paste0(local,'Data/TASS 05/8557_WUSTL-Weidenbaum05_Redacted.csv'))

#conjoint
cj <- read.csv(file = paste0(local,'data/Conjoint/CJ.csv'), header = TRUE,
               stringsAsFactors = T)
#real ads means
datc <- read.csv(paste0(local,'data/Real', '/datc.csv'))
datc <- datc[,c(2:5,7,8)]

#real ads lm
pooled     <- read.csv(paste0(local,'data/Real','/pooled.csv'))



####################
#Functions objects 
#and transformations
#for plots tables
#and models
####################

####################
#global custom theme


label.baseline = TRUE
text.size = 15
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
            "Political ads with false content should be allowed, but labeled with a warning",
            "Companies should not try to label political ads as false, even if they containing misleading information",
            "No political ads should be allowed at all",
            "Political candidates should always be allowed to post ads without restriction",
            "All ads on controversial topics should be removed, regardless of source",
            "Ads that encourage voting should always be permitted",
            "Ads from news organizations should be exempted from regulations of political advertising",
            "Political ads from news organizations should be subject to the same regulations as ads from other sources",
            "Ads that discredit voting by mail should not be permitted on social media",
            "Political ads should include information about who paid for them",
            "Political advertisers should not be required to disclose their funding sources",
            "Microtargeting political ads to specific audiences based on things like age, gender, and race should not be permitted.",
            "Political advertisers should be allowed to select who sees their ads",
            "Ads that contain misinformation about a candidate should be removed",
            "All ads about candidates should be allowed, even if they contain false content",
            "Companies should provide a publicly accessible database of all political ads",
            "Ads about candidates or upcoming elections should be banned, but ads about social issues (e.g., guns or abortion) should still be allowed",
            "Ads from certain sources like political action committees (PACs) and dark money organizations should be banned")

###########
#regulation 
#opinon
###########




sub <- c(144,141,143,155,156,149,148,154,150,151,145,146)#column numbers, custom order


remads <- Ads3[Ads3%in%sub ==F]
remtopic <- topics3[Ads3%in%sub==F]


new <- data.frame(matrix(nrow=8, ncol=4))#storage object
colnames(new)<-c('est','upper','lower','std. err')#grabbing means and cis
for(i in 1:length(remads)){#go through each column
  #weighted cross tabs Real Ad opinions with frequency and percentages
  new[i,] <- wtd.ci(as.numeric(as.factor(tass2[,remads[i]])), weights=tass2$WEIGHT)
}

#neat names for plots order the same as _sub_ ordering
new['question'] <- c('Ads with false information banned',
                     'Ads with false information allowed',
                     'Ads with false content include warning',
                     'Companies should not label false information',
                     'Candidates always allowed to post ads',
                     'Ads discrediting vote by mail banned',
                     'Ads with misinformation banned',
                     'All ads about candidates allowed')


new$question <- factor(new$question, levels= rev(new$question))#forced factoring so its not alphabetical

scale <- c('Strongly \n agree','Agree','Agree\n a little','Disagree\n a little','Disagree', 'Strongly \n disagree')#x-axis names

######
#table
#F1
new1 <-new
colnames(new1) <- c('Mean','Upper 95% CI','Lower 95% CI','Standard Err.', 'Question')
a01 <- stargazer(new1, out= paste0(local,'publish/tables/','ad-reg-opin.tex'), header =F, summary = F, title =
                   'Opinions on proposed social media advertising regulation', label = 'table:ad-reg', digits =2)

#####

###########
#Regulation opin by PID
###########

sub <- c(144,141,143,155,156,149,148,154,150,151,145,146)
sub <- rep(sub,3)
comp <- c()
for(i in 1:length(sub)){
  comp <- c(comp, which(Ads3==sub[i]))
}

opins <- data.frame(matrix(nrow=36, ncol=5))
a =b=c = data.frame(matrix(nrow=12,ncol=5))
for(i in 1:12){
  #weighted cross tabs of PID and Real Ad opinions with frequency and percentages
  a[i,] <- c(wtd.ci(as.numeric(as.factor(tass2[tass2$pid=="Democrat" ,sub[i]])), weights=tass2$WEIGHT[tass2$pid=="Democrat"]), 'Democrat')
  b[i,] <- c(wtd.ci(as.numeric(as.factor(tass2[tass2$pid=="Republican" ,sub[i]])), weights=tass2$WEIGHT[tass2$pid=="Republican"]),'pid'='Republican')
  c[i,] <- c(wtd.ci(as.numeric(as.factor(tass2[tass2$pid=="Moderate/Independent/None" ,sub[i]])), weights=tass2$WEIGHT[tass2$pid=="Moderate/Independent/None"]), 'pid'='Moderate/Independent/None')
  opins <- rbind(a,b,c)
  
}

for(i in 1:4){
  opins[,i]<-as.numeric(opins[,i])
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

cj$pid <- factor(cj$pid, 
                           levels = c('Democrat',
                                      'Republican'))

cj$messageStrength <- factor(cj$messageStrength,
                             levels = c('Weak',
                                        'Moderate',
                                        'Strong'))

cj$sourceOrient  <- factor(cj$sourceOrient, 
                           levels = c('Pro-environment',
                                      'Pro-development'))
cj$pid  <- factor(cj$pid, 
                           levels = c('Democrat',
                                      'Republican'))

cj$sourceType  <- factor(cj$sourceType, levels = c('Company',
                                                   'Candidate',
                                                   'Organization'))


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


#############
#message-pid
#############
#cjoint
mesxpid <- amce(dv1 ~ image  + messageOrient + messageStrength + sourceOrient + sourceType   +  pid + messageOrient*pid, cluster=TRUE, respondent.id="CaseId", weights ='WEIGHT', data=cj, baseline= bases, respondent.varying = 'pid', na.ignore=T)

#lm object to be overwritten
mpid <- summary(lm(dv1 ~  image  + messageOrient + messageStrength + sourceOrient + sourceType   +  pid + messageOrient*pid, weights =WEIGHT, data=cj))

# #overwrite lm object with cjoint data- main variables
mpid$coefficients[2:11,c('Estimate','Std. Error','Pr(>|t|)')] <- rbind(cbind(summary(mesxpid)$amce[,c('Estimate')],summary(mesxpid)$amce[,c('Std. Err')],summary(mesxpid)$amce[,c('Pr(>|z|)')]))

# #overwrite lm object with cjoint data- interacted variables
mpid$coefficients[12:13,c('Estimate','Std. Error','Pr(>|t|)')] <- rbind(cbind(c(as.matrix(summary(mesxpid)[[3]]['Estimate']),as.matrix(summary(mesxpid)[[4]]['Estimate'])),c(as.matrix(summary(mesxpid)[[3]]['Std. Err']),as.matrix(summary(mesxpid)[[4]]['Std. Err'])),c(as.matrix(summary(mesxpid)[[3]]['Pr(>|z|)']),as.matrix(summary(mesxpid)[[4]]['Pr(>|z|)']))))

#change rownames to prevent confusion
rownames(mpid$coefficients)[12] <- 'messageOrientPro-env:pidRepublican'
################################################

##############
#source-pid
##############
#cjoint
sourcexpid <- amce(dv1 ~ image  + messageOrient + messageStrength + sourceOrient + sourceType   +  pid + sourceOrient*pid, cluster=TRUE, respondent.id="CaseId", weights ='WEIGHT', data=cj, baseline= bases, respondent.varying= 'pid', na.ignore=T)

#lm object to be overwritten
sPid <- summary(lm(dv1 ~ image  + messageOrient + messageStrength + sourceOrient + sourceType   +  pid + sourceOrient*pid, weights =WEIGHT, data=cj))

#overwrite lm object with cjoint data- main variables
sPid$coefficients[2:11,c('Estimate','Std. Error','Pr(>|t|)')] <- rbind(cbind(summary(sourcexpid)$amce[,c('Estimate')],summary(sourcexpid)$amce[,c('Std. Err')],summary(sourcexpid)$amce[,c('Pr(>|z|)')]))

#overwrite lm object with cjoint data- interacted variables
sPid$coefficients[12:13,c('Estimate','Std. Error','Pr(>|t|)')] <- rbind(cbind(c(as.matrix(summary(sourcexpid)[[3]]['Estimate']),as.matrix(summary(sourcexpid)[[4]]['Estimate'])),c(as.matrix(summary(sourcexpid)[[3]]['Std. Err']),as.matrix(summary(sourcexpid)[[4]]['Std. Err'])),c(as.matrix(summary(sourcexpid)[[3]]['Pr(>|z|)']),as.matrix(summary(sourcexpid)[[4]]['Pr(>|z|)']))))

#change rownames to prevent confusion
rownames(sPid$coefficients)[12] <- 'sourcePidRepublicans'


#edited cj source code to extract data
plt(mesxpid,plot.display='interaction')#run code
datM <- PLOTDATA#extract output
plt(sourcexpid,plot.display='interaction')
datS <- PLOTDATA

#this grabs the interaction effects from the 2 models for this plot
#getting the cis would be more difficult so we do some tricks 

######################
#real advertisement
#(pooled)
######################


#########################
# descriptive statistics

#Distribution of ratings ad politicalness
experiment <-c('POLADSTRUMP1S4', 'POLADSTRUMP2S4', 'POLADSBIDEN1S4', 'POLADSBIDEN2S4','POLADSPOWER1S4','POLADSPOWER2S4','POLADSSIERRA1S4','POLADSSIERRA2S4', 'POLADSEXXON1S4', 'POLADSEXXON2S4', 'POLADSPATAGONIA1S4', 'POLADSPATAGONIA2S4','POLADSCOLGATE1S4','POLADSCOLGATE2S4','POLADSMSC1S4','POLADSMSC2S4')

des <-  as.data.frame(matrix(nrow=80))
colnames(des)<-'freq'
freq <- c()
for(i in 1:16){
  freq <-  c(freq,(table(pooled$estimate[pooled$experiment==experiment[i]])))
}


for(i in 1:16){
  print( experiment[i])
  print(table(pooled$estimate[pooled$experiment==experiment[i]]))
}
des$freq <- freq
des$exp <- rep(experiment,each=5)
des$name <-rep(c('Trump','Biden','Power','Sierra','Exxon','Patagonia','Colgate', 'MSC'),each=10)
des$lvl <- rep(c('High','Low'),each=5,len=80)
des$rating <- rep(1:5,16)

des1<- des[c(1:40),]
des2<- des[c(41:80),]

des1$name = factor(des1$name, levels=(c("Biden","Trump",
                                        "Power", "Sierra")))
des2$name = factor(des2$name, levels=(c("Exxon",'Patagonia',"MSC","Colgate")))

des1$lvl = factor(des1$lvl, levels=(c("Low",
                                      "High")))
des2$lvl = factor(des2$lvl, levels=(c("Low",
                                      "High")))





# _Trump_ _Biden_ etc, are dummy indicating whether a respondent saw a high or low 
# political ad in that category
#unconditional
trump3 <- lm_robust(estimate ~ Trump, data =subset(pooled,pooled$experiment=='POLADSTRUMP1S4'|pooled$experiment == 'POLADSTRUMP2S4') , weights = WEIGHT )

biden3  <- lm_robust(estimate ~ Biden, data = subset(pooled,pooled$experiment=='POLADSBIDEN1S4'|pooled$experiment == 'POLADSBIDEN2S4'), weights = WEIGHT )

patagonia3  <- lm_robust(estimate ~ Patagonia, data =subset(pooled,pooled$experiment=='POLADSPATAGONIA1S4'|pooled$experiment == 'POLADSPATAGONIA2S4'), weights = WEIGHT )

exxon3 <- lm_robust(estimate ~ Exxon, data = subset(pooled,pooled$experiment=='POLADSEXXON1S4'|pooled$experiment == 'POLADSEXXON2S4'), weights = WEIGHT )

power3  <- lm_robust(estimate ~ Power, data = subset(pooled,pooled$experiment=='POLADSPOWER1S4'|pooled$experiment == 'POLADSPOWER2S4'), weights = WEIGHT )


sierra3 <- lm_robust(estimate ~Sierra, data = subset(pooled,pooled$experiment=='POLADSSIERRA1S4'|pooled$experiment == 'POLADSSIERRA2S4'), weights = WEIGHT )


colgate3 <- lm_robust(estimate ~ Colgate, data = subset(pooled,pooled$experiment=='POLADSCOLGATE1S4'|pooled$experiment == 'POLADSCOLGATE2S4'), weights = WEIGHT )


msc3 <- lm_robust(estimate ~ MSC, data = subset(pooled,pooled$experiment=='POLADSMSC1S4'|pooled$experiment == 'POLADSMSC2S4'), weights = WEIGHT )





trump8 <- lm_robust(estimate ~ Trump + pid, data =subset(pooled,pooled$experiment=='POLADSTRUMP1S4'|pooled$experiment == 'POLADSTRUMP2S4') , weights = WEIGHT )

biden8  <- lm_robust(estimate ~ Biden+ pid, data = subset(pooled,pooled$experiment=='POLADSBIDEN1S4'|pooled$experiment == 'POLADSBIDEN2S4'), weights = WEIGHT )

patagonia8  <- lm_robust(estimate ~ Patagonia+ pid, data =subset(pooled,pooled$experiment=='POLADSPATAGONIA1S4'|pooled$experiment == 'POLADSPATAGONIA2S4'), weights = WEIGHT )

exxon8 <- lm_robust(estimate ~ Exxon+ pid, data = subset(pooled,pooled$experiment=='POLADSEXXON1S4'|pooled$experiment == 'POLADSEXXON2S4'), weights = WEIGHT )

power8  <- lm_robust(estimate ~ Power+ pid, data = subset(pooled,pooled$experiment=='POLADSPOWER1S4'|pooled$experiment == 'POLADSPOWER2S4'), weights = WEIGHT )


sierra8 <- lm_robust(estimate ~Sierra+ pid, data = subset(pooled,pooled$experiment=='POLADSSIERRA1S4'|pooled$experiment == 'POLADSSIERRA2S4'), weights = WEIGHT )


colgate8 <- lm_robust(estimate ~ Colgate+ pid, data = subset(pooled,pooled$experiment=='POLADSCOLGATE1S4'|pooled$experiment == 'POLADSCOLGATE2S4'), weights = WEIGHT )


msc8 <- lm_robust(estimate ~ MSC+ pid, data = subset(pooled,pooled$experiment=='POLADSMSC1S4'|pooled$experiment == 'POLADSMSC2S4'), weights = WEIGHT )




trump9 <- lm_robust(estimate ~ Trump * pid, data =subset(pooled,pooled$experiment=='POLADSTRUMP1S4'|pooled$experiment == 'POLADSTRUMP2S4') , weights = WEIGHT )

biden9  <- lm_robust(estimate ~ Biden* pid, data = subset(pooled,pooled$experiment=='POLADSBIDEN1S4'|pooled$experiment == 'POLADSBIDEN2S4'), weights = WEIGHT )

patagonia9  <- lm_robust(estimate ~ Patagonia* pid, data =subset(pooled,pooled$experiment=='POLADSPATAGONIA1S4'|pooled$experiment == 'POLADSPATAGONIA2S4'), weights = WEIGHT )

exxon9 <- lm_robust(estimate ~ Exxon* pid, data = subset(pooled,pooled$experiment=='POLADSEXXON1S4'|pooled$experiment == 'POLADSEXXON2S4'), weights = WEIGHT )

power9  <- lm_robust(estimate ~ Power* pid, data = subset(pooled,pooled$experiment=='POLADSPOWER1S4'|pooled$experiment == 'POLADSPOWER2S4'), weights = WEIGHT )


sierra9 <- lm_robust(estimate ~Sierra* pid, data = subset(pooled,pooled$experiment=='POLADSSIERRA1S4'|pooled$experiment == 'POLADSSIERRA2S4'), weights = WEIGHT )


colgate9 <- lm_robust(estimate ~ Colgate* pid, data = subset(pooled,pooled$experiment=='POLADSCOLGATE1S4'|pooled$experiment == 'POLADSCOLGATE2S4'), weights = WEIGHT )


msc9 <- lm_robust(estimate ~ MSC* pid, data = subset(pooled,pooled$experiment=='POLADSMSC1S4'|pooled$experiment == 'POLADSMSC2S4'), weights = WEIGHT )







#Pooled real ad experimentsl  

pooled$political <- NA

for(i in 1:nrow(pooled)){
  if(pooled$experiment[i] == 'POLADSTRUMP1S4'|pooled$experiment[i] =="POLADSTRUMP2S4"){
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

ra$pol <- ifelse(ra$political== 1,'High','Low')
ra$pol <- factor(ra$pol, levels = c('Low','High'))

# ra$nonpolcom <- NA
# for(i in 1:nrow(ra)){
#   if(ra$experiment[i] == "POLADSCOLGATE1S4"|ra$experiment[i] =="POLADSCOLGATE2S4"|
#      ra$experiment[i] == "POLADSMSC1S4"|ra$experiment[i] =="POLADSMSC2S4"){
#     ra$nonpolcom[i] <- 1
#   } else{ ra$nonpolcom[i] <-0}
#   
# }
  
# ra$polcom <- NA
# for(i in 1:nrow(ra)){
#   if(ra$experiment[i] == "POLADSEXXON1S4"|ra$experiment[i] =="POLADSEXXON2S4"|
#     ra$experiment[i] == "POLADSPATAGONIA1S4"|ra$experiment[i] =="POLADSPATAGONIA2S4"){
#     ra$polcom[i] <- 1
#   } else{ ra$polcom[i] <-0}
#   
# }


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

poolFit <- lm_robust(estimate ~ candidate+organization+ pol +sourceOrient, data = ra, weights = WEIGHT, clusters = CaseId, fixed_effects = ~CaseId, se_type = 'stata')

interFit <- lm_robust(estimate ~ pol*candidate+pol*organization+ sourceOrient, data = ra, weights = WEIGHT, clusters = CaseId, fixed_effects = ~CaseId, se_type = 'stata')

partyFit <- lm_robust(estimate ~ sourceOrient*pid+candidate+organization+ pol, data = ra, weights = WEIGHT, clusters = CaseId, se_type = 'stata')

screenreg(list(poolFit, interFit, partyFit), include.ci=F)

poolcand <- lm_robust(estimate ~ pol*candidate , data = ra, weights = WEIGHT, clusters = CaseId)

poolorg <- lm_robust(estimate ~ pol*organization, data = ra, weights = WEIGHT, clusters = CaseId)

poolcomp <- lm_robust(estimate ~ pol*company, data = ra, weights = WEIGHT, clusters = CaseId)


screenreg(list(poolFit, poolcand,poolorg,poolcomp),reorder.coef =c(1,2,3,5,7,4,6,8,9), include.ci=F,custom.model.names = c('Pol on est','Pol x cand','Pol x org','Pol x comp'))



