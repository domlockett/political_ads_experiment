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
library(fastDummies)
library(lme4)
library(coefplot)
library(ggtext)
local <- 'C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/'

################
#Cjoint 
#transformations
################
################




#Data for realads
polAdORIG <- read.csv(paste0(local,'Data/TASS 04/8557_WUSTL-Weidenbaum04_MAIN.csv'))

polAdREDO <- read.csv(paste0(local,'Data/TASS04Reask/8557_WUSTL-Weidenbaum04Reask.csv'))


#Create pid
polAdORIG$pid <- recode_factor(polAdORIG$P_PARTYID7, 'Strong Democrat' =  "Democrat", 
                               'Weak Democrat' = 'Democrat', '3' = 'Democrat',  
                               '5'= 'Republican', '6'= 'Republican', '7' = 'Republican')

polAdREDO$pid <- recode_factor(polAdREDO$PartyID7, '1' = "Democrat", 
                               '2' = 'Democrat', '3' = 'Democrat', 
                               '5'= 'Republican', '6'= 'Republican', '7' = 'Republican')



#Make a list so that I can loop over 
#experiment versions
polAd <- list()
polAd[[1]] <- polAdORIG
polAd[[2]] <- polAdREDO



#flip the order so that lower means less political
for(d in 1:2){
  range1 <- which(colnames(polAd[[d]])=='POLADSTRUMP1S4'):
    which(colnames(polAd[[d]])=='POLADSMSC2S4')
  for(i in range1){
    #flip rating order and switch skipped/refused/don't knows to NA
    polAd[[d]][,i][polAd[[d]][,i]>5] <- NA
    polAd[[d]][,i] <- 6-polAd[[d]][,i]
  }
}

#################
#Make orientation


#Add an indicator for the orientation of individual
#tercile split from additive index
#lower values pro-environment
# 
# for(d in 1:2){
#   polAd[[d]]$globalWarming <- NA
#   polAd[[d]]$globalWarming <- recode(polAd[[d]]$POLADSGLOBALWARMINGS4, '1' = "1", '3' = '2', '2' = '3', .default = NA_character_)#recode so 2 is indifference and drop higher values (NAS)
#   
#   polAd[[d]]$globalWarming <- as.numeric(polAd[[d]]$globalWarming)
#   polAd[[d]]$regulate <- polAd[[d]]$POLADSREGULATIONS4
#   
#   polAd[[d]]$regulate[polAd[[d]]$regulate >7 ] <- NA #drop nas
#   
#   polAd[[d]]$en <-  (polAd[[d]]$regulate + polAd[[d]]$globalWarming) #add these 2 indicators together
#   polAd[[d]]$prior <-cut(polAd[[d]]$en, breaks = quantile(polAd[[d]]$en, probs = seq(0, 1, length.out = 3 + 1), na.rm=T), labels = 1:3, include.lowest = TRUE) #tercile split
#   
#   polAd[[d]]$priors <- NA
#   polAd[[d]]$priors[polAd[[d]]$prior==3] <- 'Pro-oil' #useful names
#   polAd[[d]]$priors[polAd[[d]]$prior==2] <- 'Indifferent'
#   polAd[[d]]$priors[polAd[[d]]$prior==1] <- 'Pro-environment'
# 
# }

#Add an indicator for the orientation of individual
#Median split from additive index
#lower values pro-environment
polAd[[1]]$globalWarming <- recode(polAd[[1]]$POLADSGLOBALWARMINGS4, '1' = "1", '3' = '2', '2' = '3', .default = NA_character_)#recode so 2 is indifference and drop higher values (NAS)

polAd[[1]]$globalWarming <- as.numeric(polAd[[1]]$globalWarming)
polAd[[1]]$regulate <- polAd[[1]]$POLADSREGULATIONS4

polAd[[1]]$regulate[polAd[[1]]$regulate >7 ] <- NA #drop nas

polAd[[1]]$en <-  (polAd[[1]]$regulate + polAd[[1]]$globalWarming) #add these 2 indicators together
polAd[[1]]$prior <-cut(polAd[[1]]$en, breaks = quantile(polAd[[1]]$en, probs = seq(0, 1, length.out = 2 + 1), na.rm=T), labels = 1:2, include.lowest = TRUE) #med split

polAd[[1]]$priorsMED <- NA
polAd[[1]]$priorsMED[polAd[[1]]$prior==2] <- 'Pro-development' #useful names
polAd[[1]]$priorsMED[polAd[[1]]$prior==1] <- 'Pro-environment'

polAd[[2]]$globalWarming <- recode(polAd[[2]]$POLADSGLOBALWARMINGS4, '1' = "1", '3' = '2', '2' = '3', .default = NA_character_)#recode so 2 is indifference and drop higher values (NAS)

polAd[[2]]$globalWarming <- as.numeric(polAd[[2]]$globalWarming)
polAd[[2]]$regulate <- polAd[[2]]$POLADSREGULATIONS4

polAd[[2]]$regulate[polAd[[2]]$regulate >7 ] <- NA #drop nas

polAd[[2]]$en <-  (polAd[[2]]$regulate + polAd[[2]]$globalWarming) #add these 2 indicators together
polAd[[2]]$prior <-cut(polAd[[2]]$en, breaks = quantile(polAd[[2]]$en, probs = seq(0, 1, length.out = 2 + 1), na.rm=T), labels = 1:2, include.lowest = TRUE) #med split

polAd[[2]]$priorsMED <- NA
polAd[[2]]$priorsMED[polAd[[2]]$prior==2] <- 'Pro-development' #useful names
polAd[[2]]$priorsMED[polAd[[2]]$prior==1] <- 'Pro-environment'




#make sure no overlapping
#CASEIDS for clusered SEs
polAd[[2]]$CaseId <- polAd[[2]]$CaseId +5000 #add 5000 to redo 


# Datasets with only the relevant variables
 
orig <- cbind(polAd[[1]][,which(colnames(polAd[[1]])=='POLADSTRUMP1S4'):
                           which(colnames(polAd[[1]])=='POLADSMSC2S4')], 
              'WEIGHT' = polAd[[1]][,'WEIGHT'], pid = polAd[[1]][,'pid'], orientation = polAd[[1]]$priorsMED, CaseId = polAd[[1]]$CaseId)

redo<- cbind(polAd[[2]][,which(colnames(polAd[[2]])=='POLADSTRUMP1S4'):
                          which(colnames(polAd[[2]])=='POLADSMSC2S4')], 
             'WEIGHT' =polAd[[2]][,'WEIGHT'], pid = polAd[[2]][,'pid'], orientation = polAd[[2]]$priorsMED, CaseId = polAd[[2]]$CaseId)

###########
#original
###########


#Make new variables  with a dummy for whether a 
#participant saw the high or low political ads
orig$Trump <- NA
for(i in 1:nrow(orig)){
  if(is.na(orig$POLADSTRUMP1S4[i])==F){
    orig$Trump[i] <- 1
  }else if (is.na(orig$POLADSTRUMP2S4[i]) ==F){
    orig$Trump[i] <- 0
  }
}  


orig$Biden <- c()
for(i in 1:nrow(orig)){
  if(is.na(orig$POLADSBIDEN1S4[i])==F){
    orig$Biden[i] <- 1
  }else if (is.na(orig$POLADSBIDEN2S4[i]) ==F){
    orig$Biden[i] <- 0
  }
}  



orig$Exxon <- c()
for(i in 1:nrow(orig)){
  if(is.na(orig$POLADSEXXON1S4[i])==F){
    orig$Exxon[i] <- 1
  }else if (is.na(orig$POLADSEXXON2S4[i]) ==F){
    orig$Exxon[i] <- 0
  }
}  



orig$Patagonia <- c()
for(i in 1:nrow(orig)){
  if(is.na(orig$POLADSPATAGONIA1S4[i])==F){
    orig$Patagonia[i] <- 1
  }else if (is.na(orig$POLADSPATAGONIA2S4[i]) ==F){
    orig$Patagonia[i] <- 0
  }
}  



orig$Power <- c()
for(i in 1:nrow(orig)){
  if(is.na(orig$POLADSPOWER1S4[i])==F){
    orig$Power[i] <- 1
  }else if (is.na(orig$POLADSPOWER2S4[i]) ==F){
    orig$Power[i] <- 0
  }
}  



orig$Sierra <- c()
for(i in 1:nrow(orig)){
  if(is.na(orig$POLADSSIERRA1S4[i])==F){
    orig$Sierra[i] <- 1
  }else if (is.na(orig$POLADSSIERRA2S4[i]) ==F){
    orig$Sierra[i] <- 0
  }
}  



orig$Colgate <- c()
for(i in 1:nrow(orig)){
  if(is.na(orig$POLADSCOLGATE1S4[i])==F){
    orig$Colgate[i] <- 1
  }else if (is.na(orig$POLADSCOLGATE2S4[i]) ==F){
    orig$Colgate[i] <- 0
  }
}  



orig$MSC <- c()
for(i in 1:nrow(orig)){
  if(is.na(orig$POLADSMSC1S4[i])==F){
    orig$MSC[i] <- 1
  }else if (is.na(orig$POLADSMSC2S4[i]) ==F){
    orig$MSC[i] <- 0
  }
}  

#reshape the data so that column are rows
#experimental outcomes stacked
original <- gather(orig, experiment, estimate, POLADSTRUMP1S4, POLADSTRUMP2S4, POLADSBIDEN1S4, POLADSBIDEN2S4, POLADSEXXON1S4, POLADSEXXON2S4, POLADSPATAGONIA1S4, POLADSPATAGONIA2S4,POLADSPOWER1S4,POLADSPOWER2S4,POLADSSIERRA1S4,POLADSSIERRA2S4,POLADSCOLGATE1S4,POLADSCOLGATE2S4,POLADSMSC1S4,POLADSMSC2S4)

#original$estimate <-  ifelse(original$estimate<4,0,1)

orig <- spread(original, experiment, estimate)

###########
#correction
###########

redo$Trump <- NA
for(i in 1:nrow(redo)){
  if(is.na(redo$POLADSTRUMP1S4[i])==F){
    redo$Trump[i] <- 1
  }else if (is.na(redo$POLADSTRUMP2S4[i]) ==F){
    redo$Trump[i] <- 0
  }
}  


redo$Biden <- c()
for(i in 1:nrow(redo)){
  if(is.na(redo$POLADSBIDEN1S4[i])==F){
    redo$Biden[i] <- 1
  }else if (is.na(redo$POLADSBIDEN2S4[i]) ==F){
    redo$Biden[i] <- 0
  }
}  



redo$Exxon <- c()
for(i in 1:nrow(redo)){
  if(is.na(redo$POLADSEXXON1S4[i])==F){
    redo$Exxon[i] <- 1
  }else if (is.na(redo$POLADSEXXON2S4[i]) ==F){
    redo$Exxon[i] <- 0
  }
}  



redo$Patagonia <- c()
for(i in 1:nrow(redo)){
  if(is.na(redo$POLADSPATAGONIA1S4[i])==F){
    redo$Patagonia[i] <- 1
  }else if (is.na(redo$POLADSPATAGONIA2S4[i]) ==F){
    redo$Patagonia[i] <- 0
  }
}  



redo$Power <- c()
for(i in 1:nrow(redo)){
  if(is.na(redo$POLADSPOWER1S4[i])==F){
    redo$Power[i] <- 1
  }else if (is.na(redo$POLADSPOWER2S4[i]) ==F){
    redo$Power[i] <- 0
  }
}  


redo$Sierra <- c()
for(i in 1:nrow(redo)){
  if(is.na(redo$POLADSSIERRA1S4[i])==F){
    redo$Sierra[i] <- 1
  }else if (is.na(redo$POLADSSIERRA2S4[i]) ==F){
    redo$Sierra[i] <- 0
  }
}  

redo$Colgate <- c()
for(i in 1:nrow(redo)){
  if(is.na(redo$POLADSCOLGATE1S4[i])==F){
    redo$Colgate[i] <- 1
  }else if (is.na(redo$POLADSCOLGATE2S4[i]) ==F){
    redo$Colgate[i] <- 0
  }
}  

redo$MSC <- c()
for(i in 1:nrow(redo)){
  if(is.na(redo$POLADSMSC1S4[i])==F){
    redo$MSC[i] <- 1
  }else if (is.na(redo$POLADSMSC2S4[i]) ==F){
    redo$MSC[i] <- 0
  }
}  


#reshape the data so that column are rows
#experimental outcomes stacked
correction <- gather(redo, experiment, estimate, POLADSTRUMP1S4, POLADSTRUMP2S4, POLADSBIDEN1S4, POLADSBIDEN2S4, POLADSEXXON1S4, POLADSEXXON2S4, POLADSPATAGONIA1S4, POLADSPATAGONIA2S4,POLADSPOWER1S4,POLADSPOWER2S4,POLADSSIERRA1S4,POLADSSIERRA2S4,POLADSCOLGATE1S4,POLADSCOLGATE2S4,POLADSMSC1S4,POLADSMSC2S4)

#correction$estimate <-  ifelse(correction$estimate<4,0,1)

redo <- spread(correction, experiment, estimate)






pooled <- rbind(original, correction)


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
  } else if(pooled$experiment[i] == 'POLADSCOLGATE1S4'| pooled$experiment[i] == "POLADSCOLGATE2S4"){
    pooled$political[i] <- pooled$Colgate[i]
  } else if(pooled$experiment[i] == 'POLADSMSC1S4'| pooled$experiment[i] == 'POLADSMSC2S4' ){
    pooled$political[i] <- pooled$MSC[i]
  }
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
     ra$experiment[i] == "POLADSPATAGONIA1S4"|ra$experiment[i] =="POLADSPATAGONIA2S4"){  |
      ra$experiment[i] == "POLADSCOLGATE1S4"|ra$experiment[i] =="POLADSCOLGATE2S4"|
      ra$experiment[i] == "POLADSMSC1S4"|ra$experiment[i] =="POLADSMSC2S4"){
        ra$company[i] <- 1
      } else{ ra$company[i] <-0}

  }
  
  ra$pol <- ifelse(ra$political== 1,'Strong','Weak')
  ra$pol <- factor(ra$pol, levels = c('Weak','Strong'))
  
  
  ra$sourceOrient <- NA
  for(i in 1:nrow(ra)){
    if(ra$experiment[i] == "POLADSEXXON1S4" | 
       ra$experiment[i] == "POLADSEXXON2S4" |
       ra$experiment[i] == "POLADSMSC1S4" |
       ra$experiment[i] == "POLADSMSC2S4" |
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
  
  
  trump8 <- lm_robust(estimate ~ Trump, data =subset(ra,ra$experiment=='POLADSTRUMP1S4'|ra$experiment == 'POLADSTRUMP2S4') , weights = WEIGHT,clusters=CaseId)
  
  
  
  write.csv(ra, paste0(local,'data/Real' ,'/ra.csv'))

# Raw values
# Get the weighted means and cis with func:
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

#######################################################
#Pol ad list using the 
#reordered orig and redo values

#The order of ads column wise and other indicators 
adrow <-c('Trump','Biden','Exxon',
          'Patagonia','Power',
          'Sierra','Colgate', 'MSC')

politic <- c('Weak','Strong')
iter <- c('Original','Correction','Pooled')

magic_for(silent=T)
temp <- c()


polAd <- list()
polAd[[1]] <- orig
polAd[[2]] <- redo
polAd[[3]] <- rbind(orig,redo)

#######################################################
#run a function that gets weighted ci of both datasets
#does so in sets by high or low political

for(d in 1:3){#for each experiment
  temp <-c()#reset temp
  for(j in 1:2){#for each index set (1=low political 2=Strong*)
    index <- which(colnames(polAd[[d]])=='POLADSTRUMP1S4')-j
    #index starting at 1 or 2 less than variables of interest to collect every other
    for(i in 1:8){#for every ad image
      index <- index + 2 #cycle through sets of images 
      data <- cbind(as.data.frame(cbind(t(as.matrix((wtd.ci(polAd[[d]][,index]
                                                            ,polAd[[d]]$WEIGHT )))))),'Iteration'=iter[d]
                    ,'political'=politic[j], 'Ad'=adrow[i])
      temp <- rbind(temp,data)#grab statistics
    }
  }
  put(temp)#nice little grabber function to collect data from for loops
}


index <- which(colnames(polAd[[3]])=='POLADSBIDEN1S4')-1
dat1 <-matrix(NA,ncol=4,nrow=8)
for(i in 1:8){#for every ad image
  index <- index + 2 #cycle through sets of images 
  dat1[i,] <<- wtd.ci(polAd[[3]][,index],polAd[[3]]$WEIGHT)
}
dat1 <- as.data.frame(dat1)
dat1['Ad'] <- c('Biden','Colgate','Exxon','MSC','Patagonia','Power','Sierra','Trump')
dat1['political'] = 'Weak'



index <- which(colnames(polAd[[3]])=='POLADSBIDEN1S4')-2

dat2 <-matrix(NA,ncol=4,nrow=8)
for(i in 1:8){#for every ad image
  index <- index + 2 #cycle through sets of images 
  dat2[i,] <<- wtd.ci(polAd[[3]][,index],polAd[[3]]$WEIGHT)
}
dat2 <- as.data.frame(dat2)
dat2['Ad'] <- c('Biden','Colgate','Exxon','MSC','Patagonia','Power','Sierra','Trump')
dat2['political'] = 'Strong'


datc <- rbind(dat1,dat2)

colnames(datc)[1:4] <- c('Estimate','Lower.CI','Upper.CI','St Err')
datd = datc

OldRange = (max(datc$Estimate, na.rm=F) - min(datc$Estimate, na.rm=F))  
NewRange = ( 4.45- 1.25)  
datd$Estimate = (((datc$Estimate - min(datc$Estimate, na.rm=F)) * NewRange) / OldRange) + 1.25

OldRange = (max(datc$Upper.CI, na.rm=F) - min(datc$Upper.CI, na.rm=F))  
NewRange = ( 4.45- 1.25)  
datd$Upper.CI = (((datc$Upper.CI - min(datc$Upper.CI, na.rm=F)) * NewRange) / OldRange) + 1.25

OldRange = (max(datc$Lower.CI, na.rm=F) - min(datc$Lower.CI, na.rm=F))  
NewRange = ( 4.45- 1.25)  
datd$Lower.CI = (((datc$Lower.CI - min(datc$Lower.CI, na.rm=F)) * NewRange) / OldRange) + 1.25

#*1 = low political in the loop above, however, 
#for raw column names 1=Strong
################################################################
#make facet features for real
# 
# dat <- magic_result()$temp[[1]]
# datb <- magic_result()$temp[[2]]
# datc <- magic_result()$temp[[3]]
# 
# dat$Ad = factor(dat$Ad, levels=(c("Biden","Trump","Colgate","Exxon","MSC",'Patagonia',"Power", "Sierra")))
# datb$Ad = factor(datb$Ad, levels=(c("Biden","Trump","Colgate","Exxon","MSC",'Patagonia',"Power", "Sierra")))
# datc$Ad = factor(datc$Ad, levels=(c("Biden","Trump","Colgate","Exxon","MSC",'Patagonia',"Power", "Sierra")))

write.csv(dat, paste0(local,'data/Real', '/data.csv'))
write.csv(datb, paste0(local,'data/Real', '/datb.csv'))
write.csv(datc, paste0(local,'data/Real', '/datc.csv'))
write.csv(datd, paste0(local,'data/Real', '/datc.csv'))

write.csv(pooled, paste0(local,'data/Real' ,'/pooled.csv'))
write.csv(original, paste0(local,'data/Real' ,'/original.csv'))
write.csv(correction, paste0(local,'data/Real' ,'/correction.csv'))




# write.csv(original, 'C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/data/original.csv')
# write.csv(correction, 'C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/data/correction.csv')
# write.csv(orig, 'C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/data/orig.csv')
# write.csv(redo, 'C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/data/redo.csv')
################
#data for follow up
#
follow <- read.csv(paste0(local,'Data/Follow up/polad.csv'))

follow$political_affiliat[follow$political_affiliat==""] <-NA
follow$political_affiliat[follow$political_affiliat=="Other:"] <-"Independent"
follow$political_affiliat = factor(follow$political_affiliat, levels=(c("Strong Democrat","Weak Democrat","Independent","Weak Republican","Strong Republican")))

ads <- follow[,c(18,19,20,22,23,24)]

for(i in 1:ncol(ads)){
  for(j in  1:nrow(ads)){
    ads[j,i][ads[j,i]==""] <- NA}
  ads[,i] <<- factor(ads[,i], levels=(c("Not at all political","Slightly political","Somewhat political","Very political","Extremely political")))
}


vals <- data.frame(matrix(ncol=4,nrow=6,NA))
colnames(vals) <- c('Estimate','Lower 95 CI','Upper 95 CI','Standard Err.')

for(i in 1:ncol(ads)){
  vals[i,] <<- ci(as.numeric(ads[,i]), na.rm=T)
}

vals$Ad <- c( "Strong pro-development","Strong pro-environment", "Moderate pro-development","Moderate pro-environment", "Weak pro-development", "Weak pro-environment"  )
vals$Ad = factor(vals$Ad, levels=rev( c( "Strong pro-development","Strong pro-environment", "Moderate pro-development","Moderate pro-environment", "Weak pro-development", "Weak pro-environment"  )))
conjoint <- vals
#plots
f1<- ggplot(vals, aes(y = Estimate, x = Ad  ))+
  geom_pointrange(aes(y = Estimate,ymin = `Lower 95 CI`, 
                      ymax = `Upper 95 CI`),
                  size = 1, color = "sienna4") +
  coord_flip() + 
  ylim(1,4)+  xlab("")+ 
  ylab("")+ 
  scale_y_continuous(limits = c(1,5),
                     labels=c('Not \n political \n (1)',
                              '',
                              '\n\n (3)',
                              '',
                              'Extremely \n political \n (5)'))+ 
  theme(panel.spacing.x = unit(27, "mm"),
        axis.text.x=element_text(angle=0,
                                 hjust=0.5,vjust=.5))#+
# theme(axis.text.y = element_text(face=rev(c("bold","bold",
#                                             "italic","italic",
#                                             'plain','plain','plain','plain'))))

f1+theme(text=element_text(size=20)) + scale_color_manual(values =  c("sienna4", 'navajowhite3'), name = "")+ geom_hline( yintercept = 0, colour = "gray", size = 1, lty= 'dashed')+
  theme(plot.margin = unit(c(1,1,2,3), "cm"))+
  theme(axis.text.y= element_text( size = 20),axis.text.x= element_text( size = 18),legend.text=element_text(size=20))


ggsave(paste0(local,"mturk_conjoint.pdf"), width = 9, height = 7, units = "in")


ads2 <- follow[,c(33,30,27,29,42,43,36,37,34,35,40,41,31,32,38,39)]
for(i in 1:ncol(ads2)){
  for(j in  1:nrow(ads2)){
    ads2[j,i][ads[j,i]==""] <- NA}
  ads2[,i] <<- factor(ads2[,i], levels=(c("Not at all political","Slightly political","Somewhat political","Very political","Extremely political")))
}

weights <-data.frame(matrix(ncol=4,nrow=16,NA))
colnames(weights) <- c('Estimate','Lower 95 CI','Upper 95 CI','Standard Err.')

for(i in 1:ncol(ads2)){
  weights[i,] <<- ci(as.numeric(ads2[,i]), na.rm=T)
}

weights$`Political Content` <- rep(c('Strong','Weak'),8)
weights$Ad <- rep(c("Biden","Trump","Power", "Sierra","MSC",
                    'Patagonia',"Exxon","Colgate"),each =2)
weights$Ad = factor(weights$Ad, levels=rev(c("Biden","Trump","Power", "Sierra",'Patagonia',"MSC","Exxon","Colgate")))

realAds <- weights

f2 <- ggplot(weights, aes(y = Estimate, x = Ad, colour= `Political Content`   ))+
  geom_pointrange(aes(y = Estimate,ymin = `Lower 95 CI`, 
                      ymax = `Upper 95 CI`),
                  size = 1) +
  coord_flip() + 
  ylim(1,4)+  xlab("")+ 
  ylab("")+ 
  scale_y_continuous(limits = c(1,5),
                     labels=c('Not \n political \n (1)',
                              '',
                              ' \n\n (3)',
                              '',
                              'Extremely \n political \n (5)'))+ 
  theme(panel.spacing.x = unit(27, "mm"),
        axis.text.x=element_text(angle=0,
                                 hjust=0.5,vjust=.5))+
  theme(axis.text.y = element_text(face=rev(c("bold","bold",
                                              "italic","italic",
                                              'plain','plain','plain','plain'))))

f2+theme(text=element_text(size=20)) + scale_color_manual(values =  c("sienna4", 'navajowhite3'), name = "")+ geom_hline( yintercept = 0, colour = "gray", size = 1, lty= 'dashed')+
  theme(plot.margin = unit(c(1,1,2,3), "cm"))+
  theme(axis.text.y= element_text( size = 20),axis.text.x= element_text( size = 18),legend.text=element_text(size=20))




ggsave(paste0(local,"mturk_real_ads.pdf"), width = 12, height = 7, units = "in")
