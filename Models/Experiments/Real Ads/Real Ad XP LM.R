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
library(Hmisc)
library(sjPlot)
library(labelled)
library(haven)
library(png)
library(magicfor)  
library(fastDummies)
library(lme4)
library(coefplot)

#load in REAL ADS DATA

polAdORIG <- read.csv('C:/Users/dl0ck/OneDrive/Summer 2021/PolAdsTransparency/politics_user_study/Data/TASS 04/8557_WUSTL-Weidenbaum04_MAIN.csv') 

polAdREDO <- read.csv('C:/Users/dl0ck/OneDrive/Summer 2021/PolAdsTransparency/politics_user_study/Data/TASS04Reask/8557_WUSTL-Weidenbaum04Reask.csv') 
####################################################################################
#Create some variables needed later
#

polAdORIG$pid <- recode_factor(polAdORIG$P_PARTYID7, '1' = "Democrat", '2' = 'Democrat', '3' = 'Democrat', '5'= 'Republican', '6'= 'Republican', '7' = 'Republican'  )


polAdREDO$pid <- recode_factor(polAdREDO$PartyID7, '1' = "Democrat", '2' = 'Democrat', '3' = 'Democrat', '5'= 'Republican', '6'= 'Republican', '7' = 'Republican')

#####################################################################################
#Make a list so that I can loop over experiment versions

polAd <- list()

polAd[[1]] <- polAdORIG
polAd[[2]] <- polAdREDO

small <-list()


#####################################################################################
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

#####################################################################################
# Datasets with only the relevant variables
orig <- cbind(polAd[[1]][,which(colnames(polAd[[1]])=='POLADSTRUMP1S4'):
                                 which(colnames(polAd[[1]])=='POLADSMSC2S4')], 'WEIGHT' = polAd[[1]][,'WEIGHT'], pid = polAd[[1]][,'pid'])
redo<- cbind(polAd[[2]][,which(colnames(polAd[[2]])=='POLADSTRUMP1S4'):
                                 which(colnames(polAd[[2]])=='POLADSMSC2S4')], 'WEIGHT' =polAd[[2]][,'WEIGHT'], pid = polAd[[2]][,'pid'])






#####################################################################################




#Make new variables that with a dummy for whether a participant saw the high or low political ads

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

#reshape the data so that columsn are rows
#experimental outcomes stacked
original <- gather(orig, experiment, estimate, POLADSTRUMP1S4, POLADSTRUMP2S4, POLADSBIDEN1S4, POLADSBIDEN2S4, POLADSEXXON1S4, POLADSEXXON2S4, POLADSPATAGONIA1S4, POLADSPATAGONIA2S4,POLADSPOWER1S4,POLADSPOWER2S4,POLADSSIERRA1S4,POLADSSIERRA2S4,POLADSCOLGATE1S4,POLADSCOLGATE2S4,POLADSMSC1S4,POLADSMSC2S4)


#Add variable indicating message orientation:
original$orientation <- c()

for(i in 1:nrow(original)){
  if(original$experiment[i] == 'POLADSTRUMP1S4'  | original$experiment[i] == 'POLADSTRUMP2S4' | original$experiment[i] == 'POLADSEXXON1S4' | original$experiment[i] == 'POLADSEXXON2S4' |original$experiment[i] == 'POLADSPOWER1S4' |original$experiment[i] == 'POLADSPOWER2S4' |original$experiment[i] == 'POLADSMSC1S4' |original$experiment[i] == 'POLADSMSC2S4'){
    original$orientation[i] <- 'Conservative'
  }else{original$orientation[i] <- 'Liberal'}
}



#####################################################################################
#8 experiments, participants are in political high or low condition
#subset to trump high and low political and regress the estimate (how political is ad?)
#
trump1 <- lm(estimate ~ Trump, data = subset(original,original$experiment=='POLADSTRUMP1S4'|original$experiment == 'POLADSTRUMP2S4'), weights =WEIGHT)

biden1 <- lm(estimate ~ Biden, data = subset(original,original$experiment=='POLADSBIDEN1S4'|original$experiment == 'POLADSBIDEN2S4'), weights =WEIGHT)



patagonia1 <- lm(estimate ~ Patagonia, data =subset(original,original$experiment=='POLADSPATAGONIA1S4'|original$experiment == 'POLADSPATAGONIA2S4'), weights = WEIGHT)

exxon1 <- lm(estimate ~ Exxon, data = subset(original,original$experiment=='POLADSEXXON1S4'|original$experiment == 'POLADSEXXON2S4'), weights = WEIGHT)


power1 <- lm(estimate ~ Power, data = subset(original,original$experiment=='POLADSPOWER1S4'|original$experiment == 'POLADSPOWER2S4'), weights = WEIGHT)


sierra1 <- lm(estimate ~Sierra, data = subset(original,original$experiment=='POLADSSIERRA1S4'|original$experiment == 'POLADSSIERRA2S4'), weights =WEIGHT)


colgate1 <- lm(estimate ~ Colgate, data = subset(original,original$experiment=='POLADSCOLGATE1S4'|original$experiment == 'POLADSCOLGATE2S4'), weights =WEIGHT)


msc1 <- lm(estimate ~ MSC, data = subset(original,original$experiment=='POLADSMSC1S4'|original$experiment == 'POLADSMSC2S4'), weights =WEIGHT)


#####################################################################################
#Make new variables that with a dummy for whether a participant saw the high or low political ads
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


#reshape the data so that columsn are rows
#experimental outcomes stacked
correction <- gather(redo, experiment, estimate, POLADSTRUMP1S4, POLADSTRUMP2S4, POLADSBIDEN1S4, POLADSBIDEN2S4, POLADSEXXON1S4, POLADSEXXON2S4, POLADSPATAGONIA1S4, POLADSPATAGONIA2S4,POLADSPOWER1S4,POLADSPOWER2S4,POLADSSIERRA1S4,POLADSSIERRA2S4,POLADSCOLGATE1S4,POLADSCOLGATE2S4,POLADSMSC1S4,POLADSMSC2S4)

#Add variable indicating message orientation:
correction$orientation <- c()

for(i in 1:nrow(correction)){
  if(correction$experiment[i] == 'POLADSTRUMP1S4'  | correction$experiment[i] == 'POLADSTRUMP2S4' | correction$experiment[i] == 'POLADSEXXON1S4' | correction$experiment[i] == 'POLADSEXXON2S4' |correction$experiment[i] == 'POLADSPOWER1S4' |correction$experiment[i] == 'POLADSPOWER2S4' |correction$experiment[i] == 'POLADSMSC1S4' |correction$experiment[i] == 'POLADSMSC2S4'){
    correction$orientation[i] <- 'Conservative'
  }else{correction$orientation[i] <- 'Liberal'}
}


#####################################################################################
#####################################################################################
#8 experiments, participants are in political high or low condition
#subset to trump high and low political and regress the estimate (how political is ad?)
#

trump2 <- lm(estimate ~ Trump, data =subset(correction,correction$experiment=='POLADSTRUMP1S4'|correction$experiment == 'POLADSTRUMP2S4') , weights =WEIGHT)

biden2  <- lm(estimate ~ Biden, data = subset(correction,correction$experiment=='POLADSBIDEN1S4'|correction$experiment == 'POLADSBIDEN2S4'), weights =WEIGHT)

patagonia2  <- lm(estimate ~ Patagonia, data =subset(correction,correction$experiment=='POLADSPATAGONIA1S4'|correction$experiment == 'POLADSPATAGONIA2S4'), weights = WEIGHT)

exxon2 <- lm(estimate ~ Exxon, data = subset(correction,correction$experiment=='POLADSEXXON1S4'|correction$experiment == 'POLADSEXXON2S4'), weights = WEIGHT)

power2  <- lm(estimate ~ Power, data = subset(correction,correction$experiment=='POLADSPOWER1S4'|correction$experiment == 'POLADSPOWER2S4'), weights = WEIGHT)


sierra2 <- lm(estimate ~Sierra, data = subset(correction,correction$experiment=='POLADSSIERRA1S4'|correction$experiment == 'POLADSSIERRA2S4'), weights =WEIGHT)


colgate2 <- lm(estimate ~ Colgate, data = subset(correction,correction$experiment=='POLADSCOLGATE1S4'|correction$experiment == 'POLADSCOLGATE2S4'), weights =WEIGHT)


msc2 <- lm(estimate ~ MSC, data = subset(correction,correction$experiment=='POLADSMSC1S4'|correction$experiment == 'POLADSMSC2S4'), weights =WEIGHT)


#######################################################################################
#Make plots clustering by attributes




pol = multiplot(trump1,trump2, biden1, biden2, intercept = F, plot = F)
pol$face = rep('Politicians')
pol['Month'] <- c('November','May','November','May')
pol[order(pol$Value),]

pol$Coefficient = factor(pol$Coefficient, levels=rev(c("Biden","Trump")))
pol$Month = as.factor(pol$Month)


com = multiplot(exxon1,exxon2,msc1, msc2, patagonia1,patagonia2,colgate1, colgate2, intercept = F, plot = F)
com$face = rep('Companies')
com['Month'] <- c('November','May','November','May','November','May','November','May')
com[order(com$Value),]

com$Coefficient = factor(com$Coefficient, levels=rev(c("Colgate","Exxon","MSC",'Patagonia')))
com$Month = as.factor(com$Month)

org = multiplot(power1,power2, sierra1, sierra2, intercept = F, plot= F)
org$face = rep('Organizations')
org['Month'] <- c('November','May','November','May')
org[order(org$Value),]
org$Coefficient = factor(org$Coefficient, levels=rev(c("Power", "Sierra")))

org$Month = as.factor(org$Month)

lmDat <- rbind(pol,com,org)

lmPlot <- ggplot(lmDat, aes(y = Value, x = Coefficient, color = Month))+
  geom_pointrange(aes(y = Value,
                      ymin = `HighInner`,
                      ymax = `LowInner`, fill =Month),
                  position = position_dodge(width = -.6),
                  size = .05) +
  coord_flip() + 
  xlab("") + ylab("Coefficients")+
  ylim(.45,2.5)+
  scale_color_manual(values =  c("aquamarine4","lightpink3"))

lmPlot + facet_wrap(~face)
######################################################################################


######################################################################################
# Raw values
# Get the weighted means and cis with func:
# 
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

#The order of ads column wise
adrow <-c('Trump','Biden','Exxon',
          'Patagonia','Power',
          'Sierra','Colgate', 'MSC')

#Pol ad list using the reordered orig and redo values
polAd <- list()
polAd[[1]] <- orig
polAd[[2]] <- redo

politic <- c('Low Political','High Political')
iter <- c('Original','Correction')

magic_for(silent=T)
temp <- c()

#run a function that gets weighted ci of both datasets
#does so in sets by high or low political
for(d in 1:2){#for each experiment
  temp <-c()#reset temp
  for(j in 1:2){#for each index set (1=low political 2=high political)
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



data1 <- rbind(magic_result()$temp[[1]],magic_result()$temp[[2]])#the two outer loops combined into one dataframe

#flip factor order for prefered display
data1$Iteration = factor(data1$Iteration, levels=c('Original','Correction'))
new <-c()
for(i in 1:nrow(data1)){
  if(data1$Ad[i] == 'Trump'|data1$Ad[i] == 'Biden'){
    data1$face[i] <<- 'Politicians'
  }else if (data1$Ad[i] == 'Sierra'| data1$Ad[i] == 'Power'){
    data1$face[i] <<- 'Organizations'
  } else{data1$face[i] <<- 'Companies'}
  
}

#define some aesthetically pleasing colors
vir <- rep(c("salmon4","bisque2"),8)
dat1 <- subset(data1, data1$face=='Politicians')
dat2 <- subset(data1, data1$face=='Organizations')
dat3 <- subset(data1, data1$face== 'Companies')

magic_free()

data1$political <- as.factor(data1$political)

p3 <- ggplot(dat1, aes(y = Estimate, x = Ad, color = political))+
  geom_pointrange(aes(y = Estimate,
                      ymin = `CI lower`,
                      ymax = `CI upper`, fill = political),
                  position = position_dodge(width = -.6),
                  size = .5) +
  facet_wrap(~Iteration)+
  #scale_shape_discrete(name="", label=c("High Political", "Low Political")) +
  #scale_color_manual(name="", label=c("High Political", "Low Political"),values =vir) +
  #guides(shape = guide_legend(override.aes = list(fill = c("aquamarine4","lightpink3"))))+
  coord_flip() + 
  ylim(1,4)+
  xlab("") + ylab("")+
  theme_bw()+ 
  scale_y_continuous(limits = c(1,5), labels=c('Not at all political','','3','','Extremely political'))+ 
  theme(panel.spacing.x = unit(27, "mm"),axis.text.x=element_text(angle=15, hjust=0.5, vjust=.5))+ theme(axis.text.x = element_text(size = 12),strip.text.x = element_text(size = 10),
                                                                                                         legend.text = element_text(size = 10),
                                                                                                         axis.text.y = element_text(size = 10))
p3

scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
dd[order(-dd[,4], dd[,1]), ]


p4 <- ggplot(dat2, aes(y = Estimate, x = Ad, shape= as.factor(political),color = as.factor(political)))+
  geom_pointrange(aes(y = Estimate,
                      ymin = `CI lower`,
                      ymax = `CI upper`, fill = as.factor(political)),
                  position = position_dodge(width = -.6),
                  size = .5, fill  =vir) +
  facet_wrap(~Iteration)+
  scale_shape_discrete(name="", label=c("High Political", "Low Political")) +
  scale_color_manual(name="", label=c("High Political", "Low Political"),
                     values =vir) +
  guides(shape = guide_legend(override.aes = list(fill = c("aquamarine4","lightpink3"))))+
  coord_flip() + 
  xlab("") + ylab("")+
  theme_bw()+ 
  scale_y_continuous(limits = c(1,5), labels=c('Not at all political','','3','','Extremely political'))+ 
  theme(panel.spacing.x = unit(27, "mm"),axis.text.x=element_text(angle=15, hjust=0.5, vjust=.5))+ theme(axis.text.x = element_text(size = 12),strip.text.x = element_text(size = 10),
                                                                                                         legend.text = element_text(size = 10),
                                                                                                         axis.text.y = element_text(size = 10))
p4

data2 <- rbind(magic_result()$temp[[1]],magic_result()$temp[[2]])#the two outer loops combined into one dataframe


#add columns legend and facet
data2$`Treatment and Experiment:` <- paste(data2$political, data2$Iteration)
data2$`Treatment and Experiment:` <- as.factor(data2$`Treatment and Experiment:`)
#define some aesthetically pleasing colors
data2$`Treatment and Experiment:`  = factor(data2$`Treatment and Experiment:` , levels=c("High Political Original","Low Political Original" ,"High Political Correction", "Low Political Correction"))
data2$Ad = factor(data2$Ad , levels=rev(c('Biden','Trump','Colgate','Exxon','MSC','Patagonia','Sierra','Power')))


data2$face <- NA

for(i in 1:nrow(data2)){
  if(data2$Ad[i] == "Trump" | data2$Ad[i] == "Biden"){
    data2$face[i] <<- "Politicians"
  }else if(data2$Ad[i] == "Power" | data2$Ad[i] == "Sierra"){
    data2$face[i] <<- "Organizations"
  }else{data2$face[i] <<- "Companies"}
}



p3 <- ggplot(data2, aes(y = Estimate, x = Ad , color=`Treatment and Experiment:`  ))+
  geom_pointrange(aes(y = Estimate,
                      ymin = `CI lower`,
                      ymax = `CI upper`, fill = `Treatment and Experiment:` ),
                  position = position_dodge(width = -.6),
                  size = .3) +
  facet_wrap(~Iteration)+
  #scale_shape_discrete(name="", label=c("High Political", "Low Political")) +
  #scale_color_manual(name="", label=c("High Political", "Low Political"),values =vir) +
  #guides(shape = guide_legend(override.aes = list(fill = c("aquamarine4","lightpink3"))))+
  coord_flip() + 
  ylim(1,4)+
  xlab("") + ylab("")+
  theme_bw()+ 
  scale_y_continuous(limits = c(1,5), labels=c('Not \n political','','3','','Extremely \n political'))+ 
  theme(panel.spacing.x = unit(27, "mm"),axis.text.x=element_text(angle=15, hjust=0.5, vjust=.5))+ theme(axis.text.x = element_text(size = 8),strip.text.x = element_text(size = 10),
                                                                                                         legend.text = element_text(size = 10),
                                                                                                         axis.text.y = element_text(size = 8))
p3 <- p3+facet_wrap(~face,ncol=3) +   scale_color_manual(values =  c("aquamarine4","lightpink3","royalblue4","salmon4"))+ theme(legend.position="bottom")

p3

magic_free()

data1$political <- as.factor(data1$political)




d <- data1[order(data1[,'face'], data1[,'Ad'], data1[,'political']), ]
row.names(d) <- NULL
t1b <- kable( d, format = "latex", booktabs = TRUE, linesep = c("", "", "", "\\addlinespace"))
