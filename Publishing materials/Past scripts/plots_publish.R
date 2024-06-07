library(cowplot)
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
library(magicfor)  
library(fastDummies)
library(lme4)
library(coefplot)
library(estimatr)
library(ggtext)
library(texreg)
source('C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/plot_amce.R')#to extract cjoint data for plots, I modify their source code


####################
#Data for conjoint
####################
cj <- read.csv(file = 'C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/Experiments/Conjoint Analysis/CJ.csv', header = TRUE, 
stringsAsFactors = T)


##################
#Define baselines
##################

baselines <- list()
baselines$sourceType <- "Company"
baselines$sourceOrient <- "Pro-environment"
baselines$messageStrength <- "Politically neutral"
baselines$messageOrient <- "Pro-environment"
baselines$image <- "Landscape"
baselines$pid <- 'Republican'

bases <- list()
bases$image <- 'Landscape'
bases$messageOrient <- 'Pro-environment'
bases$messageStrength <- 'Politically neutral'
bases$sourceOrient <- 'Pro-environment'
bases$sourceType <- 'Company'

#####################
#Data for real ads lm
#####################

original <- read.csv('C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/data/original.csv') 
correction <- read.csv('C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/data/correction.csv') 
pooled <- read.csv('C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/data/pooled.csv') 

#####################
#Data for real means
#####################
#####################
dat <- read.csv('C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/data/dat.csv') 

datb <- read.csv('C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/data/datb.csv') 

datc <- read.csv('C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/data/datc.csv') 

###############
#export tables
###############

#small function that exports tables to .tex file
mod_texreg <- function(output.file, ...) {
  output <- capture.output(texreg(...))
  cat(paste(output, collapse = "\n"), "\n", file=output.file, append=TRUE)
}
heme_set(theme_light())
###############################################

####################
#conjoint experiment
####################


################################################
#cjoint does not vibe with texreg or stargazer
#to overcome this we will make a regular lm and
#overwrite the data with cjoint data

cj$messageStrength<- factor(cj$messageStrength, 
                            levels = c('Politically neutral',
                                       'Moderately political',
                                       'Highly political'))

cj$image <- factor(cj$image, levels = c('Landscape',
                                        'Desert','Oil rig',
                                        'Pipeline','Solar wind'))

cj$messageOrient <- factor(cj$messageOrient, 
                           levels = c('Pro-environment',
                                      'Pro-development'))

cj$messageStrength <- factor(cj$messageStrength,
                             levels = c('Politically neutral',
                                        'Moderately political',
                                        'Highly political'))

cj$sourceOrient  <- factor(cj$sourceOrient, 
                           levels = c('Pro-environment',
                                      'Pro-development'))

cj$sourceType  <- factor(cj$sourceType, levels = c('Company',
                                                   'Candidate',
                                                   'Organization'))


########################
#message-unconditional
#cjoint
unconditional <- amce(dv1 ~ image  + messageOrient + messageStrength + sourceOrient + sourceType  , respondent.id= 'CaseId', cluster=TRUE,  weights ='WEIGHT', data=cj, baseline= baselines, na.ignore=T)

#lm object to be overwritten
uncon <- summary(lm(dv1 ~  image  + messageOrient + messageStrength + sourceOrient + sourceType , data=cj, weights=WEIGHT))

#overwrite lm object with cjoint data
uncon$coefficients[2:11,c('Estimate','Std. Error','Pr(>|t|)')] <- rbind(cbind(summary(unconditional)$amce[,c('Estimate')],summary(unconditional)$amce[,c('Std. Err')],summary(unconditional)$amce[,c('Pr(>|z|)')]))
################################################

#################
#message-priors
#cjoint
mesxprior <- amce(dv1 ~  image  + messageOrient + messageStrength + sourceOrient + sourceType   + priors + messageOrient*priors, cluster=TRUE, respondent.id="CaseId", respondent.varying= 'priors', weights ='WEIGHT', data=cj, baseline= baselines, na.ignore=T)

#lm object to be overwritten
messP <- summary(lm(dv1 ~ image  + messageOrient + messageStrength + sourceOrient + sourceType   + priors + messageOrient*priors,weights =WEIGHT, data=cj))

#overwrite lm object with cjoint data- main variables
messP$coefficients[2:11,c('Estimate','Std. Error','Pr(>|t|)')] <- rbind(cbind(summary(mesxprior)$amce[,c('Estimate')],summary(mesxprior)$amce[,c('Std. Err')],summary(mesxprior)$amce[,c('Pr(>|z|)')]))

#overwrite lm object with cjoint data- interacted variables
messP$coefficients[12:13,c('Estimate','Std. Error','Pr(>|t|)')] <- rbind(cbind(c(as.matrix(summary(mesxprior)[[3]]['Estimate']),as.matrix(summary(mesxprior)[[4]]['Estimate'])),c(as.matrix(summary(mesxprior)[[3]]['Std. Err']),as.matrix(summary(mesxprior)[[4]]['Std. Err'])),c(as.matrix(summary(mesxprior)[[3]]['Pr(>|z|)']),as.matrix(summary(mesxprior)[[4]]['Pr(>|z|)']))))

#change rownames to prevent confusion
rownames(messP$coefficients)[12] <- 'messageOrientPro-development:priorsPro-environment'
################################################

##############
#source-priors
#cjoint
sourcexprior <- amce(dv1 ~  image  + messageOrient + messageStrength + sourceOrient + sourceType   + priors + sourceOrient*priors, cluster=TRUE, respondent.id="CaseId", weights ='WEIGHT', data=cj, baseline= baselines, respondent.varying= 'priors', na.ignore=T)

#lm object to be overwritten
sourceP <- summary(lm(dv1 ~  image  + messageOrient + messageStrength + sourceOrient + sourceType   + priors + sourceOrient*priors,weights =WEIGHT, data=cj))

#overwrite lm object with cjoint data- main variables
sourceP$coefficients[2:11,c('Estimate','Std. Error','Pr(>|t|)')] <- rbind(cbind(summary(sourcexprior)$amce[,c('Estimate')],summary(sourcexprior)$amce[,c('Std. Err')],summary(sourcexprior)$amce[,c('Pr(>|z|)')]))

#overwrite lm object with cjoint data- interacted variables
sourceP$coefficients[12:13,c('Estimate','Std. Error','Pr(>|t|)')] <- rbind(cbind(c(as.matrix(summary(sourcexprior)[[3]]['Estimate']),as.matrix(summary(sourcexprior)[[4]]['Estimate'])),c(as.matrix(summary(sourcexprior)[[3]]['Std. Err']),as.matrix(summary(sourcexprior)[[4]]['Std. Err'])),c(as.matrix(summary(sourcexprior)[[3]]['Pr(>|z|)']),as.matrix(summary(sourcexprior)[[4]]['Pr(>|z|)']))))

#change rownames to prevent confusion
rownames(sourceP$coefficients)[12] <- 'sourcePro-development:priorsPro-environment'
################################################



####################################
# Make cj tables
#mod_texreg('C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/tables/cj-unconditional.tex',list(unconditional),   custom.model.names = c("Average Treatment Effect"),  caption = "Average Treatment Effect",  caption.above=T,include.rsquared = F, include.adjrs = F,include.rmse=F, custom.gof.names = c('N'), float.pos='h!',custom.coef.names = c('(Intercept)','Desert','Oil rig','Pipeline','Solar wind','Message orientation: Pro-development','Moderately political','Highly political','Source orientation: Pro-development','Candidate','Organization'),omit.coef='(Intercept)', fontsize='small')


#mod_texreg('C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/tables/cj-interact-orient.tex',list(mesxprior, sourcexprior),   custom.model.names = c("Message $\\times$ Priors","Source $\\times$ Priors"),  caption = "Source and message interacted with participants' priors",  caption.above=T,include.rsquared = F, include.adjrs = F,include.rmse=F, custom.gof.names = c('N'), float.pos='h!',custom.coef.names = c('(Intercept)','Desert','Oil rig','Pipeline','Solar wind','Message orientation: Pro-development','Moderately political','Highly political','Source orientation: Pro-development','Candidate','Organization', 'Message orientation conditional on pro-environment','Message orientation conditional on Pro-development','Source orientation conditional on pro-environment','Source orientation conditional on Pro-development'),omit.coef='(Intercept)', fontsize='small')




####################################
# Make cj plots
###############
# Unconditional

#use cjoint with small modification of source code (eliminate images from plot)
f1 <- plt1(unconditional,colors = c("darkseagreen4","lightpink3","sienna4", 'navajowhite3'), xlab="Change in Pr(Ad considered more political)", text.size=20, attribute.names= c('Images','Message orientation','Message strength','Source orientation','Source type'),point.size=.8)

#save
ggsave("C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/plots/cj-uncondition-plot.pdf", width = 12, height = 9, units = "in")
####################################

###############
# Interactions

#edited cj source code to extract data
plt(mesxprior,plot.display='interaction')#run code
datM <- PLOTDATA#extract output

plt(sourcexprior,plot.display='interaction')
datS <- PLOTDATA

#put extracted data in new object
datInteract <- rbind(datM[c(3,6),],datS[c(3,6),])
datInteract$Attribute <- c('Message orientation','Message orientation','Source orientation','Source orientation')

#make plot
f2 = ggplot(datInteract, aes(y = pe, x = printvar , color=Attribute  ))+
          geom_pointrange(aes(y = pe,ymin = lower, 
                              ymax = upper, fill =Attribute),
                          position = position_dodge(width = -.6),
                          size = .8) +
          coord_flip() + 
           xlab("")+ 
          ylab("")+ 
          
          theme(panel.spacing.x = unit(27, "mm"),
                axis.text.x=element_text(angle=15,
                                         hjust=0.5,vjust=.5))+
          theme(text=element_text(size=20))

f2 + geom_hline( yintercept = 0, colour = "gray", size = 1, lty= 'dashed')+ scale_color_manual(values =  c("darkseagreen4","lightpink3")) +theme_light()+
  theme(text=element_text(size=20))+
  theme(panel.grid.minor =   element_blank(),
        panel.grid.major =   
          element_line(colour = "grey87",size=0.5))


#save to /plots
ggsave("C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/plots/cj-interact-plot.pdf", width = 12, height = 8, units = "in")

###################
#Real advertisments
###################

######################
# Pooled regressions:
# real ads FIGS 3,4,5

##############
#unconditional

trump3 <- lm_robust(estimate ~ Trump, data =subset(pooled,pooled$experiment=='POLADSTRUMP1S4'|pooled$experiment == 'POLADSTRUMP2S4') , weights = WEIGHT, clusters = CaseId)

biden3  <- lm_robust(estimate ~ Biden, data = subset(pooled,pooled$experiment=='POLADSBIDEN1S4'|pooled$experiment == 'POLADSBIDEN2S4'), weights = WEIGHT, clusters = CaseId)

patagonia3  <- lm_robust(estimate ~ Patagonia, data =subset(pooled,pooled$experiment=='POLADSPATAGONIA1S4'|pooled$experiment == 'POLADSPATAGONIA2S4'), weights = WEIGHT, clusters = CaseId)

exxon3 <- lm_robust(estimate ~ Exxon, data = subset(pooled,pooled$experiment=='POLADSEXXON1S4'|pooled$experiment == 'POLADSEXXON2S4'), weights = WEIGHT, clusters = CaseId)

power3  <- lm_robust(estimate ~ Power, data = subset(pooled,pooled$experiment=='POLADSPOWER1S4'|pooled$experiment == 'POLADSPOWER2S4'), weights = WEIGHT, clusters = CaseId)


sierra3 <- lm_robust(estimate ~Sierra, data = subset(pooled,pooled$experiment=='POLADSSIERRA1S4'|pooled$experiment == 'POLADSSIERRA2S4'), weights = WEIGHT, clusters = CaseId)


colgate3 <- lm_robust(estimate ~ Colgate, data = subset(pooled,pooled$experiment=='POLADSCOLGATE1S4'|pooled$experiment == 'POLADSCOLGATE2S4'), weights = WEIGHT, clusters = CaseId)


msc3 <- lm_robust(estimate ~ MSC, data = subset(pooled,pooled$experiment=='POLADSMSC1S4'|pooled$experiment == 'POLADSMSC2S4'), weights = WEIGHT, clusters = CaseId)


#############################
#interaction with orientation

trump6 <- lm_robust(estimate ~ Trump * orientation, data =subset(pooled,pooled$experiment=='POLADSTRUMP1S4'|pooled$experiment == 'POLADSTRUMP2S4') , weights = WEIGHT, clusters = CaseId)

biden6  <- lm_robust(estimate ~ Biden* orientation, data = subset(pooled,pooled$experiment=='POLADSBIDEN1S4'|pooled$experiment == 'POLADSBIDEN2S4'), weights = WEIGHT, clusters = CaseId)

patagonia6  <- lm_robust(estimate ~ Patagonia* orientation, data =subset(pooled,pooled$experiment=='POLADSPATAGONIA1S4'|pooled$experiment == 'POLADSPATAGONIA2S4'), weights = WEIGHT, clusters = CaseId)

exxon6 <- lm_robust(estimate ~ Exxon* orientation, data = subset(pooled,pooled$experiment=='POLADSEXXON1S4'|pooled$experiment == 'POLADSEXXON2S4'), weights = WEIGHT, clusters = CaseId)

power6  <- lm_robust(estimate ~ Power* orientation, data = subset(pooled,pooled$experiment=='POLADSPOWER1S4'|pooled$experiment == 'POLADSPOWER2S4'), weights = WEIGHT, clusters = CaseId)


sierra6 <- lm_robust(estimate ~Sierra* orientation, data = subset(pooled,pooled$experiment=='POLADSSIERRA1S4'|pooled$experiment == 'POLADSSIERRA2S4'), weights = WEIGHT, clusters = CaseId)


colgate6 <- lm_robust(estimate ~ Colgate* orientation, data = subset(pooled,pooled$experiment=='POLADSCOLGATE1S4'|pooled$experiment == 'POLADSCOLGATE2S4'), weights = WEIGHT, clusters = CaseId)


msc6 <- lm_robust(estimate ~ MSC* orientation, data = subset(pooled,pooled$experiment=='POLADSMSC1S4'|pooled$experiment == 'POLADSMSC2S4'), weights = WEIGHT, clusters = CaseId)




##############
#make datasets
#for tables
##############


#########################
# Interaction Orientation 
#########################

###########
#politicans

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



##############
#organizations

#gather data into small object
orgI = multiplot(power6,sierra6, intercept = F, plot= F)

#reorder for plot display
orgI$Coefficient = factor(orgI$Coefficient, 
                          levels= rev(c("orientationPro-development",
                                        "Power","Sierra",
                                        "Power:orientationPro-development",
                                        "Sierra:orientationPro-development")))

#remove Pro-development because it shows up in both models (looks bad in plot)
orgi <- subset(orgI,orgI$Coefficient != 'orientationPro-development')


##########
#companies

#gather data into small object
comI = multiplot(exxon6,msc6,patagonia6, colgate6,intercept = F, plot = F)

#reorder for plot display
comI$Coefficient = factor(comIa$Coefficient, 
                           levels=rev(c(  "orientationPro-development","Exxon","MSC",
                                          'Patagonia','Colgate'
                                        , "Exxon:orientationPro-development" 
                                        ,"MSC:orientationPro-development" , 
                                        "Patagonia:orientationPro-development" 
                                        ,"Colgate:orientationPro-development" )))

#remove Pro-development because it shows up in both models (looks bad in plot)
comi <- subset(comI, comI$Coefficient != 'orientationPro-development')






##############################
#save tables to file
###############################
#save table lm
#mod_texreg('C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/tables/lm-main.tex',list(trump1,biden1, trump2,biden2,trump3, biden3),  custom.header = list("Original" = 1:2, "Correction" = 3:4, 'Pooled'=5:6),custom.model.names = c("Trump","Biden","Trump","Biden","Trump","Biden"),  caption = "Perceived political level of politicians' real ads",  caption.above=T, custom.gof.names = c('R^2','Adjusted R^2','N','RMSE','N. Clusters'), float.pos='h!')

###############################
#save table interaction orient 
#politicians and organizations
#mod_texreg('C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/tables/lm-orient-polorg.tex',list(trump6, biden6, power6,sierra6),  custom.header = list("Politicians" = 1:2, "Organizations" = 3:4),  custom.model.names = c("Trump","Biden","Power","Sierra"),  caption = "Perceived political level of politicians' and organizations' real ads by issue orientation",  caption.above=T, custom.gof.names = c('R^2','Adjusted R^2','N','RMSE','N. Clusters'), float.pos='h!',custom.coef.names = c('(Intercept)',"Trump",'Pro-development','Trump x Pro-development','Biden','Biden x Pro-development',"Power",'Power x Pro-development','Sierra','Sierra x Pro-development'),include.ci=F)

###############################
#save table interaction orient 
#companies
#mod_texreg('C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/tables/lm-orient-com.tex',list(exxon6,msc6,patagonia6, colgate6),  custom.header = list("Pro-development" = 1:2, "Pro-environment" = 3:4),  custom.model.names = c('Exxon','MSC','Patagonia','Colgate'),  caption = "Perceived political level of company's real ads by issue orientation",  caption.above=T, custom.gof.names = c('R^2','Adjusted R^2','N','RMSE','N. Clusters'), float.pos='h!', custom.coef.names = c('(Intercept)','Exxon','Pro-development', 'Exxon x Pro-development','MSC','MSC x Pro-development','Patagonia','Patagonia x Pro-development','Colgate','Colgate x Pro-development'),include.ci=F)


############
#plot means
###########
###########
#pooled

#reorder level for display
datc$Ad = factor(datc$Ad, levels=rev(c("Biden","Trump",
                                    "Colgate","Exxon","MSC",'Patagonia',
                                    "Power", "Sierra")))
colnames(datc)[7] <-'Political Content Level'
f3 <- ggplot(datc, aes(y = Estimate, x = Ad , color=`Political Content Level`  ))+
             geom_pointrange(aes(y = Estimate,ymin = `CI.lower`, 
                                 ymax = `CI.upper`, fill = `Political Content Level`),
                             position = position_dodge(width = -.6),
                             size = .8) +
            coord_flip() + 
            ylim(1,4)+  xlab("")+ 
            ylab("")+ 
            scale_y_continuous(limits = c(1,5),
                               labels=c('Not \n political \n (1)',
                                        '',
                                        '3',
                                        '',
                                        'Extremely \n political \n (5)'))+ 
            theme(panel.spacing.x = unit(27, "mm"),
                  axis.text.x=element_text(angle=15,
                                           hjust=0.5,vjust=.5))+
            theme_light()+
            theme(axis.text.y = element_text(face=rev(c("bold","bold",
                                                    'plain','plain','plain','plain',
                                                    "italic","italic"))))

f3+theme(text=element_text(size=20)) + scale_color_manual(values =  c("sienna4", 'navajowhite3'))+ geom_hline( yintercept = 0, colour = "gray", size = 1, lty= 'dashed')

ggsave("C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/plots/mean-pool-plot.pdf", width = 12, height = 9, units = "in")





#############################
#Plot interaction-Orientation

############
#politicians

#make object for ylabels
polY <- c("Biden","Trump",  
          expression("Biden "%*%" Pro-development") , 
          expression("Trump "%*%" Pro-development"))


f4a <- ggplot(poli, aes(y = Value, x = Coefficient))+
  geom_pointrange(aes(y = Value,
                      ymin = `HighInner`,
                      ymax = `LowInner`),
                  position = position_dodge(width = -.6),
                  size = .8,color='lightpink3') +
  coord_flip() + 
  xlab("") +  ylab("\n Rating of high versus low real ads")+
  ylim(-.75,2.2)+
  scale_color_manual(values =  c( 'sienna4'))+
  theme_light()


f4a + geom_hline( yintercept = 0, colour = "gray", size = 1, lty= 'dashed')+
  theme(text=element_text(size=30))+
  theme(panel.grid.minor =   element_blank(),
        panel.grid.major =   element_line(colour = "grey87",size=0.5))+
  scale_x_discrete(labels = rev(polY))

ggsave("C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/plots/lm-orient-pol-plot.pdf", width = 12, height = 9, units = "in")

##############
#organizations
orgY <- c("Power","Sierra", 
          expression("Power "%*%" Pro-development") 
          ,expression("Sierra "%*%" Pro-development"))

f4b <- ggplot(orgi, aes(y = Value, x = Coefficient))+
              geom_pointrange(aes(y = Value,
                                  ymin = `HighInner`,
                                  ymax = `LowInner`),
                              position = position_dodge(width = -.6),
                              size = .8,color='sienna4') +
              coord_flip() + 
              xlab("") +  ylab("\n Rating of high versus low real ads")+
              ylim(-.75,2.2)+
              scale_color_manual(values =  c( 'sienna4'))+
              theme_light()

f4b  + geom_hline( yintercept = 0, colour = "gray", size = 1, lty= 'dashed')+
                theme(text=element_text(size=30))+
                theme(panel.grid.minor =   element_blank(),
                      panel.grid.major =   element_line(colour = "grey87",size=0.5))+
                scale_x_discrete(labels = rev(orgY))

ggsave("C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/plots/lm-orient-org-plot.pdf", width = 12, height = 9, units = "in")



###########
#companies

comYa <- c("Exxon","MSC","Colgate",'Patagonia', expression("Exxon "%*%" Pro-development" ) ,expression("MSC "%*%" Pro-development"),expression("Colgate "%*%" Pro-development" ), expression( "Patagonia "%*%" Pro-development") )


f5 <- ggplot(comi, aes(y = Value, x = Coefficient,))+
               geom_pointrange(aes(y = Value, ymin = `HighInner`,ymax = `LowInner`),
                               position = position_dodge(width = -.6), size = .8, 
                               color = "paleturquoise4") +
               coord_flip() + xlab("") +  
               ylab("\n Rating of high versus low real ads")+
               ylim(-.75,2.2)+ 
               scale_color_manual(values =  c( 'sienna4'))+
               theme_light()

f5  + geom_hline( yintercept = 0, colour = "gray", size = 1, lty= 'dashed')+
                theme(text=element_text(size=25))+
                theme(panel.grid.minor =   element_blank(),
                      panel.grid.major =   
                        element_line(colour = "grey87",size=0.5))+
                scale_x_discrete(labels = rev(comYa))





ggsave("C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/plots/lm-orient-com-plot.pdf", width = 12, height = 8, units = "in")

############################################################################
# descriptive statistics
#############################################################################

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
des2$name = factor(des2$name, levels=(c("Exxon","MSC",'Patagonia',"Colgate")))

des1$lvl = factor(des1$lvl, levels=(c("Low",
                                        "High")))
des2$lvl = factor(des2$lvl, levels=(c("Low",
                                      "High")))

ggplot(data = des1, aes(rating, freq,fill=as.factor(name))) +
  geom_bar(stat='identity') +
  labs(y = "Frequency", x = "Rating") + 
  facet_grid(name ~ lvl)+
  theme(panel.grid.minor =   element_blank(),
        panel.grid.major =   
          element_line(colour = "grey87",size=0.5)) +
  theme(strip.background =element_rect(fill="GREY87"))+
  theme(strip.text = element_text(colour = 'GREY32'))+
  scale_fill_manual( values=c("darkseagreen4","lightpink3","sienna4", 'navajowhite3')) +
  theme(legend.position = "none") +
  theme(text=element_text(size=25))

ggsave("C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/plots/dist-polorg-plot.pdf", width = 12, height = 10, units = "in")

ggplot(data = des2, aes(rating, freq,fill=as.factor(name))) +
  geom_bar( stat='identity') +
  labs(y = "Frequency", x = "Rating") + 
  facet_grid(name ~ lvl)+
  theme(panel.grid.minor =   element_blank(),
        panel.grid.major =   
          element_line(colour = "grey87",size=0.5)) +
  theme(strip.background =element_rect(fill="GREY87"))+
  theme(strip.text = element_text(colour = 'GREY32'))+
  scale_fill_manual( values=c("darkseagreen4","lightpink3","sienna4", 'navajowhite3')) +
  theme(legend.position = "none") +
  theme(text=element_text(size=25))

ggsave("C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/plots/dist-com-plot.pdf", width = 12, height = 10, units = "in")



summary(cj[,c('AGE','INCOME')])

############################################################################
# data/tables/plots not in MAIN PAPER
#############################################################################

#############
#conjoint
#############

#############
#message-pid
#############
#cjoint
mesxpid <- amce(dv1 ~ image  + messageOrient + messageStrength + sourceOrient + sourceType   +  pid + messageOrient*pid, cluster=TRUE, respondent.id="CaseId", weights ='WEIGHT', data=cj[is.na(cj$pid)==F,], baseline= bases, respondent.varying = 'pid', na.ignore=T)

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
################################################

#############
# tables pid
#############

mod_texreg('C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/tables/cj-interact-pid.tex',list(mpid, sPid),   custom.model.names = c("Message $\\times$ PID","Source $\\times$ PID"),  caption = "Source and message interacted with participants' priors",  caption.above=T,include.rsquared = F, include.adjrs = F,include.rmse=F, custom.gof.names = c('N'), float.pos='h!',custom.coef.names = c('(Intercept)','Desert','Oil rig','Pipeline','Solar wind','Message orientation (Pro-development)','Moderately political','Highly political','Source orientation (Pro-development)','Candidate','Organization', 'Message orientation pro-environment conditional on republican','Message orientation Pro-development conditional on republican','Republican','Source orientation  Pro-development conditional on Republican'),omit.coef='(Intercept)')

###########################
# Regressions for real ads
###########################

##########
#Original 
##########

trump1 <- lm_robust(estimate ~ Trump, data = subset(original,original$experiment=='POLADSTRUMP1S4'|original$experiment == 'POLADSTRUMP2S4'), weights = WEIGHT, clusters = CaseId)

biden1 <- lm_robust(estimate ~ Biden, data = subset(original,original$experiment=='POLADSBIDEN1S4'|original$experiment == 'POLADSBIDEN2S4'), weights = WEIGHT, clusters = CaseId)

patagonia1 <- lm_robust(estimate ~ Patagonia, data =subset(original,original$experiment=='POLADSPATAGONIA1S4'|original$experiment == 'POLADSPATAGONIA2S4'), weights = WEIGHT, clusters = CaseId)

exxon1 <- lm_robust(estimate ~ Exxon, data = subset(original,original$experiment=='POLADSEXXON1S4'|original$experiment == 'POLADSEXXON2S4'), weights = WEIGHT, clusters = CaseId)


power1 <- lm_robust(estimate ~ Power, data = subset(original,original$experiment=='POLADSPOWER1S4'|original$experiment == 'POLADSPOWER2S4'), weights = WEIGHT, clusters = CaseId)


sierra1 <- lm_robust(estimate ~Sierra, data = subset(original,original$experiment=='POLADSSIERRA1S4'|original$experiment == 'POLADSSIERRA2S4'), weights = WEIGHT, clusters = CaseId)


colgate1 <- lm_robust(estimate ~ Colgate, data = subset(original,original$experiment=='POLADSCOLGATE1S4'|original$experiment == 'POLADSCOLGATE2S4'), weights = WEIGHT, clusters = CaseId)


msc1 <- lm_robust(estimate ~ MSC, data = subset(original,original$experiment=='POLADSMSC1S4'|original$experiment == 'POLADSMSC2S4'), weights = WEIGHT, clusters = CaseId)

##################################
# correction
trump2 <- lm_robust(estimate ~ Trump, data =subset(correction,correction$experiment=='POLADSTRUMP1S4'|correction$experiment == 'POLADSTRUMP2S4') , weights = WEIGHT, clusters = CaseId)

biden2  <- lm_robust(estimate ~ Biden, data = subset(correction,correction$experiment=='POLADSBIDEN1S4'|correction$experiment == 'POLADSBIDEN2S4'), weights = WEIGHT, clusters = CaseId)

patagonia2  <- lm_robust(estimate ~ Patagonia, data =subset(correction,correction$experiment=='POLADSPATAGONIA1S4'|correction$experiment == 'POLADSPATAGONIA2S4'), weights = WEIGHT, clusters = CaseId)

exxon2 <- lm_robust(estimate ~ Exxon, data = subset(correction,correction$experiment=='POLADSEXXON1S4'|correction$experiment == 'POLADSEXXON2S4'), weights = WEIGHT, clusters = CaseId)

power2  <- lm_robust(estimate ~ Power, data = subset(correction,correction$experiment=='POLADSPOWER1S4'|correction$experiment == 'POLADSPOWER2S4'), weights = WEIGHT, clusters = CaseId)


sierra2 <- lm_robust(estimate ~Sierra, data = subset(correction,correction$experiment=='POLADSSIERRA1S4'|correction$experiment == 'POLADSSIERRA2S4'), weights = WEIGHT, clusters = CaseId)


colgate2 <- lm_robust(estimate ~ Colgate, data = subset(correction,correction$experiment=='POLADSCOLGATE1S4'|correction$experiment == 'POLADSCOLGATE2S4'), weights = WEIGHT, clusters = CaseId)


msc2 <- lm_robust(estimate ~ MSC, data = subset(correction,correction$experiment=='POLADSMSC1S4'|correction$experiment == 'POLADSMSC2S4'), weights = WEIGHT, clusters = CaseId)






trump4 <- lm_robust(estimate ~ Trump * orientation, data =subset(original,original$experiment=='POLADSTRUMP1S4'|original$experiment == 'POLADSTRUMP2S4') , weights = WEIGHT, clusters = CaseId)

biden4  <- lm_robust(estimate ~ Biden* orientation, data = subset(original,original$experiment=='POLADSBIDEN1S4'|original$experiment == 'POLADSBIDEN2S4'), weights = WEIGHT, clusters = CaseId)

patagonia4  <- lm_robust(estimate ~ Patagonia* orientation, data =subset(original,original$experiment=='POLADSPATAGONIA1S4'|original$experiment == 'POLADSPATAGONIA2S4'), weights = WEIGHT, clusters = CaseId)

exxon4 <- lm_robust(estimate ~ Exxon* orientation, data = subset(original,original$experiment=='POLADSEXXON1S4'|original$experiment == 'POLADSEXXON2S4'), weights = WEIGHT, clusters = CaseId)

power4  <- lm_robust(estimate ~ Power* orientation, data = subset(original,original$experiment=='POLADSPOWER1S4'|original$experiment == 'POLADSPOWER2S4'), weights = WEIGHT, clusters = CaseId)


sierra4 <- lm_robust(estimate ~Sierra* orientation, data = subset(original,original$experiment=='POLADSSIERRA1S4'|original$experiment == 'POLADSSIERRA2S4'), weights = WEIGHT, clusters = CaseId)


colgate4 <- lm_robust(estimate ~ Colgate* orientation, data = subset(original,original$experiment=='POLADSCOLGATE1S4'|original$experiment == 'POLADSCOLGATE2S4'), weights = WEIGHT, clusters = CaseId)


msc4 <- lm_robust(estimate ~ MSC* orientation, data = subset(original,original$experiment=='POLADSMSC1S4'|original$experiment == 'POLADSMSC2S4'), weights = WEIGHT, clusters = CaseId)

#########################################
# correction interaction with orientation

trump5 <- lm_robust(estimate ~ Trump * orientation, data =subset(correction,correction$experiment=='POLADSTRUMP1S4'|correction$experiment == 'POLADSTRUMP2S4') , weights = WEIGHT, clusters = CaseId)

biden5  <- lm_robust(estimate ~ Biden* orientation, data = subset(correction,correction$experiment=='POLADSBIDEN1S4'|correction$experiment == 'POLADSBIDEN2S4'), weights = WEIGHT, clusters = CaseId)

patagonia5  <- lm_robust(estimate ~ Patagonia* orientation, data =subset(correction,correction$experiment=='POLADSPATAGONIA1S4'|correction$experiment == 'POLADSPATAGONIA2S4'), weights = WEIGHT, clusters = CaseId)

exxon5 <- lm_robust(estimate ~ Exxon* orientation, data = subset(correction,correction$experiment=='POLADSEXXON1S4'|correction$experiment == 'POLADSEXXON2S4'), weights = WEIGHT, clusters = CaseId)

power5  <- lm_robust(estimate ~ Power* orientation, data = subset(correction,correction$experiment=='POLADSPOWER1S4'|correction$experiment == 'POLADSPOWER2S4'), weights = WEIGHT, clusters = CaseId)


sierra5 <- lm_robust(estimate ~Sierra* orientation, data = subset(correction,correction$experiment=='POLADSSIERRA1S4'|correction$experiment == 'POLADSSIERRA2S4'), weights = WEIGHT, clusters = CaseId)


colgate5 <- lm_robust(estimate ~ Colgate* orientation, data = subset(correction,correction$experiment=='POLADSCOLGATE1S4'|correction$experiment == 'POLADSCOLGATE2S4'), weights = WEIGHT, clusters = CaseId)


msc5 <- lm_robust(estimate ~ MSC* orientation, data = subset(correction,correction$experiment=='POLADSMSC1S4'|correction$experiment == 'POLADSMSC2S4'), weights = WEIGHT, clusters = CaseId)




trump7 <- lm_robust(estimate ~ Trump * pid, data =subset(original,original$experiment=='POLADSTRUMP1S4'|original$experiment == 'POLADSTRUMP2S4') , weights = WEIGHT, clusters = CaseId)

biden7  <- lm_robust(estimate ~ Biden* pid, data = subset(original,original$experiment=='POLADSBIDEN1S4'|original$experiment == 'POLADSBIDEN2S4'), weights = WEIGHT, clusters = CaseId)

patagonia7  <- lm_robust(estimate ~ Patagonia* pid, data =subset(original,original$experiment=='POLADSPATAGONIA1S4'|original$experiment == 'POLADSPATAGONIA2S4'), weights = WEIGHT, clusters = CaseId)

exxon7 <- lm_robust(estimate ~ Exxon* pid, data = subset(original,original$experiment=='POLADSEXXON1S4'|original$experiment == 'POLADSEXXON2S4'), weights = WEIGHT, clusters = CaseId)

power7  <- lm_robust(estimate ~ Power* pid, data = subset(original,original$experiment=='POLADSPOWER1S4'|original$experiment == 'POLADSPOWER2S4'), weights = WEIGHT, clusters = CaseId)


sierra7 <- lm_robust(estimate ~Sierra* pid, data = subset(original,original$experiment=='POLADSSIERRA1S4'|original$experiment == 'POLADSSIERRA2S4'), weights = WEIGHT, clusters = CaseId)


colgate7 <- lm_robust(estimate ~ Colgate* pid, data = subset(original,original$experiment=='POLADSCOLGATE1S4'|original$experiment == 'POLADSCOLGATE2S4'), weights = WEIGHT, clusters = CaseId)


msc7 <- lm_robust(estimate ~ MSC* pid, data = subset(original,original$experiment=='POLADSMSC1S4'|original$experiment == 'POLADSMSC2S4'), weights = WEIGHT, clusters = CaseId)



trump8 <- lm_robust(estimate ~ Trump * pid, data =subset(correction,correction$experiment=='POLADSTRUMP1S4'|correction$experiment == 'POLADSTRUMP2S4') , weights = WEIGHT, clusters = CaseId)

biden8  <- lm_robust(estimate ~ Biden* pid, data = subset(correction,correction$experiment=='POLADSBIDEN1S4'|correction$experiment == 'POLADSBIDEN2S4'), weights = WEIGHT, clusters = CaseId)

patagonia8  <- lm_robust(estimate ~ Patagonia* pid, data =subset(correction,correction$experiment=='POLADSPATAGONIA1S4'|correction$experiment == 'POLADSPATAGONIA2S4'), weights = WEIGHT, clusters = CaseId)

exxon8<- lm_robust(estimate ~ Exxon* pid, data = subset(correction,correction$experiment=='POLADSEXXON1S4'|correction$experiment == 'POLADSEXXON2S4'), weights = WEIGHT, clusters = CaseId)

power8  <- lm_robust(estimate ~ Power* pid, data = subset(correction,correction$experiment=='POLADSPOWER1S4'|correction$experiment == 'POLADSPOWER2S4'), weights = WEIGHT, clusters = CaseId)


sierra8 <- lm_robust(estimate ~Sierra* pid, data = subset(correction,correction$experiment=='POLADSSIERRA1S4'|correction$experiment == 'POLADSSIERRA2S4'), weights = WEIGHT, clusters = CaseId)


colgate8 <- lm_robust(estimate ~ Colgate* pid, data = subset(correction,correction$experiment=='POLADSCOLGATE1S4'|correction$experiment == 'POLADSCOLGATE2S4'), weights = WEIGHT, clusters = CaseId)


msc8 <- lm_robust(estimate ~ MSC* pid, data = subset(correction,correction$experiment=='POLADSMSC1S4'|correction$experiment == 'POLADSMSC2S4'), weights = WEIGHT, clusters = CaseId)



trump9 <- lm_robust(estimate ~ Trump * pid, data =subset(pooled,pooled$experiment=='POLADSTRUMP1S4'|pooled$experiment == 'POLADSTRUMP2S4') , weights = WEIGHT, clusters = CaseId)

biden9  <- lm_robust(estimate ~ Biden* pid, data = subset(pooled,pooled$experiment=='POLADSBIDEN1S4'|pooled$experiment == 'POLADSBIDEN2S4'), weights = WEIGHT, clusters = CaseId)

patagonia9  <- lm_robust(estimate ~ Patagonia* pid, data =subset(pooled,pooled$experiment=='POLADSPATAGONIA1S4'|pooled$experiment == 'POLADSPATAGONIA2S4'), weights = WEIGHT, clusters = CaseId)

exxon9 <- lm_robust(estimate ~ Exxon* pid, data = subset(pooled,pooled$experiment=='POLADSEXXON1S4'|pooled$experiment == 'POLADSEXXON2S4'), weights = WEIGHT, clusters = CaseId)

power9  <- lm_robust(estimate ~ Power* pid, data = subset(pooled,pooled$experiment=='POLADSPOWER1S4'|pooled$experiment == 'POLADSPOWER2S4'), weights = WEIGHT, clusters = CaseId)


sierra9 <- lm_robust(estimate ~Sierra* pid, data = subset(pooled,pooled$experiment=='POLADSSIERRA1S4'|pooled$experiment == 'POLADSSIERRA2S4'), weights = WEIGHT, clusters = CaseId)


colgate9 <- lm_robust(estimate ~ Colgate* pid, data = subset(pooled,pooled$experiment=='POLADSCOLGATE1S4'|pooled$experiment == 'POLADSCOLGATE2S4'), weights = WEIGHT, clusters = CaseId)


msc9 <- lm_robust(estimate ~ MSC* pid, data = subset(pooled,pooled$experiment=='POLADSMSC1S4'|pooled$experiment == 'POLADSMSC2S4'), weights = WEIGHT, clusters = CaseId)

##############
#make datasets
#for tables
##############


####################
#PID for appendix
###################
#politicians

polI2 = multiplot(trump9, biden9, power9,sierra9, intercept = F, plot = F)

polI2$Coefficient = factor(polI2$Coefficient, levels= rev(c("pidRepublican","Biden","Trump", "Biden:pidRepublican" ,"Trump:pidRepublican","Power", "Sierra",'Power:pidRepublican','Sierra:pidRepublican')))
poli2 <- subset(polI2, polI2$Coefficient != 'pidRepublican')


##################
#prooil companies

comIa2 = multiplot( exxon9,msc9,patagonia9,colgate9,intercept = F, plot = F)

comIa2$Coefficient = factor(comIa2$Coefficient, 
                            levels=rev(c("orientationPro-environment" 
                                         ,"orientationPro-development","Exxon","MSC",
                                         "Exxon:orientationPro-development" ,
                                         "MSC:orientationPro-development","Colgate"
                                         ,"Colgate:orientationPro-development"
                                         ,"Patagonia:orientationPro-development" )))
comia2 <- subset(comIa2, comIa2$Coefficient != 'orientationPro-environment' & comIa2$Coefficient != 'orientationPro-development')

###################


################################
#save table interaction pid 
#politicians and organizations
mod_texreg('C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/tables/lm-pid-polorg.tex',list(trump9, biden9, power9,sierra9),   custom.header = list("Politicians" = 1:2, "Organizations" = 3:4),  custom.model.names = c("Trump","Biden","Power","Sierra"),  caption = "Perceived political level of politicians' and organizations' real ads by issue orientation",  caption.above=T, custom.gof.names = c('R^2','Adjusted R^2','N','RMSE','N. Clusters'), float.pos='h!',custom.coef.names = c('(Intercept)',"Trump",'Republican','Trump x Repbulican','Biden','Biden x Republican',"Power",'Power x Republican','Sierra','Sierra x Republican'),include.ci=F)

#####################################
#save table interaction pid companies
mod_texreg('C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/tables/lm-pid-com.tex',list(exxon9,msc9,patagonia9,colgate9),  custom.header = list("Pro-development" = 1:2, "Pro-environment" = 3:4),  custom.model.names = c('Exxon','MSC','Patagonia','Colgate'),  caption = "Perceived political level of company's real ads by issue orientation",  caption.above=T, custom.gof.names = c('R^2','Adjusted R^2','N','RMSE','N. Clusters'), float.pos='h!',  custom.coef.names = c('(Intercept)','Exxon','Republican', 'Exxon x Republican','MSC','MSC x Republican','Patagonia','Patagonia x Republican','Colgate','Colgage x Republican'),include.ci=F)










###########
# original
#############
# politicians
pol = multiplot(trump1, trump2,trump3, biden1, biden2, biden3, intercept = F, plot = F) #collect data from lms
pol['Experiment'] <- c('Original','Correction','Pooled','Original','Correction','Pooled')#add indicator for which datset experiment its from
pol$Coefficient = factor(pol$Coefficient, levels=(c("Biden","Trump")))#relevel so that it goes from least to greatest
pol$Experiment = as.factor(pol$Experiment)

#####################################

##########
#companies
com = multiplot(exxon1, exxon2,exxon3,msc1, msc2, msc3, patagonia1, patagonia2,patagonia3,colgate1,colgate2, colgate3, intercept = F, plot = F)
com['Experiment'] <- c('Original','Correction','Pooled','Original','Correction','Pooled','Original','Correction','Pooled','Original','Correction','Pooled')
com$Coefficient = factor(com$Coefficient, levels=(c("Colgate","Exxon","MSC",'Patagonia')))
com$Experiment = as.factor(com$Experiment)


#####################################

##############
#organizations
org = multiplot(power1,power2,power3, sierra1, sierra2, sierra3, intercept = F, plot= F)
org['Experiment'] <- c('Original','Correction','Pooled','Original','Correction','Pooled')
org$Coefficient = factor(org$Coefficient, levels=(c("Power", "Sierra")))
org$Experiment = as.factor(org$Experiment)

#####################################


lmDat <- rbind(pol,com,org)#combine data
lmDat$Experiment <- factor(lmDat$Experiment, levels = c('Original','Correction','Pooled'))#relevel so that it displays in sensible order

#####################################
#plot
lmDat$Coefficient <- with(lmDat, factor(Coefficient, levels= rev(levels(Coefficient))))
theme_set(theme_light())
f1 <- ggplot(lmDat, aes(y = Value, x = Coefficient, color = Experiment))+
  geom_pointrange(aes(y = Value,
                      ymin = `HighInner`,
                      ymax = `LowInner`, fill =Experiment),
                  position = position_dodge(width = -.6),
                  size = .3)+
  theme(axis.text.y = element_text(face=c("bold","bold",'plain','plain','plain','plain',"italic","italic")))+
  coord_flip() + 
  xlab("") + ylab("\n Difference in rating of high and low political content")+
  ylim(0,2.5)+
  scale_color_manual(values =  c("rosybrown3","paleturquoise4", 'sienna4'))




f1 + theme(text=element_text(size=20))+
  theme(panel.grid.minor =element_blank(),
        panel.grid.major = element_line(colour = "grey87",size=0.5))


#ggsave("C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/plots/lm-main-plot.pdf", width = 12, height = 8, units = "in")

###############
#PLOT MEANS
###############

dat$Ad = factor(dat$Ad, levels=(c("Biden","Trump","Colgate","Exxon","MSC",'Patagonia',"Power", "Sierra")))
datb$Ad = factor(datb$Ad, levels=(c("Biden","Trump","Colgate","Exxon","MSC",'Patagonia',"Power", "Sierra")))

f3a <- ggplot(dat, aes(y = Estimate, x = Ad , color=political  ))+
  geom_pointrange(aes(y = Estimate,
                      ymin = `CI.lower`,
                      ymax = `CI.upper`, fill = political),
                  position = position_dodge(width = -.6),
                  size = .5) +
  coord_flip() + 
  ylim(1,4)+
  xlab("") + ylab("")+
  scale_y_continuous(limits = c(1,5), 
                     labels=c('Not \n political \n (1)','','3','','Extremely \n political \n (5)'))+ 
  theme(panel.spacing.x = unit(27, "mm"),
        axis.text.x=element_text(angle=15, hjust=0.5, vjust=.5))+ 
  theme(axis.text.x = element_text(size = 20),
        strip.text.x = element_text(size = 20), 
        legend.text = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  theme(axis.text.y = element_text(
    face=c("bold","bold",'plain','plain',
           'plain','plain',"italic","italic")))
f3a+ scale_color_manual(values =  c("darkseagreen4","navajowhite3"))

#ggsave("C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/plots/mean-orig-plot.pdf", width = 12, height = 9, units = "in")

f3b <- ggplot(datb, aes(y = Estimate, x = Ad , color=political  ))+
  geom_pointrange(aes(y = Estimate,
                      ymin = `CI.lower`,
                      ymax = `CI.upper`, fill = political),
                  position = position_dodge(width = -.6),
                  size = .5) +
  coord_flip() + 
  ylim(1,4)+
  xlab("") + ylab("")+
  scale_y_continuous(limits = c(1,5), 
                     labels=c('Not \n political','','3','','Extremely \n political'))+ 
  theme(panel.spacing.x = unit(27, "mm"),
        axis.text.x=element_text(angle=15, hjust=0.5, vjust=.5))+ 
  theme(axis.text.x = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  theme(axis.text.y = element_text(
    face=c("bold","bold",'plain','plain',
           'plain','plain',"italic","italic")))

f3b+ scale_color_manual(values =  c("darkseagreen4","navajowhite3"))
