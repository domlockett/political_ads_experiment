
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

#data part 1

CJ <- read.csv(file = 'C:/Users/dl0ck/OneDrive/Summer 2021/PolAdsTransparency/politics_user_study/Experiments/Conjoint Analysis/CJ.csv', header = TRUE, stringsAsFactors = T)


cj <- CJ[(complete.cases(CJ$dv1)&complete.cases(CJ$priors)),]

cj <- CJ[(complete.cases(CJ$dv1)&complete.cases(CJ$priors)),]

#data part 2

polAdORIG <- read.csv('C:/Users/dl0ck/OneDrive/Summer 2021/PolAdsTransparency/politics_user_study/Data/TASS 04/8557_WUSTL-Weidenbaum04_MAIN.csv') 

polAdREDO <- read.csv('C:/Users/dl0ck/OneDrive/Summer 2021/PolAdsTransparency/politics_user_study/Data/TASS04Reask/8557_WUSTL-Weidenbaum04Reask.csv') 

adrow <-c('Trump','Biden','Exxon',
          'Patagonia','Power',
          'Sierra','Colgate', 'SMSC')

#data part 3

tass2 <- read.csv('C:/Users/dl0ck/OneDrive/Summer 2021/PolAdsTransparency/politics_user_study/Data/TASS 05/8557_WUSTL-Weidenbaum05_Redacted.csv', header = TRUE, stringsAsFactors = FALSE)



## ----edit-data------------------------------------
baselines <- list()
baselines$sourceType <- "Company"
baselines$sourceOrient <- "Pro-oil"
baselines$messageStrength <- "Politically neutral"
baselines$messageOrient <- "Pro-oil"
baselines$priors <- "Pro-oil prior"
baselines$image <- "Landscape"
baselines$pid <- 'Republican'

base <- list()
base$pid <- 'Republican'
base$sourceOrient <- 'Pro-oil'

bases <- list()
bases$pid <- 'Republican'
bases$messageOrient <- 'Pro-oil'

# results <- amce(dv1 ~  image + sourceType + messageStrength + sourceOrient  + messageOrient + priors + , cluster=TRUE, respondent.id="CaseId", weights ='WEIGHT', data=cj, baseline= baselines)
# # plot(results, xlab="Change in Pr(Ad considered more political)", text.size=13)

messageUN <- amce(dv1 ~  image + sourceType + messageStrength + sourceOrient  + messageOrient + priors , cluster=TRUE,  weights ='WEIGHT', data=cj, baseline= baselines)
plot(messageUN, xlab="Change in Pr(Ad considered more political)", text.size=13)

## ----message-priors-------------------------------

messageAtt <- amce(dv1 ~  image + sourceType + messageStrength + sourceOrient  + messageOrient + priors + messageOrient*priors, cluster=TRUE, respondent.id="CaseId", respondent.varying= 'priors', weights ='WEIGHT', data=cj, baseline= baselines)


## ----message-priors-plot, echo=FALSE, fig.width=9, fig.height=5----
plot(messageAtt, xlab="Change in Pr(Ad considered more political)", text.size=13, title("How Individuals' Evaluate if Ads Are Political" ))


## ----message-priors-summ, echo=FALSE, results ='asis'----

x <- as.data.frame(summary(messageAtt)[1])
y <-  as.data.frame(summary(messageAtt)[3])
z <-  as.data.frame(summary(messageAtt)[4])

colnames(x) <- c("Attribute","Level","Estimate","Std. Err", "Z.value" , "Pr(>|z|)" , " ")
colnames(y) <- c("Attribute","Level","Estimate","Std. Err", "Z.value" , "Pr(>|z|)" , " ")
colnames(z) <- c("Attribute","Level","Estimate","Std. Err", "Z.value" , "Pr(>|z|)" , " ")

kable(rbind(x,y,z),booktabs=T)


## ----source-priors--------------------------------


sourcesAtt <- amce(dv1 ~  image + sourceType + messageStrength + sourceOrient  + messageOrient + priors + sourceOrient*priors, cluster=TRUE, respondent.id="CaseId", weights ='WEIGHT', data=cj, baseline= baselines, respondent.varying= 'priors')



## ----source-priors-plot, fig.width=13, fig.height=9,echo=F----

# Plot results
plot(sourcesAtt, xlab="Change in Pr(Ad considered more political)", text.size=13)


## ----source-priors-summ, fig.width=12, fig.height=8,echo=F----

x <- as.data.frame(summary(sourcesAtt)[1])
y <-  as.data.frame(summary(sourcesAtt)[3])
z <-  as.data.frame(summary(sourcesAtt)[4])

colnames(x) <- c("Attribute","Level","Estimate","Std. Err", "Z.value" , "Pr(>|z|)" , " ")
colnames(y) <- c("Attribute","Level","Estimate","Std. Err", "Z.value" , "Pr(>|z|)" , " ")
colnames(z) <- c("Attribute","Level","Estimate","Std. Err", "Z.value" , "Pr(>|z|)" , " ")

kable(rbind(x,y,z),booktabs=T)


## ----pid-message----------------------------------

messagePid <- amce(dv1 ~ image + sourceType + messageStrength + sourceOrient  + messageOrient + priors +  pid + messageOrient*pid, cluster=TRUE, respondent.id="CaseId", weights ='WEIGHT', data=cj[is.na(cj$pid)==F,], baseline= bases, respondent.varying = 'pid')



## ----pid-message-plot-----------------------------


plot(messagePid, facet_wrap=c("pid"), xlab="Change in Pr(Ad considered more political)")




## ----pid-source-----------------------------------

sourcePid <- amce(dv1 ~ sourceOrient +  pid+ pid*sourceOrient , data=cj[is.na(cj$pid)==F,], cluster=TRUE, respondent.id="CaseId", respondent.varying = 'pid',weights = 'WEIGHT', baseline= base)



## ----pid-source-plot------------------------------


plot(sourcePid, facet_wrap=c("pid"))


## ----weighted-mean--------------------------------
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




## ----Randomization-check, results='asis'----------
#Checking randomization and balance of variables against basic demographic variables
magic_for(silent = TRUE)

polAd <- list()

polAd[[1]] <- polAdORIG
polAd[[2]] <- polAdREDO

for(d in 1:2){
  #tables:
  n <- 0
  x <- which(colnames(polAd[[d]])=='POLADSTRUMP1S4')-2#+2 to column 239 which is trump1
  random <- c()
  for(i in 1:8){
    
    x <- x+2#every other colname to indicate which picture
    n <- n+1 #indx for rownames
    random <- rbind(random,table(polAd[[d]][,which(colnames(polAd[[d]])=='RND_00')+i]))#columns 23:30 = RND_01:08
    rownames(random)[n] <- adrow[n]
  }
  colnames(random)<-c('0','1')
put(random)}
t1a <-  kbl(magic_result()$random[[1]], format = "latex", booktabs = TRUE,linesep = c("", "", "", ""))
t2a <-  kable( magic_result()$random[[2]], format = "latex", booktabs = TRUE,linesep = c("", "", "", ""))



## ----Randomization-check-display, results='asis'----

  
cat(c("\\begin{table}[H]\\vspace{10em}
  \\caption{Randomization balance}
    \\begin{minipage}{.6\\textwidth}
      \\subcaption{Original}
      \\centering",
        t1a,
    "\\end{minipage}%
    \\begin{minipage}{.1\\textwidth}
      \\centering
       \\subcaption{Correction}",
        t2a,
    "\\end{minipage} 
\\end{table}"
))  



## ----balance-check-long, results='asis'-----------
magic_free()
magic_for(silent = TRUE)

columns <- cbind( c('AGE','GENDER','P_PARTYID7','IDEO1S4') , c('AGE','GENDER','PartyID7','IDEO1S4') )

for(d in 1:2){
  #balance:
  n <- 0
  x <- which(colnames(polAd[[d]])=='POLADSTRUMP1S4')-2#+2  trump1
  tabs <- c()
  demo <- columns[,d] #colnames of demos I want
  for(i in 1:8){
    x <- x+2#every other column for image indicator
    for(j in 1:4){
      n <- n+1#indx for tabs rownames
      dem <- polAd[[d]][,which( colnames(polAd[[d]])==demo[j])] #grab the indx of the
      #specific demographic variable I wanat
      rnd <- polAd[[d]][,which(colnames(polAd[[d]])=='RND_00')+i] #colnames associated with RND_0X
      tabs <- rbind(tabs,cbind(summary(dem[rnd==0],na.rm=T)[4],
                               summary(dem[rnd==1],na.rm=T)[4]))
      #4 rows for each of the demographics and 2 columns for the mean
      #when RND_0X == 1 or 0

      rownames(tabs)[n] <-paste(rep(adrow,each =4)[n],
                                 ":",capitalize(tolower(demo[j])))
    }
  }
  colnames(tabs)<- c('RND = 0','RND = 1')
  
  put(tabs)

}

t1b <- kable( magic_result()$tabs[[1]], format = "latex", booktabs = TRUE, linesep = c("", "", "", "\\addlinespace"))
t2b <-  kable( magic_result()$tabs[[2]], format = "latex", booktabs = TRUE, linesep = c("", "", "", '\\addlinespace'))


## ----balance-check-display, results='asis'--------

  
cat(c("\\begin{table}[H]
  \\caption{Balance of randomization by image and demographic}
    \\begin{minipage}{.5\\textwidth}
      \\subcaption{Original}
      \\centering",
        t1b,
    "\\end{minipage}%
    \\begin{minipage}{.5\\textwidth}
      \\centering
        \\subcaption{Correction}",
        t2b,
    "\\end{minipage} 
\\end{table}"
))  



## ----recod-for-plot, include=FALSE----------------

#Data is reordered so that higher values mean more political and skipped/refused/don't know changed to NA's
for(d in 1:2){
  range1 <<- which(colnames(polAd[[d]])=='POLADSTRUMP1S4'):
              which(colnames(polAd[[d]])=='POLADSMSC2S4')
  for(i in range1){
    #flip rating order and switch skipped/refused/don't knows to NA
     polAd[[d]][,i][polAd[[d]][,i]>5] <<- NA
     polAd[[d]][,i] <<- 6-polAd[[d]][,i]
  }
}




## ----experiments,fig.width=12, fig.height=6,fig.cap="Averages and confidence intervals by image"----



politic <- c('Low Political','High Political')
iter <- c('Original','Correction')

adfacet <- c(1,1,1,1,2,2,2,2)
magic_for(silent=T)
temp <- c()

for(d in 1:2){#for each experiment
  temp <-c()#reset temp
  for(j in 1:2){#for each indx set (1=low political 2=high political)
    indx <- which(colnames(polAd[[d]])=='POLADSTRUMP1S4')-j
    #indx starting at 1 or 2 less than variables of interest to collect every other
    for(i in 1:8){#for every ad image
      indx <- indx + 2 #cycle through sets of images 
      #i.e. indx 239 is trump1 which is the first in the consecutive 16 images 
      #trmp1 trump2 biden1 biden2 etc. high pol, low pol, high pol, low pol etc.
      #indx(= 239) - j(=1) -> indx(= 238) + 2(= 240) = trmp2-- low political
      #indx(= 239) - j(=2) -> indx(= 237) + 2(= 239) = trmp1-- high political
      data <- cbind(as.data.frame(cbind(t(as.matrix((wtd.ci(polAd[[d]][,indx],
               polAd[[d]]$WEIGHT )))))),'Iteration'=iter[d],'political'=politic[j], 'Ad'=adrow[i], 'face'=adfacet[i])
      #convert the confidence interval output to columns and ad indicator columns
      #based on indx d1=original, d2=correction, indx1:8=ad images
      temp <- rbind(temp,data)#grab statistics
    }
  }
  put(temp)#nice little grabber function to collect data from for loops
}


data1 <- rbind(magic_result()$temp[[1]],magic_result()$temp[[2]])#the two outer loops combined into one dataframe

#flip factor order for prefered display
data1$Iteration = factor(data1$Iteration, levels=c('Original','Correction'))

#define some aesthetically pleasing colors
vir <- rep(c("salmon4","bisque2"),8)
dat1 <- subset(data1, data1$face==1)
dat2 <- subset(data1, data1$face==2)


## ----experiments-p1,fig.width=12, fig.height=6,fig.cap="Averages and confidence intervals by image-- Trump, Biden, Exxon, Patagonia"----

p1 <- ggplot(dat1, aes(y = Estimate, x = Ad, shape= as.factor(political),color = as.factor(political)))+
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
    ylim(1,4)+
    xlab("") + ylab("")+
    theme_bw()+ 
    scale_y_continuous(limits = c(1,5), labels=c('Not at all political','','3','','Extremely political'))+ 
    theme(panel.spacing.x = unit(27, "mm"),axis.text.x=element_text(angle=15, hjust=0.5, vjust=.5))+ theme(axis.text.x = element_text(size = 12),strip.text.x = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.text.y = element_text(size = 10))
p1



## ----experiments-p2,fig.width=12, fig.height=7,fig.cap="Averages and confidence intervals by image-- Patagonia, Power, Sierra, Colgate, SMSC"----

p2 <- ggplot(dat2, aes(y = Estimate, x = Ad, shape= as.factor(political),color = as.factor(political)))+
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
p2



## ----PID broad, echo=FALSE------------------------
#PID
tass2$pid <- NA 
tass2$pid[tass2$PartyID7 == -1 ] = NA
tass2$pid[tass2$PartyID7 == 4 ] = "Moderate/Independent/None"
tass2$pid[tass2$PartyID7 < 4 & tass2$PartyID7 > 0] = "Democrat"
tass2$pid[tass2$PartyID7 > 4 & tass2$PartyID7  < 8] = "Republican"


##Real ad opinions: Should 'insert' ad be allowed on facebook
#Identify which columns correspond to real ads
# which(colnames(tass2)=='ADS5')
# which(colnames(tass2)=='ADS12')
#ADS5:12 -> cols [164:170]
#
Ads1 <- 164:170
for(i in 1:7){
 tass2[,Ads1[i]] <<- to_factor(tass2[,Ads1[i]])
 tass2[,Ads1[i]] <<- recode(tass2[,Ads1[i]], "1" = "Definetly allow", 
                "2" = "Probably allow", "3" = "Probably not allow", 
                "4" = "Definetly not allow", "77" = NA_character_, 
                "98" = NA_character_, "99" = NA_character_)}

#Source names of real ads--plot titles
images <- c('Truthout-Antifa & antiracist blamed for far right/police violence','Lake
             City AntiFa-Social programs, not police, prevent crime','American Action
             News- Are Democrats stealing your vote?','Vote.org-Register to vote'
             ,'Democrats-[Spanish] Register to vote to stop Donald Trump','Daily
             Cos-Sign petition for Senator Susan Collins to vote to convict
             Trump','Turning Point Action-Sign the petition to keep police funded and
             stop Antifa violence')
topics1 <- c()
for(i in 1:7){topics1 <<- rbind(topics1, paste('Should FB allow images like this:',
              images[i]))}
######################################################################################
##Opinions about platforms: How much trust/confidence do you have in X to
## appropriately handle political advertising
#Identify which columns relate to trust/confidence measures
# which(colnames(tass2)=='ADS3_A')
# which(colnames(tass2)=='ADS3_B')
# which(colnames(tass2)=='ADS3_C')

Ads2 <- 114:116

for(i in 1:3){
 tass2[,Ads2[i]] <<- to_factor(tass2[,Ads2[i]])
 tass2[,Ads2[i]] <<- recode(tass2[,Ads2[i]], "1" = "Great deal", 
                "2" = "Fair amount", "3" = "Not very much", 
                "4" = "None at all", "77" = NA_character_, 
                "98" = NA_character_, "99" = NA_character_)}
company <<- c(' Facebook ',' Twitter ',' Google ')
topics2 <<- c('How much trust and confidence do you have in' , 'to appropriately handle political advertising')


 
######################################################################################
##Broad ad opinion: X ads should be allowed if Y
# which(colnames(tass2)=='ADS4_A')
# which(colnames(tass2)=='ADS4_T')
#ADS4_A:T -> 137:156
Ads3 <- 137:156


for(i in 1:20){
 tass2[,Ads3[i]] <<- to_factor(tass2[,Ads3[i]])
 tass2[,Ads3[i]] <<- recode(tass2[,Ads3[i]], "1" = "Strongly agree",
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



## ----allow-real-ads, echo=FALSE, fig.height=6, fig.width=10, out.width='.49\\linewidth', results='asis'----
theme_set(theme_classic(base_size = 18))
            
for(i in 1:length(Ads1)){
  #weighted cross tabs of PID and Real Ad opinions with frequency and percentages
ct <<- cbind(as.data.frame((crosstab(tass2$pid,tass2[,Ads1[i]],
                          weight=tass2$WEIGHT,prop.r  =T, plot =F)$prop.row)
                          ), as.data.frame(crosstab(tass2$pid,tass2[,Ads1[i]],
                          weight=tass2$WEIGHT,prop.r  =T, plot = F)$tab)$Freq)
#colnames
colnames(ct) <<- c('PID','Response','Percentage','Frequency')
#whole number percentages
ct$Percentage <<- round(ct$Percentage*100,2)
ct <- ct %>% arrange(PID)


#plot
p <<- ggplot(ct, aes(x=PID, y = Percentage, fill = Response )) +
      geom_bar(position = 'dodge', stat = 'identity') + 
      labs(title = str_wrap(paste0(topics1[i],' [Column: ',Ads1[i],']'),50), xlab) +#wrap long titles
      theme(axis.text.x = element_text(angle = 45, vjust =.6))+
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+#wrap long xlab
      coord_cartesian(ylim=c(0,max(ct$Percentage)+5))+#flexible ylim with appropriate #
    scale_fill_manual(values=c("salmon3", "darkslategray4",
                               "palegreen4",'coral4'))
color <<- c('gray84', 'gray69')#set up table row color
len <<- c(4,4,4)#every set of results
tab = rep(color, length.out = length(len))#fill
x <<- rep(tab, times = len)
color_df <- data.frame(color = c(x),
                       stringsAsFactors = FALSE)
indx <<- c(1:5,7:8)

img <<- paste0('C:/Users/', Sys.getenv("USERNAME"), '/OneDrive/Summer 2021/PolAdsTransparency/politics_user_study/Data/Ads and Images 2-25-21/images/8557_AD_IMAGE_',indx[i],'.png')

my_table_theme <<- ttheme_minimal(core=list(bg_params = list(fill = color_df$color, col=NA)))
plt <<- print(p)
plt

grid.newpage()
grid.table(ct, theme = my_table_theme, rows = NULL)

grid.newpage()


(cat('![](', img, '){width=250px}\n\n', sep = ''))

}



## ----company-opinions, echo=FALSE, out.width='.49\\linewidth'----

for(i in 1:length(Ads2)){
ct <<- cbind(as.data.frame((crosstab(tass2$pid,tass2[,Ads2[i]],
                          weight=tass2$WEIGHT,prop.r  =T, plot =F)$prop.row)),
                          as.data.frame(crosstab(tass2$pid,tass2[,Ads2[i]],
                          weight=tass2$WEIGHT,prop.r  =T, plot = F)$tab)$Freq)

colnames(ct) <<- c('PID','Response','Percentage','Frequency')

ct$Percentage <<- round(ct$Percentage*100,2)

ct <- ct %>% arrange(PID)

p <<- ggplot(ct, aes(x=PID, y = Percentage, fill = Response )) +
      geom_bar(position = 'dodge', stat = 'identity') + 
      labs(title= str_wrap(paste0(topics2[1] , company[i] ,topics2[2],' [Column: ',Ads2[i],']'), 50))+
      theme(axis.text.x = element_text(angle = 45, vjust =.6))+
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
      coord_cartesian(ylim=c(0,max(ct$Percentage)+5))+
       scale_fill_manual(values=c("salmon3", "darkslategray4",
                                  "palegreen4",'coral4'))
plot1 <<- print(p)
plot1
color <<- c('gray84', 'gray69')
len <<- c(4,4,4)
tab = rep(color, length.out = length(len))
x <<- rep(tab, times = len)
color_df <<- data.frame(color = c(x),
                       stringsAsFactors = FALSE)


my_table_theme <<- ttheme_minimal(core=list(bg_params = list(fill = color_df$color, col=NA)))

grid.newpage()
grid.table(ct, theme = my_table_theme, rows = NULL)
}


## ----political-ad-opinions, echo=FALSE, fig.height=6, fig.width=10, out.width='.49\\linewidth', results='asis'----
for(i in 1:length(Ads3)){
ct <<- cbind(as.data.frame((crosstab(tass2$pid,tass2[,Ads3[i]],
                          weight=tass2$WEIGHT,prop.r  =T, plot =F)$prop.row)),
                          as.data.frame(crosstab(tass2$pid,tass2[,Ads3[i]],
                          weight=tass2$WEIGHT,prop.r  =T, plot = F)$tab)$Freq)

colnames(ct) <<- c('PID','Response','Percentage','Frequency')

ct$Percentage <<- round(ct$Percentage*100,2)
ct <<- ct %>% arrange(PID)

p <<- ggplot(ct, aes(x=PID, y = Percentage, fill = Response )) +
      geom_bar(position = 'dodge', stat = 'identity') + 
      labs(title=str_wrap(paste0(topics3[i],' [Column: ',Ads3[i],']'),50)) +
      theme(axis.text.x = element_text(angle = 45, vjust =.6))+
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
      coord_cartesian(ylim=c(0,max(ct$Percentage)+5))+
       scale_fill_manual(values=c("salmon3", "darkslategray4",
                                  "palegreen4",'coral4', 'moccasin','rosybrown1'))

writeLines("\n")
color <<- c('gray84', 'gray69')
len <<- c(6,6,6)
tab = rep(color, length.out = length(len))
x <<- rep(tab, times = len)
color_df <<- data.frame(color = c(x),
                       stringsAsFactors = FALSE)


my_table_theme <<- ttheme_minimal(core=list(bg_params = list(fill = color_df$color, col=NA)))
plt <<- print(p)
plt

grid.newpage()
grid.table(ct, theme = my_table_theme, rows = NULL)


}

