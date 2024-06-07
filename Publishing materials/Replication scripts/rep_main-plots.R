#models and functions
local <- 'C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/'
source(paste0(local,'Publishing materials/Replication scripts/rep_main-models.R'))
# plots and tables ordered as in main paper
####################
#conjoint experiment
####################
#Fig 1.
#Unconditional

#use cjoint with small modification of source code (eliminate images from plot)
#
nums <<- 7:20
plt1(unconditional)


plt1(unconditional, 
     colors=c("paleturquoise4","lightpink3","sienna4", 'navajowhite3'),
     xlab="Change in \n Pr(Ad considered more political)", 
     attribute.names= c('Images','Message orientation',
                        'Message strength','Source orientation',
                        'Source type'),
     point.size=1.5, 
     plot.theme=theme_set(theme_bw1())+
       theme(axis.text.y = element_text(size=rev(c(30,24 ,30 , 30, 24, 30 ,30,30 ,24,30,30, 24, 30, 30))))+
       theme(axis.text.y = element_text(face=rev(
         c("bold","italic", "plain","bold", 'italic',
           'plain','plain','bold', "italic","plain",
           'bold','italic','plain','plain'))))+
       theme(axis.text.x = element_text(size=24))+
       theme(legend.position = 'none'))


#save
ggsave(paste0(local,"Publishing materials/Replication plots/","cj-uncondition-plot.pdf"), width = 12, height = 9, units = "in")

################################                               
#source and message interaction:
#Fig. 2

labels= c('Message orientation:','  (Baseline: Pro-environment) ','  Pro-development ','Source orientation:','  (Baseline: Pro-environment)', '  Pro-development')
col <-c("paleturquoise4","lightpink3")


p=ggplot(d, aes(y = pe, x = var, color = `Prior attitudes:`, shape=`Prior attitudes:`))+
  theme_set(theme_bw1())+
  geom_hline(yintercept = 0, size = 1, colour = "gray", 
                   linetype = "dashed") +
  geom_pointrange(aes(y=pe,ymin = lower, ymax = upper, color = `Prior attitudes:`, shape=`Prior attitudes:`), 
                  position=position_dodge(width = -.8),
                  size = 1.5)+
  xlab("")+
  ylab('Change in \n Pr(Ad considered more political)')+
  ylim(-.13,.13)+
  coord_flip()


p +scale_x_discrete(labels=rev(labels))+
  theme(axis.text.y = element_text(face=rev(c("bold","italic", "plain","bold", 'italic','plain'))))+  
  theme(axis.text.y = element_text(size=rev(c(30,24 ,30 , 30, 24, 30))))+
  theme(legend.position = 'bottom',legend.justification = "left")+
  scale_color_manual(values=c("paleturquoise4","lightpink3"), 
                    guide="legend")+
  theme(axis.text.x= element_text( size = 20),legend.text=element_text(size=20))+
  theme(axis.title.x = element_text(size=24))+
  theme(legend.title = element_text(size=22, face = 'bold'))+
  theme(plot.margin = margin(1,2,0,0, "cm"))

#save
ggsave(paste0(local,"Publishing materials/Replication plots/","cj-interact-plot.pdf"), width = 12, height = 7, units = "in")
#table




screenreg(list(sourceP), include.ci=F)  
custom.model.names =  c('Model 1' ,'Model 2', 'Model 3'),
caption = "Effect of exposure to strong or weak political ads interacted with source on perceived politicalness of ads: Pooled analysis, \textit{non-political companies exlcuded}",
caption.above=T,float.pos='h!',  
fontsize='footnotesize', dcolumn=T, digits =2,use.packages=F, include.n.clusters=T,
include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2','N Respondents' ,'N'))

mod_texreg(paste0(local,'Publishing materials/Replication tables/','ra-source-political-bin.tex'),
           list(sourceP2),  
           caption = "Effect of exposure to strong or weak political ads interacted with source and message on perceived politicalness of ads: Pooled analysis, \textit{non-political companies exlcuded}",
           caption.above=T,float.pos='h!',  
           fontsize='footnotesize', dcolumn=T, digits =2,use.packages=F, include.n.clusters=T,
           label='table:ra-source-interact',  booktabs =T,  #custom.coef.names = c(
           # 'Candidate','Organization','Strong ads','Pro-development source', 
           #  'Strong ads \u00D7 candidate',
           ##),
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2','N Respondents' ,'N'))





texreg(list(sourceP2),include.ci=F)


#############
#pooled means

#table
## F4
#datc = datc[,-1]
datc1 <-datc
colnames(datc1) <- c('Mean','Lower 95 CI','Upper 95 CI','Standard Err.', 'Political level', 'Advertisement')
stargazer(datc1, out= paste0(local,'Publishing materials/Replication tables/','high-low-means.tex'),
          
          title = 'Average perceived politicalness of high and low political ads', label = 'table:pool-means', 
          summary = F, header =F, digits =2)



#reorder level for display
datc$Ad = factor(datc$Ad, levels=rev(c("Biden","Trump","Power", "Sierra",
                                       'Patagonia',"Exxon","MSC","Colgate")))
datc$`Political Content` <- datc$political

#plots
f4 <- ggplot(datc, aes(y = Estimate, x = Ad , color=`Political Content`  ))+
  geom_pointrange(aes(y = Estimate,ymin = `Lower.CI`, 
                      ymax = `Upper.CI`),
                  position = position_dodge(width = -.6),
                  size = 1) +
  coord_flip() + 
  ylab("")+ xlab("")+
   theme(panel.spacing.x = unit(27, "mm"),
        axis.text.x=element_text(angle=0,
                                 hjust=0.5,vjust=.5))+
  theme(axis.text.y = element_text(face=rev(c("bold","bold",
                                              "italic","italic",
                                              'plain','plain','plain','plain'))))

f4+theme(text=element_text(size=20)) + scale_color_manual(values =  c("sienna4", 'navajowhite3'), name = "")+ geom_hline( yintercept = 0, colour = "gray", size = 1, lty= 'dashed')+
  theme(plot.margin = unit(c(1,1,2,3), "cm"))+
  theme(axis.text.y= element_text( size = 20),axis.text.x= element_text( size = 18),legend.text=element_text(size=20))


#you should be able to add these labels if you change your plot region to 12x9 
#grid.locator(unit="native")
grid.brackets(100, 172,  100, 91, type=4, ticks=NA,h =.05)
grid.brackets(100, 325,  100, 245, type=4, ticks=NA,h =.05)
grid.brackets(100, 490,  100, 405, type=4,ticks=NA,h =.008, col ='gray')
grid.brackets(100, 650,  100, 560, type=4,ticks=NA,h =.008, col ='gray')
grid.brackets(100, 655,  100, 400, type=4,ticks=NA,h =.05)



grid.text(x=unit(33, 'native'), y=unit(100, 'native'), label='Politicians', rot=90,
          gp = gpar(fontsize = 18, fontface = "bold") )
grid.text(x=unit(33, 'native'), y=unit(225, 'native'), label='Organizations', rot=90,
          gp = gpar(fontsize = 18, fontface = "italic") )
grid.text(x=unit(33, 'native'), y=unit(400, 'native'), label='Companies', rot=90,
          gp = gpar(fontsize = 18) )
grid.text(x=unit(62, 'native'), y=unit(335, 'native'), label='Political', rot=90,
          gp = gpar(fontsize = 13))

grid.text(x=unit(62, 'native'), y=unit(450, 'native'), label='Non-political', rot=90,
          gp = gpar(fontsize = 13))

#stacked ads
#Table 3

#PID- politicians organizations

mod_texreg(paste0(local,'Publishing materials/Replication tables/','ra-source-political-bin.tex'),
           list(poolFit, interFit, partyFit),  
           custom.model.names =  c('Model 1' ,'Model 2', 'Model 3'),
           caption = "Effect of exposure to strong or weak political ads interacted with source on perceived politicalness of ads: Pooled analysis, \textit{non-political companies exlcuded}",
           caption.above=T,float.pos='h!',  
           fontsize='footnotesize', dcolumn=T, digits =2,use.packages=F, include.n.clusters=T,
           label='table:ra-source-interact',  booktabs =T,  #custom.coef.names = c(
           # 'Candidate','Organization','Strong ads','Pro-development source', 
           #  'Strong ads \u00D7 candidate',
           ##),
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2','N Respondents' ,'N'))



screenreg(list(poolFit, interFit, partyFit), include.ci=F)  
          custom.model.names =  c('Model 1' ,'Model 2', 'Model 3'),
          caption = "Effect of exposure to strong or weak political ads interacted with source on perceived politicalness of ads: Pooled analysis, \textit{non-political companies exlcuded}",
          caption.above=T,float.pos='h!',  
          fontsize='footnotesize', dcolumn=T, digits =2,use.packages=F, include.n.clusters=T,
          include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2','N Respondents' ,'N'))



#################
#opinions survey
#misc value reqs
#################

#funding sources displayed on ad


funding1 <- tass2[,c(sub[7])]
funding1[funding1 >6] <- NA
funding1[funding1<4] <- 1
funding1[funding1>3]<-0

#public database for companies political ads
funding2 <- tass2[,c(sub[8])]
funding2[funding2 >6] <- NA
funding2[funding2<4] <- 1
funding2[funding2>3]<-0


#dark money ads banned
funding3 <- tass2[,c(sub[5])]
funding3[funding3 >6] <- NA
funding3[funding3<4] <- 1
funding3[funding3>3]<-0















