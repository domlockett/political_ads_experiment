local <- 'C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/'
#models
source(paste0(local,'publish/models.R'))

###########################
#opinions on web regulation
#r1
############################


#neat names for plots order the same as _sub_ ordering
new['question'] <- c('Ads encouraging voting allowed',
                       'No political ads', 
                       'All controversial ads banned', 
                       'Candidate ads banned but issue ads allowed',
                       'PACs/dark money ads banned',
                       'Political advertisers funding sources private',
                       'Political advertisers funding sources displayed on ad ',
                       "Public database for companies' political ads",   
                       'Microtargeting political ads banned',
                       'Advertisers choose who sees',
                       'News orgs exempt from political ad regulations ',
                       "News orgs' political ads regulated" )


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
#plot
f1 = ggplot(new, aes(y = rev(est), x = question  ))+
  geom_pointrange(aes(y = est,ymin = lower, 
                      ymax = upper),
                  position = position_dodge(width = -.6),
                  size = .9 , color ="paleturquoise4") +
  coord_flip() + 
  xlab("")+ 
  ylab("")+ 
  theme(text=element_text(size=20))+ 
  scale_y_continuous(limits = c(1, 6),
                     breaks = seq(1,6,1), labels=scale)
f1 +  
  theme(axis.text.x = element_text(angle=45,vjust=0.5))+
  theme(text=element_text(size=25))+
  theme(panel.grid.minor =   element_blank(),
        panel.grid.major =   
          element_line(colour = "grey87",size=0.5))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  theme(axis.text.y= element_text( size = 20),axis.text.x= element_text( size = 20),legend.text=element_text(size=20))


ggsave(paste0(local,"Publishing materials/Replication plots/ad-reg-opin.pdf"), width = 15, height = 10, units = "in")



####################
#conjoint experiment
####################

#######
#tables
# Define location where you want tex file to save in paste0, first argument
# F2
#mod_texreg(paste0(local,'publish/tables/','cj-unconditional.tex'),
                 # list(uncon),
                 # custom.model.names = c("Average Treatment Effect"),
                 # caption = "Effect of advertisement's attributes on perception of ad's politicalness (Average Treatment Effect)",
                 # custom.coef.names = c('(Intercept)','Desert','Oil rig','Pipeline','Solar wind','Message orientation: Pro-development','Moderate','Strong','Source orientation: Pro-development','Candidate','Organization'),
                 # caption.above=T, float.pos='h!', 
                 # fontsize='small', dcolumn=T, digits =2,use.packages=F, label='table:ate',
                 # booktabs =T,
                 # custom.gof.names = c('R^2','Adjusted R^2','N','RMSE'))

#a1
# # F3
# mod_texreg(paste0(local,'publish/tables/','cj-interact-orient.tex'),
#                  list(messP, sourceP),   
#                  custom.model.names = c("Message $\\times$ Priors","Source $\\times$ Priors"),  
#                  caption = "Effect of advertisement's attributes on perception of politicalness conditioned on pro-development source and message",  
#                  custom.coef.names = c('(Intercept)','Desert','Oil rig','Pipeline','Solar wind','Message orientation: Pro-development','Moderate','Strong','Source orientation: Pro-development','Candidate','Organization', 'Message orientation conditioned on prefers development','Message orientation conditioned on prefers environment','Source orientation conditioned on prefers development','Source orientation conditioned on prefers environment'),
#                    fontsize='small',caption.above=T, float.pos='h!', dcolumn=T, digits =2,
#                  use.packages=F, label='table:cj-interact-orient', booktabs = T,  
#                  custom.gof.names = c('R^2','Adjusted R^2','N','RMSE'))


#a2


#######
#plots

#Unconditional
#use cjoint with small modification of source code (eliminate images from plot)
plt1(unconditional, 
     colors=c("paleturquoise4","lightpink3","sienna4", 'navajowhite3'),
     xlab="Change in \n Pr(Ad considered more political)", 
     attribute.names= c('Images','Message orientation',
                        'Message strength','Source orientation',
                        'Source type'),
     point.size=.9, 
     plot.theme=theme_set(theme_bw1())+
       theme(axis.text.y = element_text(size=rev(c(30,24 ,30 , 30, 24, 30 ,30,30 ,24,30,30, 24, 30, 30))))+
       theme(axis.text.y = element_text(face=rev(
         c("bold","italic", "plain","bold", 'italic',
           'plain','plain','bold', "italic","plain",
           'bold','italic','plain','plain'))))+
       theme(axis.text.x = element_text(size=24)))


#save
ggsave(paste0(local,"publish/plots/","cj-uncondition-plot.pdf"), width = 12, height = 9, units = "in")




#interaction
f3 = ggplot(cjIn, aes(y = est, x = Attribute , color=`Prior attitudes`  ))+
  geom_pointrange(aes(y = est,ymin = lower, 
                      ymax = upper, fill =`Prior attitudes`),
                  position = position_dodge(width = -.6),
                  size = .8) +
  coord_flip() + 
  xlab("")+ 
  ylab("")+ 
  theme(panel.spacing.x = unit(27, "mm"),
        axis.text.x=element_text(
                                 hjust=0.5,vjust=.5))+
  theme(text=element_text(size=20))


f3 + geom_hline( yintercept = 0, colour = "gray", size = 1, lty= 'dashed')+ scale_color_manual(values =  c("paleturquoise4","lightpink3")) +
  theme(axis.text.y= element_text( size = 20),axis.text.x= element_text( size = 18),legend.text=element_text(size=20))+ theme(legend.position="bottom")


#save
ggsave(paste0(local,"publish/plots/","cj-interact-plot.pdf"), width = 14, height = 7, units = "in")


#############
#Real Ads
#############

############
#pooled means

#table
## F4
## 
datc1 <-datc
colnames(datc1) <- c('Mean','Lower 95 CI','Upper 95 CI','Standard Err.', 'Political level', 'Advertisement')
stargazer(datc1, out= paste0(local,'publish/tables/','high-low-means.tex'),
          
          title = 'Average perceived politicalness of high and low political ads', label = 'table:pool-means', 
          summary = F, header =F, digits =2)



#reorder level for display
datc$Ad = factor(datc$Ad, levels=rev(c("Biden","Trump",
                                       "Power", "Sierra",'Patagonia',"Exxon","MSC",
                                       "Colgate")))
colnames(datc)[5] <-'Political Content'
#plots
f4 <- ggplot(datc, aes(y = Estimate, x = Ad , color=`Political Content`  ))+
  geom_pointrange(aes(y = Estimate,ymin = `Lower.CI`, 
                      ymax = `Upper.CI`),
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
  theme(axis.text.y = element_text(face=rev(c("bold","bold",
                                              "italic","italic",
                                              'plain','plain','plain','plain'))))

f4+theme(text=element_text(size=20)) + scale_color_manual(values =  c("sienna4", 'navajowhite3'), name = "")+ geom_hline( yintercept = 0, colour = "gray", size = 1, lty= 'dashed')+
  theme(plot.margin = unit(c(1,1,1,3), "cm"))+
      theme(axis.text.y= element_text( size = 20),axis.text.x= element_text( size = 18),legend.text=element_text(size=20))


dat <- dat[,-1]
datb <- datb[,-1]
datb$Ad = factor(datb$Ad, levels=rev(c("Biden","Trump",
                                       "Power", "Sierra",'Patagonia',"Exxon","MSC", "Colgate")))
dat$Ad = factor(dat$Ad, levels=rev(c("Biden","Trump",
                                     "Power", "Sierra",'Patagonia',"Exxon","MSC",  "Colgate")))

colnames(dat) <- c('Mean','Lower 95 CI','Upper 95 CI','Standard Err.','Iteration', 'Political level', 'Advertisement')
colnames(datb) <- c('Mean','Lower 95 CI','Upper 95 CI','Standard Err.','Iteration',  'Political level', 'Advertisement')

colnames(dat)[6] <-'Political Content'
colnames(datb)[6] <-'Political Content'





d <- rbind(dat, datb)

d['Iter'] <- paste(d$`Political Content`,d$Iteration,sep=', ')
d$Iter<- factor(d$Iter, levels= c('Strong, Original','Weak, Original','Strong, Correction','Weak, Correction'))

f4 <- ggplot(d, aes(y = Mean, x = Advertisement , colour=Iter,shape = Iter ))+
  geom_pointrange(aes(y = Mean,ymin = `Lower 95 CI`, 
                      ymax = `Upper 95 CI`),
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
  theme(axis.text.y = element_text(face=rev(c("bold","bold",
                                              "italic","italic",
                                              'plain','plain','plain','plain'))))

f4+theme(text=element_text(size=20))  +
  scale_colour_manual(name = "",
                     labels = c( "Strong, Original", "Weak, Original","Strong, Correction", "Weak, Correction"),
                     values = c("sienna4", 'navajowhite3',"sienna4", 'navajowhite3')) + 
  scale_shape_manual(name = "", 
                     labels =  c( "Strong, Original", "Weak, Original","Strong, Correction", "Weak, Correction"),
                     values = c(19, 19, 17, 17))+
  geom_hline( yintercept = 0, colour = "gray", size = 1, lty= 'dashed')+
  theme(plot.margin = unit(c(1,1,1,3), "cm"))+
  theme(axis.text.y= element_text( size = 20),axis.text.x= element_text( size = 18),legend.text=element_text(size=20))


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

##############
#politicians
#organization
##############
#interacted w/
#orientation
##############



#tables
# # F5
# mod_texreg(paste0(local,'publish/tables/','ra-orient-polorg.tex'),
#                  list(trump6, biden6, power6,sierra6),  
#                  custom.header = list("Politicians" = 1:2, "Organizations" = 3:4),  
#                  custom.model.names = c("Trump ads","Biden ads","Power ads","Sierra ads"),  
#                  caption = "Effect of pro-development priors on perceived politicalness of ads with high or low political content: politicians and organizations",
#                  custom.coef.names = c('(Intercept)',"Trump political ad",'Pro-development prior','Trump political ad \n\u00D7 pro-development prior','Biden political ad','Biden political ad \u00D7 pro-development prior',"Power political ad",' Power political ad \u00D7 pro-development prior','Sierra political ad','Sierra political ad \u00D7 pro-development prior'),
#                  caption.above=T,float.pos='h!',  
#                  fontsize='small', dcolumn=T, digits =2,use.packages=F, 
#                  label='table:ra-orient-polorg',  booktabs =T,  
#                  custom.gof.names = c('R^2','Adjusted R^2','N','RMSE','N. Clusters'), reorder.coef =c(4,1,6, 8,2,5,3,7,9),include.ci=F)
# 

# # F6
# mod_texreg(paste0(local,'publish/tables/','ra-orient-com.tex'),
#                  list(exxon6,msc6,patagonia6, colgate6), 
#                  custom.header = list("Pro-development" = 1:2, "Pro-environment" = 3:4),  
#                  custom.model.names = c('Exxon ads','MSC ads','Patagonia ads','Colgate ads'),  
#                  caption = "Effect of pro-development priors on perceived politicalness of ads with high or low political content: companies",  
#                  custom.coef.names = c('(Intercept)','Exxon political ad','Pro-development prior', 'Exxon political ad \u00D7 pro-development prior','MSC political ad','MSC political ad \u00D7 pro-development prior','Patagonia political ad','Patagonia political ad \u00D7 pro-development prior','Colgate political ad','Colgate political ad \u00D7 pro-development prior'),
#                  caption.above=T, float.pos='h!',  
#                  fontsize='small', dcolumn=T, digits =2,use.packages=F, label='table:ra-orient-com', 
#                  booktabs =T,custom.gof.names = c('R^2','Adjusted R^2','N','RMSE','N. Clusters'), reorder.coef =c(1,4,8,6,2,3,5,9,7) ,include.ci=F)
# 
# 


#plot
#make object for legend
poli['leg'] <- 'Politicians'
orgi['leg'] <- 'Organizations'

polorg <- rbind(poli, orgi)#combine two datasets
polorg$Coefficient <- as.factor(c("Trump political ad", 
                                  "Trump political ad \u00D7 pro-development prior",
                        
                        "Biden political ad","Biden political ad \u00D7 pro-development prior", 
                        "Power political ad","Power political ad \u00D7 pro-development prior",
                        "Sierra political ad","Sierra political ad \u00D7 pro-development prior"))#make ylables

polorg$Coefficient <- factor(polorg$Coefficient, levels =rev(c("Biden political ad","Trump political ad",
                       "Power political ad","Sierra political ad",  
                       "Biden political ad \u00D7 pro-development prior", 
                       "Trump political ad \u00D7 pro-development prior",
                       "Power political ad \u00D7 pro-development prior" ,
                      "Sierra political ad \u00D7 pro-development prior")))

f5 <- ggplot(polorg, aes(y = Value, x = Coefficient, color=leg))+
  geom_pointrange(aes(y = Value,
                      ymin = `HighInner`,
                      ymax = `LowInner`),
                      position = position_dodge(width = -.6),
                      size = .7) +
  coord_flip() + 
  xlab("") + 
  ylab(expression(paste("\nDifference in politicalness of high and low",bold(' real ') ,"ads")))+
  ylim(-.75,2.2)+
  theme(axis.title.x = element_text(vjust=-0.5))

f5 + geom_hline( yintercept = 0, colour = "gray", size = 1, lty= 'dashed')+
  theme(axis.text.y= element_text( size = 18),axis.title.x= element_text( size = 20),legend.text=element_text(size=18), axis.text.x=element_text(size=18))+
  scale_color_manual(values=c('lightpink3','paleturquoise4'))+
  labs(color="")

ggsave(paste0(local,"publish/plots/","lm-orient-polorg-plot.pdf"), width = 12, height = 8, units = "in")




###########
#companies
###########

#yaxis labels
comYa <- c("Exxon political ad","MSC political ad",'Patagonia political ad',"Colgate political ad", "Exxon political ad \u00D7 pro-development prior" ,"MSC political ad \u00D7 pro-development prior", "Patagonia political ad \u00D7 pro-development prior","Colgate political ad \u00D7 pro-development prior"  )


f6 <- ggplot(comi, aes(y = Value, x = Coefficient,))+
  geom_pointrange(aes(y = Value, ymin = `HighInner`,ymax = `LowInner`),
                  position = position_dodge(width = -.6), size = .8, 
                  color = "sienna4") +
  coord_flip() + xlab("") +  
  ylab(expression(paste("\nDifference in politicalness of high and low",bold(' real ') ,"ads")))+
  ylim(-.75,2.2)

f6  + geom_hline( yintercept = 0, colour = "gray", size = 1, lty= 'dashed')+
  scale_x_discrete(labels = rev(comYa))+
  theme(axis.text.y= element_text( size = 18),axis.title.x= element_text( size = 20),legend.text=element_text(size=18), axis.text.x=element_text(size=18))




ggsave("C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/publish/plots/lm-orient-com-plot.pdf", width = 12, height = 8, units = "in")

















## Figure XX- OPINIONS ON REGULATIONS BY PID


sub <- c(144,141,143,155,156,149,148,154,150,151,145,146)
sub <- rep(sub,3)
comp <- c()
for(i in 1:length(sub)){
  comp <- c(comp, which(Ads3==sub[i]))
}

or <- data.frame(matrix(nrow=36, ncol =2))
or[,1] <- rep(topics3[comp],3)
or[,2] <- as.numeric(rep(Ads3[comp],3))

or
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

opins['question'] <- as.factor(rep(c('Ads encouraging voting allowed',
                                     'No political ads', 
                                     'All controversial ads banned', 
                                     'Candidate ads banned but issue ads allowed',
                                     'PACs/dark money ads banned',
                                     'Political advertisers funding sources private',
                                     'Political advertisers funding sources displayed on ad ',
                                     "Public database for companies' political ads",   
                                     'Microtargeting ads banned',
                                     'Advertisers choose who sees',
                                     'News orgs exempt from political ad regulations ',
                                     "News orgs' political ads regulated" ),3))


opins$question <- factor(opins$question, levels= rev(opins$question[1:12]))
opins$pid[opins$pid=='Moderate/Independent/None'] <- 'Independent'

colnames(opins)<-c('est','upper','lower','std. err', 'pid','question')

for(i in 1:4){
  opins[,i]<-as.numeric(opins[,i])
}
scale <- c('Strongly \n agree','Agree','Agree\n a little','Disagree\n a little','Disagree', 'Strongly \n disagree')
#make plot
f2 = ggplot(opins, aes(y = rev(est), x = question, shape = pid, colour = pid))+
  geom_pointrange(aes(y = est,ymin = lower, 
                      ymax = upper),
                  position = position_dodge(width = -.7),
                  size = .9 ) +
  coord_flip() + 
  xlab("")+ 
  ylab("")+ 
  theme(text=element_text(size=20))+ 
  scale_y_continuous(limits = c(1, 6),
                     breaks = seq(1,6,1), labels=scale)
f2 +  
  theme(axis.text.x = element_text(angle=45,vjust=0.5))+
  theme(text=element_text(size=25))+
  theme(panel.grid.minor =   element_blank(),
        panel.grid.major =   
          element_line(colour = "grey87",size=0.5))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))+ 
  scale_colour_manual(values =  c('paleturquoise4', 'lightpink3',"sienna4"), name = 'Party ID')+
  scale_shape_manual(values =  15:19, name = 'Party ID')
  
ggsave(paste0(local,"publish/plots/ad-reg-pid.pdf"), width = 15, height = 10, units = "in")






