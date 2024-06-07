local <- 'C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/'

#models
source(paste0(local,'publish/models-appendix.R'))


###########
#regulation 
#opinon
###########


new1 <-new
colnames(new1) <- c('Mean','Upper 95% CI','Lower 95% CI','Standard Err.', 'Question')
a02 <- stargazer(new1, out= paste0(local,'publish/tables/','ad-reg-opin-rem.tex'), header= T, summary = F, title =
                   'Opinions on proposed social media advertising regulation', label = 'table:ad-reg2', digits =2)

####################
#conjoint experiment
####################

#######
#tables
# Define location where you want tex file to save in paste0, first argument
# a0
mod_texreg(paste0(local,'publish/tables/','cj-interact-pid.tex'),
           list(mpid, sPid),   
           custom.model.names = c("Message $\\times$ Party ID","Source $\\times$ Party ID"),  
           caption = "Effect of advertisement's attributes on perception of politicalness conditioned on party ID",  
           custom.coef.names = c('(Intercept)','Desert','Oil rig','Pipeline','Solar wind','Message orientation: Pro-development','Moderate','Strong','Source orientation: Pro-development','Candidate','Organization', 'Pro-development message conditioned on Democrat','Pro-development message conditioned on Republican','Pro-development source orientation conditioned on Democrat','Pro-development source conditioned on Republican'),
             fontsize='small',caption.above=T, float.pos='h!', dcolumn=T, digits =2,
           use.packages=F, label='table:cj-interact-orient', booktabs = T,  
           custom.gof.names = c('R^2','Adjusted R^2','RMSE','N'))

#a2


#######
#plots
#interaction
##a04 = ggplot(cjIn, aes(y = est, x = Attribute , color=`Party `  ))+
#   geom_pointrange(aes(y = est,ymin = lower, 
#                       ymax = upper, fill =`Prior attitudes`),
#                   position = position_dodge(width = -.6),
#                   size = .8) +
#   coord_flip() + 
#   xlab("")+ 
#   ylab("")+ 
#   theme(panel.spacing.x = unit(27, "mm"),
#         axis.text.x=element_text(
#           hjust=0.5,vjust=.5))+
#   theme(text=element_text(size=20))
# 
# 
# a04 + geom_hline( yintercept = 0, colour = "gray", size = 1, lty= 'dashed')+ scale_color_manual(values =  c("paleturquoise4","lightpink3")) +
#   theme(axis.text.y= element_text( size = 20),axis.text.x= element_text( size = 18),legend.text=element_text(size=20))


#save
#ggsave(paste0(local,"publish/plots/","cj-interactPID-plot.pdf"), width = 14, height = 7, units = "in")

#############
#Real Ads
#############
#screenreg(list(poolFit,interFit), include.ci=F)

############
# Distributions
des1$lvl <- ifelse(des1$lvl == 'High','Strong','Weak')
des2$lvl <- ifelse(des2$lvl == 'High','Strong','Weak')
des1$lvl <- factor(des1$lvl , levels = c('Weak','Strong'))
des2$lvl <- factor(des2$lvl , levels = c('Weak','Strong'))

ggplot(data = des1, aes(rating, freq,fill=name)) +
  geom_bar(stat='identity') +
  labs(y = "Frequency", x = "Rating") + 
  facet_grid(name ~ lvl)+
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



#tables
# ate- politicians organizations
mod_texreg(paste0(local,'publish/tables/','ra-polorgcom.tex'),
           list( biden3,trump3, power3,sierra3,exxon3,patagonia3,msc3, colgate3),  
           custom.header = list("Politicians" = 1:2, "Organizations" = 3:4,"Political company" = 5:6, "Non-political company" = 7:8),
           custom.model.names =  c('Biden ads' ,'Trump ads','Power ads','Sierra ads','Exxon ads','Patagonia ads','MSC ads','Colgate ads'),
           caption = "Effect of exposure to ads with high or low political content on perceived politicalness of ads: politicians and organizations",
           caption.above=T,float.pos='h!',
           fontsize='footnotesize', dcolumn=T, digits =2,use.packages=F, include.n.clusters=F,
           label='table:ra-polorgcom',  booktabs =T,  custom.coef.names = c('(Intercept)',"Biden political ad",'Trump political ad','Power political ad', 'Sierra political ad','Exxon political ad','Patagonia political ad','MSC political ad','Colgate political ad'),
           include.ci=F)


screenreg(list( biden3,trump3, power3,sierra3,exxon3,patagonia3,msc3, colgate3), include.ci=F)

mod_texreg(paste0(local,'publish/tables/','ra-source-interact-POLIICALCOMONLY.tex'),
           list(poolFit, interFit, partyFit),  
           custom.model.names =  c('Model 1' ,'Model 2', 'Model 3'),
           caption = "  Effect of exposure to strong or weak political ads interacted with source on perceived politicalness of ads: Pooled analysis, \textit{non-political companies exlcuded}",
           caption.above=T,float.pos='h!',  
           fontsize='footnotesize', dcolumn=T, digits =2,use.packages=F, include.n.clusters=T,
           label='table:ra-source-interact',  booktabs =T,  #custom.coef.names = c(
          # 'Candidate','Organization','Strong ads','Pro-development source', 
           #  'Strong ads \u00D7 candidate',
           ##),
           include.ci=F)

#PID- politicians organizations
mod_texreg(paste0(local,'publish/tables/','ra-pid.tex'),
           list( biden8,trump8, power8,sierra8, exxon8, patagonia8, msc8,colgate8),  
           custom.model.names = c("Biden ads","Trump ads","Power ads","Sierra ads",'Exxon ads','Patagonia ads','MSC ads','Colgate ads'), 
           custom.header = list("Politicians" = 1:2, "Organizations" = 3:4,"Political company" = 5:6, "Non-political company" = 7:8),  
           caption = "Effect of exposure to ads with high or low political content on perceived politicalness of ads",
           custom.coef.names = c('(Intercept)',"Strong ad", 
                                 'Republican',
                                 "Strong ad",
                                 "Strong ad",
                                 "Strong ad", 
                                 "Strong ad",
                                 "Strong ad",
                                 "Strong ad",
                                 "Strong ad"),
           caption.above=T,float.pos='h!', 
           fontsize='footnotesize', dcolumn=T, digits =2,use.packages=F, include.n.clusters=F,
           label='table:ra-pid',  booktabs =T,include.ci=F)

# PID-m companies
mod_texreg(paste0(local,'publish/tables/','ra-pid-com.tex'),
           list(exxon9,patagonia9,msc9, colgate9), 
           custom.header = list("Political company" = 1:2, "Non-political company" = 3:4),  
           custom.model.names = c('Exxon ads','Patagonia ads','MSC ads','Colgate ads'),  
           caption = "Effect of exposure to ads with high or low political content on perceived politicalness of ads: companies",  
           custom.coef.names =  c('(Intercept)',
                                  "Exxon political ad", 
                                  'Republican',
                                  "Exxon political ad \u00D7 Republican prior", 
                                  "Patagonia political ad",
                                  "Patagonia political ad \u00D7 Republican prior",
                                  "MSC political ad",
                                  "MSC political ad \u00D7 Republican prior",
                                  "Colgate political ad",
                                  "Colgate political ad \u00D7 Republican prior"),
           caption.above=T, float.pos='h!',  
           fontsize='footnotesize', dcolumn=T, digits =2,use.packages=F, label='table:ra-pid-com', 
           booktabs =T,
           reorder.coef =c(1,4,6,8,2,3,5,7,9),include.ci=F)
