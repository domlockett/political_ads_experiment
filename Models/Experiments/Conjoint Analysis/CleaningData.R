
library(stringr)
library(radiant)
library(tidyverse)
library(readxl)
library(prefmod)
library(readxl)
library(gmodels)
library(fabricatr)
local <- 'C:/Users/dl0ck/OneDrive/Fall 2021/PolAdsTransparency/politics_user_study/'

tass1 <- read.csv(paste0(local,'Data/TASS04Reask/8557_WUSTL-Weidenbaum04Reask.csv'))#load data
######################################
#P_IMAGEXX is 3 number profile description; 123 =source,message, image)
#Each participant will have a row per image; 16 rows
#image_set == original column it came from P_IMAGEXX
#image_data == the profile info from column 

tassLong <- gather(tass1, image_set, image_data, P_IMAGE1A, P_IMAGE1B, P_IMAGE2A, P_IMAGE2B, P_IMAGE3A, P_IMAGE3B, P_IMAGE4A, P_IMAGE4B,P_IMAGE5A,P_IMAGE5B,P_IMAGE6A,P_IMAGE6B,P_IMAGE7A,P_IMAGE7B,P_IMAGE8A,P_IMAGE8B)

tass <- tassLong

#separate attributes (123==[source,message,image]) into their own columns 
tass <-cbind(tass, as.data.frame(matrix(unlist(strsplit(as.character(tass$image_data), "")), ncol=3, byrow=T)))

colnames(tass)[(ncol(tass)-2):(ncol(tass))] <- c('source','message','image')#add names

#confirm this performed as expected
# count <- 0
#  
# for(i in 1:20){#do 20 random pulls
#   tassDrop <- tass[!rowSums(is.na(tass['image_data'])),]#only grab complete data so loop doesnt break
#   rand <- sample(1:nrow(tassDrop),10)#random sample of ten rows
#   for(i in 1:10){#check each item in sample of ten
#     count <- count+1
#     newCols <- as.numeric(paste(tass$source[rand[i]], tass$message[rand[i]], tass$image[rand[i]], sep ="" ))#recombine the columns separated above
#     if(all(newCols == tass$image_data[rand[i]]) == T ){
#       #if the combined values of the separate columns equals image_data in that row...cool
#       print(paste('Done; Iteration:', count, sep = " "))
#       #cool
#     }else{print(paste('check row', rand[i]))
#       #otherwise tell me what is messing up
#     }
#   }
# }




############
#Making dvs
############

##images is the image descriptor, now rowwise 1a:8b
tass$images <- stringr::str_sub(tass$image_set, start = -2)


#Num the column number of the 8 conjoint comparison outcomes
num <- which(colnames(tass)=='MONTGOMERY1S4'):which(colnames(tass)=='MONTGOMERY8S4')

tass$dv <- NA 

for(i in 1:nrow(tass)){#for each row
  for(j in num){#for each of the eight outcome variables (MONTGOMERY1:8S4)
    if(str_sub(tass$images[i], 1,1) == str_sub(colnames(tass)[j], 11,11)){
      #if the rows image detail matches the columns comparison value
      #MONTGOMERY1S4 == 1A <- the conjoint outcome col num and the image num 
       tass$dv[i] <-  tass[i,j]
       #put the value for that row,column in the dv
    }
  }
}
# unique(tass$dv)
tass$dv[tass$dv == 98] <- NA

tass$dv1  <-NA
tass$dv1[tass$dv==1 & str_sub(tass$images, 2,2)== 'A'] <- 1
tass$dv1[tass$dv==2 & str_sub(tass$images, 2,2)== 'A'] <- 0

tass$dv2  <-NA
tass$dv2[tass$dv==1 & str_sub(tass$images, 2,2)== 'B'] <- 0
tass$dv2[tass$dv==2 & str_sub(tass$images, 2,2)== 'B'] <- 1



#random assessment of success

#lets look at 500 random participants.
#Examine their 6 selections in the MONTGOMERY columns 
#and compare them to the outcome of for loop from above
#count <- 0
# # #select ten random participants 50 times
# for(j in 1:50){
#   #drop na's and only check participants who answered all comparisons
#   #dropping from original MONTGOMERY columns only in case the new dv variable returned NA's
#   tassDrop <- tass[!rowSums(is.na(tass[,num])),]
# 
# 
#   #create random index from complete cases
#   rand <- sample(1:nrow(tassDrop),10)
# 
#   #grab 10 participants
#   subjects <- tassDrop[rand,'CaseId']
# 
#   #for each of the ten random participants
#   for(i in 1:10){
#     count <- 1 + count
# 
#     #collect all rows associated with one subject
#     #this will result in 12 rows for the six comparisions
#     indexSubject <- as.numeric(c(rownames(tass)[tass$CaseId == as.character(subjects[i])]))
# 
#     #store subjects' selection for image according to above for loop
#     dvValues12 <- c(tass$dv[indexSubject])
# 
#     #each selecton will show up twice (1a == 1b) so grab every other
#     #let's be super obsessive and make sure 1a and 1b (etc.) are perfect matches
#     dvValues6a <- as.matrix(dvValues12[c(T, F)])
#     dvValues6b <- as.matrix(dvValues12[c(F, T)])
# 
#     #subjects' selection as in original six columns
#     #the same MONTGOMERY data will show up in all 12 rows from a single subject
#     #just look at first instance
#     colValues <- matrix(tass[indexSubject[1],num],)
# 
#     #if they match my loop works
#     if(all(dvValues6a == dvValues6b) & all(dvValues6a == colValues) ){
#     print(paste('Good job, Kid;', 'iteration:', count, 'subject:', subjects[i]))
#       }else{#if not good job wasting your time
#       print(paste('Try again;', 'investigate subject', subjects[i]))
#     }
#   }
# }



######################################
#add profile labels
tass$profile <-c()
profile_types <- unique(tass$image_data)
profile_labels <- c()

for(i in 1:length(profile_types)){
  profile_labels[i] <- paste('Profile',i, sep = " ")

}

for(i in 1:nrow(tass)){
    for(j in 1:length(profile_types)){
      if(tass$image_data[i] == profile_types[j]){
        tass$profile[i] <- profile_labels[j]
    }
  }
}



##########################
#First number: Source
# 1.Trump
# 2.Biden
# 3.Power the Future
# 4.Patagonia
# 5.ExxonMobil
# 6.Sierra Club


##########################
#Second number: Message
# 1.Pro pipeline
# 2.Oppose pipline
# 3.Oil and gas
# 4.Cut Methane
# 5.EngerySector
# 6.Nature

##########################
#Third number: Image
# 1.Landscape
# 2.Pipeline
# 3.Oil rig
# 4.Solar wind
# 5.Desert

## ##########################

tass$source <- recode_factor(tass$source, '1' = "Trump", '2' = 'Biden', '3' = 'Power the Future', '4' = 'Patagonia', '5'= 'ExxonMobil', '6'= 'Sierra Club' , .defualt = levels(tass$source) )

tass$message <- recode_factor(tass$message, '1' = "Pro pipeline", '2' = 'Anti pipeline', '3' = 'Oil and gas', '4' = 'Cut methane', '5'= 'Energy security', '6'= 'Nature' , .defualt = levels(tass$message) )

tass$image <- recode_factor(tass$image, '1' = "Landscape", '2' = 'Pipeline', '3' = 'Oil rig', '4' = 'Solar wind', '5'= 'Desert' , .defualt = levels(tass$image) )



######################################
#make some PID indicators
#
tass$PID <- tass$PartyID7
tass$pid <- tass$PartyID7
tass$PID <- recode_factor(tass$PartyID7, '1' = "Strong Democrat", '2' = 'Moderate Democrat', '3' = 'Lean Democrat', '4' = "Don't Lean/Independent/None", '5'= 'Lean Republican', '6'= 'Moderate Republican', '7' = 'Strong Republican' , .defualt = levels(tass$PID) )
tass$pid <- recode_factor(tass$PartyID7, '1' = "Democrat", '2' = 'Democrat', '3' = 'Democrat', '4' = 'NA', '5'= 'Republican', '6'= 'Republican', '7' = 'Republican' , .defualt = levels(tass$pid) )
tass$pid[tass$pid == 'NA'] <- NA


######################################
#separate out attributes

# There are actually 5 attributes for this conjoint:
# Source type: (Candidate/company/political org)
# 
# Source orientation: (consistent/inconsistent with ind. attitudes)
# 
# Message strength: (Low political/medium/high)
# 
# Message orientation: (consistent/inconsistent with ind. attitudes)
# 
# Picture: As is currently

## Source type:
### Biden, Trump - candidates
### Exxon Patagonia- companies
### Sierra PTF- organizations

## Source orientation:
### Trump, Exxon, Power the Future - Pro oil
### Biden, Sierra Club, Patagonia - Pro environment

## Message strength
### Pro pipeline, Anti pipeline - Highly politicized
### Oil and gas, Cut Methane - Somewhat politicized
### Nature, Energy security - Neutral

## Message orientation
### Pro pipeline, Oil and Gas, Energy security- Pro oil
### Anti pipeline, cut methane, nature- Pro environment

tass$sourceType <- NA
for(i in 1:nrow(tass)){
  if(tass$source[i] == 'Biden' | tass$source[i] == 'Trump'){
    tass$sourceType[i] <- 'Candidate'
  }else if(tass$source[i] =='ExxonMobil' | tass$source[i] == 'Patagonia'){
    tass$sourceType[i] <- 'Company'
  }else if(tass$source[i] == 'Sierra Club' | tass$source[i] == 'Power the Future'){
    tass$sourceType[i] <- 'Organization'
  }
}
tass$sourceType <- as.factor(tass$sourceType)



tass$sourceOrient <- NA
for(i in 1:nrow(tass)){
  if(tass$source[i]== 'Trump' | tass$source[i] == 'ExxonMobil' | tass$source[i] == 'Power the Future'){
    tass$sourceOrient[i] <- 'Pro-development'
  }else if(tass$source[i]== 'Biden' | tass$source[i] == 'Sierra Club' | tass$source[i] == 'Patagonia'){
    tass$sourceOrient[i] <- 'Pro-environment'
  } 
}
tass$sourceOrient <- as.factor(tass$sourceOrient)

tass$messageStrength <- NA
for(i in 1:nrow(tass)){
  if(tass$message[i] == 'Pro pipeline' | tass$message[i] == 'Anti pipeline' ){
    tass$messageStrength[i] <- 'Strong'
  }else if(tass$message[i] == 'Oil and gas'| tass$message[i] == 'Cut methane'){
    tass$messageStrength[i] <- 'Moderate'
  }else if(tass$message[i] == 'Nature' | tass$message[i] == 'Energy security'){
    tass$messageStrength[i] <- 'Weak'
  }
}
tass$messageStrength <- as.factor(tass$messageStrength)

tass$messageOrient <- NA
for(i in 1:nrow(tass)){
  if(tass$message[i]== 'Pro pipeline' | tass$message[i] == 'Oil and gas' | tass$message[i] == 'Energy security'){
    tass$messageOrient[i] <- 'Pro-development'
  }else if(tass$message[i]== 'Anti pipeline' | tass$message[i] == 'Cut methane' | tass$message[i] == 'Nature'){
    tass$messageOrient[i] <- 'Pro-environment'}
}
tass$messageOrient <- as.factor(tass$messageOrient)


######################################
#Clean up priors

tass$globalWarming <- NA
tass$globalWarming <- recode(tass$POLADSGLOBALWARMINGS4, '1' = "1", '3' = '2', '2' = '3', .default = NA_character_)

tass$globalWarming <- as.numeric(tass$globalWarming)

tass$regulate <- tass$POLADSREGULATIONS4

tass$regulate[tass$regulate >7 ] <- NA



tass$en <-  (tass$regulate + tass$globalWarming) #add these 2 indicators together
tass$prior <-cut(tass$en, breaks = quantile(tass$en, probs = seq(0, 1, length.out = 2 + 1), na.rm=T), labels = 1:2, include.lowest = TRUE) #med split

tass$priors <- NA
tass$priors[tass$prior==2] <- 'Pro-development' #useful names
tass$priors[tass$prior==1] <- 'Pro-environment'



write.csv(tass, paste0(local,'Data/Conjoint/CJ.csv'))

