library(foreign)
library(irr)
##########################3. PREPROCESSING AND VARIABLE CREATION STUDY 2: Exam data########################################

#####3a. loading in data#####
examdata = read.spss("data/Study 2_exam.sav", to.data.frame=TRUE)

#making a pre-post variable in the whole dataset
examdata$prepost <- ifelse(examdata$exam_beepnum >= 0, 
                           c("Post"), c("Pre")) 

#making a subset of the data with the post exam beeps only
examdata_postexam <- examdata[ which(examdata$exam_beepnum>=0), ]

#making pass variable into actual percentage
examdata_postexam$perc_pass <- examdata_postexam$perc_pass*100

####3b. Making negative emotion scale####
examdata_postexam$negaff_composite <-rowMeans(subset(examdata_postexam, select=c("emotion_sad", "emotion_angry", "emotion_disapp", "emotion_ashamed", 
                                                                                 "emotion_anxious", "emotion_stressed")), na.rm=TRUE)


######3ci) Making emotion differentiation index#####
subjects_exam=unique(examdata_postexam$Participant)

icc_list_exam=NULL
for (sub in subjects_exam){
  sub_examdata_postexam=examdata_postexam[examdata_postexam[,1]==sub,c("emotion_sad", "emotion_angry", "emotion_disapp", "emotion_ashamed", 
                                                                       "emotion_anxious", "emotion_stressed")]  #selects data for the current subject
  icc=icc(sub_examdata_postexam, model="twoway",unit="average")  #computes ICC
  icc_temp=icc$value
  
  fz_iccTemp=fisherz(icc_temp)
  
  row_temp=c(sub,icc_temp,fz_iccTemp)
  icc_list_exam=rbind(icc_list_exam,row_temp)
  print(fz_iccTemp)
}

colnames(icc_list_exam)=c("Participant","ICCneg_after","ICCneg_after.fz")
icc_list_exam=as.data.frame(icc_list_exam)    


icc_list_exam=icc_list_exam[complete.cases(icc_list_exam),]  #choose subjects where both icc and icc_fz 
icc_list_exam=icc_list_exam[icc_list_exam[,2]>0,]            #choose subjects with positive ICC - negative ICCs are uninterpretable

#####3cii) Merging in these  ICCs to the main dataset####
examdata_postexam <- merge(examdata_postexam,icc_list_exam,by=c("Participant"))

#####3ciii) reverse scoring the ICC so higher = higher differentiation####
examdata_postexam$RICCneg_after.fz <- -1 *examdata_postexam$ICCneg_after.fz
examdata_postexam$RICCneg_after <- 1 - examdata_postexam$ICCneg_after

#####3d. Making lagged variables####
#Lags exclude overnight
examdata_postexam <- examdata_postexam[order(examdata_postexam$Participant, examdata_postexam$exam_beepnum),]
examdata_postexam <- examdata_postexam %>% group_by(Participant,beepday) %>% mutate(negaff_composite_1 = lag(negaff_composite,1))
examdata_postexam <- examdata_postexam %>% group_by(Participant,beepday) %>% mutate(ER_rumination_1 = lag(ER_rumination,1))
examdata_postexam <- examdata_postexam %>% group_by(Participant,beepday) %>% mutate(ER_distraction_1 = lag(ER_distraction,1))
examdata_postexam <- examdata_postexam %>% group_by(Participant,beepday) %>% mutate(ER_acceptance_1 = lag(ER_acceptance,1))
examdata_postexam <- examdata_postexam %>% group_by(Participant,beepday) %>% mutate(ER_reapp_1 = lag(ER_reapp,1))
examdata_postexam <- examdata_postexam %>% group_by(Participant,beepday) %>% mutate(ER_supp_1 = lag(ER_supp,1))
examdata_postexam <- examdata_postexam %>% group_by(Participant,beepday) %>% mutate(ER_soc_sharing_1 = lag(ER_soc_sharing,1))

#####3e. making person mean per wave of negative emotion (control variable)####
examdata_postexam$negaff_composite_meanpostexam <- ave(examdata_postexam$negaff_composite, examdata_postexam$Participant, FUN=function(x) mean(x, na.rm=TRUE))

#####3f. Grand mean centered variables####
examdata_postexam$cRICCneg_after.fz <- scale(examdata_postexam$RICCneg_after.fz, center = TRUE, scale = FALSE)
examdata_postexam$cnegaff_composite_meanpostexam <-scale(examdata_postexam$negaff_composite_meanpostexam, center = TRUE, scale = FALSE)
examdata_postexam$cperc_pass <- scale(examdata_postexam$perc_pass, center = TRUE, scale = FALSE)

#####3g. Group-mean centered variables#####
examdata_postexam$cER_rumination <- examdata_postexam$ER_rumination - ave(examdata_postexam$ER_rumination, examdata_postexam$Participant, FUN=function(x) mean(x, na.rm=TRUE))
examdata_postexam$cER_distraction <- examdata_postexam$ER_distraction - ave(examdata_postexam$ER_distraction, examdata_postexam$Participant, FUN=function(x) mean(x, na.rm=TRUE))
examdata_postexam$cER_acceptance <- examdata_postexam$ER_acceptance - ave(examdata_postexam$ER_acceptance, examdata_postexam$Participant, FUN=function(x) mean(x, na.rm=TRUE))
examdata_postexam$cER_reapp <- examdata_postexam$ER_reapp - ave(examdata_postexam$ER_reapp, examdata_postexam$Participant, FUN=function(x) mean(x, na.rm=TRUE))
examdata_postexam$cER_supp <- examdata_postexam$ER_supp - ave(examdata_postexam$ER_supp, examdata_postexam$Participant, FUN=function(x) mean(x, na.rm=TRUE))
examdata_postexam$cER_soc_sharing <- examdata_postexam$ER_soc_sharing - ave(examdata_postexam$ER_soc_sharing, examdata_postexam$Participant, FUN=function(x) mean(x, na.rm=TRUE))
examdata_postexam$cnegaff_composite_1 <- examdata_postexam$negaff_composite_1 - ave(examdata_postexam$negaff_composite_1, examdata_postexam$Participant, FUN=function(x) mean(x, na.rm=TRUE))

######3h. Making scaled variables to use in analyses#####
examdata_postexam$Snegaff_composite <- scale(examdata_postexam$negaff_composite)
examdata_postexam$ScER_rumination <- scale(examdata_postexam$cER_rumination)
examdata_postexam$ScER_distraction <- scale(examdata_postexam$cER_distraction)
examdata_postexam$ScER_reapp <- scale(examdata_postexam$cER_reapp)
examdata_postexam$ScER_supp <- scale(examdata_postexam$cER_supp)
examdata_postexam$ScER_soc_sharing <- scale(examdata_postexam$cER_soc_sharing)
examdata_postexam$ScER_acceptance <- scale(examdata_postexam$cER_acceptance)
examdata_postexam$SER_rumination <- scale(examdata_postexam$ER_rumination)
examdata_postexam$SER_distraction <- scale(examdata_postexam$ER_distraction)
examdata_postexam$SER_reapp <- scale(examdata_postexam$ER_reapp)
examdata_postexam$SER_supp <- scale(examdata_postexam$ER_supp)
examdata_postexam$SER_soc_sharing <- scale(examdata_postexam$ER_soc_sharing)
examdata_postexam$SER_acceptance <- scale(examdata_postexam$ER_acceptance)
examdata_postexam$ScRICCneg_after.fz <- scale(examdata_postexam$cRICCneg_after.fz)
examdata_postexam$Scperc_pass <- scale(examdata_postexam$cperc_pass)
examdata_postexam$Scnegaff_composite_1 <- scale(examdata_postexam$cnegaff_composite_1)
examdata_postexam$Scnegaff_composite_meanpostexam <- scale(examdata_postexam$cnegaff_composite_meanpostexam)

######3i. exporting version of dataset with preprocessing completed#####
# write_sav(ldata, "Study 2_exam_preprocessing complete.sav")


###########################  5. MAIN ANALYSES STUDY 2 #########################################
length(unique(examdata_postexam$Participant))
#to get number of completed beeps need a dataset with non-completed ones removed. Can do with any variable since must answer all items to complete
examdata_postexam_numbeeps <- subset(examdata_postexam, !(is.na(ER_reapp)))
nrow(examdata_postexam_numbeeps)#number of completed beeps

#####5a. Descriptive Statistics######
######5ai) time between beeps####
examdata_postexam <- examdata_postexam[order(examdata_postexam$Participant, examdata_postexam$exam_beepnum),]
examdata_postexam <- examdata_postexam %>% group_by(Participant,beepday) %>% mutate(beeptime1 = lag(beeptime,1))
examdata_postexam$beepinterval <- examdata_postexam$beeptime - examdata_postexam$beeptime1 
mean(examdata_postexam$beepinterval, na.rm = TRUE)
sd(examdata_postexam$beepinterval, na.rm = TRUE)

##### 5aii) checking total number of valid beeps####
examdata_postexam$beepssent <- ave(examdata_postexam$Participant, examdata_postexam$Participant, FUN=length)
summary(sapply(split(examdata_postexam$beepssent, examdata_postexam$Participant), function(x) x[1]))
#note that our ESM app is coded so that all questions must be answered for the survey to be submitted, so can use any survey item in the below code
examdata_postexam <- subset(examdata_postexam, !(is.na(ER_reapp)))
examdata_postexam$beepsdone <- ave(examdata_postexam$Participant, examdata_postexam$Participant, FUN=length)
summary(sapply(split(examdata_postexam$beepsdone, examdata_postexam$Participant), function(x) x[1]))
examdata_postexam$compliance <- examdata_postexam$beepsdone/examdata_postexam$beepssent*100
summary(sapply(split(examdata_postexam$compliance, examdata_postexam$Participant), function(x) x[1]))
sd(sapply(split(examdata_postexam$compliance, examdata_postexam$Participant), function(x) x[1]))

#####5aiii) Descriptive Statistics######

#making a subset of the data for descriptive statistics for the paper
des_examdata_postexam <- data.frame (examdata_postexam$Participant, examdata_postexam$RICCneg_after.fz, examdata_postexam$perc_pass, examdata_postexam$ER_rumination, 
                                     examdata_postexam$ER_distraction, examdata_postexam$ER_reapp, examdata_postexam$ER_acceptance, examdata_postexam$ER_supp, 
                                     examdata_postexam$ER_soc_sharing, examdata_postexam$negaff_composite)

#now using the statsBy command in the psych package to get within and between correlations
des_examdata_postexam <- statsBy(des_examdata_postexam, "examdata_postexam.Participant")
print(des_examdata_postexam,short=FALSE)
lowerMat(des_examdata_postexam$pbg)
lowerMat(des_examdata_postexam$pwg)

#means and between person sds
#gender
table(sapply(split(examdata_postexam$gender, examdata_postexam$Participant), function(x) x[1]))

#age
summary(sapply(split(examdata_postexam$age, examdata_postexam$Participant), function(x) x[1]))
sd(sapply(split(examdata_postexam$age, examdata_postexam$Participant), function(x) x[1]))

#emo differentiation (untransformed for interpretability)
summary(sapply(split(examdata_postexam$RICCneg_after, examdata_postexam$Participant), function(x) x[1]))
sd(sapply(split(examdata_postexam$RICCneg_after, examdata_postexam$Participant), function(x) x[1]), na.rm=TRUE)

#percentage passed
summary(sapply(split(examdata_postexam$perc_pass, examdata_postexam$Participant), function(x) x[1]))
sd(sapply(split(examdata_postexam$perc_pass, examdata_postexam$Participant), function(x) x[1]), na.rm=TRUE)

#ER strategies
#rumination
summary(sapply(split(examdata_postexam$ER_rumination, examdata_postexam$Participant), function(x) x[1]))
sd(sapply(split(examdata_postexam$ER_rumination, examdata_postexam$Participant), function(x) x[1]), na.rm=TRUE)
#distraction
summary(sapply(split(examdata_postexam$ER_distraction, examdata_postexam$Participant), function(x) x[1]))
sd(sapply(split(examdata_postexam$ER_distraction, examdata_postexam$Participant), function(x) x[1]), na.rm=TRUE)
#reappraisal
summary(sapply(split(examdata_postexam$ER_reapp, examdata_postexam$Participant), function(x) x[1]))
sd(sapply(split(examdata_postexam$ER_reapp, examdata_postexam$Participant), function(x) x[1]), na.rm=TRUE)
#acceptance
summary(sapply(split(examdata_postexam$ER_acceptance, examdata_postexam$Participant), function(x) x[1]))
sd(sapply(split(examdata_postexam$ER_acceptance, examdata_postexam$Participant), function(x) x[1]), na.rm=TRUE)
#suppression
summary(sapply(split(examdata_postexam$ER_supp, examdata_postexam$Participant), function(x) x[1]))
sd(sapply(split(examdata_postexam$ER_supp, examdata_postexam$Participant), function(x) x[1]), na.rm=TRUE)
#social sharing
summary(sapply(split(examdata_postexam$ER_soc_sharing, examdata_postexam$Participant), function(x) x[1]))
sd(sapply(split(examdata_postexam$ER_soc_sharing, examdata_postexam$Participant), function(x) x[1]), na.rm=TRUE)

#negative emotion
summary(sapply(split(examdata_postexam$negaff_composite, examdata_postexam$Participant), function(x) x[1]))
sd(sapply(split(examdata_postexam$negaff_composite, examdata_postexam$Participant), function(x) x[1]), na.rm=TRUE)

######5aiv) Reliability#####
#note that I'm removing the missing data to reduce computational time
#each of these models takes about 5 minutes to run. Make sure aov is set to false - this method falls down with large datasets
#as per the documentation with the package
#NEGATIVE
negaff_exam <- data.frame(examdata_postexam$Participant, examdata_postexam$exam_beepnum, examdata_postexam$emotion_sad, examdata_postexam$emotion_angry,
                          examdata_postexam$emotion_disapp, examdata_postexam$emotion_ashamed, examdata_postexam$emotion_anxious, 
                          examdata_postexam$emotion_stressed)
negaff_exam <- subset(negaff_exam, (!is.na(examdata_postexam.emotion_sad)))
#need to zap labels or it won't work for some reason.
negaff_exam <- zap_labels(negaff_exam)

# mlr(negaff_exam,grp="examdata_postexam.Participant", Time="examdata_postexam.exam_beepnum", 
#     items=c("examdata_postexam.emotion_sad","examdata_postexam.emotion_angry","examdata_postexam.emotion_disapp",
#             "examdata_postexam.emotion_ashamed","examdata_postexam.emotion_anxious","examdata_postexam.emotion_stressed"),
#     aov = FALSE, lmer = TRUE, lme = FALSE)

####5b. MODEL 1: Does ED predict ER strategy intensity? ##########
#Controlling for person-mean NA - model in paper
exrumi <- lmer(SER_rumination ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exdist <- lmer(SER_distraction ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exreapp <- lmer(SER_reapp ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exsupp <- lmer(SER_supp ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exssh <- lmer(SER_soc_sharing ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exacc <- lmer(SER_acceptance ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)

summary(exrumi)
summary(exdist)
summary(exreapp)
summary(exsupp)
summary(exssh)
summary(exacc)

confint(exrumi, method="Wald")
confint(exdist, method="Wald")
confint(exreapp, method="Wald")
confint(exsupp, method="Wald")
confint(exssh, method="Wald")
confint(exacc, method="Wald")

#Not Controlling for person-mean NA - to check for robustness
exrumi1 <- lmer(scale(ER_rumination) ~ scale(cRICCneg_after.fz)+ scale(cperc_pass) + (1 | Participant), data=examdata_postexam)
exdist1 <- lmer(scale(ER_distraction) ~ scale(cRICCneg_after.fz)+ scale(cperc_pass)  + (1 | Participant), data=examdata_postexam)
exreapp1 <- lmer(scale(ER_reapp) ~ scale(cRICCneg_after.fz)+ scale(cperc_pass)  + (1 | Participant), data=examdata_postexam)
exsupp1 <- lmer(scale(ER_supp) ~ scale(cRICCneg_after.fz)+ scale(cperc_pass)  + (1 | Participant), data=examdata_postexam)
exssh1 <- lmer(scale(ER_soc_sharing) ~ scale(cRICCneg_after.fz)+ scale(cperc_pass) + (1 | Participant), data=examdata_postexam)
exacc1 <- lmer(scale(ER_acceptance) ~ scale(cRICCneg_after.fz)+ scale(cperc_pass) + (1 | Participant), data=examdata_postexam)

summary(exrumi1)
summary(exdist1)
summary(exreapp1)
summary(exsupp1)
summary(exssh1)
summary(exacc1)

####5c. MODEL 2: How does the interaction between ED and ER predict NA? ###########
#Not Controlling for person-mean NA - model in paper
exrumiNA <- lmer(Snegaff_composite ~ ScER_rumination*ScRICCneg_after.fz + Scperc_pass
                 + Scnegaff_composite_1 + (ScER_rumination+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exreappNA <- lmer(Snegaff_composite ~ ScER_reapp*ScRICCneg_after.fz + Scperc_pass
                  + Scnegaff_composite_1 + (ScER_reapp+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exsuppNA <- lmer(Snegaff_composite ~ ScER_supp*ScRICCneg_after.fz + Scperc_pass
                 + Scnegaff_composite_1 + (ScER_supp+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exsshNA <- lmer(Snegaff_composite ~ ScER_soc_sharing*ScRICCneg_after.fz + Scperc_pass
                + Scnegaff_composite_1 + (ScER_soc_sharing+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exdistNA <- lmer(Snegaff_composite ~ ScER_distraction*ScRICCneg_after.fz + Scperc_pass
                 + Scnegaff_composite_1 + (ScER_distraction+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exaccNA <- lmer(Snegaff_composite ~ ScER_acceptance*ScRICCneg_after.fz + Scperc_pass
                + Scnegaff_composite_1 + (ScER_acceptance+ Scnegaff_composite_1| Participant), data=examdata_postexam)

summary(exrumiNA)
summary(exdistNA)
summary(exreappNA)
summary(exsuppNA)
summary(exsshNA)
summary(exaccNA)

confint(exrumiNA, method="Wald")
confint(exdistNA, method="Wald")
confint(exreappNA, method="Wald")
confint(exsuppNA, method="Wald")
confint(exsshNA, method="Wald")
confint(exaccNA, method="Wald")

#Controlling for person-mean NA - to check for robustness
exrumiNA1 <- lmer(scale(negaff_composite) ~ scale(cER_rumination)*scale(cRICCneg_after.fz)+scale(cperc_pass)
                  + scale(cER_rumination)*scale(negaff_composite_meanpostexam) + scale(cnegaff_composite_1) 
                  + (scale(cER_rumination)+ scale(cnegaff_composite_1)| Participant), data=examdata_postexam)
exreappNA1 <- lmer(scale(negaff_composite) ~ scale(cER_reapp)*scale(cRICCneg_after.fz)+scale(cperc_pass)
                   + scale(cER_reapp)*scale(negaff_composite_meanpostexam) + scale(cnegaff_composite_1) 
                   + (scale(cER_reapp)+ scale(cnegaff_composite_1)| Participant), data=examdata_postexam)
exsuppNA1 <- lmer(scale(negaff_composite) ~ scale(cER_supp)*scale(cRICCneg_after.fz)+scale(cperc_pass)
                  + scale(cER_supp)*scale(negaff_composite_meanpostexam) + scale(cnegaff_composite_1) 
                  + (scale(cER_supp)+ scale(cnegaff_composite_1)| Participant), data=examdata_postexam)
exsshNA1 <- lmer(scale(negaff_composite) ~ scale(cER_soc_sharing)*scale(cRICCneg_after.fz)+scale(cperc_pass)
                 + scale(cER_soc_sharing)*scale(negaff_composite_meanpostexam) + scale(cnegaff_composite_1) 
                 + (scale(cER_soc_sharing)+ scale(cnegaff_composite_1)| Participant), data=examdata_postexam)
exdistNA1 <- lmer(scale(negaff_composite) ~ scale(cER_distraction)*scale(cRICCneg_after.fz)+scale(cperc_pass)
                  + scale(cER_distraction)*scale(negaff_composite_meanpostexam) + scale(cnegaff_composite_1) 
                  + (scale(cER_distraction)+ scale(cnegaff_composite_1)| Participant), data=examdata_postexam)
exaccNA1 <- lmer(scale(negaff_composite) ~ scale(cER_acceptance)*scale(cRICCneg_after.fz)+scale(cperc_pass)
                 + scale(cER_acceptance)*scale(negaff_composite_meanpostexam) + scale(cnegaff_composite_1) 
                 + (scale(cnegaff_composite_1)| Participant), data=examdata_postexam)

summary(exrumiNA1)
summary(exdistNA1)
summary(exreappNA1)
summary(exsuppNA1)
summary(exsshNA1)
summary(exaccNA1)

#####5d. Simple slopes for interactions and graph####
#simple slopes calculated using Kris Preacher's online calculator (Preacher, Curran, & Bauer, 2006)
#http://www.quantpsy.org/interact/hlm2.htm - case 3 cross-level interaction
#to calculate, need model coefficients (From above) and variance/covariance matrix (calculated below)
#calculated at plus and minus one SD (-1, 1 because these are standardized variables)
#df is N - k - 1 (Sample size - predictors in model - 1) = 95 (101 - 5 - 1)
#output of these models are saved in a word document called Simple Slopes
vcov(exrumiNA)
vcov(exdistNA)
vcov(exsshNA)
vcov(exaccNA)
#have to hand calculate confidence intervals for simple slopes: 1.96 * SE +/- coefficent

#PLOTS OF SIGNIFICANT INTERACTIONS
#for these plots, I'll use unstandardized variables, since that'll make them more interpretable
#to do that, I'll take the simple slopes calculated above and unstandardize each

#RUMINATION
#to unstandardize the simple slopes, multiply standardized slope by sd of DV/sd of predictor (low ED first then high ED)
uslowdiffslope_rumiE <- 0.1076 * (sd(examdata_postexam$negaff_composite, na.rm = TRUE))/(sd(examdata_postexam$cER_rumination, na.rm = TRUE))
ushighdiffslope_rumiE <- 0.0579 * (sd(examdata_postexam$negaff_composite, na.rm = TRUE))/(sd(examdata_postexam$cER_rumination, na.rm = TRUE))
#to unstandardize the simple intercepts, mean of DV plus standardized score * standard deviation of DV
uslowdiffint_rumiE <- mean(examdata_postexam$negaff_composite, na.rm = TRUE)+ (0.0215*sd(examdata_postexam$negaff_composite, na.rm = TRUE))
ushighdiffint_rumiE <- mean(examdata_postexam$negaff_composite, na.rm = TRUE)+ (-0.0216*sd(examdata_postexam$negaff_composite, na.rm = TRUE))

#PLOT
#first I'm getting all the parameters I need to make the plot
#I'm going to use dotted lines for when the lines drop below -3SD from the mean, and above +3SD from the mean
#so that it's clear that the lines are interpolated here  based on very few datapoints
plus3rumiEsd <- ((sd(examdata_postexam$cER_rumination, na.rm=TRUE))*3)
minus3rumiEsd <- 0-((sd(examdata_postexam$cER_rumination, na.rm=TRUE))*3)

#figuring out the points to start and finish each of the lines using the regression equation (y = intercept + slope*xvalue)
#-6 and 6 are the theoretical min and max in the data
lowdifflinestart_rumiE <- uslowdiffint_rumiE+(uslowdiffslope_rumiE*minus3rumiEsd) #low differentiation line begin
lowdifflineend_rumiE <- uslowdiffint_rumiE+(uslowdiffslope_rumiE*plus3rumiEsd) #low differentiation line end
lowdiffprestart_rumiE <- uslowdiffint_rumiE +(uslowdiffslope_rumiE*-6) #low differentiation pre-line segment begin 
lowdiffpostend_rumiE <- uslowdiffint_rumiE +(uslowdiffslope_rumiE*6) #low differentiation post-line segment end
highdifflinestart_rumiE <- ushighdiffint_rumiE+(ushighdiffslope_rumiE*minus3rumiEsd) #low differentiation line begin
highdifflineend_rumiE <- ushighdiffint_rumiE+(ushighdiffslope_rumiE*plus3rumiEsd) #low differentiation line end
highdiffprestart_rumiE <- ushighdiffint_rumiE +(ushighdiffslope_rumiE*-6) #low differentiation pre-line segment begin 
highdiffpostend_rumiE <- ushighdiffint_rumiE +(ushighdiffslope_rumiE*6) #low differentiation post-line segment end

#here is the plot code
examrumiNAplot <-ggplot(examdata_postexam, aes(x = cER_rumination, y = negaff_composite)) +
  geom_point(aes(colour = cRICCneg_after.fz), size = 0.5) +
  scale_colour_gradient2(low = "red", mid = "lightcyan", high = "blue") +
  labs(x = 'Rumination (person-mean centered)', y = 'Negative emotion', color="Emotion differentiation") +
  geom_segment(aes(x = minus3rumiEsd, y = lowdifflinestart_rumiE, xend = plus3rumiEsd, yend = lowdifflineend_rumiE, linetype='Low emotion differentiation (-1SD)')) +
  geom_segment(aes(x = minus3rumiEsd, y = highdifflinestart_rumiE, xend = plus3rumiEsd, yend = highdifflineend_rumiE, linetype='High emotion differentiation (+1SD)')) +
  geom_segment(x=-6, y=lowdiffprestart_rumiE, xend = minus3rumiEsd, yend = lowdifflinestart_rumiE, linetype = "dotted") +
  geom_segment(x=plus3rumiEsd, y=lowdifflineend_rumiE, xend = 6, yend = lowdiffpostend_rumiE, linetype = "dotted") +
  geom_segment(x=-6, y=highdiffprestart_rumiE, xend = minus3rumiEsd, yend = highdifflinestart_rumiE, linetype = "dotted") +
  geom_segment(x=plus3rumiEsd, y=highdifflineend_rumiE, xend = 6, yend = highdiffpostend_rumiE, linetype = "dotted") +
  scale_linetype_manual(values=c('longdash','solid'), 
                        breaks=c('Low emotion differentiation (-1SD)','High emotion differentiation (+1SD)'),
                        name='Simple Slopes')+ scale_y_continuous(limits=c(0, 100), expand = c(0, 0))+
  scale_x_continuous(limits=c(min(examdata_postexam$cER_rumination, na.rm=TRUE),(max(examdata_postexam$cER_rumination, na.rm=TRUE))), expand = c(0,0), 
                     breaks = seq(-6, 6, 1)) + apatheme 


#DISTRACTION
#to unstandardize the simple slopes, multiply standardized slope by sd of DV/sd of predictor (low ED first then high ED)
uslowdiffslope_distE <-0.0504 * (sd(examdata_postexam$negaff_composite, na.rm = TRUE))/(sd(examdata_postexam$cER_distraction, na.rm = TRUE))
ushighdiffslope_distE <--0.0007 * (sd(examdata_postexam$negaff_composite, na.rm = TRUE))/(sd(examdata_postexam$cER_distraction, na.rm = TRUE))
#to unstandardize the simple intercepts, mean of DV plus standardized score * standard deviation of DV
uslowdiffint_distE <- mean(examdata_postexam$negaff_composite, na.rm = TRUE)+ (0.032*sd(examdata_postexam$negaff_composite, na.rm = TRUE))
ushighdiffint_distE <- mean(examdata_postexam$negaff_composite, na.rm = TRUE)+ (-0.0382*sd(examdata_postexam$negaff_composite, na.rm = TRUE))

#PLOT
#first I'm getting all the parameters I need to make the plot
#I'm going to use dotted lines for when the lines drop below -3SD from the mean, and above +3SD from the mean
#so that it's clear that the lines are interpolated here  based on very few datapoints
plus3distEsd <- ((sd(examdata_postexam$cER_distraction, na.rm=TRUE))*3)
minus3distEsd <- 0-((sd(examdata_postexam$cER_distraction, na.rm=TRUE))*3)

#figuring out the points to start and finish each of the lines using the regression equation (y = intercept + slope*xvalue)
#-6 and 6 are the theoretical min and max in the data
lowdifflinestart_distE <- uslowdiffint_distE+(uslowdiffslope_distE*minus3distEsd) #low differentiation line begin
lowdifflineend_distE <- uslowdiffint_distE+(uslowdiffslope_distE*plus3distEsd) #low differentiation line end
lowdiffprestart_distE <- uslowdiffint_distE +(uslowdiffslope_distE*-6) #low differentiation pre-line segment begin 
lowdiffpostend_distE <- uslowdiffint_distE +(uslowdiffslope_distE*6) #low differentiation post-line segment end
highdifflinestart_distE <- ushighdiffint_distE+(ushighdiffslope_distE*minus3distEsd) #low differentiation line begin
highdifflineend_distE <- ushighdiffint_distE+(ushighdiffslope_distE*plus3distEsd) #low differentiation line end
highdiffprestart_distE <- ushighdiffint_distE +(ushighdiffslope_distE*-6) #low differentiation pre-line segment begin 
highdiffpostend_distE <- ushighdiffint_distE +(ushighdiffslope_distE*6) #low differentiation post-line segment end

#here is the plot code
examdistNAplot <-ggplot(examdata_postexam, aes(x = cER_distraction, y = negaff_composite)) +
  geom_point(aes(colour = cRICCneg_after.fz), size = 0.5) +
  scale_colour_gradient2(low = "red", mid = "lightcyan", high = "blue") +
  labs(x = 'Distraction (person-mean centered)', y = 'Negative emotion', color="Emotion differentiation") +
  geom_segment(aes(x = minus3distEsd, y = lowdifflinestart_distE, xend = plus3distEsd, yend = lowdifflineend_distE, linetype='Low emotion differentiation (-1SD)')) +
  geom_segment(aes(x = minus3distEsd, y = highdifflinestart_distE, xend = plus3distEsd, yend = highdifflineend_distE, linetype='High emotion differentiation (+1SD)')) +
  geom_segment(x=-6, y=lowdiffprestart_distE, xend = minus3distEsd, yend = lowdifflinestart_distE, linetype = "dotted") +
  geom_segment(x=plus3distEsd, y=lowdifflineend_distE, xend = 6, yend = lowdiffpostend_distE, linetype = "dotted") +
  geom_segment(x=-6, y=highdiffprestart_distE, xend = minus3distEsd, yend = highdifflinestart_distE, linetype = "dotted") +
  geom_segment(x=plus3distEsd, y=highdifflineend_distE, xend = 6, yend = highdiffpostend_distE, linetype = "dotted") +
  scale_linetype_manual(values=c('longdash','solid'), 
                        breaks=c('Low emotion differentiation (-1SD)','High emotion differentiation (+1SD)'),
                        name='Simple Slopes')+ scale_y_continuous(limits=c(0, 100), expand = c(0, 0))+
  scale_x_continuous(limits=c(min(examdata_postexam$cER_distraction, na.rm=TRUE),(max(examdata_postexam$cER_distraction, na.rm=TRUE))), expand = c(0,0), 
                     breaks = seq(-6, 6, 1)) + apatheme 

#SOCIAL SHARING
#to unstandardize the simple slopes, multiply standardized slope by sd of DV/sd of predictor (low ED first then high ED)
uslowdiffslope_sshE <- 0.0675 * (sd(examdata_postexam$negaff_composite, na.rm = TRUE))/(sd(examdata_postexam$cER_soc_sharing, na.rm = TRUE))
ushighdiffslope_sshE <-0.0071 * (sd(examdata_postexam$negaff_composite, na.rm = TRUE))/(sd(examdata_postexam$cER_soc_sharing, na.rm = TRUE))
#to unstandardize the simple intercepts, mean of DV plus standardized score * standard deviation of DV
uslowdiffint_sshE <- mean(examdata_postexam$negaff_composite, na.rm = TRUE)+ (0.0298*sd(examdata_postexam$negaff_composite, na.rm = TRUE))
ushighdiffint_sshE <- mean(examdata_postexam$negaff_composite, na.rm = TRUE)+ (-0.0341*sd(examdata_postexam$negaff_composite, na.rm = TRUE))

#PLOT
#first I'm getting all the parameters I need to make the plot
#I'm going to use dotted lines for when the lines drop below -3SD from the mean, and above +3SD from the mean
#so that it's clear that the lines are interpolated here  based on very few datapoints
plus3sshEsd <- ((sd(examdata_postexam$cER_soc_sharing, na.rm=TRUE))*3)
minus3sshEsd <- 0-((sd(examdata_postexam$cER_soc_sharing, na.rm=TRUE))*3)

#figuring out the points to start and finish each of the lines using the regression equation (y = intercept + slope*xvalue)
#-6 and 6 are the theoretical min and max in the data
lowdifflinestart_sshE <- uslowdiffint_sshE+(uslowdiffslope_sshE*minus3sshEsd) #low differentiation line begin
lowdifflineend_sshE <- uslowdiffint_sshE+(uslowdiffslope_sshE*plus3sshEsd) #low differentiation line end
lowdiffprestart_sshE <- uslowdiffint_sshE +(uslowdiffslope_sshE*-6) #low differentiation pre-line segment begin 
lowdiffpostend_sshE <- uslowdiffint_sshE +(uslowdiffslope_sshE*6) #low differentiation post-line segment end
highdifflinestart_sshE <- ushighdiffint_sshE+(ushighdiffslope_sshE*minus3sshEsd) #low differentiation line begin
highdifflineend_sshE <- ushighdiffint_sshE+(ushighdiffslope_sshE*plus3sshEsd) #low differentiation line end
highdiffprestart_sshE <- ushighdiffint_sshE +(ushighdiffslope_sshE*-6) #low differentiation pre-line segment begin 
highdiffpostend_sshE <- ushighdiffint_sshE +(ushighdiffslope_sshE*6) #low differentiation post-line segment end

#here is the plot code
examsshNAplot <-ggplot(examdata_postexam, aes(x = cER_soc_sharing, y = negaff_composite)) +
  geom_point(aes(colour = cRICCneg_after.fz), size = 0.5) +
  scale_colour_gradient2(low = "red", mid = "lightcyan", high = "blue") +
  labs(x = 'Social sharing (person-mean centered)', y = 'Negative emotion', color="Emotion differentiation") +
  geom_segment(aes(x = minus3sshEsd, y = lowdifflinestart_sshE, xend = plus3sshEsd, yend = lowdifflineend_sshE, linetype='Low emotion differentiation (-1SD)')) +
  geom_segment(aes(x = minus3sshEsd, y = highdifflinestart_sshE, xend = plus3sshEsd, yend = highdifflineend_sshE, linetype='High emotion differentiation (+1SD)')) +
  geom_segment(x=-6, y=lowdiffprestart_sshE, xend = minus3sshEsd, yend = lowdifflinestart_sshE, linetype = "dotted") +
  geom_segment(x=plus3sshEsd, y=lowdifflineend_sshE, xend = 6, yend = lowdiffpostend_sshE, linetype = "dotted") +
  geom_segment(x=-6, y=highdiffprestart_sshE, xend = minus3sshEsd, yend = highdifflinestart_sshE, linetype = "dotted") +
  geom_segment(x=plus3sshEsd, y=highdifflineend_sshE, xend = 6, yend = highdiffpostend_sshE, linetype = "dotted") +
  scale_linetype_manual(values=c('longdash','solid'), 
                        breaks=c('Low emotion differentiation (-1SD)','High emotion differentiation (+1SD)'),
                        name='Simple Slopes')+ scale_y_continuous(limits=c(0, 100), expand = c(0, 0))+
  scale_x_continuous(expand = c(0,0), 
                     breaks = seq(-4, 6, 1)) + apatheme 

#ACCEPTANCE
#to unstandardize the simple slopes, multiply standardized slope by sd of DV/sd of predictor (low ED first then high ED)
uslowdiffslope_accE <- -0.0686 * (sd(examdata_postexam$negaff_composite, na.rm = TRUE))/(sd(examdata_postexam$cER_acceptance, na.rm = TRUE))
ushighdiffslope_accE <- -0.0059 * (sd(examdata_postexam$negaff_composite, na.rm = TRUE))/(sd(examdata_postexam$cER_acceptance, na.rm = TRUE))
#to unstandardize the simple intercepts, mean of DV plus standardized score * standard deviation of DV
uslowdiffint_accE <- mean(examdata_postexam$negaff_composite, na.rm = TRUE)+ (0.04*sd(examdata_postexam$negaff_composite, na.rm = TRUE))
ushighdiffint_accE <- mean(examdata_postexam$negaff_composite, na.rm = TRUE)+ (-0.048*sd(examdata_postexam$negaff_composite, na.rm = TRUE))

#PLOT
#first I'm getting all the parameters I need to make the plot
#I'm going to use dotted lines for when the lines drop below -3SD from the mean, and above +3SD from the mean
#so that it's clear that the lines are interpolated here  based on very few datapoints
plus3accEsd <- ((sd(examdata_postexam$cER_acceptance, na.rm=TRUE))*3)
minus3accEsd <- 0-((sd(examdata_postexam$cER_acceptance, na.rm=TRUE))*3)

#figuring out the points to start and finish each of the lines using the regression equation (y = intercept + slope*xvalue)
#-6 and 6 are the theoretical min and max in the data
lowdifflinestart_accE <- uslowdiffint_accE+(uslowdiffslope_accE*minus3accEsd) #low differentiation line begin
lowdifflineend_accE <- uslowdiffint_accE+(uslowdiffslope_accE*plus3accEsd) #low differentiation line end
lowdiffprestart_accE <- uslowdiffint_accE +(uslowdiffslope_accE*-6) #low differentiation pre-line segment begin 
lowdiffpostend_accE <- uslowdiffint_accE +(uslowdiffslope_accE*6) #low differentiation post-line segment end
highdifflinestart_accE <- ushighdiffint_accE+(ushighdiffslope_accE*minus3accEsd) #low differentiation line begin
highdifflineend_accE <- ushighdiffint_accE+(ushighdiffslope_accE*plus3accEsd) #low differentiation line end
highdiffprestart_accE <- ushighdiffint_accE +(ushighdiffslope_accE*-6) #low differentiation pre-line segment begin 
highdiffpostend_accE <- ushighdiffint_accE +(ushighdiffslope_accE*6) #low differentiation post-line segment end

#here is the plot code
examaccNAplot <-ggplot(examdata_postexam, aes(x = cER_acceptance, y = negaff_composite)) +
  geom_point(aes(colour = cRICCneg_after.fz), size = 0.5) +
  scale_colour_gradient2(low = "red", mid = "lightcyan", high = "blue") +
  labs(x = 'Acceptance (person-mean centered)', y = 'Negative emotion', color="Emotion differentiation") +
  geom_segment(aes(x = minus3accEsd, y = lowdifflinestart_accE, xend = plus3accEsd, yend = lowdifflineend_accE, linetype='Low emotion differentiation (-1SD)')) +
  geom_segment(aes(x = minus3accEsd, y = highdifflinestart_accE, xend = plus3accEsd, yend = highdifflineend_accE, linetype='High emotion differentiation (+1SD)')) +
  geom_segment(x=-6, y=lowdiffprestart_accE, xend = minus3accEsd, yend = lowdifflinestart_accE, linetype = "dotted") +
  geom_segment(x=plus3accEsd, y=lowdifflineend_accE, xend = 6, yend = lowdiffpostend_accE, linetype = "dotted") +
  geom_segment(x=-6, y=highdiffprestart_accE, xend = minus3accEsd, yend = highdifflinestart_accE, linetype = "dotted") +
  geom_segment(x=plus3accEsd, y=highdifflineend_accE, xend = 6, yend = highdiffpostend_accE, linetype = "dotted") +
  scale_linetype_manual(values=c('longdash','solid'), 
                        breaks=c('Low emotion differentiation (-1SD)','High emotion differentiation (+1SD)'),
                        name='Simple Slopes')+ scale_y_continuous(limits=c(0, 100), expand = c(0, 0))+
  scale_x_continuous(expand = c(0,0), 
                     breaks = seq(-6, 6, 1)) + apatheme 

#####5e. Exporting plots into multi-panel figure#####
png('simpleslopes_examdataNA_R1.png', units="cm", width=25, height=15, res=300)
ggarrange(examrumiNAplot, examdistNAplot, examaccNAplot, examsshNAplot,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
dev.off()

tiff('simpleslopes_examdataNA_highres_R1.tiff', units="cm", width=25, height=15, res=300)
ggarrange(examrumiNAplot, examdistNAplot, examaccNAplot, examsshNAplot,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
dev.off()


######6. EFFECT SIZES#####

#Run the multilevel_effect_size_function R code #

multilevel.effect.size<-function(lmer.model){
  #Information of the measures can be found in LaHuis, Hartman, Hakoyama, and Clark (2014).
  #This function will produce Multiple R squared measures for multilevel models based 
  #on lmer models from lme4 package in R.
  #This function should be used for research and education (and/or entertainment) purposes only.
  #contact shotaro.hakoyama@gmail.com for bugs, suggestions, and comments. 
  
  require(lme4);
  #extracting the name of the data frame from the lmer object import them to use within the function.
  data<-lmer.model@frame
  
  #extracting pieces of the lmer model syntax.
  model<-strsplit(as.character(attributes(lmer.model)$call)[2], split=" + (", fixed=T)
  criterion<-names(data)[1]
  group.id<-names(data)[length(names(data))]
  #computing null model needed for R2 approx and R2 S&B
  null<-update(lmer.model,as.formula(paste(criterion, " ~ 1 + (1 | ", group.id,")", sep="")))
  
  #extracting variance components and coefficients from the models. 
  null.var<-lme4::VarCorr(null)
  null.tau00<-attr(null.var[[group.id]],"stddev")^2
  null.sigma<-attr(null.var,'sc')^2
  
  fixed.model<-update(lmer.model,as.formula(paste(model[[1]][1], " + ( 1 | ", group.id,")" ,sep="")))
  fixed.model.fixef<-fixef(fixed.model)
  fixed.model.var<-lme4::VarCorr(fixed.model)
  fixed.model.tau00<-attr(fixed.model.var[[group.id]],"stddev")^2
  fixed.model.sigma<-attr(fixed.model.var,'sc')^2
  
  lmer.model.fixef<-fixef(lmer.model)
  lmer.model.var<-lme4::VarCorr(lmer.model)
  tau<-diag(lmer.model.var[[group.id]][,])
  lmer.model.sigma<-attr(lmer.model.var,'sc')^2
  
  slope.var<-NULL
  for ( zz in 2:length(tau)){
    slope.var<-append(slope.var, tau[zz]*var(data[names(tau)[zz]]))
  }
  
  if(is.na(tau["(Intercept)"])){
    tau.int<-fixed.model.tau00 }else {
      tau.int<-tau["(Intercept)"]
    }
  
  tau.sigma = tau.int+lmer.model.sigma+sum(slope.var)
  
  
  r2.approx.level1<-(null.sigma-fixed.model.sigma)/null.sigma
  r2.approx.level2<-(null.tau00-fixed.model.tau00)/null.tau00
  r2.s.b<-1-(tau.sigma)/(null.sigma+null.tau00)
  r2.ols<-summary(lm(as.formula(model[[1]][1]), data=data))$r.squared
  
  predicted.y<-predict(lmer.model, re.form=NA)
  r2.mvp<-var(predicted.y)/(var(predicted.y)+tau.sigma)
  
  R.squared<-data.frame(r2.approx.level1, r2.approx.level2, r2.s.b, r2.ols,r2.mvp )
  names(R.squared)<-c("R^2 Approx Level1", "R^2 Approx Level2", "R^2 Snijders & Bosker", "R^2 OLS","R^2 MVP")
  rownames(R.squared)<-NULL
  
  return(R.squared)
}

#####6a. EFFECT SIZES for Study 1#####
######6ai) Effect sizes for Model 1 Study 1#####
#Code for overall model if not already run above#
lrumi <- lmer(Srumi ~ ScRICCneg.fzW + Scnegaff_meanW + (1| PpID/Wave), data=ldata)
ldist <- lmer(Sdist ~ ScRICCneg.fzW + Scnegaff_meanW + (1| PpID/Wave), data=ldata)
lreapp <- lmer(Sreapp ~ ScRICCneg.fzW + Scnegaff_meanW + (1| PpID/Wave), data=ldata)
lsupp <- lmer(Ssupp ~ ScRICCneg.fzW + Scnegaff_meanW + (1| PpID/Wave), data=ldata)
lssh <- lmer(Sssh ~ ScRICCneg.fzW + Scnegaff_meanW + (1| PpID/Wave), data=ldata)

lrumiES <- multilevel.effect.size(lrumi)
ldistES <- multilevel.effect.size(ldist)
lreappES <- multilevel.effect.size(lreapp)
lsuppES <- multilevel.effect.size(lsupp)
lsshES <- multilevel.effect.size(lssh)

#model without emotion differentiation so I can look at the difference in variance explained without this variable#
lrumi_NED <- lmer(rumi ~ cnegaff_meanW + (1| PpID/Wave), data=ldata)
ldist_NED <- lmer(dist ~ cnegaff_meanW + (1| PpID/Wave), data=ldata)
lreapp_NED <- lmer(reapp ~ cnegaff_meanW + (1| PpID/Wave), data=ldata)
lsupp_NED <- lmer(supp ~ cnegaff_meanW + (1| PpID/Wave), data=ldata)
lssh_NED <- lmer(ssh ~ cnegaff_meanW + (1| PpID/Wave), data=ldata)

lrumi_NED_ES <- multilevel.effect.size(lrumi_NED)
ldist_NED_ES <- multilevel.effect.size(ldist_NED)
lreapp_NED_ES <- multilevel.effect.size(lreapp_NED)
lsupp_NED_ES <- multilevel.effect.size(lsupp_NED)
lssh_NED_ES <- multilevel.effect.size(lssh_NED)

#model without mean negative emotion so I can look at variance explained by this variable#
lrumi_NNE <- lmer(Srumi ~ ScRICCneg.fzW + (1| PpID/Wave), data=ldata)
ldist_NNE <- lmer(Sdist ~ ScRICCneg.fzW + (1| PpID/Wave), data=ldata)
lreapp_NNE <- lmer(Sreapp ~ ScRICCneg.fzW  + (1| PpID/Wave), data=ldata)
lsupp_NNE <- lmer(Ssupp ~ ScRICCneg.fzW  + (1| PpID/Wave), data=ldata)
lssh_NNE <- lmer(Sssh ~ ScRICCneg.fzW  + (1| PpID/Wave), data=ldata)

lrumi_NNE_ES <- multilevel.effect.size(lrumi_NNE)
ldist_NNE_ES <- multilevel.effect.size(ldist_NNE)
lreapp_NNE_ES <- multilevel.effect.size(lreapp_NNE)
lsupp_NNE_ES <- multilevel.effect.size(lsupp_NNE)
lssh_NNE_ES <- multilevel.effect.size(lssh_NNE)

#####6aii) Effect sizes for Model 2 Study 1####
#Code for overall model if not already run above#
lrumiNA <- lmer(Snegaff ~ Scrumi*ScRICCneg.fzW
                + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Scrumi + Scnegaff_1 | PpID) 
                + (0 + Scrumi + Scnegaff_1 | Wave) +  (0 + Scrumi + Scnegaff_1 || PpID:Wave), data=ldata)
ldistNA <- lmer(Snegaff ~ Scdist*ScRICCneg.fzW  
                + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Scdist + Scnegaff_1 | PpID) 
                + (0 + Scdist + Scnegaff_1 | Wave) +  (0 + Scdist + Scnegaff_1 || PpID:Wave), data=ldata)
lreappNA <- lmer(Snegaff ~ Screapp*ScRICCneg.fzW 
                 + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Screapp + Scnegaff_1 | PpID) 
                 + (0 + Screapp + Scnegaff_1 | Wave) +  (0 + Screapp + Scnegaff_1 || PpID:Wave), data=ldata)
lsuppNA <- lmer(Snegaff ~ Scsupp*ScRICCneg.fzW 
                + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Scsupp + Scnegaff_1 | PpID) 
                + (0 + Scsupp + Scnegaff_1 | Wave) +  (0 + Scsupp + Scnegaff_1 || PpID:Wave), data=ldata)
lsshNA <- lmer(Snegaff ~ Scssh*ScRICCneg.fzW 
               + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Scssh + Scnegaff_1 | PpID) 
               + (0 + Scssh + Scnegaff_1 | Wave) +  (0 + Scssh + Scnegaff_1 || PpID:Wave), data=ldata)

lrumiNAES <- multilevel.effect.size(lrumiNA)
ldistNAES <- multilevel.effect.size(ldistNA)
lreappNAES <- multilevel.effect.size(lreappNA)
lsuppNAES <- multilevel.effect.size(lsuppNA)
lsshNAES <- multilevel.effect.size(lsshNA)

#model without emotion differentiation*emotion regulation interaction so I can look at the difference in variance explained without this variable#
lrumiNA_Nint <- lmer(Snegaff ~ Scrumi+ScRICCneg.fzW
                     + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Scrumi + Scnegaff_1 | PpID) 
                     + (0 + Scrumi + Scnegaff_1 | Wave) +  (0 + Scrumi + Scnegaff_1 || PpID:Wave), data=ldata)
ldistNA_Nint <- lmer(Snegaff ~ Scdist+ScRICCneg.fzW  
                     + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Scdist + Scnegaff_1 | PpID) 
                     + (0 + Scdist + Scnegaff_1 | Wave) +  (0 + Scdist + Scnegaff_1 || PpID:Wave), data=ldata)
lreappNA_Nint <- lmer(Snegaff ~ Screapp+ScRICCneg.fzW 
                      + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Screapp + Scnegaff_1 | PpID) 
                      + (0 + Screapp + Scnegaff_1 | Wave) +  (0 + Screapp + Scnegaff_1 || PpID:Wave), data=ldata)
lsuppNA_Nint <- lmer(Snegaff ~ Scsupp+ScRICCneg.fzW 
                     + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Scsupp + Scnegaff_1 | PpID) 
                     + (0 + Scsupp + Scnegaff_1 | Wave) +  (0 + Scsupp + Scnegaff_1 || PpID:Wave), data=ldata)
lsshNA_Nint <- lmer(Snegaff ~ Scssh+ScRICCneg.fzW 
                    + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Scssh + Scnegaff_1 | PpID) 
                    + (0 + Scssh + Scnegaff_1 | Wave) +  (0 + Scssh + Scnegaff_1 || PpID:Wave), data=ldata)

lrumiNA_NintES <- multilevel.effect.size(lrumiNA_Nint)
ldistNA_NintES <- multilevel.effect.size(ldistNA_Nint)
lreappNA_NintES <- multilevel.effect.size(lreappNA_Nint)
lsuppNA_NintES <- multilevel.effect.size(lsuppNA_Nint)
lsshNA_NintES <- multilevel.effect.size(lsshNA_Nint)

#model without emotion regulation strategy so I can look at difference in variance explained
lNA_NER <- lmer(Snegaff ~ ScRICCneg.fzW
                + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Scnegaff_1 | PpID) 
                + (0  + Scnegaff_1 | Wave) +  (0 + Scnegaff_1 || PpID:Wave), data=ldata)
lNA_NER_ES <- multilevel.effect.size(lNA_NER)

#models without differentiation so I can look at difference in variance explained
lrumiNA_NED <- lmer(Snegaff ~ Scrumi
                    + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Scrumi + Scnegaff_1 | PpID) 
                    + (0 + Scrumi + Scnegaff_1 | Wave) +  (0 + Scrumi + Scnegaff_1 || PpID:Wave), data=ldata)
ldistNA_NED <- lmer(Snegaff ~ Scdist  
                    + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Scdist + Scnegaff_1 | PpID) 
                    + (0 + Scdist + Scnegaff_1 | Wave) +  (0 + Scdist + Scnegaff_1 || PpID:Wave), data=ldata)
lreappNA_NED <- lmer(Snegaff ~ Screapp
                     + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Screapp + Scnegaff_1 | PpID) 
                     + (0 + Screapp + Scnegaff_1 | Wave) +  (0 + Screapp + Scnegaff_1 || PpID:Wave), data=ldata)
lsuppNA_NED <- lmer(Snegaff ~ Scsupp
                    + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Scsupp + Scnegaff_1 | PpID) 
                    + (0 + Scsupp + Scnegaff_1 | Wave) +  (0 + Scsupp + Scnegaff_1 || PpID:Wave), data=ldata)
lsshNA_NED <- lmer(Snegaff ~ Scssh
                   + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Scssh + Scnegaff_1 | PpID) 
                   + (0 + Scssh + Scnegaff_1 | Wave) +  (0 + Scssh + Scnegaff_1 || PpID:Wave), data=ldata)

lrumiNA_NEDES <- multilevel.effect.size(lrumiNA_NED)
ldistNA_NEDES <- multilevel.effect.size(ldistNA_NED)
lreappNA_NEDES <- multilevel.effect.size(lreappNA_NED)
lsuppNA_NEDES <- multilevel.effect.size(lsuppNA_NED)
lsshNA_NEDES <- multilevel.effect.size(lsshNA_NED)

#model without lagged negative affect so I can look at difference in variance explained
lrumiNA_NNA <- lmer(Snegaff ~ Scrumi*ScRICCneg.fzW
                    + (1|PpID) + (1|Wave)  + (0 + Scrumi | PpID) 
                    + (0 + Scrumi  | Wave) +  (0 + Scrumi  || PpID:Wave), data=ldata)
ldistNA_NNA <- lmer(Snegaff ~ Scdist*ScRICCneg.fzW  
                    + (1|PpID) + (1|Wave)  + (0 + Scdist  | PpID) 
                    + (0 + Scdist  | Wave) +  (0 + Scdist  || PpID:Wave), data=ldata)
lreappNA_NNA <- lmer(Snegaff ~ Screapp*ScRICCneg.fzW 
                     + (1|PpID) + (1|Wave)  + (0 + Screapp  | PpID) 
                     + (0 + Screapp  | Wave) +  (0 + Screapp  || PpID:Wave), data=ldata)
lsuppNA_NNA <- lmer(Snegaff ~ Scsupp*ScRICCneg.fzW 
                    + (1|PpID) + (1|Wave)  + (0 + Scsupp  | PpID) 
                    + (0 + Scsupp  | Wave) +  (0 + Scsupp  || PpID:Wave), data=ldata)
lsshNA_NNA <- lmer(Snegaff ~ Scssh*ScRICCneg.fzW 
                   + (1|PpID) + (1|Wave)  + (0 + Scssh  | PpID) 
                   + (0 + Scssh  | Wave) +  (0 + Scssh  || PpID:Wave), data=ldata)

lrumiNA_NNAES <- multilevel.effect.size(lrumiNA_NNA)
ldistNA_NNAES <- multilevel.effect.size(ldistNA_NNA)
lreappNA_NNAES <- multilevel.effect.size(lreappNA_NNA)
lsuppNA_NNAES <- multilevel.effect.size(lsuppNA_NNA)
lsshNA_NNAES <- multilevel.effect.size(lsshNA_NNA)

#####6aiii) Making a dataset of effect sizes#####
#first adding an identifying variable to each of the separate models
lrumiES$model <- "Longit Model 1 rumi full"
ldistES$model <- "Longit Model 1 dist full"
lreappES$model <- "Longit Model 1 reapp full"
lsuppES$model <- "Longit Model 1 supp full"
lsshES$model <- "Longit Model 1 ssh full"

lrumi_NED_ES$model <- "Longit Model 1 rumi no ED"
ldist_NED_ES$model <- "Longit Model 1 dist no ED"
lreapp_NED_ES$model <- "Longit Model 1 reapp no ED"
lsupp_NED_ES$model <- "Longit Model 1 supp no ED"
lssh_NED_ES$model <- "Longit Model 1 ssh no ED"

lrumi_NNE_ES$model <- "Longit Model 1 rumi no person neg emo"
ldist_NNE_ES$model <- "Longit Model 1 dist no person neg emo"
lreapp_NNE_ES$model <- "Longit Model 1 reapp no person neg emo"
lsupp_NNE_ES$model <- "Longit Model 1 supp no person neg emo"
lssh_NNE_ES$model <- "Longit Model 1 ssh no person neg emo"

lrumiNAES$model <- "Longit Model 2 rumi full"
ldistNAES$model <- "Longit Model 2 dist full"
lreappNAES$model <- "Longit Model 2 reapp full"
lsuppNAES$model <- "Longit Model 2 supp full"
lsshNAES$model <- "Longit Model 2 ssh full"

lrumiNA_NintES$model <- "Longit Model 2 rumi no ED*strategy"
ldistNA_NintES$model <- "Longit Model 2 dist no ED*strategy"
lreappNA_NintES$model <- "Longit Model 2 reapp no ED*strategy"
lsuppNA_NintES$model <- "Longit Model 2 supp no ED*strategy"
lsshNA_NintES$model <- "Longit Model 2 ssh no ED*strategy"

lrumiNA_NEDES$model <- "Longit Model 2 rumi no ED (and no interaction)"
ldistNA_NEDES$model <- "Longit Model 2 dist no ED (and no interaction)"
lreappNA_NEDES$model <- "Longit Model 2 reapp no ED (and no interaction)"
lsuppNA_NEDES$model <- "Longit Model 2 supp no ED (and no interaction)"
lsshNA_NEDES$model <- "Longit Model 2 ssh no ED (and no interaction)"

lNA_NER_ES$model <- "Longit Model 2 ssh no ER (and no interaction)"

lrumiNA_NNAES$model <- "Longit Model 2 rumi no lagged NA"
ldistNA_NNAES$model <- "Longit Model 2 dist no lagged NA"
lreappNA_NNAES$model <- "Longit Model 2 reapp no lagged NA"
lsuppNA_NNAES$model <- "Longit Model 2 supp no lagged NA"
lsshNA_NNAES$model <- "Longit Model 2 ssh no lagged NA"

longit_effectsizes <- rbind(lrumiES,ldistES,lreappES,lsuppES,lsshES,
                            lrumi_NED_ES, ldist_NED_ES, lreapp_NED_ES, lsupp_NED_ES, lssh_NED_ES,
                            lrumi_NNE_ES, ldist_NNE_ES, lreapp_NNE_ES, lsupp_NNE_ES, lssh_NNE_ES,
                            lrumiNAES, ldistNAES, lreappNAES, lsuppNAES, lsshNAES, 
                            lrumiNA_NintES, ldistNA_NintES,lreappNA_NintES, lsuppNA_NintES, lsshNA_NintES,
                            lrumiNA_NEDES, ldistNA_NEDES, lreappNA_NEDES, lsuppNA_NEDES, lsshNA_NEDES,
                            lNA_NER_ES,
                            lrumiNA_NNAES, ldistNA_NNAES, lreappNA_NNAES, lsuppNA_NNAES, lsshNA_NNAES)

#####6aiv) Computing partial effect sizes#####
#adding the partial effect sizes to the dataframe by subtracting reduced model from full model for each ER strategy

# first making a version of the effect sizes document with no factor because otherwise can't manipulate it easily
longit_effectsizes_nofactor <- longit_effectsizes
longit_effectsizes_nofactor$model <- NULL

#now taking the rows from each other - can get row information from opening the "longit_effectsizes" datafile and looking at row number there
#partial effect size for model 1 emotion differentiation
lrumiEDES <- longit_effectsizes_nofactor[1, ] - longit_effectsizes_nofactor[6, ]
ldistEDES <- longit_effectsizes_nofactor[2, ] - longit_effectsizes_nofactor[7, ]
lreappEDES <- longit_effectsizes_nofactor[3, ] - longit_effectsizes_nofactor[8, ]
lsuppEDES <- longit_effectsizes_nofactor[4, ] - longit_effectsizes_nofactor[9, ]
lsshEDES <- longit_effectsizes_nofactor[5, ] - longit_effectsizes_nofactor[10, ]

lrumiEDES$model <- "Longit Model 1 rumi partial R2 ED"
ldistEDES$model <- "Longit Model 1 dist partial R2 ED"
lreappEDES$model <- "Longit Model 1 reapp partial R2 ED"
lsuppEDES$model <- "Longit Model 1 supp partial R2 ED"
lsshEDES$model <- "Longit Model 1 ssh partial R2 ED"

#partial effect size for model 1 negative emotion
lrumiNEES <- longit_effectsizes_nofactor[1, ] - longit_effectsizes_nofactor[11, ]
ldistNEES <- longit_effectsizes_nofactor[2, ] - longit_effectsizes_nofactor[12, ]
lreappNEES <- longit_effectsizes_nofactor[3, ] - longit_effectsizes_nofactor[13, ]
lsuppNEES <- longit_effectsizes_nofactor[4, ] - longit_effectsizes_nofactor[14, ]
lsshNEES <- longit_effectsizes_nofactor[5, ] - longit_effectsizes_nofactor[15, ]

lrumiNEES$model <- "Longit Model 1 rumi partial R2 person neg emo"
ldistNEES$model <- "Longit Model 1 dist partial R2 person neg emo"
lreappNEES$model <- "Longit Model 1 reapp partial R2 person neg emo"
lsuppNEES$model <- "Longit Model 1 supp partial R2 person neg emo"
lsshNEES$model <- "Longit Model 1 ssh partial R2 person neg emo"

#partial effect size for model 2 interaction ED * strategy
lrumiNAintES <- longit_effectsizes_nofactor[16, ] - longit_effectsizes_nofactor[21, ]
ldistNAintES <- longit_effectsizes_nofactor[17, ] - longit_effectsizes_nofactor[22, ]
lreappNAintES <- longit_effectsizes_nofactor[18, ] - longit_effectsizes_nofactor[23, ]
lsuppNAintES <- longit_effectsizes_nofactor[19, ] - longit_effectsizes_nofactor[24, ]
lsshNAintES <- longit_effectsizes_nofactor[20, ] - longit_effectsizes_nofactor[25, ]

lrumiNAintES$model <- "Longit Model 2 rumi partial R2 ED*strategy"
ldistNAintES$model <- "Longit Model 2 dist partial R2 ED*strategy"
lreappNAintES$model <- "Longit Model 2 reapp partial R2 ED*strategy"
lsuppNAintES$model <- "Longit Model 2 supp partial R2 ED*strategy"
lsshNAintES$model <- "Longit Model 2 ssh partial R2 ED*strategy"

#partial effect size for model 2 ED (comparing model with no ED and no interaction to model with no interaction)
lrumiNAEDES <- longit_effectsizes_nofactor[21, ] - longit_effectsizes_nofactor[26, ]
ldistNAEDES <- longit_effectsizes_nofactor[22, ] - longit_effectsizes_nofactor[27, ]
lreappNAEDES <- longit_effectsizes_nofactor[23, ] - longit_effectsizes_nofactor[28, ]
lsuppNAEDES <- longit_effectsizes_nofactor[24, ] - longit_effectsizes_nofactor[29, ]
lsshNAEDES <- longit_effectsizes_nofactor[25, ] - longit_effectsizes_nofactor[30, ]

lrumiNAEDES$model <- "Longit Model 2 rumi partial R2 ED"
ldistNAEDES$model <- "Longit Model 2 dist partial R2 ED"
lreappNAEDES$model <- "Longit Model 2 reapp partial R2 ED"
lsuppNAEDES$model <- "Longit Model 2 supp partial R2 ED"
lsshNAEDES$model <- "Longit Model 2 ssh partial R2 ED"

#partial effect size for model 2 ER (comparing model with no ER and no interaction to models with no interaction)
lrumiNAERES <- longit_effectsizes_nofactor[21, ] - longit_effectsizes_nofactor[31, ]
ldistNAERES <- longit_effectsizes_nofactor[22, ] - longit_effectsizes_nofactor[31, ]
lreappNAERES <- longit_effectsizes_nofactor[23, ] - longit_effectsizes_nofactor[31, ]
lsuppNAERES <- longit_effectsizes_nofactor[24, ] - longit_effectsizes_nofactor[31, ]
lsshNAERES <- longit_effectsizes_nofactor[25, ] - longit_effectsizes_nofactor[31, ]

lrumiNAERES$model <- "Longit Model 2 rumi partial R2 ER"
ldistNAERES$model <- "Longit Model 2 dist partial R2 ER"
lreappNAERES$model <- "Longit Model 2 reapp partial R2 ER"
lsuppNAERES$model <- "Longit Model 2 supp partial R2 ER"
lsshNAERES$model <- "Longit Model 2 ssh partial R2 ER"

#partial effect size for model 2 lagged NA
lrumiNANAES <- longit_effectsizes_nofactor[16, ] - longit_effectsizes_nofactor[32, ]
ldistNANAES <- longit_effectsizes_nofactor[17, ] - longit_effectsizes_nofactor[33, ]
lreappNANAES <- longit_effectsizes_nofactor[18, ] - longit_effectsizes_nofactor[34, ]
lsuppNANAES <- longit_effectsizes_nofactor[19, ] - longit_effectsizes_nofactor[35, ]
lsshNANAES <- longit_effectsizes_nofactor[20, ] - longit_effectsizes_nofactor[36, ]

lrumiNANAES$model <- "Longit Model 2 rumi partial R2 NA lag"
ldistNANAES$model <- "Longit Model 2 dist partial R2 NA lag"
lreappNANAES$model <- "Longit Model 2 reapp partial R2 NA lag"
lsuppNANAES$model <- "Longit Model 2 supp partial R2 NA lag"
lsshNANAES$model <- "Longit Model 2 ssh partial R2 NA lag"

#new version of dataset with these partials included
longit_effectsizes <- rbind(lrumiES,ldistES,lreappES,lsuppES,lsshES,
                            lrumi_NED_ES, ldist_NED_ES, lreapp_NED_ES, lsupp_NED_ES, lssh_NED_ES,
                            lrumi_NNE_ES, ldist_NNE_ES, lreapp_NNE_ES, lsupp_NNE_ES, lssh_NNE_ES,
                            lrumiNAES, ldistNAES, lreappNAES, lsuppNAES, lsshNAES, 
                            lrumiNA_NintES, ldistNA_NintES,lreappNA_NintES, lsuppNA_NintES, lsshNA_NintES,
                            lrumiNA_NEDES, ldistNA_NEDES, lreappNA_NEDES, lsuppNA_NEDES, lsshNA_NEDES,
                            lNA_NER_ES,
                            lrumiNA_NNAES, ldistNA_NNAES, lreappNA_NNAES, lsuppNA_NNAES, lsshNA_NNAES,
                            lrumiEDES, ldistEDES, lreappEDES, lsuppEDES, lsshEDES,
                            lrumiNEES, ldistNEES, lreappNEES, lsuppNEES, lsshNEES,
                            lrumiNAintES, ldistNAintES, lreappNAintES, lsuppNAintES, lsshNAintES,
                            lrumiNAEDES, ldistNAEDES, lreappNAEDES, lsuppNAEDES, lsshNAEDES,
                            lrumiNAERES, ldistNAERES, lreappNAERES, lsuppNAERES, lsshNAERES,
                            lrumiNANAES, ldistNANAES, lreappNANAES, lsuppNANAES, lsshNANAES)

#####6av) Exporting new effect sizes dataset to csv#####
write.csv(longit_effectsizes, file = "Effect sizes longitudinal study.csv")

#####6b. EFFECT SIZES for Study 2#####
######6bi) Effect sizes for Model 1 Study 2#####
#Code for overall model if not already run above#
exrumi <- lmer(SER_rumination ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exdist <- lmer(SER_distraction ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exreapp <- lmer(SER_reapp ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exsupp <- lmer(SER_supp ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exssh <- lmer(SER_soc_sharing ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exacc <- lmer(SER_acceptance ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)

#pulling out effect sizes for this overall model#
exrumiES <- multilevel.effect.size(exrumi)
exdistES <- multilevel.effect.size(exdist)
exreappES <- multilevel.effect.size(exreapp)
exsuppES <- multilevel.effect.size(exsupp)
exsshES <- multilevel.effect.size(exssh)
exaccES <- multilevel.effect.size(exacc)

#model without emotion differentiation so I can look at the difference in variance explained without this variable#
exrumi_NED <- lmer(SER_rumination ~ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exdist_NED <- lmer(SER_distraction ~ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exreapp_NED <- lmer(SER_reapp ~ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exsupp_NED <- lmer(SER_supp ~ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exssh_NED <- lmer(SER_soc_sharing ~ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exacc_NED <- lmer(SER_acceptance ~ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)

exrumi_NED_ES <- multilevel.effect.size(exrumi_NED)
exdist_NED_ES <- multilevel.effect.size(exdist_NED)
exreapp_NED_ES <- multilevel.effect.size(exreapp_NED)
exsupp_NED_ES <- multilevel.effect.size(exsupp_NED)
exssh_NED_ES <- multilevel.effect.size(exssh_NED)
exacc_NED_ES <- multilevel.effect.size(exacc_NED)

#model without percentage passed so I can look at the difference in variance explained without this variable
exrumi_NPP <- lmer(SER_rumination ~ ScRICCneg_after.fz+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exdist_NPP <- lmer(SER_distraction ~ ScRICCneg_after.fz+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exreapp_NPP <- lmer(SER_reapp ~ ScRICCneg_after.fz+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exsupp_NPP <- lmer(SER_supp ~ ScRICCneg_after.fz+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exssh_NPP <- lmer(SER_soc_sharing ~ ScRICCneg_after.fz+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exacc_NPP <- lmer(SER_acceptance ~ ScRICCneg_after.fz+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)

exrumi_NPP_ES <- multilevel.effect.size(exrumi_NPP)
exdist_NPP_ES <- multilevel.effect.size(exdist_NPP)
exreapp_NPP_ES <- multilevel.effect.size(exreapp_NPP)
exsupp_NPP_ES <- multilevel.effect.size(exsupp_NPP)
exssh_NPP_ES <- multilevel.effect.size(exssh_NPP)
exacc_NPP_ES <- multilevel.effect.size(exacc_NPP)

#model without negative emotion mean so I can look at the difference in variance explained without this variable
exrumi_NNE <- lmer(SER_rumination ~ ScRICCneg_after.fz+ Scperc_pass+ (1 | Participant), data=examdata_postexam)
exdist_NNE <- lmer(SER_distraction ~ ScRICCneg_after.fz+ Scperc_pass  + (1 | Participant), data=examdata_postexam)
exreapp_NNE <- lmer(SER_reapp ~ ScRICCneg_after.fz+ Scperc_pass  + (1 | Participant), data=examdata_postexam)
exsupp_NNE <- lmer(SER_supp ~ ScRICCneg_after.fz+ Scperc_pass  + (1 | Participant), data=examdata_postexam)
exssh_NNE <- lmer(SER_soc_sharing ~ ScRICCneg_after.fz+ Scperc_pass  + (1 | Participant), data=examdata_postexam)
exacc_NNE <- lmer(SER_acceptance ~ ScRICCneg_after.fz+ Scperc_pass  + (1 | Participant), data=examdata_postexam)

exrumi_NNE_ES <- multilevel.effect.size(exrumi_NNE)
exdist_NNE_ES <- multilevel.effect.size(exdist_NNE)
exreapp_NNE_ES <- multilevel.effect.size(exreapp_NNE)
exsupp_NNE_ES <- multilevel.effect.size(exsupp_NNE)
exssh_NNE_ES <- multilevel.effect.size(exssh_NNE)
exacc_NNE_ES <- multilevel.effect.size(exacc_NNE)


#####6bii) Effect sizes for Model 2 Study 2####
#Code for overall model if not already run above#
exrumiNA <- lmer(Snegaff_composite ~ ScER_rumination*ScRICCneg_after.fz + Scperc_pass
                 + Scnegaff_composite_1 + (ScER_rumination+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exreappNA <- lmer(Snegaff_composite ~ ScER_reapp*ScRICCneg_after.fz + Scperc_pass
                  + Scnegaff_composite_1 + (ScER_reapp+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exsuppNA <- lmer(Snegaff_composite ~ ScER_supp*ScRICCneg_after.fz + Scperc_pass
                 + Scnegaff_composite_1 + (ScER_supp+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exsshNA <- lmer(Snegaff_composite ~ ScER_soc_sharing*ScRICCneg_after.fz + Scperc_pass
                + Scnegaff_composite_1 + (ScER_soc_sharing+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exdistNA <- lmer(Snegaff_composite ~ ScER_distraction*ScRICCneg_after.fz + Scperc_pass
                 + Scnegaff_composite_1 + (ScER_distraction+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exaccNA <- lmer(Snegaff_composite ~ ScER_acceptance*ScRICCneg_after.fz + Scperc_pass
                + Scnegaff_composite_1 + (ScER_acceptance+ Scnegaff_composite_1| Participant), data=examdata_postexam)

exrumiNAES <- multilevel.effect.size(exrumiNA)
exdistNAES <- multilevel.effect.size(exdistNA)
exreappNAES <- multilevel.effect.size(exreappNA)
exsuppNAES <- multilevel.effect.size(exsuppNA)
exsshNAES <- multilevel.effect.size(exsshNA)
exaccNAES <- multilevel.effect.size(exaccNA)

#model without emotion differentiation*ER interaction so I can look at the difference in variance explained
exrumiNA_Nint <- lmer(Snegaff_composite ~ ScER_rumination+ScRICCneg_after.fz + Scperc_pass
                      + Scnegaff_composite_1 + (ScER_rumination+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exreappNA_Nint <- lmer(Snegaff_composite ~ ScER_reapp+ScRICCneg_after.fz + Scperc_pass
                       + Scnegaff_composite_1 + (ScER_reapp+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exsuppNA_Nint <- lmer(Snegaff_composite ~ ScER_supp+ScRICCneg_after.fz + Scperc_pass
                      + Scnegaff_composite_1 + (ScER_supp+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exsshNA_Nint <- lmer(Snegaff_composite ~ ScER_soc_sharing+ScRICCneg_after.fz + Scperc_pass
                     + Scnegaff_composite_1 + (ScER_soc_sharing+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exdistNA_Nint <- lmer(Snegaff_composite ~ ScER_distraction+ScRICCneg_after.fz + Scperc_pass
                      + Scnegaff_composite_1 + (ScER_distraction+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exaccNA_Nint <- lmer(Snegaff_composite ~ ScER_acceptance+ScRICCneg_after.fz + Scperc_pass
                     + Scnegaff_composite_1 + (ScER_acceptance+ Scnegaff_composite_1| Participant), data=examdata_postexam)

exrumiNA_NintES <- multilevel.effect.size(exrumiNA_Nint)
exdistNA_NintES <- multilevel.effect.size(exdistNA_Nint)
exreappNA_NintES <- multilevel.effect.size(exreappNA_Nint)
exsuppNA_NintES <- multilevel.effect.size(exsuppNA_Nint)
exsshNA_NintES <- multilevel.effect.size(exsshNA_Nint)
exaccNA_NintES <- multilevel.effect.size(exaccNA_Nint)

#Model without ER (and without interaction)
exNAnoER <- lmer(Snegaff_composite ~ ScRICCneg_after.fz + Scperc_pass
                 + Scnegaff_composite_1 + (Scnegaff_composite_1| Participant), data=examdata_postexam)

exNAnoERES <- multilevel.effect.size(exNAnoER)

#Model without ED (and without interaction)
exrumiNA_NED <- lmer(Snegaff_composite ~ ScER_rumination + Scperc_pass
                     + Scnegaff_composite_1 + (ScER_rumination+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exreappNA_NED <- lmer(Snegaff_composite ~ ScER_reapp + Scperc_pass
                      + Scnegaff_composite_1 + (ScER_reapp+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exsuppNA_NED <- lmer(Snegaff_composite ~ ScER_supp + Scperc_pass
                     + Scnegaff_composite_1 + (ScER_supp+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exsshNA_NED <- lmer(Snegaff_composite ~ ScER_soc_sharing + Scperc_pass
                    + Scnegaff_composite_1 + (ScER_soc_sharing+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exdistNA_NED <- lmer(Snegaff_composite ~ ScER_distraction + Scperc_pass
                     + Scnegaff_composite_1 + (ScER_distraction+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exaccNA_NED <- lmer(Snegaff_composite ~ ScER_acceptance + Scperc_pass
                    + Scnegaff_composite_1 + (ScER_acceptance+ Scnegaff_composite_1| Participant), data=examdata_postexam)

exrumiNA_NEDES <- multilevel.effect.size(exrumiNA_NED)
exdistNA_NEDES <- multilevel.effect.size(exdistNA_NED)
exreappNA_NEDES <- multilevel.effect.size(exreappNA_NED)
exsuppNA_NEDES <- multilevel.effect.size(exsuppNA_NED)
exsshNA_NEDES <- multilevel.effect.size(exsshNA_NED)
exaccNA_NEDES <- multilevel.effect.size(exaccNA_NED)

#without percentage passed
exrumiNA_NPP <- lmer(Snegaff_composite ~ ScER_rumination*ScRICCneg_after.fz
                     + Scnegaff_composite_1 + (ScER_rumination+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exreappNA_NPP <- lmer(Snegaff_composite ~ ScER_reapp*ScRICCneg_after.fz
                      + Scnegaff_composite_1 + (ScER_reapp+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exsuppNA_NPP <- lmer(Snegaff_composite ~ ScER_supp*ScRICCneg_after.fz
                     + Scnegaff_composite_1 + (ScER_supp+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exsshNA_NPP <- lmer(Snegaff_composite ~ ScER_soc_sharing*ScRICCneg_after.fz
                    + Scnegaff_composite_1 + (ScER_soc_sharing+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exdistNA_NPP <- lmer(Snegaff_composite ~ ScER_distraction*ScRICCneg_after.fz
                     + Scnegaff_composite_1 + (ScER_distraction+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exaccNA_NPP <- lmer(Snegaff_composite ~ ScER_acceptance*ScRICCneg_after.fz
                    + Scnegaff_composite_1 + (ScER_acceptance+ Scnegaff_composite_1| Participant), data=examdata_postexam)

exrumiNA_NPPES <- multilevel.effect.size(exrumiNA_NPP)
exdistNA_NPPES <- multilevel.effect.size(exdistNA_NPP)
exreappNA_NPPES <- multilevel.effect.size(exreappNA_NPP)
exsuppNA_NPPES <- multilevel.effect.size(exsuppNA_NPP)
exsshNA_NPPES <- multilevel.effect.size(exsshNA_NPP)
exaccNA_NPPES <- multilevel.effect.size(exaccNA_NPP)

#without lagged negative emotion

exrumiNA_NNA <- lmer(Snegaff_composite ~ ScER_rumination*ScRICCneg_after.fz + Scperc_pass
                     +  (ScER_rumination | Participant), data=examdata_postexam)
exreappNA_NNA <- lmer(Snegaff_composite ~ ScER_reapp*ScRICCneg_after.fz + Scperc_pass
                      + (ScER_reapp| Participant), data=examdata_postexam)
exsuppNA_NNA <- lmer(Snegaff_composite ~ ScER_supp*ScRICCneg_after.fz + Scperc_pass
                     + (ScER_supp| Participant), data=examdata_postexam)
exsshNA_NNA <- lmer(Snegaff_composite ~ ScER_soc_sharing*ScRICCneg_after.fz + Scperc_pass
                    + (ScER_soc_sharing| Participant), data=examdata_postexam)
exdistNA_NNA <- lmer(Snegaff_composite ~ ScER_distraction*ScRICCneg_after.fz + Scperc_pass
                     + (ScER_distraction| Participant), data=examdata_postexam)
exaccNA_NNA <- lmer(Snegaff_composite ~ ScER_acceptance*ScRICCneg_after.fz + Scperc_pass
                    + (ScER_acceptance| Participant), data=examdata_postexam)

exrumiNA_NNAES <- multilevel.effect.size(exrumiNA_NNA)
exdistNA_NNAES <- multilevel.effect.size(exdistNA_NNA)
exreappNA_NNAES <- multilevel.effect.size(exreappNA_NNA)
exsuppNA_NNAES <- multilevel.effect.size(exsuppNA_NNA)
exsshNA_NNAES <- multilevel.effect.size(exsshNA_NNA)
exaccNA_NNAES <- multilevel.effect.size(exaccNA_NNA)

#####6biii) Making a dataset of effect sizes#####
#first adding an identifying variable to each of the separate models
exrumiES$model <- "Exam Model 1 rumi full"
exdistES$model <- "Exam Model 1 dist full"
exreappES$model <- "Exam Model 1 reapp full"
exsuppES$model <- "Exam Model 1 supp full"
exsshES$model <- "Exam Model 1 ssh full"
exaccES$model <- "Exam Model 1 acc full"

exrumi_NED_ES$model <- "Exam Model 1 rumi no ED"
exdist_NED_ES$model <- "Exam Model 1 dist no ED"
exreapp_NED_ES$model <- "Exam Model 1 reapp no ED"
exsupp_NED_ES$model <- "Exam Model 1 supp no ED"
exssh_NED_ES$model <- "Exam Model 1 ssh no ED"
exacc_NED_ES$model <- "Exam Model 1 acc no ED"

exrumi_NPP_ES$model <- "Exam Model 1 rumi no perc pass"
exdist_NPP_ES$model <- "Exam Model 1 dist no perc pass"
exreapp_NPP_ES$model <- "Exam Model 1 reapp no perc pass"
exsupp_NPP_ES$model <- "Exam Model 1 supp no perc pass"
exssh_NPP_ES$model <- "Exam Model 1 ssh no perc pass"
exacc_NPP_ES$model <- "Exam Model 1 acc no perc pass"

exrumi_NNE_ES$model <- "Exam Model 1 rumi no person neg emo"
exdist_NNE_ES$model <- "Exam Model 1 dist no person neg emo"
exreapp_NNE_ES$model <- "Exam Model 1 reapp no person neg emo"
exsupp_NNE_ES$model <- "Exam Model 1 supp no person neg emo"
exssh_NNE_ES$model <- "Exam Model 1 ssh no person neg emo"
exacc_NNE_ES$model <- "Exam Model 1 acc no person neg emo"

exrumiNAES$model <- "Exam Model 2 rumi full"
exdistNAES$model <- "Exam Model 2 dist full"
exreappNAES$model <- "Exam Model 2 reapp full"
exsuppNAES$model <- "Exam Model 2 supp full"
exsshNAES$model <- "Exam Model 2 ssh full"
exaccNAES$model <- "Exam Model 2 acc full"

exrumiNA_NintES$model <- "Exam Model 2 rumi no ED*strategy"
exdistNA_NintES$model <- "Exam Model 2 dist no ED*strategy"
exreappNA_NintES$model <- "Exam Model 2 reapp no ED*strategy"
exsuppNA_NintES$model <- "Exam Model 2 supp no ED*strategy"
exsshNA_NintES$model <- "Exam Model 2 ssh no ED*strategy"
exaccNA_NintES$model <- "Exam Model 2 acc no ED*strategy"

exNAnoERES$model <- "Exam Model 2 no ER (and no interaction)"

exrumiNA_NEDES$model <- "Exam Model 2 rumi no ED (and no interaction)"
exdistNA_NEDES$model <- "Exam Model 2 dist no ED (and no interaction)"
exreappNA_NEDES$model <- "Exam Model 2 reapp no ED (and no interaction)"
exsuppNA_NEDES$model <- "Exam Model 2 supp no ED (and no interaction)"
exsshNA_NEDES$model <- "Exam Model 2 ssh no ED (and no interaction)"
exaccNA_NEDES$model <- "Exam Model 2 acc no ED (and no interaction)"

exrumiNA_NPPES$model <- "Exam Model 2 rumi no perc passed"
exdistNA_NPPES$model <- "Exam Model 2 dist no perc passed"
exreappNA_NPPES$model <- "Exam Model 2 reapp no perc passed"
exsuppNA_NPPES$model <- "Exam Model 2 supp no perc passed"
exsshNA_NPPES$model <- "Exam Model 2 ssh no perc passed"
exaccNA_NPPES$model <- "Exam Model 2 acc no perc passed"

exrumiNA_NNAES$model <- "Exam Model 2 rumi no lagged NA"
exdistNA_NNAES$model <- "Exam Model 2 dist no lagged NA"
exreappNA_NNAES$model <- "Exam Model 2 reapp no lagged NA"
exsuppNA_NNAES$model <- "Exam Model 2 supp no lagged NA"
exsshNA_NNAES$model <- "Exam Model 2 ssh no lagged NA"
exaccNA_NNAES$model <- "Exam Model 2 acc no lagged NA"

exam_effectsizes <- rbind(exrumiES,exdistES,exreappES,exsuppES,exsshES,exaccES, 
                          exrumi_NED_ES, exdist_NED_ES, exreapp_NED_ES, exsupp_NED_ES, exssh_NED_ES, exacc_NED_ES,
                          exrumi_NPP_ES, exdist_NPP_ES, exreapp_NPP_ES, exsupp_NPP_ES, exssh_NPP_ES, exacc_NPP_ES,
                          exrumi_NNE_ES, exdist_NNE_ES, exreapp_NNE_ES, exsupp_NNE_ES, exssh_NNE_ES, exacc_NNE_ES,
                          exrumiNAES, exdistNAES, exreappNAES, exsuppNAES, exsshNAES, exaccNAES, 
                          exrumiNA_NintES, exdistNA_NintES, exreappNA_NintES, exsuppNA_NintES, exsshNA_NintES, exaccNA_NintES,
                          exNAnoERES,
                          exrumiNA_NEDES, exdistNA_NEDES, exreappNA_NEDES, exsuppNA_NEDES, exsshNA_NEDES, exaccNA_NEDES,
                          exrumiNA_NPPES, exdistNA_NPPES, exreappNA_NPPES, exsuppNA_NPPES, exsshNA_NPPES, exaccNA_NPPES,
                          exrumiNA_NNAES, exdistNA_NNAES, exreappNA_NNAES, exsuppNA_NNAES, exsshNA_NNAES, exaccNA_NNAES)

#####6biv) Computing partial effect sizes for each variable#####
#adding the partial effect sizes to the dataframe by subtracting reduced model from full model for each ER strategy

# first making a version of the effect sizes document with no factor because otherwise can't manipulate it easily
exam_effectsizes_nofactor <- exam_effectsizes
exam_effectsizes_nofactor$model <- NULL

#now taking the rows from each other - can get row information from opening the "longit_effectsizes" datafile and looking at row number there
#partial effect size for model 1 emotion differentiation
exrumiEDES <- exam_effectsizes_nofactor[1, ] - exam_effectsizes_nofactor[7, ]
exdistEDES <- exam_effectsizes_nofactor[2, ] - exam_effectsizes_nofactor[8, ]
exreappEDES <- exam_effectsizes_nofactor[3, ] - exam_effectsizes_nofactor[9, ]
exsuppEDES <- exam_effectsizes_nofactor[4, ] - exam_effectsizes_nofactor[10, ]
exsshEDES <- exam_effectsizes_nofactor[5, ] - exam_effectsizes_nofactor[11, ]
exaccEDES <- exam_effectsizes_nofactor[6, ] - exam_effectsizes_nofactor[12, ]

exrumiEDES$model <- "Exam Model 1 rumi partial R2 ED"
exdistEDES$model <- "Exam Model 1 dist partial R2 ED"
exreappEDES$model <- "Exam Model 1 reapp partial R2 ED"
exsuppEDES$model <- "Exam Model 1 supp partial R2 ED"
exsshEDES$model <- "Exam Model 1 ssh partial R2 ED"
exaccEDES$model <- "Exam Model 1 acc partial R2 ED"

#partial effect size for model 1 percentage passed
exrumiPPES <- exam_effectsizes_nofactor[1, ] - exam_effectsizes_nofactor[13, ]
exdistPPES <- exam_effectsizes_nofactor[2, ] - exam_effectsizes_nofactor[14, ]
exreappPPES <- exam_effectsizes_nofactor[3, ] - exam_effectsizes_nofactor[15, ]
exsuppPPES <- exam_effectsizes_nofactor[4, ] - exam_effectsizes_nofactor[16, ]
exsshPPES <- exam_effectsizes_nofactor[5, ] - exam_effectsizes_nofactor[17, ]
exaccPPES <- exam_effectsizes_nofactor[6, ] - exam_effectsizes_nofactor[18, ]

exrumiPPES$model <- "Exam Model 1 rumi partial R2 perc pass"
exdistPPES$model <- "Exam Model 1 dist partial R2 perc pass"
exreappPPES$model <- "Exam Model 1 reapp partial R2 perc pass"
exsuppPPES$model <- "Exam Model 1 supp partial R2 perc pass"
exsshPPES$model <- "Exam Model 1 ssh partial R2 perc pass"
exaccPPES$model <- "Exam Model 1 acc partial R2 perc pass"

#partial effect size for model 1 person negative emotion 

exrumiNEES <- exam_effectsizes_nofactor[1, ] - exam_effectsizes_nofactor[19, ]
exdistNEES <- exam_effectsizes_nofactor[2, ] - exam_effectsizes_nofactor[20, ]
exreappNEES <- exam_effectsizes_nofactor[3, ] - exam_effectsizes_nofactor[21, ]
exsuppNEES <- exam_effectsizes_nofactor[4, ] - exam_effectsizes_nofactor[22, ]
exsshNEES <- exam_effectsizes_nofactor[5, ] - exam_effectsizes_nofactor[23, ]
exaccNEES <- exam_effectsizes_nofactor[6, ] - exam_effectsizes_nofactor[24, ]

exrumiNEES$model <- "Exam Model 1 rumi partial R2 person neg emo"
exdistNEES$model <- "Exam Model 1 dist partial R2 person neg emo"
exreappNEES$model <- "Exam Model 1 reapp partial R2 person neg emo"
exsuppNEES$model <- "Exam Model 1 supp partial R2 person neg emo"
exsshNEES$model <- "Exam Model 1 ssh partial R2 person neg emo"
exaccNEES$model <- "Exam Model 1 acc partial R2 person neg emo"

#partial effect size for model 2 ED*ER interaction
exrumiNAintES <- exam_effectsizes_nofactor[25, ] - exam_effectsizes_nofactor[31, ]
exdistNAintES <- exam_effectsizes_nofactor[26, ] - exam_effectsizes_nofactor[32, ]
exreappNAintES <- exam_effectsizes_nofactor[27, ] - exam_effectsizes_nofactor[33, ]
exsuppNAintES <- exam_effectsizes_nofactor[28, ] - exam_effectsizes_nofactor[34, ]
exsshNAintES <- exam_effectsizes_nofactor[29, ] - exam_effectsizes_nofactor[35, ]
exaccNAintES <- exam_effectsizes_nofactor[30, ] - exam_effectsizes_nofactor[36, ]

exrumiNAintES$model <- "Exam Model 2 rumi partial R2 ED*strategy"
exdistNAintES$model <- "Exam Model 2 dist partial R2 ED*strategy"
exreappNAintES$model <- "Exam Model 2 reapp partial R2 ED*strategy"
exsuppNAintES$model <- "Exam Model 2 supp partial R2 ED*strategy"
exsshNAintES$model <- "Exam Model 2 ssh partial R2 ED*strategy"
exaccNAintES$model <- "Exam Model 2 acc partial R2 ED*strategy"

#partial effect size for model 2 ER (comparing model with no ER and no interaction to models with no interaction)
exrumiNAERES <- exam_effectsizes_nofactor[31, ] - exam_effectsizes_nofactor[37, ]
exdistNAERES <- exam_effectsizes_nofactor[32, ] - exam_effectsizes_nofactor[37, ]
exreappNAERES <- exam_effectsizes_nofactor[33, ] - exam_effectsizes_nofactor[37, ]
exsuppNAERES <- exam_effectsizes_nofactor[34, ] - exam_effectsizes_nofactor[37, ]
exsshNAERES <- exam_effectsizes_nofactor[35, ] - exam_effectsizes_nofactor[37, ]
exaccNAERES <- exam_effectsizes_nofactor[36, ] - exam_effectsizes_nofactor[37, ]

exrumiNAERES$model <- "Exam Model 2 rumi partial R2 ER"
exdistNAERES$model <- "Exam Model 2 dist partial R2 ER"
exreappNAERES$model <- "Exam Model 2 reapp partial R2 ER"
exsuppNAERES$model <- "Exam Model 2 supp partial R2 ER"
exsshNAERES$model <- "Exam Model 2 ssh partial R2 ER"
exaccNAERES$model <- "Exam Model 2 acc partial R2 ER"

#partial effect size for model 2 ED (comparing model with no ED and no interaction to models with no interaction)
exrumiNAEDES <- exam_effectsizes_nofactor[31, ] - exam_effectsizes_nofactor[38, ]
exdistNAEDES <- exam_effectsizes_nofactor[32, ] - exam_effectsizes_nofactor[39, ]
exreappNAEDES <- exam_effectsizes_nofactor[33, ] - exam_effectsizes_nofactor[40, ]
exsuppNAEDES <- exam_effectsizes_nofactor[34, ] - exam_effectsizes_nofactor[41, ]
exsshNAEDES <- exam_effectsizes_nofactor[35, ] - exam_effectsizes_nofactor[42, ]
exaccNAEDES <- exam_effectsizes_nofactor[36, ] - exam_effectsizes_nofactor[43, ]

exrumiNAEDES$model <- "Exam Model 2 rumi partial R2 ED"
exdistNAEDES$model <- "Exam Model 2 dist partial R2 ED"
exreappNAEDES$model <- "Exam Model 2 reapp partial R2 ED"
exsuppNAEDES$model <- "Exam Model 2 supp partial R2 ED"
exsshNAEDES$model <- "Exam Model 2 ssh partial R2 ED"
exaccNAEDES$model <- "Exam Model 2 acc partial R2 ED"

#partial effect size for model 2 percentage passed
exrumiNAPPES <- exam_effectsizes_nofactor[25, ] - exam_effectsizes_nofactor[44, ]
exdistNAPPES <- exam_effectsizes_nofactor[26, ] - exam_effectsizes_nofactor[45, ]
exreappNAPPES <- exam_effectsizes_nofactor[27, ] - exam_effectsizes_nofactor[46, ]
exsuppNAPPES <- exam_effectsizes_nofactor[28, ] - exam_effectsizes_nofactor[47, ]
exsshNAPPES <- exam_effectsizes_nofactor[29, ] - exam_effectsizes_nofactor[48, ]
exaccNAPPES <- exam_effectsizes_nofactor[30, ] - exam_effectsizes_nofactor[49, ]

exrumiNAPPES$model <- "Exam Model 2 rumi partial R2 perc pass"
exdistNAPPES$model <- "Exam Model 2 dist partial R2 perc pass"
exreappNAPPES$model <- "Exam Model 2 reapp partial R2 perc pass"
exsuppNAPPES$model <- "Exam Model 2 supp partial R2 perc pass"
exsshNAPPES$model <- "Exam Model 2 ssh partial R2 perc pass"
exaccNAPPES$model <- "Exam Model 2 acc partial R2 perc pass"

#partial effect size for model 2 lagged negative emotion
exrumiNANEES <- exam_effectsizes_nofactor[25, ] - exam_effectsizes_nofactor[50, ]
exdistNANEES <- exam_effectsizes_nofactor[26, ] - exam_effectsizes_nofactor[51, ]
exreappNANEES <- exam_effectsizes_nofactor[27, ] - exam_effectsizes_nofactor[52, ]
exsuppNANEES <- exam_effectsizes_nofactor[28, ] - exam_effectsizes_nofactor[53, ]
exsshNANEES <- exam_effectsizes_nofactor[29, ] - exam_effectsizes_nofactor[54, ]
exaccNANEES <- exam_effectsizes_nofactor[30, ] - exam_effectsizes_nofactor[55, ]

exrumiNANEES$model <- "Exam Model 2 rumi partial R2 lagged neg emo"
exdistNANEES$model <- "Exam Model 2 dist partial R2 lagged neg emo"
exreappNANEES$model <- "Exam Model 2 reapp partial R2 lagged neg emo"
exsuppNANEES$model <- "Exam Model 2 supp partial R2 lagged neg emo"
exsshNANEES$model <- "Exam Model 2 ssh partial R2 lagged neg emo"
exaccNANEES$model <- "Exam Model 2 acc partial R2 lagged neg emo"


#new version of dataset with these partials included
exam_effectsizes <- rbind(exrumiES,exdistES,exreappES,exsuppES,exsshES,exaccES, 
                          exrumi_NED_ES, exdist_NED_ES, exreapp_NED_ES, exsupp_NED_ES, exssh_NED_ES, exacc_NED_ES,
                          exrumi_NPP_ES, exdist_NPP_ES, exreapp_NPP_ES, exsupp_NPP_ES, exssh_NPP_ES, exacc_NPP_ES,
                          exrumi_NNE_ES, exdist_NNE_ES, exreapp_NNE_ES, exsupp_NNE_ES, exssh_NNE_ES, exacc_NNE_ES,
                          exrumiEDES, exdistEDES, exreappEDES, exsuppEDES, exsshEDES, exaccEDES,
                          exrumiPPES, exdistPPES, exreappPPES, exsuppPPES, exsshPPES, exaccPPES,
                          exrumiNEES, exdistNEES, exreappNEES, exsuppNEES, exsshNEES, exaccNEES,
                          exrumiNAES, exdistNAES, exreappNAES, exsuppNAES, exsshNAES, exaccNAES, 
                          exrumiNA_NintES, exdistNA_NintES, exreappNA_NintES, exsuppNA_NintES, exsshNA_NintES, exaccNA_NintES,
                          exNAnoERES,
                          exrumiNA_NEDES, exdistNA_NEDES, exreappNA_NEDES, exsuppNA_NEDES, exsshNA_NEDES, exaccNA_NEDES,
                          exrumiNA_NPPES, exdistNA_NPPES, exreappNA_NPPES, exsuppNA_NPPES, exsshNA_NPPES, exaccNA_NPPES,
                          exrumiNA_NNAES, exdistNA_NNAES, exreappNA_NNAES, exsuppNA_NNAES, exsshNA_NNAES, exaccNA_NNAES,
                          exrumiNAintES, exdistNAintES, exreappNAintES, exsuppNAintES, exsshNAintES, exaccNAintES,
                          exrumiNAERES, exdistNAERES, exreappNAERES, exsuppNAERES, exsshNAERES, exaccNAERES,
                          exrumiNAEDES, exdistNAEDES, exreappNAEDES, exsuppNAEDES, exsshNAEDES, exaccNAEDES,
                          exrumiNAPPES, exdistNAPPES, exreappNAPPES, exsuppNAPPES, exsshNAPPES, exaccNAPPES, 
                          exrumiNANEES, exdistNANEES, exreappNANEES, exsuppNANEES, exsshNANEES, exaccNANEES)

#####6bv) Exporting new effect sizes dataset to csv#####
write.csv(exam_effectsizes, file = "Effect sizes exam study.csv")


########OTHER SUPPLEMENTAL ANALYSES########

#####7. LOOKING AT ROLE OF GRADE EXPECTATIONS IN STUDY 2#####
#variable cleaning
#making expected pass percentage passed an actual percentage
examdata_postexam$perc_expass <- examdata_postexam$perc_expass*100

#taking away expected from passed (so higher is better than expected)
examdata_postexam$perc_exbetter <- examdata_postexam$perc_pass - examdata_postexam$perc_expass

#making grand-mean centered variables 
examdata_postexam$cperc_expass <- scale(examdata_postexam$perc_expass, center = TRUE, scale = FALSE)
examdata_postexam$cperc_exbetter <- scale(examdata_postexam$perc_exbetter, center = TRUE, scale = FALSE)

#####7a. Controlling for expected pass percentage####
#Model 1: predicting ER strategies
exrumiEP <- lmer(scale(ER_rumination) ~ scale(cRICCneg_after.fz)+ scale(cperc_expass)+ scale(cperc_pass)+ scale(cnegaff_composite_meanpostexam)  + (1 | Participant), data=examdata_postexam)
exdistEP <- lmer(scale(ER_distraction) ~ scale(cRICCneg_after.fz)+ scale(cperc_expass)+ scale(cperc_pass)+scale(cnegaff_composite_meanpostexam)  + (1 | Participant), data=examdata_postexam)
exreappEP <- lmer(scale(ER_reapp) ~ scale(cRICCneg_after.fz)+ scale(cperc_expass)+ scale(cperc_pass)+scale(cnegaff_composite_meanpostexam)  + (1 | Participant), data=examdata_postexam)
exsuppEP <- lmer(scale(ER_supp) ~ scale(cRICCneg_after.fz)+ scale(cperc_expass)+ scale(cperc_pass)+scale(cnegaff_composite_meanpostexam)  + (1 | Participant), data=examdata_postexam)
exsshEP <- lmer(scale(ER_soc_sharing) ~ scale(cRICCneg_after.fz)+ scale(cperc_expass)+ scale(cperc_pass)+scale(cnegaff_composite_meanpostexam)  + (1 | Participant), data=examdata_postexam)
exaccEP <- lmer(scale(ER_acceptance) ~ scale(cRICCneg_after.fz)+ scale(cperc_expass)+ scale(cperc_pass)+scale(cnegaff_composite_meanpostexam)  + (1 | Participant), data=examdata_postexam)

summary(exrumiEP)
summary(exdistEP)
summary(exreappEP)
summary(exsuppEP)
summary(exsshEP)
summary(exaccEP)

confint(exrumiEP, method="Wald")
confint(exdistEP, method="Wald")
confint(exreappEP, method="Wald")
confint(exsuppEP, method="Wald")
confint(exsshEP, method="Wald")
confint(exaccEP, method="Wald")

#Model 2: predicting NA from interaction between strategies and ED
exrumiNAEP <- lmer(scale(negaff_composite) ~ scale(cER_rumination)*scale(cRICCneg_after.fz) + scale(cperc_expass)+ scale(cperc_pass)
                   + scale(cnegaff_composite_1) + (scale(cER_rumination)+ scale(cnegaff_composite_1)| Participant), data=examdata_postexam)
exreappNAEP <- lmer(scale(negaff_composite) ~ scale(cER_reapp)*scale(cRICCneg_after.fz) + scale(cperc_expass)+ scale(cperc_pass)
                    + scale(cnegaff_composite_1) + (scale(cER_reapp)+ scale(cnegaff_composite_1)| Participant), data=examdata_postexam)
exsuppNAEP <- lmer(scale(negaff_composite) ~ scale(cER_supp)*scale(cRICCneg_after.fz) + scale(cperc_expass)+ scale(cperc_pass)
                   + scale(cnegaff_composite_1) + (scale(cER_supp)+ scale(cnegaff_composite_1)| Participant), data=examdata_postexam)
exsshNAEP <- lmer(scale(negaff_composite) ~ scale(cER_soc_sharing)*scale(cRICCneg_after.fz) + scale(cperc_expass)+ scale(cperc_pass)
                  + scale(cnegaff_composite_1) + (scale(cER_soc_sharing)+ scale(cnegaff_composite_1)| Participant), data=examdata_postexam)
exdistNAEP <- lmer(scale(negaff_composite) ~ scale(cER_distraction)*scale(cRICCneg_after.fz) + scale(cperc_expass)+ scale(cperc_pass)
                   + scale(cnegaff_composite_1) + (scale(cER_distraction)+ scale(cnegaff_composite_1)| Participant), data=examdata_postexam)
exaccNAEP <- lmer(scale(negaff_composite) ~ scale(cER_acceptance)*scale(cRICCneg_after.fz) + scale(cperc_expass)+ scale(cperc_pass)
                  + scale(cnegaff_composite_1) + (scale(cER_acceptance)+ scale(cnegaff_composite_1)| Participant), data=examdata_postexam)

summary(exrumiNAEP)
summary(exdistNAEP)
summary(exreappNAEP)
summary(exsuppNAEP)
summary(exsshNAEP)
summary(exaccNAEP)

confint(exrumiNAEP, method="Wald")
confint(exdistNAEP, method="Wald")
confint(exreappNAEP, method="Wald")
confint(exsuppNAEP, method="Wald")
confint(exsshNAEP, method="Wald")
confint(exaccNAEP, method="Wald")

######7b. Difference between actual and expected pass percentage####
#Model 1: predicting ER strategies
exrumiEB <- lmer(scale(ER_rumination) ~ scale(cRICCneg_after.fz)+ scale(cperc_exbetter)+ scale(cnegaff_composite_meanpostexam)  + (1 | Participant), data=examdata_postexam)
exdistEB <- lmer(scale(ER_distraction) ~ scale(cRICCneg_after.fz)+ scale(cperc_exbetter)+scale(cnegaff_composite_meanpostexam)  + (1 | Participant), data=examdata_postexam)
exreappEB <- lmer(scale(ER_reapp) ~ scale(cRICCneg_after.fz)+ scale(cperc_exbetter)+scale(cnegaff_composite_meanpostexam)  + (1 | Participant), data=examdata_postexam)
exsuppEB <- lmer(scale(ER_supp) ~ scale(cRICCneg_after.fz)+ scale(cperc_exbetter)+scale(cnegaff_composite_meanpostexam)  + (1 | Participant), data=examdata_postexam)
exsshEB <- lmer(scale(ER_soc_sharing) ~ scale(cRICCneg_after.fz)+ scale(cperc_exbetter)+scale(cnegaff_composite_meanpostexam)  + (1 | Participant), data=examdata_postexam)
exaccEB <- lmer(scale(ER_acceptance) ~ scale(cRICCneg_after.fz)+ scale(cperc_exbetter)+scale(cnegaff_composite_meanpostexam)  + (1 | Participant), data=examdata_postexam)

summary(exrumiEB)
summary(exdistEB)
summary(exreappEB)
summary(exsuppEB)
summary(exsshEB)
summary(exaccEB)

confint(exrumiEB, method="Wald")
confint(exdistEB, method="Wald")
confint(exreappEB, method="Wald")
confint(exsuppEB, method="Wald")
confint(exsshEB, method="Wald")
confint(exaccEB, method="Wald")

#Model 2: predicting NA from interaction between strategies and ED
exrumiNAEB <- lmer(scale(negaff_composite) ~ scale(cER_rumination)*scale(cRICCneg_after.fz) + scale(cperc_exbetter)
                   + scale(cnegaff_composite_1) + (scale(cER_rumination)+ scale(cnegaff_composite_1)| Participant), data=examdata_postexam)
exreappNAEB <- lmer(scale(negaff_composite) ~ scale(cER_reapp)*scale(cRICCneg_after.fz) + scale(cperc_exbetter)
                    + scale(cnegaff_composite_1) + (scale(cER_reapp)+ scale(cnegaff_composite_1)| Participant), data=examdata_postexam)
exsuppNAEB <- lmer(scale(negaff_composite) ~ scale(cER_supp)*scale(cRICCneg_after.fz) + scale(cperc_exbetter)
                   + scale(cnegaff_composite_1) + (scale(cER_supp)+ scale(cnegaff_composite_1)| Participant), data=examdata_postexam)
exsshNAEB <- lmer(scale(negaff_composite) ~ scale(cER_soc_sharing)*scale(cRICCneg_after.fz) + scale(cperc_exbetter)
                  + scale(cnegaff_composite_1) + (scale(cER_soc_sharing)+ scale(cnegaff_composite_1)| Participant), data=examdata_postexam)
exdistNAEB <- lmer(scale(negaff_composite) ~ scale(cER_distraction)*scale(cRICCneg_after.fz) + scale(cperc_exbetter)
                   + scale(cnegaff_composite_1) + (scale(cER_distraction)+ scale(cnegaff_composite_1)| Participant), data=examdata_postexam)
exaccNAEB <- lmer(scale(negaff_composite) ~ scale(cER_acceptance)*scale(cRICCneg_after.fz) + scale(cperc_exbetter)
                  + scale(cnegaff_composite_1) + (scale(cER_acceptance)+ scale(cnegaff_composite_1)| Participant), data=examdata_postexam)

summary(exrumiNAEB)
summary(exdistNAEB)
summary(exreappNAEB)
summary(exsuppNAEB)
summary(exsshNAEB)
summary(exaccNAEB)

confint(exrumiNAEB, method="Wald")
confint(exdistNAEB, method="Wald")
confint(exreappNAEB, method="Wald")
confint(exsuppNAEB, method="Wald")
confint(exsshNAEB, method="Wald")
confint(exaccNAEB, method="Wald")

######8. RUNNING MODELS IN THE REVERSE DIRECTIONAL ORDER####
#using emotion at T-1 to predict emotion regulation at T, controlling for emotion regulation at T-1#
#####8a. STUDY 1 models####
#first group mean centering new ER lags
ldata$crumi_1 <- ldata$rumi_1  - ave(ldata$rumi_1 , ldata$Wave, ldata$PpID, FUN=function(x) mean(x, na.rm=TRUE))
ldata$cdist_1  <- ldata$dist_1  - ave(ldata$dist_1 , ldata$Wave, ldata$PpID, FUN=function(x) mean(x, na.rm=TRUE))
ldata$creapp_1  <- ldata$reapp_1  - ave(ldata$reapp_1 , ldata$Wave, ldata$PpID, FUN=function(x) mean(x, na.rm=TRUE))
ldata$csupp_1  <- ldata$supp_1  - ave(ldata$supp_1 , ldata$Wave, ldata$PpID, FUN=function(x) mean(x, na.rm=TRUE))
ldata$cssh_1  <- ldata$ssh_1  - ave(ldata$ssh_1 , ldata$Wave, ldata$PpID, FUN=function(x) mean(x, na.rm=TRUE))

#scaling new lags
ldata$Scrumi_1 <- scale(ldata$crumi_1)
ldata$Scdist_1 <- scale(ldata$cdist_1)
ldata$Screapp_1 <- scale(ldata$creapp_1)
ldata$Scsupp_1 <- scale(ldata$csupp_1)
ldata$Scssh_1 <- scale(ldata$cssh_1)

#modelling reverse order

#getting convergence errors so removing the wave random slope of negative emotion (explains almost no variance)
#converges after removing that slope and effects are the same 
lrumiNArev <- lmer(Srumi ~ Scnegaff_1*ScRICCneg.fzW  
                   + Scrumi_1 + (1|PpID) + (1|Wave)  + (0 + Scnegaff_1 + Scrumi_1 | PpID) 
                   + (0 + Scrumi_1 | Wave) +  (0 + Scnegaff_1 + Scrumi_1 || PpID:Wave), data=ldata)
ldistNArev <- lmer(Sdist ~ Scnegaff_1*ScRICCneg.fzW  
                   + Scdist_1 + (1|PpID) + (1|Wave)  + (0 + Scnegaff_1 + Scdist_1 | PpID) 
                   + (0 + Scnegaff_1 + Scdist_1 | Wave) +  (0 + Scnegaff_1 + Scdist_1 || PpID:Wave), data=ldata)
lreappNArev <- lmer(Sreapp ~ Scnegaff_1*ScRICCneg.fzW  
                    + Screapp_1 + (1|PpID) + (1|Wave)  + (0 + Scnegaff_1 + Screapp_1 | PpID) 
                    + (0 + Scnegaff_1 + Screapp_1 | Wave) +  (0 + Scnegaff_1 + Screapp_1 || PpID:Wave), data=ldata)
lsuppNArev <- lmer(Ssupp ~ Scnegaff_1*ScRICCneg.fzW  
                   + Scsupp_1 + (1|PpID) + (1|Wave)  + (0 + Scnegaff_1 + Scsupp_1 | PpID) 
                   + (0 + Scsupp_1 | Wave) +  (0 + Scnegaff_1 + Scsupp_1 || PpID:Wave), data=ldata)
lsshNArev <- lmer(Sssh ~ Scnegaff_1*ScRICCneg.fzW  
                  + Scssh_1 + (1|PpID) + (1|Wave)  + (0 + Scnegaff_1 + Scssh_1 | PpID) 
                  + (0 + Scnegaff_1 + Scssh_1 | Wave) +  (0 + Scnegaff_1 + Scssh_1 || PpID:Wave), data=ldata)

summary(lrumiNArev)
summary(ldistNArev)
summary(lreappNArev)
summary(lsuppNArev)
summary(lsshNArev)

confint(lrumiNArev, method="Wald")
confint(ldistNArev, method="Wald")
confint(lreappNArev, method="Wald")
confint(lsuppNArev, method="Wald")
confint(lsshNArev, method="Wald")

######8b. STUDY 2 models#####
#group mean centering new ER lags
#centering group mean - beep level
examdata_postexam$cER_rumination_1 <- examdata_postexam$ER_rumination_1 - ave(examdata_postexam$ER_rumination_1, examdata_postexam$Participant, FUN=function(x) mean(x, na.rm=TRUE))
examdata_postexam$cER_distraction_1 <- examdata_postexam$ER_distraction_1 - ave(examdata_postexam$ER_distraction_1, examdata_postexam$Participant, FUN=function(x) mean(x, na.rm=TRUE))
examdata_postexam$cER_acceptance_1 <- examdata_postexam$ER_acceptance_1 - ave(examdata_postexam$ER_acceptance_1, examdata_postexam$Participant, FUN=function(x) mean(x, na.rm=TRUE))
examdata_postexam$cER_reapp_1 <- examdata_postexam$ER_reapp_1 - ave(examdata_postexam$ER_reapp_1, examdata_postexam$Participant, FUN=function(x) mean(x, na.rm=TRUE))
examdata_postexam$cER_supp_1 <- examdata_postexam$ER_supp_1 - ave(examdata_postexam$ER_supp_1, examdata_postexam$Participant, FUN=function(x) mean(x, na.rm=TRUE))
examdata_postexam$cER_soc_sharing_1 <- examdata_postexam$ER_soc_sharing_1 - ave(examdata_postexam$ER_soc_sharing_1, examdata_postexam$Participant, FUN=function(x) mean(x, na.rm=TRUE))

#modeling reverse order#
exrumiNArev <- lmer(scale(ER_rumination) ~ scale(cnegaff_composite_1)*scale(cRICCneg_after.fz) + scale(cperc_pass)
                    + scale(cER_rumination_1) + (scale(cnegaff_composite_1)+ scale(cER_rumination_1)| Participant), data=examdata_postexam)
exdistNArev <- lmer(scale(ER_distraction) ~ scale(cnegaff_composite_1)*scale(cRICCneg_after.fz) + scale(cperc_pass)
                    + scale(cER_distraction_1) + (scale(cnegaff_composite_1)+ scale(cER_distraction_1)| Participant), data=examdata_postexam)
exaccNArev <- lmer(scale(ER_acceptance) ~ scale(cnegaff_composite_1)*scale(cRICCneg_after.fz) + scale(cperc_pass)
                   + scale(cER_acceptance_1) + (scale(cnegaff_composite_1)+ scale(cER_acceptance_1)| Participant), data=examdata_postexam)
exsuppNArev <- lmer(scale(ER_supp) ~ scale(cnegaff_composite_1)*scale(cRICCneg_after.fz) + scale(cperc_pass)
                    + scale(cER_supp_1) + (scale(cnegaff_composite_1)+ scale(cER_supp_1)| Participant), data=examdata_postexam)
exsshNArev <- lmer(scale(ER_soc_sharing) ~ scale(cnegaff_composite_1)*scale(cRICCneg_after.fz) + scale(cperc_pass)
                   + scale(cER_soc_sharing_1) + (scale(cnegaff_composite_1)+ scale(cER_soc_sharing_1)| Participant), data=examdata_postexam)
exreappNArev <- lmer(scale(ER_reapp) ~ scale(cnegaff_composite_1)*scale(cRICCneg_after.fz) + scale(cperc_pass)
                     + scale(cER_reapp_1) + (scale(cnegaff_composite_1)+ scale(cER_reapp_1)| Participant), data=examdata_postexam)

summary(exrumiNArev)
summary(exdistNArev)
summary(exreappNArev)
summary(exsuppNArev)
summary(exsshNArev)
summary(exaccNArev)

confint(exrumiNArev, method="Wald")
confint(exdistNArev, method="Wald")
confint(exreappNArev, method="Wald")
confint(exsuppNArev, method="Wald")
confint(exsshNArev, method="Wald")
confint(exaccNArev, method="Wald")

######9. CONTROLLING FOR TIME IN STUDY 1 TO MODEL FATIGUE####

ldata$cBeepNrW <- ldata$BeepNr - ave(ldata$BeepNr, ldata$Wave, FUN=function(x) mean(x, na.rm=TRUE))
#decided to enter beep number uncentered since it makes sense that way (and already scaled)

#model 1 predicting strategies#
lrumiT <- lmer(scale(rumi) ~ scale(cRICCneg.fzW) + scale(cnegaff_meanW) + scale(BeepNr) + (1| PpID/Wave), data=ldata)
ldistT <- lmer(scale(dist) ~ scale(cRICCneg.fzW) + scale(cnegaff_meanW) + scale(BeepNr) + (1| PpID/Wave), data=ldata)
lreappT <- lmer(scale(reapp) ~ scale(cRICCneg.fzW) + scale(cnegaff_meanW) + scale(BeepNr) + (1| PpID/Wave), data=ldata)
lsuppT <- lmer(scale(supp) ~ scale(cRICCneg.fzW) + scale(cnegaff_meanW) + scale(BeepNr) + (1| PpID/Wave), data=ldata)
lsshT <- lmer(scale(ssh) ~ scale(cRICCneg.fzW) + scale(cnegaff_meanW) + scale(BeepNr) + (1| PpID/Wave), data=ldata)

summary(lrumiT)
summary(ldistT)
summary(lreappT)
summary(lsuppT)
summary(lsshT)

confint(lrumiT, method="Wald")
confint(ldistT, method="Wald")
confint(lreappT, method="Wald")
confint(lsuppT, method="Wald")
confint(lsshT, method="Wald")

#model 2 predicting negative emotion#
lrumiNAT <- lmer(scale(negaff) ~ scale(crumi)*scale(cRICCneg.fzW)  
                 + scale(cnegaff_1)+ scale(BeepNr) + (1|PpID) + (1|Wave)  + (0 + scale(crumi) + scale(cnegaff_1) | PpID) 
                 + (0 + scale(crumi) + scale(cnegaff_1) | Wave) +  (0 + scale(crumi) + scale(cnegaff_1) || PpID:Wave), data=ldata)
ldistNAT <- lmer(scale(negaff) ~ scale(cdist)*scale(cRICCneg.fzW)  
                 + scale(cnegaff_1)+ scale(BeepNr) + (1|PpID) + (1|Wave)  + (0 + scale(cdist) + scale(cnegaff_1) | PpID) 
                 + (0 + scale(cdist) + scale(cnegaff_1) | Wave) +  (0 + scale(cdist) + scale(cnegaff_1) || PpID:Wave), data=ldata)
lreappNAT <- lmer(scale(negaff) ~ scale(creapp)*scale(cRICCneg.fzW) 
                  + scale(cnegaff_1)+ scale(BeepNr) + (1|PpID) + (1|Wave)  + (0 + scale(creapp) + scale(cnegaff_1) | PpID) 
                  + (0 + scale(creapp) + scale(cnegaff_1) | Wave) +  (0 + scale(creapp) + scale(cnegaff_1) || PpID:Wave), data=ldata)
lsuppNAT <- lmer(scale(negaff) ~ scale(csupp)*scale(cRICCneg.fzW) 
                 + scale(cnegaff_1)+ scale(BeepNr) + (1|PpID) + (1|Wave)  + (0 + scale(csupp) + scale(cnegaff_1) | PpID) 
                 + (0 + scale(csupp) + scale(cnegaff_1) | Wave) +  (0 + scale(csupp) + scale(cnegaff_1) || PpID:Wave), data=ldata)
lsshNAT <- lmer(scale(negaff) ~ scale(cssh)*scale(cRICCneg.fzW) 
                + scale(cnegaff_1)+ scale(BeepNr) + (1|PpID) + (1|Wave)  + (0 + scale(cssh) + scale(cnegaff_1) | PpID) 
                + (0 + scale(cssh) + scale(cnegaff_1) | Wave) +  (0 + scale(cssh) + scale(cnegaff_1) || PpID:Wave), data=ldata)

summary(lrumiNAT)
summary(ldistNAT)
summary(lreappNAT)
summary(lsuppNAT)
summary(lsshNAT)

confint(lrumiNAT, method="Wald")
confint(ldistNAT, method="Wald")
confint(lreappNAT, method="Wald")
confint(lsuppNAT, method="Wald")
confint(lsshNAT, method="Wald")

######10. STANDARDIZED GRAPHS #####

######10a. Study 1 Longitudinal data#####
#simple slopes calculated using Kris Preacher's online calculator (Preacher, Curran, & Bauer, 2006)

#PLOTS OF SIGNIFICANT INTERACTIONS
#plots have colour - to change to black and white, replace the scale_color_gradient2 with scale_color_distiller(palette="Greys")

#RUMINATION
#simple slopes
Slowdiffslope_rumi <- 0.3241 
Shighdiffslope_rumi <- 0.1176 
Slowdiffint_rumi <- 0.134
Shighdiffint_rumi <- -0.1526

#PLOT
#first I'm getting all the parameters I need to make the plot
#I'm going to use dotted lines for when the lines drop below -3SD from the mean, and above +3SD from the mean
#so that it's clear that the lines are interpolated here  based on very few datapoints
Splus3rumisd <- ((sd(ldata$Scrumi, na.rm=TRUE))*3)
Sminus3rumisd <- 0-((sd(ldata$Scrumi, na.rm=TRUE))*3)

#figuring out the points to start and finish each of the lines using the regression equation (y = intercept + slope*xvalue)
#-100 and 100 are the theoretical min and max in the data
Slowdifflinestart_rumi <- Slowdiffint_rumi+(Slowdiffslope_rumi*Sminus3rumisd) #low differentiation line begin
Slowdifflineend_rumi <- Slowdiffint_rumi+(Slowdiffslope_rumi*Splus3rumisd) #low differentiation line end
Slowdiffprestart_rumi <- Slowdiffint_rumi +(Slowdiffslope_rumi*-100) #low differentiation pre-line segment begin 
Slowdiffpostend_rumi <- Slowdiffint_rumi +(Slowdiffslope_rumi*100) #low differentiation post-line segment end
Shighdifflinestart_rumi <- Shighdiffint_rumi+(Shighdiffslope_rumi*Sminus3rumisd) #low differentiation line begin
Shighdifflineend_rumi <- Shighdiffint_rumi+(Shighdiffslope_rumi*Splus3rumisd) #low differentiation line end
Shighdiffprestart_rumi <- Shighdiffint_rumi +(Shighdiffslope_rumi*-100) #low differentiation pre-line segment begin 
Shighdiffpostend_rumi <- Shighdiffint_rumi +(Shighdiffslope_rumi*100) #low differentiation post-line segment end

#here is the plot code
SldatarumiNAplot <-ggplot(ldata, aes(x = Scrumi, y = Snegaff)) + 
  geom_point(aes(colour = ScRICCneg.fzW), size = 0.5, alpha = 0.3) +
  scale_colour_gradient2(low = "red", mid = "lightcyan", high = "blue") +
  labs(x = 'Rumination (standardized and\nperson-mean centered)', y = 'Negative emotion\n(standardized)', color="Emotion differentiation\n(standardized)") +
  geom_segment(aes(x = Sminus3rumisd, y = Slowdifflinestart_rumi, xend = Splus3rumisd, yend = Slowdifflineend_rumi, linetype='Low emotion differentiation (-1SD)')) +
  geom_segment(aes(x = Sminus3rumisd, y = Shighdifflinestart_rumi, xend = Splus3rumisd, yend = Shighdifflineend_rumi, linetype='High emotion differentiation (+1SD)')) +
  geom_segment(x=-100, y=Slowdiffprestart_rumi, xend = Sminus3rumisd, yend = Slowdifflinestart_rumi, linetype = "dotted") +
  geom_segment(x=Splus3rumisd, y=Slowdifflineend_rumi, xend = 100, yend = Slowdiffpostend_rumi, linetype = "dotted") +
  geom_segment(x=-100, y=Shighdiffprestart_rumi, xend = Sminus3rumisd, yend = Shighdifflinestart_rumi, linetype = "dotted") +
  geom_segment(x=Splus3rumisd, y=Shighdifflineend_rumi, xend = 100, yend = Shighdiffpostend_rumi, linetype = "dotted") +
  scale_linetype_manual(values=c('longdash','solid'), 
                        breaks=c('Low emotion differentiation (-1SD)','High emotion differentiation (+1SD)'),
                        name='Simple Slopes')+ 
  scale_y_continuous(limits=c(min(ldata$Snegaff, na.rm=TRUE),(max(ldata$Snegaff, na.rm=TRUE))), expand = c(0,0)) +
  scale_x_continuous(limits=c(-4.4, 6.61), expand = c(0,0))+ apatheme 

#DISTRACTION
#simple slopes
Slowdiffslope_dist <- 0.1806 
Shighdiffslope_dist <- 0.0715 
Slowdiffint_dist <- 0.1319
Shighdiffint_dist <- -0.154

#PLOT
#first I'm getting all the parameters I need to make the plot
#I'm going to use dotted lines for when the lines drop below -3SD from the mean, and above +3SD from the mean
#so that it's clear that the lines are interpolated here  based on very few datapoints
Splus3distsd <- ((sd(ldata$Scdist, na.rm=TRUE))*3)
Sminus3distsd <- 0-((sd(ldata$Scdist, na.rm=TRUE))*3)

#figuring out the points to start and finish each of the lines using the regression equation (y = intercept + slope*xvalue)
#-100 and 100 are the theoretical min and max in the data
Slowdifflinestart_dist <- Slowdiffint_dist+(Slowdiffslope_dist*Sminus3distsd) #low differentiation line begin
Slowdifflineend_dist <- Slowdiffint_dist+(Slowdiffslope_dist*Splus3distsd) #low differentiation line end
Slowdiffprestart_dist <- Slowdiffint_dist +(Slowdiffslope_dist*-100) #low differentiation pre-line segment begin 
Slowdiffpostend_dist <- Slowdiffint_dist +(Slowdiffslope_dist*100) #low differentiation post-line segment end
Shighdifflinestart_dist <- Shighdiffint_dist+(Shighdiffslope_dist*Sminus3distsd) #low differentiation line begin
Shighdifflineend_dist <- Shighdiffint_dist+(Shighdiffslope_dist*Splus3distsd) #low differentiation line end
Shighdiffprestart_dist <- Shighdiffint_dist +(Shighdiffslope_dist*-100) #low differentiation pre-line segment begin 
Shighdiffpostend_dist <- Shighdiffint_dist +(Shighdiffslope_dist*100) #low differentiation post-line segment end

#here is the plot code
SldatadistNAplot <-ggplot(ldata, aes(x = Scdist, y = Snegaff)) + 
  geom_point(aes(colour = ScRICCneg.fzW), size = 0.5, alpha = 0.3) +
  scale_colour_gradient2(low = "red", mid = "lightcyan", high = "blue") +
  labs(x = 'Distraction (standardized and\nperson-mean centered)', y = 'Negative emotion\n(standardized)', color="Emotion differentiation\n(standardized)") +
  geom_segment(aes(x = Sminus3distsd, y = Slowdifflinestart_dist, xend = Splus3distsd, yend = Slowdifflineend_dist, linetype='Low emotion differentiation (-1SD)')) +
  geom_segment(aes(x = Sminus3distsd, y = Shighdifflinestart_dist, xend = Splus3distsd, yend = Shighdifflineend_dist, linetype='High emotion differentiation (+1SD)')) +
  geom_segment(x=-100, y=Slowdiffprestart_dist, xend = Sminus3distsd, yend = Slowdifflinestart_dist, linetype = "dotted") +
  geom_segment(x=Splus3distsd, y=Slowdifflineend_dist, xend = 100, yend = Slowdiffpostend_dist, linetype = "dotted") +
  geom_segment(x=-100, y=Shighdiffprestart_dist, xend = Sminus3distsd, yend = Shighdifflinestart_dist, linetype = "dotted") +
  geom_segment(x=Splus3distsd, y=Shighdifflineend_dist, xend = 100, yend = Shighdiffpostend_dist, linetype = "dotted") +
  scale_linetype_manual(values=c('longdash','solid'), 
                        breaks=c('Low emotion differentiation (-1SD)','High emotion differentiation (+1SD)'),
                        name='Simple Slopes')+ 
  scale_y_continuous(limits=c(min(ldata$Snegaff, na.rm=TRUE),(max(ldata$Snegaff, na.rm=TRUE))), expand = c(0,0)) +
  scale_x_continuous(limits=c(-4.4, 6.61), expand = c(0,0))+ apatheme 


#REAPPRAISAL
#simple slopes
Slowdiffslope_reapp <- 0.1611
Shighdiffslope_reapp <- 0.0651 
Slowdiffint_reapp <- 0.1331
Shighdiffint_reapp <- -0.1545

#PLOT
#first I'm getting all the parameters I need to make the plot
#I'm going to use dotted lines for when the lines drop below -3SD from the mean, and above +3SD from the mean
#so that it's clear that the lines are interpolated here  based on very few datapoints
Splus3reappsd <- ((sd(ldata$Screapp, na.rm=TRUE))*3)
Sminus3reappsd <- 0-((sd(ldata$Screapp, na.rm=TRUE))*3)

#figuring out the points to start and finish each of the lines using the regression equation (y = intercept + slope*xvalue)
#-100 and 100 are the theoretical min and max in the data
Slowdifflinestart_reapp <- Slowdiffint_reapp+(Slowdiffslope_reapp*Sminus3reappsd) #low differentiation line begin
Slowdifflineend_reapp <- Slowdiffint_reapp+(Slowdiffslope_reapp*Splus3reappsd) #low differentiation line end
Slowdiffprestart_reapp <- Slowdiffint_reapp +(Slowdiffslope_reapp*-100) #low differentiation pre-line segment begin 
Slowdiffpostend_reapp <- Slowdiffint_reapp +(Slowdiffslope_reapp*100) #low differentiation post-line segment end
Shighdifflinestart_reapp <- Shighdiffint_reapp+(Shighdiffslope_reapp*Sminus3reappsd) #low differentiation line begin
Shighdifflineend_reapp <- Shighdiffint_reapp+(Shighdiffslope_reapp*Splus3reappsd) #low differentiation line end
Shighdiffprestart_reapp <- Shighdiffint_reapp +(Shighdiffslope_reapp*-100) #low differentiation pre-line segment begin 
Shighdiffpostend_reapp <- Shighdiffint_reapp +(Shighdiffslope_reapp*100) #low differentiation post-line segment end

#here is the plot code
SldatareappNAplot <-ggplot(ldata, aes(x = Screapp, y = Snegaff)) + 
  geom_point(aes(colour = ScRICCneg.fzW), size = 0.5, alpha = 0.3) +
  scale_colour_gradient2(low = "red", mid = "lightcyan", high = "blue") +
  labs(x = 'Reappraisal (standardized and\nperson-mean centered)', y = 'Negative emotion\n(standardized)', color="Emotion differentiation\n(standardized)") +
  geom_segment(aes(x = Sminus3reappsd, y = Slowdifflinestart_reapp, xend = Splus3reappsd, yend = Slowdifflineend_reapp, linetype='Low emotion differentiation (-1SD)')) +
  geom_segment(aes(x = Sminus3reappsd, y = Shighdifflinestart_reapp, xend = Splus3reappsd, yend = Shighdifflineend_reapp, linetype='High emotion differentiation (+1SD)')) +
  geom_segment(x=-100, y=Slowdiffprestart_reapp, xend = Sminus3reappsd, yend = Slowdifflinestart_reapp, linetype = "dotted") +
  geom_segment(x=Splus3reappsd, y=Slowdifflineend_reapp, xend = 100, yend = Slowdiffpostend_reapp, linetype = "dotted") +
  geom_segment(x=-100, y=Shighdiffprestart_reapp, xend = Sminus3reappsd, yend = Shighdifflinestart_reapp, linetype = "dotted") +
  geom_segment(x=Splus3reappsd, y=Shighdifflineend_reapp, xend = 100, yend = Shighdiffpostend_reapp, linetype = "dotted") +
  scale_linetype_manual(values=c('longdash','solid'), 
                        breaks=c('Low emotion differentiation (-1SD)','High emotion differentiation (+1SD)'),
                        name='Simple Slopes')+ 
  scale_y_continuous(limits=c(min(ldata$Snegaff, na.rm=TRUE),(max(ldata$Snegaff, na.rm=TRUE))), expand = c(0,0)) +
  scale_x_continuous(limits=c(-4.4, 6.61), expand = c(0,0))+ apatheme 


#SUPPRESSION
#simple slopes
Slowdiffslope_supp <- 0.2746
Shighdiffslope_supp <- 0.0907 
Slowdiffint_supp <- 0.1318
Shighdiffint_supp <- -0.1532

#PLOT
#first I'm getting all the parameters I need to make the plot
#I'm going to use dotted lines for when the lines drop below -3SD from the mean, and above +3SD from the mean
#so that it's clear that the lines are interpolated here  based on very few datapoints
Splus3suppsd <- ((sd(ldata$Scsupp, na.rm=TRUE))*3)
Sminus3suppsd <- 0-((sd(ldata$Scsupp, na.rm=TRUE))*3)

#figuring out the points to start and finish each of the lines using the regression equation (y = intercept + slope*xvalue)
#-100 and 100 are the theoretical min and max in the data
Slowdifflinestart_supp <- Slowdiffint_supp+(Slowdiffslope_supp*Sminus3suppsd) #low differentiation line begin
Slowdifflineend_supp <- Slowdiffint_supp+(Slowdiffslope_supp*Splus3suppsd) #low differentiation line end
Slowdiffprestart_supp <- Slowdiffint_supp +(Slowdiffslope_supp*-100) #low differentiation pre-line segment begin 
Slowdiffpostend_supp <- Slowdiffint_supp +(Slowdiffslope_supp*100) #low differentiation post-line segment end
Shighdifflinestart_supp <- Shighdiffint_supp+(Shighdiffslope_supp*Sminus3suppsd) #low differentiation line begin
Shighdifflineend_supp <- Shighdiffint_supp+(Shighdiffslope_supp*Splus3suppsd) #low differentiation line end
Shighdiffprestart_supp <- Shighdiffint_supp +(Shighdiffslope_supp*-100) #low differentiation pre-line segment begin 
Shighdiffpostend_supp <- Shighdiffint_supp +(Shighdiffslope_supp*100) #low differentiation post-line segment end

#here is the plot code
SldatasuppNAplot <-ggplot(ldata, aes(x = Scsupp, y = Snegaff)) + 
  geom_point(aes(colour = ScRICCneg.fzW), size = 0.5, alpha = 0.3) +
  scale_colour_gradient2(low = "red", mid = "lightcyan", high = "blue") +
  labs(x = 'Suppression (standardized and\nperson-mean centered)', y = 'Negative emotion\n(standardized)', color="Emotion differentiation\n(standardized)") +
  geom_segment(aes(x = Sminus3suppsd, y = Slowdifflinestart_supp, xend = Splus3suppsd, yend = Slowdifflineend_supp, linetype='Low emotion differentiation (-1SD)')) +
  geom_segment(aes(x = Sminus3suppsd, y = Shighdifflinestart_supp, xend = Splus3suppsd, yend = Shighdifflineend_supp, linetype='High emotion differentiation (+1SD)')) +
  geom_segment(x=-100, y=Slowdiffprestart_supp, xend = Sminus3suppsd, yend = Slowdifflinestart_supp, linetype = "dotted") +
  geom_segment(x=Splus3suppsd, y=Slowdifflineend_supp, xend = 100, yend = Slowdiffpostend_supp, linetype = "dotted") +
  geom_segment(x=-100, y=Shighdiffprestart_supp, xend = Sminus3suppsd, yend = Shighdifflinestart_supp, linetype = "dotted") +
  geom_segment(x=Splus3suppsd, y=Shighdifflineend_supp, xend = 100, yend = Shighdiffpostend_supp, linetype = "dotted") +
  scale_linetype_manual(values=c('longdash','solid'), 
                        breaks=c('Low emotion differentiation (-1SD)','High emotion differentiation (+1SD)'),
                        name='Simple Slopes')+ 
  scale_y_continuous(limits=c(min(ldata$Snegaff, na.rm=TRUE),(max(ldata$Snegaff, na.rm=TRUE))), expand = c(0,0)) +
  scale_x_continuous(limits=c(-4.4, 6.61), expand = c(0,0))+ apatheme 


#SOCIAL SHARING
#simple slopes
Slowdiffslope_ssh <- 0.1523
Shighdiffslope_ssh <- 0.0304 
Slowdiffint_ssh <- 0.1336
Shighdiffint_ssh <- -0.1553

#PLOT
#first I'm getting all the parameters I need to make the plot
#I'm going to use dotted lines for when the lines drop below -3SD from the mean, and above +3SD from the mean
#so that it's clear that the lines are interpolated here  based on very few datapoints
Splus3sshsd <- ((sd(ldata$Scssh, na.rm=TRUE))*3)
Sminus3sshsd <- 0-((sd(ldata$Scssh, na.rm=TRUE))*3)

#figuring out the points to start and finish each of the lines using the regression equation (y = intercept + slope*xvalue)
#-100 and 100 are the theoretical min and max in the data
Slowdifflinestart_ssh <- Slowdiffint_ssh+(Slowdiffslope_ssh*Sminus3sshsd) #low differentiation line begin
Slowdifflineend_ssh <- Slowdiffint_ssh+(Slowdiffslope_ssh*Splus3sshsd) #low differentiation line end
Slowdiffprestart_ssh <- Slowdiffint_ssh +(Slowdiffslope_ssh*-100) #low differentiation pre-line segment begin 
Slowdiffpostend_ssh <- Slowdiffint_ssh +(Slowdiffslope_ssh*100) #low differentiation post-line segment end
Shighdifflinestart_ssh <- Shighdiffint_ssh+(Shighdiffslope_ssh*Sminus3sshsd) #low differentiation line begin
Shighdifflineend_ssh <- Shighdiffint_ssh+(Shighdiffslope_ssh*Splus3sshsd) #low differentiation line end
Shighdiffprestart_ssh <- Shighdiffint_ssh +(Shighdiffslope_ssh*-100) #low differentiation pre-line segment begin 
Shighdiffpostend_ssh <- Shighdiffint_ssh +(Shighdiffslope_ssh*100) #low differentiation post-line segment end

#here is the plot code
SldatasshNAplot <-ggplot(ldata, aes(x = Scssh, y = Snegaff)) + 
  geom_point(aes(colour = ScRICCneg.fzW), size = 0.5, alpha = 0.3) +
  scale_colour_gradient2(low = "red", mid = "lightcyan", high = "blue") +
  labs(x = 'Social sharing (standardized and\nperson-mean centered)', y = 'Negative emotion\n(standardized)', color="Emotion differentiation\n(standardized)") +
  geom_segment(aes(x = Sminus3sshsd, y = Slowdifflinestart_ssh, xend = Splus3sshsd, yend = Slowdifflineend_ssh, linetype='Low emotion differentiation (-1SD)')) +
  geom_segment(aes(x = Sminus3sshsd, y = Shighdifflinestart_ssh, xend = Splus3sshsd, yend = Shighdifflineend_ssh, linetype='High emotion differentiation (+1SD)')) +
  geom_segment(x=-100, y=Slowdiffprestart_ssh, xend = Sminus3sshsd, yend = Slowdifflinestart_ssh, linetype = "dotted") +
  geom_segment(x=Splus3sshsd, y=Slowdifflineend_ssh, xend = 100, yend = Slowdiffpostend_ssh, linetype = "dotted") +
  geom_segment(x=-100, y=Shighdiffprestart_ssh, xend = Sminus3sshsd, yend = Shighdifflinestart_ssh, linetype = "dotted") +
  geom_segment(x=Splus3sshsd, y=Shighdifflineend_ssh, xend = 100, yend = Shighdiffpostend_ssh, linetype = "dotted") +
  scale_linetype_manual(values=c('longdash','solid'), 
                        breaks=c('Low emotion differentiation (-1SD)','High emotion differentiation (+1SD)'),
                        name='Simple Slopes')+ 
  scale_y_continuous(limits=c(min(ldata$Snegaff, na.rm=TRUE),(max(ldata$Snegaff, na.rm=TRUE))), expand = c(0,0)) +
  scale_x_continuous(limits=c(-4.4, 6.61), expand = c(0,0))+ apatheme 


#Exporting all these plots into a multi-panel figure##
png('standardized_simpleslopes_ldataNA.png', units="cm", width=30, height=15, res=300)
ggarrange(SldatarumiNAplot, SldatadistNAplot, SldatareappNAplot, SldatasuppNAplot, SldatasshNAplot,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom")
dev.off()

tiff('standardized_simpleslopes_ldataNA_highres.tiff', units="cm", width=30, height=15, res=300)
ggarrange(SldatarumiNAplot, SldatadistNAplot, SldatareappNAplot, SldatasuppNAplot, SldatasshNAplot,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom")
dev.off()


#####10b. Study 2 - Exam data#####

#RUMINATION
#simple slopes
Slowdiffslope_rumiE <- 0.1076
Shighdiffslope_rumiE <- 0.0579 
Slowdiffint_rumiE <- 0.0215
Shighdiffint_rumiE <- -0.0216

#PLOT
#first I'm getting all the parameters I need to make the plot
#I'm going to use dotted lines for when the lines drop below -3SD from the mean, and above +3SD from the mean
#so that it's clear that the lines are interpolated here  based on very few datapoints
Splus3rumiEsd <- ((sd(examdata_postexam$ScER_rumination, na.rm=TRUE))*3)
Sminus3rumiEsd <- 0-((sd(examdata_postexam$ScER_rumination, na.rm=TRUE))*3)

#figuring out the points to start and finish each of the lines using the regression equation (y = intercept + slope*xvalue)
#-6 and 6 are the theoretical min and max in the data
Slowdifflinestart_rumiE <- Slowdiffint_rumiE+(Slowdiffslope_rumiE*Sminus3rumiEsd) #low differentiation line begin
Slowdifflineend_rumiE <- Slowdiffint_rumiE+(Slowdiffslope_rumiE*Splus3rumiEsd) #low differentiation line end
Slowdiffprestart_rumiE <- Slowdiffint_rumiE +(Slowdiffslope_rumiE*-6) #low differentiation pre-line segment begin 
Slowdiffpostend_rumiE <- Slowdiffint_rumiE +(Slowdiffslope_rumiE*6) #low differentiation post-line segment end
Shighdifflinestart_rumiE <- Shighdiffint_rumiE+(Shighdiffslope_rumiE*Sminus3rumiEsd) #low differentiation line begin
Shighdifflineend_rumiE <- Shighdiffint_rumiE+(Shighdiffslope_rumiE*Splus3rumiEsd) #low differentiation line end
Shighdiffprestart_rumiE <- Shighdiffint_rumiE +(Shighdiffslope_rumiE*-6) #low differentiation pre-line segment begin 
Shighdiffpostend_rumiE <- Shighdiffint_rumiE +(Shighdiffslope_rumiE*6) #low differentiation post-line segment end

#here is the plot code
SexamrumiNAplot <-ggplot(examdata_postexam, aes(x = ScER_rumination, y = Snegaff_composite)) +
  geom_point(aes(colour = ScRICCneg_after.fz), size = 0.5) +
  scale_colour_gradient2(low = "red", mid = "lightcyan", high = "blue") +
  labs(x = 'Rumination (standardized\nand person-mean centered)', y = 'Negative emotion\n(standardized)', color="Emotion differentiation") +
  geom_segment(aes(x = Sminus3rumiEsd, y = Slowdifflinestart_rumiE, xend = Splus3rumiEsd, yend = Slowdifflineend_rumiE, linetype='Low emotion differentiation (-1SD)')) +
  geom_segment(aes(x = Sminus3rumiEsd, y = Shighdifflinestart_rumiE, xend = Splus3rumiEsd, yend = Shighdifflineend_rumiE, linetype='High emotion differentiation (+1SD)')) +
  geom_segment(x=-6, y=Slowdiffprestart_rumiE, xend = Sminus3rumiEsd, yend = Slowdifflinestart_rumiE, linetype = "dotted") +
  geom_segment(x=Splus3rumiEsd, y=Slowdifflineend_rumiE, xend = 6, yend = Slowdiffpostend_rumiE, linetype = "dotted") +
  geom_segment(x=-6, y=Shighdiffprestart_rumiE, xend = Sminus3rumiEsd, yend = Shighdifflinestart_rumiE, linetype = "dotted") +
  geom_segment(x=Splus3rumiEsd, y=Shighdifflineend_rumiE, xend = 6, yend = Shighdiffpostend_rumiE, linetype = "dotted") +
  scale_linetype_manual(values=c('longdash','solid'), 
                        breaks=c('Low emotion differentiation (-1SD)','High emotion differentiation (+1SD)'),
                        name='Simple Slopes')+ 
  scale_y_continuous(limits=c(min(examdata_postexam$Snegaff_composite,na.rm=TRUE),(max(examdata_postexam$Snegaff_composite, na.rm=TRUE))), expand = c(0, 0))+
  scale_x_continuous(limits=c(-4.48,6.54), expand = c(0,0))+ apatheme 


#DISTRACTION
#simple slopes
Slowdiffslope_distE <- 0.0504
Shighdiffslope_distE <- -0.0007
Slowdiffint_distE <- 0.032
Shighdiffint_distE <- -0.0382

#PLOT
#first I'm getting all the parameters I need to make the plot
#I'm going to use dotted lines for when the lines drop below -3SD from the mean, and above +3SD from the mean
#so that it's clear that the lines are interpolated here  based on very few datapoints
Splus3distEsd <- ((sd(examdata_postexam$ScER_distraction, na.rm=TRUE))*3)
Sminus3distEsd <- 0-((sd(examdata_postexam$ScER_distraction, na.rm=TRUE))*3)

#figuring out the points to start and finish each of the lines using the regression equation (y = intercept + slope*xvalue)
#-6 and 6 are the theoretical min and max in the data
Slowdifflinestart_distE <- Slowdiffint_distE+(Slowdiffslope_distE*Sminus3distEsd) #low differentiation line begin
Slowdifflineend_distE <- Slowdiffint_distE+(Slowdiffslope_distE*Splus3distEsd) #low differentiation line end
Slowdiffprestart_distE <- Slowdiffint_distE +(Slowdiffslope_distE*-6) #low differentiation pre-line segment begin 
Slowdiffpostend_distE <- Slowdiffint_distE +(Slowdiffslope_distE*6) #low differentiation post-line segment end
Shighdifflinestart_distE <- Shighdiffint_distE+(Shighdiffslope_distE*Sminus3distEsd) #low differentiation line begin
Shighdifflineend_distE <- Shighdiffint_distE+(Shighdiffslope_distE*Splus3distEsd) #low differentiation line end
Shighdiffprestart_distE <- Shighdiffint_distE +(Shighdiffslope_distE*-6) #low differentiation pre-line segment begin 
Shighdiffpostend_distE <- Shighdiffint_distE +(Shighdiffslope_distE*6) #low differentiation post-line segment end

#here is the plot code
SexamdistNAplot <-ggplot(examdata_postexam, aes(x = ScER_distraction, y = Snegaff_composite)) +
  geom_point(aes(colour = ScRICCneg_after.fz), size = 0.5) +
  scale_colour_gradient2(low = "red", mid = "lightcyan", high = "blue") +
  labs(x = 'Distraction (standardized\nand person-mean centered)', y = 'Negative emotion\n(standardized)', color="Emotion differentiation") +
  geom_segment(aes(x = Sminus3distEsd, y = Slowdifflinestart_distE, xend = Splus3distEsd, yend = Slowdifflineend_distE, linetype='Low emotion differentiation (-1SD)')) +
  geom_segment(aes(x = Sminus3distEsd, y = Shighdifflinestart_distE, xend = Splus3distEsd, yend = Shighdifflineend_distE, linetype='High emotion differentiation (+1SD)')) +
  geom_segment(x=-6, y=Slowdiffprestart_distE, xend = Sminus3distEsd, yend = Slowdifflinestart_distE, linetype = "dotted") +
  geom_segment(x=Splus3distEsd, y=Slowdifflineend_distE, xend = 6, yend = Slowdiffpostend_distE, linetype = "dotted") +
  geom_segment(x=-6, y=Shighdiffprestart_distE, xend = Sminus3distEsd, yend = Shighdifflinestart_distE, linetype = "dotted") +
  geom_segment(x=Splus3distEsd, y=Shighdifflineend_distE, xend = 6, yend = Shighdiffpostend_distE, linetype = "dotted") +
  scale_linetype_manual(values=c('longdash','solid'), 
                        breaks=c('Low emotion differentiation (-1SD)','High emotion differentiation (+1SD)'),
                        name='Simple Slopes')+ 
  scale_y_continuous(limits=c(min(examdata_postexam$Snegaff_composite,na.rm=TRUE),(max(examdata_postexam$Snegaff_composite, na.rm=TRUE))), expand = c(0, 0))+
  scale_x_continuous(limits=c(-4.48,6.54), expand = c(0,0))+ apatheme 

#SOCIAL SHARING
#simple slopes
Slowdiffslope_sshE <- 0.0675
Shighdiffslope_sshE <- 0.0071
Slowdiffint_sshE <- 0.0298
Shighdiffint_sshE <- -0.0341

#PLOT
#first I'm getting all the parameters I need to make the plot
#I'm going to use dotted lines for when the lines drop below -3SD from the mean, and above +3SD from the mean
#so that it's clear that the lines are interpolated here  based on very few datapoints
Splus3sshEsd <- ((sd(examdata_postexam$ScER_soc_sharing, na.rm=TRUE))*3)
Sminus3sshEsd <- 0-((sd(examdata_postexam$ScER_soc_sharing, na.rm=TRUE))*3)

#figuring out the points to start and finish each of the lines using the regression equation (y = intercept + slope*xvalue)
#-6 and 6 are the theoretical min and max in the data
Slowdifflinestart_sshE <- Slowdiffint_sshE+(Slowdiffslope_sshE*Sminus3sshEsd) #low differentiation line begin
Slowdifflineend_sshE <- Slowdiffint_sshE+(Slowdiffslope_sshE*Splus3sshEsd) #low differentiation line end
Slowdiffprestart_sshE <- Slowdiffint_sshE +(Slowdiffslope_sshE*-6) #low differentiation pre-line segment begin 
Slowdiffpostend_sshE <- Slowdiffint_sshE +(Slowdiffslope_sshE*6) #low differentiation post-line segment end
Shighdifflinestart_sshE <- Shighdiffint_sshE+(Shighdiffslope_sshE*Sminus3sshEsd) #low differentiation line begin
Shighdifflineend_sshE <- Shighdiffint_sshE+(Shighdiffslope_sshE*Splus3sshEsd) #low differentiation line end
Shighdiffprestart_sshE <- Shighdiffint_sshE +(Shighdiffslope_sshE*-6) #low differentiation pre-line segment begin 
Shighdiffpostend_sshE <- Shighdiffint_sshE +(Shighdiffslope_sshE*6) #low differentiation post-line segment end

#here is the plot code
SexamsshNAplot <-ggplot(examdata_postexam, aes(x = ScER_soc_sharing, y = Snegaff_composite)) +
  geom_point(aes(colour = ScRICCneg_after.fz), size = 0.5) +
  scale_colour_gradient2(low = "red", mid = "lightcyan", high = "blue") +
  labs(x = 'Social sharing (standardized\nand person-mean centered)', y = 'Negative emotion\n(standardized)', color="Emotion differentiation") +
  geom_segment(aes(x = Sminus3sshEsd, y = Slowdifflinestart_sshE, xend = Splus3sshEsd, yend = Slowdifflineend_sshE, linetype='Low emotion differentiation (-1SD)')) +
  geom_segment(aes(x = Sminus3sshEsd, y = Shighdifflinestart_sshE, xend = Splus3sshEsd, yend = Shighdifflineend_sshE, linetype='High emotion differentiation (+1SD)')) +
  geom_segment(x=-6, y=Slowdiffprestart_sshE, xend = Sminus3sshEsd, yend = Slowdifflinestart_sshE, linetype = "dotted") +
  geom_segment(x=Splus3sshEsd, y=Slowdifflineend_sshE, xend = 6, yend = Slowdiffpostend_sshE, linetype = "dotted") +
  geom_segment(x=-6, y=Shighdiffprestart_sshE, xend = Sminus3sshEsd, yend = Shighdifflinestart_sshE, linetype = "dotted") +
  geom_segment(x=Splus3sshEsd, y=Shighdifflineend_sshE, xend = 6, yend = Shighdiffpostend_sshE, linetype = "dotted") +
  scale_linetype_manual(values=c('longdash','solid'), 
                        breaks=c('Low emotion differentiation (-1SD)','High emotion differentiation (+1SD)'),
                        name='Simple Slopes')+ 
  scale_y_continuous(limits=c(min(examdata_postexam$Snegaff_composite,na.rm=TRUE),(max(examdata_postexam$Snegaff_composite, na.rm=TRUE))), expand = c(0, 0))+
  scale_x_continuous(limits=c(-4.48,6.54), expand = c(0,0))+ apatheme 

#ACCEPTANCE
#simple slopes
Slowdiffslope_accE <- -0.0686
Shighdiffslope_accE <- -0.0059 
Slowdiffint_accE <- 0.04
Shighdiffint_accE <- -0.048

#PLOT
#first I'm getting all the parameters I need to make the plot
#I'm going to use dotted lines for when the lines drop below -3SD from the mean, and above +3SD from the mean
#so that it's clear that the lines are interpolated here  based on very few datapoints
Splus3accEsd <- ((sd(examdata_postexam$ScER_acceptance, na.rm=TRUE))*3)
Sminus3accEsd <- 0-((sd(examdata_postexam$ScER_acceptance, na.rm=TRUE))*3)

#figuring out the points to start and finish each of the lines using the regression equation (y = intercept + slope*xvalue)
#-6 and 6 are the theoretical min and max in the data
Slowdifflinestart_accE <- Slowdiffint_accE+(Slowdiffslope_accE*Sminus3accEsd) #low differentiation line begin
Slowdifflineend_accE <- Slowdiffint_accE+(Slowdiffslope_accE*Splus3accEsd) #low differentiation line end
Slowdiffprestart_accE <- Slowdiffint_accE +(Slowdiffslope_accE*-6) #low differentiation pre-line segment begin 
Slowdiffpostend_accE <- Slowdiffint_accE +(Slowdiffslope_accE*6) #low differentiation post-line segment end
Shighdifflinestart_accE <- Shighdiffint_accE+(Shighdiffslope_accE*Sminus3accEsd) #low differentiation line begin
Shighdifflineend_accE <- Shighdiffint_accE+(Shighdiffslope_accE*Splus3accEsd) #low differentiation line end
Shighdiffprestart_accE <- Shighdiffint_accE +(Shighdiffslope_accE*-6) #low differentiation pre-line segment begin 
Shighdiffpostend_accE <- Shighdiffint_accE +(Shighdiffslope_accE*6) #low differentiation post-line segment end

#here is the plot code
SexamaccNAplot <-ggplot(examdata_postexam, aes(x = ScER_acceptance, y = Snegaff_composite)) +
  geom_point(aes(colour = ScRICCneg_after.fz), size = 0.5) +
  scale_colour_gradient2(low = "red", mid = "lightcyan", high = "blue") +
  labs(x = 'Acceptance (standardized\nand person-mean centered)', y = 'Negative emotion\n(standardized)', color="Emotion differentiation") +
  geom_segment(aes(x = Sminus3accEsd, y = Slowdifflinestart_accE, xend = Splus3accEsd, yend = Slowdifflineend_accE, linetype='Low emotion differentiation (-1SD)')) +
  geom_segment(aes(x = Sminus3accEsd, y = Shighdifflinestart_accE, xend = Splus3accEsd, yend = Shighdifflineend_accE, linetype='High emotion differentiation (+1SD)')) +
  geom_segment(x=-6, y=Slowdiffprestart_accE, xend = Sminus3accEsd, yend = Slowdifflinestart_accE, linetype = "dotted") +
  geom_segment(x=Splus3accEsd, y=Slowdifflineend_accE, xend = 6, yend = Slowdiffpostend_accE, linetype = "dotted") +
  geom_segment(x=-6, y=Shighdiffprestart_accE, xend = Sminus3accEsd, yend = Shighdifflinestart_accE, linetype = "dotted") +
  geom_segment(x=Splus3accEsd, y=Shighdifflineend_accE, xend = 6, yend = Shighdiffpostend_accE, linetype = "dotted") +
  scale_linetype_manual(values=c('longdash','solid'), 
                        breaks=c('Low emotion differentiation (-1SD)','High emotion differentiation (+1SD)'),
                        name='Simple Slopes')+ 
  scale_y_continuous(limits=c(min(examdata_postexam$Snegaff_composite,na.rm=TRUE),(max(examdata_postexam$Snegaff_composite, na.rm=TRUE))), expand = c(0, 0))+
  scale_x_continuous(limits=c(-4.48,6.54), expand = c(0,0))+ apatheme 

#Exporting plots into multi-panel figures#
png('standardized_simpleslopes_examdataNA_R1.png', units="cm", width=25, height=15, res=300)
ggarrange(SexamrumiNAplot, SexamdistNAplot, SexamaccNAplot, SexamsshNAplot,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
dev.off()

tiff('standardized_simpleslopes_examdataNA_highres_R1.tiff', units="cm", width=25, height=15, res=300)
ggarrange(SexamrumiNAplot, SexamdistNAplot, SexamaccNAplot, SexamsshNAplot,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
dev.off()