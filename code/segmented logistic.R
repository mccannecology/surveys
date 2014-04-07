############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA #
# Segmented logistic regression            #
# 2 methods: manual or package             #
# Data: dataFPsmall (n=54)                 #
# Waterbodies <5 ha with FP present        #
# MJM 2/25/2014                            #
############################################

library(ggplot2)
library(segmented)

#################################
# segmented logistic regression #
# dataONEperpond                #
# link = logit                  #
# via iterative searching       #
#################################
breaks <- dataONEperpond$TOTP_avg[which(dataONEperpond$TOTP_avg >= 0.00001 & dataONEperpond$TOTP_avg <= 0.5)]    # create a vector to hold potential breakpoints 

mse <- numeric(length(breaks)) # create a blank vector to hold MSE     

for(i in 1:length(breaks)){ # loop over all of the potential breakpoints & actually try them out in a lm()
  piecewise <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg < breaks[i]) + TOTP_avg*(TOTP_avg>=breaks[i]), family=binomial(link=logit), data=dataONEperpond)
  mse[i] <- summary(piecewise)[4] # If this is a lm() I should index [6], if it's a glm() I should index [4]
}

mse <- as.numeric(mse) # converts list to numeric 
breakpoint<-breaks[which(mse==min(mse))] # picks the breakpoint with the lowest mse
breakpoint # returns the breakpoint 

# re-run the glm() using this breakpoint 
segmented_dataONEperpond_binomial_logit <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg<breakpoint) + TOTP_avg*(TOTP_avg>=breakpoint), family=binomial(link=logit), data=dataONEperpond)
summary(segmented_dataONEperpond_binomial_logit)     
-2*logLik(segmented_dataONEperpond_binomial_logit)[1]+2*5
breakpoint

# Add new variables to the data frame 
# used to plot the segmented data in ggplot2
dataONEperpond$breakpoint <- ifelse(dataONEperpond$TOTP_avg <= breakpoint, "below", "above")

rm(breaks,mse,breakpoint,piecewise) # clean up your workspace 

#################################
# segmented logistic regression #
# dataFP                        #
# link = logit                  #
# via iterative searching       #
#################################
breaks <- dataFP$TOTP_avg[which(dataFP$TOTP_avg >= 0.00001 & dataFP$TOTP_avg <= 0.5)]    # create a vector to hold potential breakpoints 

mse <- numeric(length(breaks)) # create a blank vector to hold MSE     

for(i in 1:length(breaks)){ # loop over all of the potential breakpoints & actually try them out in a lm()
  piecewise <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg < breaks[i]) + TOTP_avg*(TOTP_avg>=breaks[i]), family=binomial(link=logit), data=dataFP)
  mse[i] <- summary(piecewise)[4] # If this is a lm() I should index [6], if it's a glm() I should index [4]
}

mse <- as.numeric(mse) # converts list to numeric 
breakpoint<-breaks[which(mse==min(mse))] # picks the breakpoint with the lowest mse
breakpoint # returns the breakpoint 

# re-run the glm() using this breakpoint 
segmented_dataFP_binomial_logit <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg<breakpoint) + TOTP_avg*(TOTP_avg>=breakpoint), family=binomial(link=logit), data=dataFP)
summary(segmented_dataFP_binomial_logit)     
-2*logLik(segmented_dataFP_binomial_logit)[1]+2*5
breakpoint

# Add new variables to the data frame 
# used to plot the segmented data in ggplot2
dataFP$breakpoint <- ifelse(dataFP$TOTP_avg <= breakpoint, "below", "above")

rm(breaks,mse,breakpoint,piecewise) # clean up your workspace 

#################################
# segmented logistic regression #
# dataFPsmall                   #
# link = logit                  #
# via iterative searching       #
#################################
breaks <- dataFPsmall$TOTP_avg[which(dataFPsmall$TOTP_avg >= 0.00001 & dataFPsmall$TOTP_avg <= 0.5)]    # create a vector to hold potential breakpoints 

mse <- numeric(length(breaks)) # create a blank vector to hold MSE     

for(i in 1:length(breaks)){ # loop over all of the potential breakpoints & actually try them out in a lm()
  piecewise <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg < breaks[i]) + TOTP_avg*(TOTP_avg>=breaks[i]), family=binomial(link=logit), data=dataFPsmall)
  mse[i] <- summary(piecewise)[4] # If this is a lm() I should index [6], if it's a glm() I should index [4]
}

mse <- as.numeric(mse) # converts list to numeric 
breakpoint<-breaks[which(mse==min(mse))] # picks the breakpoint with the lowest mse
breakpoint # returns the breakpoint 

# re-run the glm() using this breakpoint 
segmented_dataFPsmall_binomial_logit <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg<breakpoint) + TOTP_avg*(TOTP_avg>=breakpoint), family=binomial(link=logit), data=dataFPsmall)
summary(segmented_dataFPsmall_binomial_logit)     
-2*logLik(segmented_dataFPsmall_binomial_logit)[1]+2*5
breakpoint

# Add new variables to the data frame 
# used to plot the segmented data in ggplot2
dataFPsmall$breakpoint <- ifelse(dataFPsmall$TOTP_avg <= breakpoint, "below", "above")

rm(breaks,mse,breakpoint,piecewise) # clean up your workspace

#################################
# segmented logistic regression #
# dataFPoutliers                #
# link = logit                  #
# via iterative searching       #
#################################
breaks <- dataFPoutliers$TOTP_avg[which(dataFPoutliers$TOTP_avg >= 0.00001 & dataFPoutliers$TOTP_avg <= 0.5)]    # create a vector to hold potential breakpoints 

mse <- numeric(length(breaks)) # create a blank vector to hold MSE     

for(i in 1:length(breaks)){ # loop over all of the potential breakpoints & actually try them out in a lm()
  piecewise <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg < breaks[i]) + TOTP_avg*(TOTP_avg>=breaks[i]), family=binomial(link=logit), data=dataFPoutliers)
  mse[i] <- summary(piecewise)[4] # If this is a lm() I should index [6], if it's a glm() I should index [4]
}

mse <- as.numeric(mse) # converts list to numeric 
breakpoint<-breaks[which(mse==min(mse))] # picks the breakpoint with the lowest mse
breakpoint # returns the breakpoint 

# re-run the glm() using this breakpoint 
segmented_dataFPoutliers_binomial_logit <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg<breakpoint) + TOTP_avg*(TOTP_avg>=breakpoint), family=binomial(link=logit), data=dataFPoutliers)
summary(segmented_dataFPoutliers_binomial_logit)     
-2*logLik(segmented_dataFPoutliers_binomial_logit)[1]+2*5
breakpoint

# Add new variables to the data frame 
# used to plot the segmented data in ggplot2
dataFPoutliers$breakpoint <- ifelse(dataFPoutliers$TOTP_avg <= breakpoint, "below", "above")

rm(breaks,mse,breakpoint,piecewise) # clean up your workspace

#################################
# segmented logistic regression #
# dataFPoutlierssmall           #
# link = logit                  #
# via iterative searching       #
#################################
breaks <- dataFPoutlierssmall$TOTP_avg[which(dataFPoutlierssmall$TOTP_avg >= 0.00001 & dataFPoutlierssmall$TOTP_avg <= 0.5)]    # create a vector to hold potential breakpoints 

mse <- numeric(length(breaks)) # create a blank vector to hold MSE     

for(i in 1:length(breaks)){ # loop over all of the potential breakpoints & actually try them out in a lm()
  piecewise <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg < breaks[i]) + TOTP_avg*(TOTP_avg>=breaks[i]), family=binomial(link=logit), data=dataFPoutlierssmall)
  mse[i] <- summary(piecewise)[4] # If this is a lm() I should index [6], if it's a glm() I should index [4]
}

mse <- as.numeric(mse) # converts list to numeric 
breakpoint<-breaks[which(mse==min(mse))] # picks the breakpoint with the lowest mse
breakpoint # returns the breakpoint 

# re-run the glm() using this breakpoint 
segmented_dataFPoutlierssmall_binomial_logit <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg<breakpoint) + TOTP_avg*(TOTP_avg>=breakpoint), family=binomial(link=logit), data=dataFPoutlierssmall)
summary(segmented_dataFPoutlierssmall_binomial_logit)     
-2*logLik(segmented_dataFPoutlierssmall_binomial_logit)[1]+2*5
breakpoint

# Add new variables to the data frame 
# used to plot the segmented data in ggplot2
dataFPoutlierssmall$breakpoint <- ifelse(dataFPoutlierssmall$TOTP_avg <= breakpoint, "below", "above")

rm(breaks,mse,breakpoint,piecewise) # clean up your workspace

#################################
# segmented logistic regression #
# dataONEperpond                #
# link = probit                  #
# via iterative searching       #
#################################
breaks <- dataONEperpond$TOTP_avg[which(dataONEperpond$TOTP_avg >= 0.00001 & dataONEperpond$TOTP_avg <= 0.5)]    # create a vector to hold potential breakpoints 

mse <- numeric(length(breaks)) # create a blank vector to hold MSE     

for(i in 1:length(breaks)){ # loop over all of the potential breakpoints & actually try them out in a lm()
  piecewise <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg < breaks[i]) + TOTP_avg*(TOTP_avg>=breaks[i]), family=binomial(link=probit), data=dataONEperpond)
  mse[i] <- summary(piecewise)[4] # If this is a lm() I should index [6], if it's a glm() I should index [4]
}

mse <- as.numeric(mse) # converts list to numeric 
breakpoint<-breaks[which(mse==min(mse))] # picks the breakpoint with the lowest mse
breakpoint # returns the breakpoint 

# re-run the glm() using this breakpoint 
segmented_dataONEperpond_binomial_probit <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg<breakpoint) + TOTP_avg*(TOTP_avg>=breakpoint), family=binomial(link=probit), data=dataONEperpond)
summary(segmented_dataONEperpond_binomial_probit)     
-2*logLik(segmented_dataONEperpond_binomial_probit)[1]+2*5
breakpoint

# Add new variables to the data frame 
# used to plot the segmented data in ggplot2
dataONEperpond$breakpoint <- ifelse(dataONEperpond$TOTP_avg <= breakpoint, "below", "above")

rm(breaks,mse,breakpoint,piecewise) # clean up your workspace 

#################################
# segmented logistic regression #
# dataFP                        #
# link = probit                  #
# via iterative searching       #
#################################
breaks <- dataFP$TOTP_avg[which(dataFP$TOTP_avg >= 0.00001 & dataFP$TOTP_avg <= 0.5)]    # create a vector to hold potential breakpoints 

mse <- numeric(length(breaks)) # create a blank vector to hold MSE     

for(i in 1:length(breaks)){ # loop over all of the potential breakpoints & actually try them out in a lm()
  piecewise <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg < breaks[i]) + TOTP_avg*(TOTP_avg>=breaks[i]), family=binomial(link=probit), data=dataFP)
  mse[i] <- summary(piecewise)[4] # If this is a lm() I should index [6], if it's a glm() I should index [4]
}

mse <- as.numeric(mse) # converts list to numeric 
breakpoint<-breaks[which(mse==min(mse))] # picks the breakpoint with the lowest mse
breakpoint # returns the breakpoint 

# re-run the glm() using this breakpoint 
segmented_dataFP_binomial_probit <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg<breakpoint) + TOTP_avg*(TOTP_avg>=breakpoint), family=binomial(link=probit), data=dataFP)
summary(segmented_dataFP_binomial_probit)     
-2*logLik(segmented_dataFP_binomial_probit)[1]+2*5
breakpoint

# Add new variables to the data frame 
# used to plot the segmented data in ggplot2
dataFP$breakpoint <- ifelse(dataFP$TOTP_avg <= breakpoint, "below", "above")

rm(breaks,mse,breakpoint,piecewise) # clean up your workspace 

#################################
# segmented logistic regression #
# dataFPsmall                   #
# link = probit                  #
# via iterative searching       #
#################################
breaks <- dataFPsmall$TOTP_avg[which(dataFPsmall$TOTP_avg >= 0.00001 & dataFPsmall$TOTP_avg <= 0.5)]    # create a vector to hold potential breakpoints 

mse <- numeric(length(breaks)) # create a blank vector to hold MSE     

for(i in 1:length(breaks)){ # loop over all of the potential breakpoints & actually try them out in a lm()
  piecewise <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg < breaks[i]) + TOTP_avg*(TOTP_avg>=breaks[i]), family=binomial(link=probit), data=dataFPsmall)
  mse[i] <- summary(piecewise)[4] # If this is a lm() I should index [6], if it's a glm() I should index [4]
}

mse <- as.numeric(mse) # converts list to numeric 
breakpoint<-breaks[which(mse==min(mse))] # picks the breakpoint with the lowest mse
breakpoint # returns the breakpoint 

# re-run the glm() using this breakpoint 
segmented_dataFPsmall_binomial_probit <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg<breakpoint) + TOTP_avg*(TOTP_avg>=breakpoint), family=binomial(link=probit), data=dataFPsmall)
summary(segmented_dataFPsmall_binomial_probit)     
-2*logLik(segmented_dataFPsmall_binomial_probit)[1]+2*5
breakpoint

# Add new variables to the data frame 
# used to plot the segmented data in ggplot2
dataFPsmall$breakpoint <- ifelse(dataFPsmall$TOTP_avg <= breakpoint, "below", "above")

rm(breaks,mse,breakpoint,piecewise) # clean up your workspace

#################################
# segmented logistic regression #
# dataFPoutliers                #
# link = probit                  #
# via iterative searching       #
#################################
breaks <- dataFPoutliers$TOTP_avg[which(dataFPoutliers$TOTP_avg >= 0.00001 & dataFPoutliers$TOTP_avg <= 0.5)]    # create a vector to hold potential breakpoints 

mse <- numeric(length(breaks)) # create a blank vector to hold MSE     

for(i in 1:length(breaks)){ # loop over all of the potential breakpoints & actually try them out in a lm()
  piecewise <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg < breaks[i]) + TOTP_avg*(TOTP_avg>=breaks[i]), family=binomial(link=probit), data=dataFPoutliers)
  mse[i] <- summary(piecewise)[4] # If this is a lm() I should index [6], if it's a glm() I should index [4]
}

mse <- as.numeric(mse) # converts list to numeric 
breakpoint<-breaks[which(mse==min(mse))] # picks the breakpoint with the lowest mse
breakpoint # returns the breakpoint 

# re-run the glm() using this breakpoint 
segmented_dataFPoutliers_binomial_probit <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg<breakpoint) + TOTP_avg*(TOTP_avg>=breakpoint), family=binomial(link=probit), data=dataFPoutliers)
summary(segmented_dataFPoutliers_binomial_probit)     
-2*logLik(segmented_dataFPoutliers_binomial_probit)[1]+2*5
breakpoint

# Add new variables to the data frame 
# used to plot the segmented data in ggplot2
dataFPoutliers$breakpoint <- ifelse(dataFPoutliers$TOTP_avg <= breakpoint, "below", "above")

rm(breaks,mse,breakpoint,piecewise) # clean up your workspace

#################################
# segmented logistic regression #
# dataFPoutlierssmall           #
# link = probit                  #
# via iterative searching       #
#################################
breaks <- dataFPoutlierssmall$TOTP_avg[which(dataFPoutlierssmall$TOTP_avg >= 0.00001 & dataFPoutlierssmall$TOTP_avg <= 0.5)]    # create a vector to hold potential breakpoints 

mse <- numeric(length(breaks)) # create a blank vector to hold MSE     

for(i in 1:length(breaks)){ # loop over all of the potential breakpoints & actually try them out in a lm()
  piecewise <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg < breaks[i]) + TOTP_avg*(TOTP_avg>=breaks[i]), family=binomial(link=probit), data=dataFPoutlierssmall)
  mse[i] <- summary(piecewise)[4] # If this is a lm() I should index [6], if it's a glm() I should index [4]
}

mse <- as.numeric(mse) # converts list to numeric 
breakpoint<-breaks[which(mse==min(mse))] # picks the breakpoint with the lowest mse
breakpoint # returns the breakpoint 

# re-run the glm() using this breakpoint 
segmented_dataFPoutlierssmall_binomial_probit <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg<breakpoint) + TOTP_avg*(TOTP_avg>=breakpoint), family=binomial(link=probit), data=dataFPoutlierssmall)
summary(segmented_dataFPoutlierssmall_binomial_probit)     
-2*logLik(segmented_dataFPoutlierssmall_binomial_probit)[1]+2*5
breakpoint

# Add new variables to the data frame 
# used to plot the segmented data in ggplot2
dataFPoutlierssmall$breakpoint <- ifelse(dataFPoutlierssmall$TOTP_avg <= breakpoint, "below", "above")

rm(breaks,mse,breakpoint,piecewise) # clean up your workspace
























############################################ OLD STUFF #################################################3

#################################
# segmented logistic regression #
# with package: segmented       #
#################################
segmented.glmFPsmallTOTPbinomial <- segmented(glmFPsmallTOTPbinomial, seg.Z=~TOTP_avg, psi=list(TOTP_avg=c(0.05)), data = dataFPsmall)
summary(segmented.glmFPsmallTOTPbinomial)

# U1.TOTP_avg is not the slop of the second segment 
# it is the difference in slopes between the 1st and 2nd segments 

# so instead, you need to extract these manually: 
slope(segmented.glmFPsmallTOTPbinomial)
intercept(segmented.glmFPsmallTOTPbinomial)

# You have to manually enter the breakpoint from the summary here 
breakpoint01 <- 0.019810
# manually definte slope & intercept for 1st line 
intercept01 <- -10.8300
slope01 <- 518.800
# manually definte slope & intercept for 2nd line 
intercept02 <- -0.6459
slope02 <- 5.012

# define the functions that will be used to plot this on a ggplot2 object
# Make these functions not return any values if they are outside of the range specified by the breakpoint 
segment1 <- function(x) ifelse(x<breakpoint, (100 * (exp(intercept01 + slope01*x)) / (1 + exp(intercept01 + slope01*x))), NA)
segment2 <- function(x) ifelse(x>=breakpoint, (100 * (exp(intercept02 + slope02*x)) / (1 + exp(intercept02 + slope02*x))), NA)

# need a function that plots the SE for this line 

# model fit
AIC(segmented.glmFPsmallTOTPbinomial)

# Add new variables to the data frame just for plotting the segmented data in ggplot2
# segmented data: TOTP_avg < breakpoint01 and TOTP_avg >= breakpoint01 
dataFPsmall$breakpoint01 <- ifelse(dataFPsmall$TOTP_avg <= breakpoint01, "below", "above")

stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(breakpoint01)))


#################################
# segmented logistic regression #
# via iterative searching       #
#################################
breaks <- dataFPsmall$TOTP_avg[which(dataFPsmall$TOTP_avg >= 0.00001 & dataFPsmall$TOTP_avg <= 0.2)]    # create a vector to hold potential breakpoints 

mse <- numeric(length(breaks)) # create a blank vector to hold MSE     

for(i in 1:length(breaks)){ # loop over all of the potential breakpoints & actually try them out in a lm()
  piecewise <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg < breaks[i]) + TOTP_avg*(TOTP_avg>=breaks[i]), family = binomial, data=dataFPsmall)
  mse[i] <- summary(piecewise)[4] # If this is a lm() I should index [6], if it's a glm() I should index [4]
}

mse <- as.numeric(mse) # converts list to numeric 

breakpoint02<-breaks[which(mse==min(mse))] # picks the breakpoint with the lowest mse
breakpoint02

# Add new variables to the data frame 
# used to plot the segmented data in ggplot2
dataFPsmall$breakpoint02 <- ifelse(dataFPsmall$TOTP_avg <= breakpoint02, "below", "above")

# re-run the glm() using this breakpoint 
segmented.glmFPsmallTOTPbinomial2 <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg<breakpoint02) + TOTP_avg*(TOTP_avg>=breakpoint02), family=binomial, data=dataFPsmall)
summary(segmented.glmFPsmallTOTPbinomial)     
rm(breaks,mse,breakpoint02,piecewise) # clean up your workspace 

# Look at each segment separately 

# segment 1
dataFPsmallseg1 <- subset(dataFPsmall, TOTP_avg < 0.03698)
segmented.glmFPsmallTOTPbinomial.seg1 <- glm(FPcover_max ~ TOTP_avg, family=binomial, data=dataFPsmallseg1)
summary(segmented.glmFPsmallTOTPbinomial.seg1)     

# segment 2
dataFPsmallseg2 <- subset(dataFPsmall, TOTP_avg >= 0.03698)
segmented.glmFPsmallTOTPbinomial.seg2 <- glm(FPcover_max ~ TOTP_avg, family=binomial, data=dataFPsmallseg2)
summary(segmented.glmFPsmallTOTPbinomial.seg2)     




