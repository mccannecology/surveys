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

# plot it 
ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point()

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
breakpoint <- 0.019810
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
# segmented data: TOTP_avg < breakpoint and TOTP_avg >= breakpoint 
dataFPsmall$breakpoint <- ifelse(dataFPsmall$TOTP_avg <= breakpoint, "below", "above")

stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(breakpoint)))


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
breakpoint<-breaks[which(mse==min(mse))] # picks the breakpoint with the lowest mse

# re-run the glm() using this breakpoint 
segmented.glmFPsmallTOTPbinomial.ver2 <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg<breakpoint) + TOTP_avg*(TOTP_avg>=breakpoint), family=binomial, data=dataFPsmall)
summary(segmented.glmFPsmallTOTPbinomial.ver2)     
rm(breaks,mse,breakpoint,piecewise) # clean up your workspace 