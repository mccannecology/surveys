##############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA   #
# Fitting and comparing models               #
# UPDATED: 02/24/2014 by MJ McCann           #
##############################################

# FIRST: Import your datasets - use script "importing XX-XX-20XX.R" 
# This script has been cleaned up (unncessary analyses done in early January 2014 were removed)
# Extra exploratory analyses (e.g., other glms, gams, loess, beta regression, etc.) can be found in script "fitting models xx-xx-20xx.R" 
# Sample code for segmented regression (via iterative searching) can be found here:
# http://climateecology.wordpress.com/2012/08/19/r-for-ecologists-putting-together-a-piecewise-regression/

setwd("C:/Users/Mike/Desktop/Dropbox/CT & LI  Duckweed Surveys/Survey Analysis All") # Set working directory 

###############################################################################################
# LOAD SOME PACKAGES 
###############################################################################################
library(ggplot2) # plotting 
library(segmented) # piecewise regression - segments must be nearly continuous - one segment starts where other leaves off 
library(strucchange) # breakpoint/changepoint detection 
library(tree)

#############################################################################################################
#                                                                                                           #
#                                                                                                           #
#                                   FITTING MODELS: FPcover_max & TOTP                                      #
#                                                                                                           #
#                                                                                                           #
#############################################################################################################

##########################
# for dataFPsmall (n=54) #
##########################
# plot it 
ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point()

# linear regression
lmFPsmallTOTP <- lm(FPcover_max ~ TOTP_avg, data=dataFPsmall)
ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() + ylim(0,1) + geom_smooth(method="lm",col="blue") # plot it 
summary(lmFPsmallTOTP)
AIC(lmFPsmallTOTP)

# logistic regression (generalized linear model, family = binomial)
glmFPsmallTOTPbinomial <- glm(FPcover_max ~ TOTP_avg, family=binomial, data=dataFPsmall)
summary(glmFPsmallTOTPbinomial)
ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() + ylim(0,1) + geom_smooth(method="glm", family=binomial,col="red")
# same plot but without standard error 
ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() + ylim(0,1) + stat_smooth(method="glm", family=binomial,col="red",se=F)

# plot it to compare logistic and linear fitted models 
ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() + ylim(0,1) + geom_smooth(method="glm", family=binomial,col="red") + geom_smooth(method="lm",col="blue")

# segmented linear regression, with package: segmented
segmented.lmFPsmallTOTP <- segmented(lmFPsmallTOTP, seg.Z=~TOTP_avg, psi=list(TOTP_avg=c(0.05)), data = dataFPsmall)
summary(segmented.lmFPsmallTOTP)
AIC(segmented.lmFPsmallTOTP)
# plotting the segmented model
plot(FPcover_max ~ TOTP_avg,data=dataFPsmall)
plot(segmented.lmFPsmallTOTP, add=T, conf.level=0.95, shade=F, rug=F)

# segmented linear regression, via iterative searching
breaks <- dataFPsmall$TOTP_avg[which(dataFPsmall$TOTP_avg >= 0.00001 & dataFPsmall$TOTP_avg <= 0.2)]    # create a vector to hold potential breakpoints 
mse <- numeric(length(breaks)) # create a blank vector to hold MSE     
for(i in 1:length(breaks)){ # loop over all of the potential breakpoints & actually try them out in a lm()
  piecewise <- lm(FPcover_max ~ TOTP_avg*(TOTP_avg < breaks[i]) + TOTP_avg*(TOTP_avg>=breaks[i]), data=dataFPsmall)
  mse[i] <- summary(piecewise)[6] # If this is a lm() I should index [6], if it's a glm() I should index [4]
}
mse <- as.numeric(mse) # converts list to numeric 
breakpoint<-breaks[which(mse==min(mse))] # picks the breakpoint with the lowest mse
# re-run the glm() using this breakpoint 
segmented.lmFPsmallTOTP.ver2 <- lm(FPcover_max ~ TOTP_avg*(TOTP_avg<breakpoint) + TOTP_avg*(TOTP_avg>=breakpoint), data=dataFPsmall)
summary(segmented.lmFPsmallTOTP.ver2)     
AIC(segmented.lmFPsmallTOTP.ver2)
plot(dataFPsmall$TOTP_avg, dataFPsmall$FPcover_max)
x<-dataFPsmall$TOTP_avg
curve((0.7902-.3562)+(0.1500-1.4854)*x, add=T,from=0,to=breakpoint) 
curve((0.7902)+(.1500)*x, add=T,from=breakpoint,to=max(dataFPsmall$TOTP_avg,na.rm=T))
abline(v=breakpoint,lty=3)
rm(breaks,mse,breakpoint,piecewise) # clean up your workspace 

# segmented logistic regression, with package: segmented
segmented.glmFPsmallTOTPbinomial <- segmented(glmFPsmallTOTPbinomial, seg.Z=~TOTP_avg, psi=list(TOTP_avg=c(0.05)), data = dataFPsmall)
summary(segmented.glmFPsmallTOTPbinomial)
AIC(segmented.glmFPsmallTOTPbinomial)
# plotting the segmented model
plot(FPcover_max ~ TOTP_avg,data=dataFPsmall)
plot(segmented.glmFPsmallTOTPbinomial, add=T, conf.level=0.95, shade=F, rug=F)

# segmented logistic regression, via iterative searching
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

# threshold, with package: strucchange 
breakpointFPsmallTOTP <- breakpoints(FPcover_max ~ TOTP_avg, h=5, data=dataFPsmall)
summary(breakpointFPsmallTOTP)

# threshold, with package: tree 
treeFPsmallTOTP <- tree(dataFPsmall$FPcover_max ~ dataFPsmall$TOTP_avg)
treeFPsmallTOTP

# Add new variables to the data frame just for plotting the partitioned data in ggplot2
# partition data: FPcover_max >= 0.5 and FPcover_max < 0.5 
dataFPsmall$half <- ifelse(dataFPsmall$FPcover_max >= 0.5, "greater", "lesser")
# partition data: FPcover_max >= 0.6666 and FPcover_max < 0.6666 
dataFPsmall$third <- ifelse(dataFPsmall$FPcover_max >= 0.6666, "greater", "lesser")

# partitioned 50:50 y-variable + linear
lmFPsmallupper50 <- lm(FPcover_max ~ TOTP_avg, data = subset(dataFPsmall, FPcover_max >= 0.50)) 
lmFPsmalllower50 <- lm(FPcover_max ~ TOTP_avg, data = subset(dataFPsmall, FPcover_max < 0.50)) 
summary(lmFPsmallupper50)
summary(lmFPsmalllower50)
ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() + stat_smooth(method=lm, aes(fill=factor(half)))

# partitioned 33:67 y-variable + linear
lmFPsmallupperthird <- lm(FPcover_max ~ TOTP_avg, data = subset(dataFPsmall, FPcover_max >= 0.6666)) 
lmFPsmalllowerthird <- lm(FPcover_max ~ TOTP_avg, data = subset(dataFPsmall, FPcover_max < 0.6666)) 
summary(lmFPsmallupperthird)
summary(lmFPsmalllowerthird)
ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() + stat_smooth(method=lm, aes(fill=factor(third)))

# partitioned 50:50 y-variable + logistic 
glmFPsmallupper50 <- glm(FPcover_max ~ TOTP_avg, family = binomial, data = subset(dataFPsmall, FPcover_max >= 0.50)) 
glmFPsmalllower50 <- glm(FPcover_max ~ TOTP_avg, family = binomial, data = subset(dataFPsmall, FPcover_max < 0.50)) 
summary(glmFPsmallupper50)
summary(glmFPsmalllower50)
ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() + stat_smooth(method=glm, family=binomial, aes(fill=factor(half)))

# partitioned 33:67 y-variable + linear
glmFPsmallupperthird <- glm(FPcover_max ~ TOTP_avg, family = binomial, data = subset(dataFPsmall, FPcover_max >= 0.6666)) 
glmFPsmalllowerthird <- glm(FPcover_max ~ TOTP_avg, family = binomial, data = subset(dataFPsmall, FPcover_max < 0.6666)) 
summary(glmFPsmallupperthird)
summary(glmFPsmalllowerthird)
ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() + stat_smooth(method=glm, family=binomial, aes(fill=factor(third)))

##################################
# for dataFPoutlierssmall (n=52) #
##################################
# plot it 
ggplot(dataFPoutlierssmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point()

# linear regression
lmFPoutlierssmallTOTP <- lm(FPcover_max ~ TOTP_avg, data=dataFPoutlierssmall)
ggplot(dataFPoutlierssmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() + ylim(0,1) + geom_smooth(method="lm",col="blue") # plot it 
summary(lmFPoutlierssmallTOTP)
AIC(lmFPoutlierssmallTOTP)

# logistic regression (generalized linear model, family = binomial)
glmFPoutlierssmallTOTPbinomial <- glm(FPcover_max ~ TOTP_avg, family=binomial, data=dataFPoutlierssmall)
ggplot(dataFPoutlierssmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() + ylim(0,1) + geom_smooth(method="glm", family=binomial,col="red")
summary(glmFPoutlierssmallTOTPbinomial)

# plot it to compare logistic and linear fitted models 
Z <- ggplot(dataFPoutlierssmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() + ylim(0,1) + geom_smooth(method="glm", family=binomial,col="red") + geom_smooth(method="lm",col="blue")
Z <- Z + xlab("Total phosphorus (mg/L)") + ylab("Proportion floating plant cover")
Z
ggsave(file="total phosphorus and floating plant cover - linear vs logistic regression - dataFPoutlierssmall.jpg")

# segmented linear regression, with package: segmented
segmented.lmFPoutlierssmallTOTP <- segmented(lmFPoutlierssmallTOTP, seg.Z=~TOTP_avg, psi=list(TOTP_avg=c(0.05)), data = dataFPoutlierssmall)
summary(segmented.lmFPoutlierssmallTOTP)
AIC(segmented.lmFPoutlierssmallTOTP)
# plotting the segmented model
plot(FPcover_max ~ TOTP_avg,data=dataFPoutlierssmall)
plot(segmented.lmFPoutlierssmallTOTP, add=T, conf.level=0.95, shade=F, rug=F)

# segmented linear regression, via iterative searching of breakpoints with the lowest residual MSE 
breaks <- dataFPoutlierssmall$TOTP_avg[which(dataFPoutlierssmall$TOTP_avg >= 0.00001 & dataFPoutlierssmall$TOTP_avg <= 0.2)]    # create a vector to hold potential breakpoints 
mse <- numeric(length(breaks)) # create a blank vector to hold MSE     
for(i in 1:length(breaks)){ # loop over all of the potential breakpoints & actually try them out in a lm()
  piecewise <- lm(FPcover_max ~ TOTP_avg*(TOTP_avg < breaks[i]) + TOTP_avg*(TOTP_avg>=breaks[i]), data=dataFPoutlierssmall)
  mse[i] <- summary(piecewise)[6] # If this is a lm() I should index [6], if it's a glm() I should index [4]
}
mse <- as.numeric(mse) # converts list to numeric 
breakpoint<-breaks[which(mse==min(mse))] # picks the breakpoint with the lowest mse
# re-run the glm() using this breakpoint 
segmented.lmFPoutlierssmallTOTP.ver2 <- lm(FPcover_max ~ TOTP_avg*(TOTP_avg<breakpoint) + TOTP_avg*(TOTP_avg>=breakpoint), data=dataFPoutlierssmall)
summary(segmented.lmFPoutlierssmallTOTP.ver2)     
AIC(segmented.lmFPoutlierssmallTOTP.ver2)     
plot(dataFPoutlierssmall$TOTP_avg, dataFPoutlierssmall$FPcover_max)
x<-dataFPoutlierssmall$TOTP_avg
curve((0.1688-.4619)+(2.0910+29.6998)*x, add=T,from=0,to=breakpoint) 
curve((0.1688)+(2.0910)*x, add=T,from=breakpoint,to=max(dataFPoutlierssmall$TOTP_avg,na.rm=T))
abline(v=breakpoint,lty=3)
rm(breaks,mse,breakpoint,piecewise,x) # clean up your workspace 

# segmented logistic regression, with package: segmented
segmented.glmFPoutlierssmallTOTPbinomial <- segmented(glmFPoutlierssmallTOTPbinomial, seg.Z=~TOTP_avg, psi=list(TOTP_avg=c(0.05)), data = dataFPoutlierssmall)
summary(segmented.glmFPoutlierssmallTOTPbinomial)
# plotting the segmented model
plot(FPcover_max ~ TOTP_avg,data=dataFPoutlierssmall)
plot(segmented.glmFPoutlierssmallTOTPbinomial, add=T, conf.level=0.95, shade=F, rug=F)

# segmented logistic regression, via iterative searching
breaks <- dataFPoutlierssmall$TOTP_avg[which(dataFPoutlierssmall$TOTP_avg >= 0.00001 & dataFPoutlierssmall$TOTP_avg <= 0.2)]    # create a vector to hold potential breakpoints 
mse <- numeric(length(breaks)) # create a blank vector to hold MSE     
for(i in 1:length(breaks)){ # loop over all of the potential breakpoints & actually try them out in a lm()
  piecewise <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg < breaks[i]) + TOTP_avg*(TOTP_avg>=breaks[i]), family = binomial, data=dataFPoutlierssmall)
  mse[i] <- summary(piecewise)[4] # If this is a lm() I should index [6], if it's a glm() I should index [4]
}
mse <- as.numeric(mse) # converts list to numeric 
breakpoint<-breaks[which(mse==min(mse))] # picks the breakpoint with the lowest mse
# re-run the glm() using this breakpoint 
segmented.glmFPoutlierssmallTOTPbinomial.ver2 <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg<breakpoint) + TOTP_avg*(TOTP_avg>=breakpoint), family=binomial, data=dataFPoutlierssmall)
summary(segmented.glmFPoutlierssmallTOTPbinomial.ver2)     
rm(breaks,mse,breakpoint,piecewise) # clean up your workspace 

# threshold, with package: strucchange 
breakpointFPoutlierssmallTOTP <- breakpoints(FPcover_max ~ TOTP_avg, h=5, data=dataFPoutlierssmall)
summary(breakpointFPoutlierssmallTOTP)

# threshold, with package: tree 
treeFPoutlierssmallTOTP <- tree(dataFPoutlierssmall$FPcover_max ~ dataFPoutlierssmall$TOTP_avg)
treeFPoutlierssmallTOTP

# Add new variables to the data frame just for plotting the partitioned data in ggplot2
# partition data: FPcover_max >= 0.5 and FPcover_max < 0.5 
dataFPoutlierssmall$half <- ifelse(dataFPoutlierssmall$FPcover_max >= 0.5, "greater", "lesser")
# partition data: FPcover_max >= 0.6666 and FPcover_max < 0.6666 
dataFPoutlierssmall$third <- ifelse(dataFPoutlierssmall$FPcover_max >= 0.6666, "greater", "lesser")

# partitioned 50:50 y-variable + linear
lmFPsmalloutliersupper50 <- lm(FPcover_max ~ TOTP_avg, data = subset(dataFPoutlierssmall, FPcover_max >= 0.50)) 
lmFPsmalloutlierslower50 <- lm(FPcover_max ~ TOTP_avg, data = subset(dataFPoutlierssmall, FPcover_max < 0.50)) 
summary(lmFPsmalloutliersupper50)
summary(lmFPsmalloutlierslower50)
ggplot(dataFPoutlierssmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() + stat_smooth(method=lm, aes(fill=factor(half)))

# partitioned 33:67 y-variable + linear
lmFPsmalloutliersupperthird <- lm(FPcover_max ~ TOTP_avg, data = subset(dataFPoutlierssmall, FPcover_max >= 0.6666)) 
lmFPsmalloutlierslowerthird <- lm(FPcover_max ~ TOTP_avg, data = subset(dataFPoutlierssmall, FPcover_max < 0.6666)) 
summary(lmFPsmalloutliersupperthird)
summary(lmFPsmalloutlierslowerthird)
ggplot(dataFPoutlierssmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() + stat_smooth(method=lm, aes(fill=factor(third)))
# plot it again but with a limit on the y-variable at 1
ggplot(dataFPoutlierssmall,aes(x=TOTP_avg,y=FPcover_max)) + ylim(0,1) + geom_point() + stat_smooth(method=lm, aes(fill=factor(third)))

# partitioned 50:50 y-variable + logistic 
glmFPsmalloutliersupper50 <- glm(FPcover_max ~ TOTP_avg, family = binomial, data = subset(dataFPoutlierssmall, FPcover_max >= 0.50)) 
glmFPsmalloutlierslower50 <- glm(FPcover_max ~ TOTP_avg, family = binomial, data = subset(dataFPoutlierssmall, FPcover_max < 0.50)) 
summary(glmFPsmalloutliersupper50)
summary(glmFPsmalloutlierslower50)
ggplot(dataFPoutlierssmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() + stat_smooth(method=glm, family=binomial, aes(fill=factor(half)))

# partitioned 33:67 y-variable + logistic
glmFPsmalloutliersupperthird <- glm(FPcover_max ~ TOTP_avg, family = binomial, data = subset(dataFPoutlierssmall, FPcover_max >= 0.6666)) 
glmFPsmalloutlierslowerthird <- glm(FPcover_max ~ TOTP_avg, family = binomial, data = subset(dataFPoutlierssmall, FPcover_max < 0.6666)) 
summary(glmFPsmalloutliersupperthird)
summary(glmFPsmalloutlierslowerthird)
ggplot(dataFPoutlierssmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() + stat_smooth(method=glm, family=binomial, aes(fill=factor(third)))









############################ OLD STUFF DOWN HERE ######################################

#########################################################
#                                                       #
#                                                       #
#                 STEP FUNCTION                         #
#                                                       #
#                                                       #  
#########################################################
# Step function: FPcover_max ~ log(TOTP) 
# uses a tree model to find threshold (split)
# model is simply the mean FPcover value for that range of total P (i.e., above or below the threshold)

library(tree)
thresFPlogTOTP <- tree(dataFP$FPcover_max ~ log(dataFP$TOTP_avg)) # 
thresFPlogTOTP # look at the tree results - the threshold is the value of total P for the first split 
thFPlogTOTP <- -1.73935 # assign that threshold valeu to "thFPlogTOTP"

# not sure what this is in here for....
# "loess" is in the name of the object
# but it doesn't look like any sort of loess is in the formula 
loessFPlogTOTP2 <- aov(dataFP$FPcover_max ~ (log(dataFP$TOTP_avg) > thFPlogTOTP))
summary(loessFPlogTOTP2)

# Get the mean FPcover value above and below the threshold 
tapply(dataFP$FPcover_max,(log(dataFP$TOTP_avg) > thFPlogTOTP), mean) 

plot(dataFP$FPcover_max ~ log(dataFP$TOTP_avg)) # scatterplot the data 
lines(c(-5,thFPlogTOTP), c(0.2040976,0.2040976),lty=2) # draw a line from the min x value to the threshold @ the mean FPcover value for this range 
lines(c(thFPlogTOTP,1), c(0.6978727,0.6978727),lty=2) # draw a line from the threshold to the max x value @ the mean FPcover value for this range
lines(c(thFPlogTOTP,thFPlogTOTP), c(0.6978727,0.2040976),lty=2) # draw a vertical line @ the threshold 

##################################
# save the plot 
jpeg("log TOTP ~ FPcover_max - FP ponds - two regressions.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(FPcover_max ~ log(TOTP_avg), data=dataFP, xlab="Total P (mg/L)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=93)") # plots the data 
abline(lmFPregimelogTOTP) # plots the line from the linear model 
abline(lmNOTFPregimelogTOTP) # plots the line from the linear model 
dev.off()

