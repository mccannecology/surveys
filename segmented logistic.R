############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA #
# Segmented logistic regression            #
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
AIC(segmented.glmFPsmallTOTPbinomial)

# plotting the segmented model
plot(FPcover_max ~ TOTP_avg,data=dataFPsmall)
plot(segmented.glmFPsmallTOTPbinomial, add=T, conf.level=0.95, shade=F, rug=F)

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