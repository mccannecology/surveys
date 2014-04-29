############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA #
# Beta regression                          #
#                                          #
# dataFP                                   # 
#                                          #
# Created by MJM 4/29/2014                 #
############################################
library(betareg)

# Get rid of any 0s and 1s in the dependent (Y) variable
# Transformation suggested by: Smithson M, Verkuilen J (2006). Psychological Methods, 11(1), 54–71
# IMPORTANT: Re-import these data sets when you are done with beta regression analysis 
dataFP$FPcover_max[dataFP$FPcover_max == 1] <- (1*(length(dataFP$FPcover_max)-1)+0.5)/(length(dataFP$FPcover_max))
dataFP$FPcover_max[dataFP$FPcover_max == 0] <- (0*(length(dataFP$FPcover_max)-1)+0.5)/(length(dataFP$FPcover_max))

####################### 
# Beta regression     #
# dataFP              #
# link: logit         #
# Constant dispersion #
#######################
# Null model: Just an intercept - TOTP not included 
betareg_dataFP_logit_null <- betareg(FPcover_max ~ 1, data=dataFP, link="logit")
summary(betareg_dataFP_logit_null)
AIC(betareg_dataFP_logit_null)
# plot it 
plot(dataFP$FPcover_max ~ dataFP$TOTP_avg,main="dataFP",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(dataFP$TOTP_avg,betareg_dataFP_logit_null$fitted,type="p",col="red")

# Real model: with TOTP included 
betareg_dataFP_logit <- betareg(FPcover_max ~ TOTP_avg, data=dataFP, link="logit")
summary(betareg_dataFP_logit)
logLik(betareg_dataFP_logit)
AIC(betareg_dataFP_logit)

####################### 
# Beta regression     #
# dataFP              #
# link: logit         #
# Variable dispersion #
#######################
# This is the same model as the null model for constant dispersion - betareg_dataFP_logit_null
# Null model: Intercept only. No effect of TOTP
formula <- FPcover_max ~ 1 | 1
betareg_dataFP_vardisp_null <- betareg(formula, data=dataFP)
summary(betareg_dataFP_vardisp_null) 
AIC(betareg_dataFP_vardisp_null) 
# plot 
# arrange the plots 
par(mfrow=c(2,1))
# plot fitted model 
plot(dataFP$FPcover_max ~ dataFP$TOTP_avg,main="dataFP - logit link",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(dataFP$TOTP_avg,betareg_dataFP_vardisp_null$fitted,type="p",col="red")
# plot the variable dispersion
plot(predict(betareg_dataFP_vardisp_null, type="precision") ~ dataFP$TOTP_avg,xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

# Real model: 
formula <- FPcover_max ~ TOTP_avg | TOTP_avg
betareg_dataFP_logit_vardisp <- betareg(formula, data=dataFP, link="logit")
summary(betareg_dataFP_logit_vardisp) 
plot(betareg_dataFP_logit_vardisp)
AIC(betareg_dataFP_logit_vardisp) 
# works

# arrange the plots 
par(mfrow=c(2,1))

# plot fitted model 
plot(dataFP$FPcover_max ~ dataFP$TOTP_avg,main="dataFP - logit link",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(dataFP$TOTP_avg,betareg_dataFP_logit_vardisp$fitted,type="p",col="red")

# plot the variable dispersion
plot(predict(betareg_dataFP_logit_vardisp, type="precision") ~ dataFP$TOTP_avg,xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

# Compare the two models
# Am I better off including TOTP in my model? 
# McFadden’s pseudo-R-squared
1 - as.vector(logLik(betareg_dataFP_vardisp_null)/logLik(betareg_dataFP_logit_vardisp))

###################################
# Beta regression                 #
# dataFP                          #
# link: logit                     #
# Mixture model                   #
# No prior cluster prob specified #
###################################
# Without any prior cluster probabilities specified 
formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataFP_logit <- betamix(formula, link="logit", data=dataFP, k = 2, nstart = 100)
print(betareg_mix_dataFP_logit) 
summary(betareg_mix_dataFP_logit) 
logLik(betareg_mix_dataFP_logit) 
AIC(betareg_mix_dataFP_logit) 

# Null 
# Without any prior cluster probabilities specified 
formula <- FPcover_max ~ 1
betareg_mix_dataFP_logit_null <- betamix(formula, link="logit", data=dataFP, k = 2, nstart = 100)
print(betareg_mix_dataFP_logit_null) 
summary(betareg_mix_dataFP_logit_null) 
logLik(betareg_mix_dataFP_logit_null) 
AIC(betareg_mix_dataFP_logit_null) 

###################################
# Beta regression                 #
# dataFP                          #
# link: logit                     #
# Mixture model                   #
# Prior cluster prob specified    #
###################################
# create the matrix for initial cluster probabilities 
dataFP$prior_cluster1 <- sqrt(dataFP$FPcover_max)
dataFP$prior_cluster2 <- 1-sqrt(dataFP$FPcover_max)

# This is the one problematic outlier that keeps getting assigned to the not-FP-regime cluster
dataFP$FPcover_max[64]
dataFP$TOTP_avg[64]

# Give that point a 0 % prob. of being in not-FP-regime cluster 
dataFP$prior_cluster1[64] <- 1
dataFP$prior_cluster2[64] <- 0

# These are the initial cluster probabilities 
dataFP$prior_cluster1 
dataFP$prior_cluster2

# The real model 
formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataFP_logit_priorcluster <- betamix(formula, link="logit", data=dataFP, k = 2, nstart = 100, cluster=cbind(dataFP$prior_cluster1,dataFP$prior_cluster2))
betareg_mix_dataFP_logit_priorcluster
summary(betareg_mix_dataFP_logit_priorcluster) 
logLik(betareg_mix_dataFP_logit_priorcluster) 
AIC(betareg_mix_dataFP_logit_priorcluster) 
clusters(betareg_mix_dataFP_logit_priorcluster) 

# The null model 
formula <- FPcover_max ~ 1
betareg_mix_dataFP_logit_priorcluster_null <- betamix(formula, link="logit", data=dataFP, k = 2, nstart = 100, cluster=cbind(dataFP$prior_cluster1,dataFP$prior_cluster2))
betareg_mix_dataFP_logit_priorcluster_null
summary(betareg_mix_dataFP_logit_priorcluster_null) 
logLik(betareg_mix_dataFP_logit_priorcluster_null) 
AIC(betareg_mix_dataFP_logit_priorcluster_null) 
clusters(betareg_mix_dataFP_logit_priorcluster_null) 



# Failed attemps below #

####################### 
# Segmented           # 
# Beta regression     #
# iterative searching #
# dataFP              # # This does not work. I think it is a problem w/ how betareg() handles segmented formula 
# link: logit         #
# Constant dispersion #
#######################
breaks <- dataFP$TOTP_avg[which(dataFP$TOTP_avg >= 0.00001 & dataFP$TOTP_avg <= 0.5)]    # create a vector to hold potential breakpoints 

LOGLIKE <- numeric(length(breaks)) # create a blank vector to hold MSE     

for(i in 1:length(breaks)){ # loop over all of the potential breakpoints & actually try them out in a lm()
  
  piecewise <- betareg(FPcover_max ~ TOTP_avg*(TOTP_avg < breaks[i]) + TOTP_avg*(TOTP_avg>=breaks[i]),link = "logit", data=dataFP)
  LOGLIKE[i] <- summary(piecewise)[15] # If this is a lm() I should index [6], if it's a glm() I should index [4]
}

LOGLIKE <- as.numeric(LOGLIKE) # converts list to numeric 
breakpoint<-breaks[which(LOGLIKE==max(LOGLIKE))] # picks the breakpoint with the lowest mse
breakpoint # returns the breakpoint 

# re-run the glm() using this breakpoint 
segmented_dataFP_beta_logit <- betareg(FPcover_max ~ TOTP_avg*(TOTP_avg<breakpoint) + TOTP_avg*(TOTP_avg>=breakpoint), link="logit", data=dataFP)
summary(segmented_dataFP_beta_logit)  
logLik(segmented_dataFP_beta_logit)
AIC(segmented_dataFP_beta_logit) # gives you the incorrect AIC - does not account for estimating the breakpoint parameter 
-2*logLik(segmented_dataFP_beta_logit)[1]+2*5 # calculate the actual AIC for this model 
breakpoint

# Add new variables to the data frame 
# used to plot the segmented data in ggplot2
dataFP$breakpoint_beta <- ifelse(dataFP$TOTP_avg <= breakpoint, "below", "above")

rm(breaks,LOGLIKE,breakpoint,piecewise) # clean up your workspace 


#########################
# Structural change     #
# in beta reg paramters #
# dataPF                #
#########################
# this works... but I'm not sure if it tells me anything 
# Parameters don't cross a red line 
# in fact, red line is not even present 
library("strucchange")
stucchange_beta_dataFP <- gefp(FPcover_max ~ TOTP_avg, fit=betareg, data=dataFP)
summary(stucchange_beta_dataFP)
plot(stucchange_beta_dataFP,aggregate=FALSE)

# try see if the parameters are stable across water body surface area 
stucchange_beta_dataFP <- gefp(FPcover_max ~ TOTP_avg, fit=betareg, data=dataFP, order.by = dataFP$surfacearea_ha)
summary(stucchange_beta_dataFP)
plot(stucchange_beta_dataFP,aggregate=FALSE)
