############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA #
# Beta regression                          #
#                                          #
# Created by MJM 3/20/2014                 #
############################################
library(betareg)

formuala <- FPcover_max ~ TOTP_avg # formula for the beta regression 

data(dataFPsmall) # load  your dataset

# you need to get rid of any 0s and 1s in the dependent (Y) variable
# modify dataFPsmall$FPcover_max so there are no 0s or 1s 
dataFPsmall$FPcover_max[dataFPsmall$FPcover_max == 1] <- 0.999
dataFPoutlierssmall$FPcover_max[dataFPoutlierssmall$FPcover_max == 1] <- 0.999
dataONEperpond$FPcover_max[dataONEperpond$FPcover_max == 1] <- 0.999

###################
# Beta-regression #
###################
# try different link functions

betaregFPsmall_logit <- betareg(FPcover_max ~ TOTP_avg, data = dataFPsmall, link="logit", type="ML")
summary(betaregFPsmall_logit)

betaregFPsmall_loglog <- betareg(FPcover_max ~ TOTP_avg, data = dataFPsmall, link="loglog", type="ML")
summary(betaregFPsmall_loglog)
# a slight improvement in pseudo-r-squared

#######################################
# Variable dispersion beta-regression #
#######################################
# try different link functions

betaregFPsmall_logit_vardisp <- betareg(FPcover_max ~ TOTP_avg | TOTP_avg, data = dataFPsmall, link="logit", type="ML")
summary(betaregFPsmall_logit_vardisp)

betaregFPsmall_loglog_vardisp <- betareg(FPcover_max ~ TOTP_avg | TOTP_avg, data = dataFPsmall, link="loglog", type="ML")
summary(betaregFPsmall_loglog_vardisp)

###############################################
# Mixture beta-regression with latent classes #
###############################################
# If you think you have >1 class (groups) but you don't know what they are a priori 

# try it with dataFPsmall 
betareg_mix_FPsmall_loglog <- betamix(FPcover_max ~ TOTP_avg, link="loglog", data = dataFPsmall, k = 3, nstart = 100)
summary(betareg_mix_FPsmall_loglog)
# this only returns 1 cluster (group)

# try it with ALL ponds 
betareg_mix_oneperpond_loglog <- betamix(FPcover_max ~ TOTP_avg, link="loglog", data = dataONEperpond, k = 3, nstart = 100)
summary(betareg_mix_oneperpond_loglog)
# does not converge 

# try it with dataFPoutlierssmall - 3 clusters
betareg_mix_FPsmalloutliers_loglog_3clusters <- betamix(FPcover_max ~ TOTP_avg, link="loglog", data = dataFPoutlierssmall, k = 3, nstart = 100)
summary(betareg_mix_FPsmalloutliers_loglog_3clusters)
# converges on a solution with 3 clusters 

# try it with dataFPoutlierssmall - 2 clusters
betareg_mix_FPsmalloutliers_loglog_2clusters <- betamix(FPcover_max ~ TOTP_avg, link="loglog", data = dataFPoutlierssmall, k = 2, nstart = 100)
summary(betareg_mix_FPsmalloutliers_loglog_2clusters)
# returns a solution with 2 clusters 



##################
# Compare models #
##################
# log-likelihood ratio test (for nested models)
library(lmtest)
lrtest(betaregFPsmall_logit, betaregFPsmall_vardisp)
# this doesn't look like it really improves things - p = 0.2089

# compare AIC values
AIC(betaregFPsmall_logit, betaregFPsmall_loglog,betaregFPsmall_logit_vardisp,betaregFPsmall_loglog_vardisp)

#############################
# Retrieve estimated values #
#############################
library(sandwich)
estfun(betaregFPsmall_logit_vardisp)
# I can do this for all of the other models too 

# extract predicted response variables from beta regression  
predict(betaregFPsmall_logit_vardisp, type="response")

# extract predicted response variables from mixture beta regression  
predict(betareg_mix_FPsmalloutliers_loglog_3clusters)
# this may not work properly 

##########################################
# Fluctuation test for structural change # 
##########################################
# test whether parameters are stable over range of observations

library(strucchange)
plot(gefp(FPcover_max ~ 1, fit=betareg, data=dataFPsmall), aggregate=FALSE)

# I'm not sure that this worked 


