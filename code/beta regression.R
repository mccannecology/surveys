############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA #
# Beta regression                          #
#                                          #
# Created by MJM 3/20/2014                 #
############################################
library(betareg)

# Get rid of any 0s and 1s in the dependent (Y) variable
# Transformation suggested by: Smithson M, Verkuilen J (2006). Psychological Methods, 11(1), 54–71
dataONEperpond$FPcover_max[dataONEperpond$FPcover_max == 1] <- (1*(length(dataONEperpond$FPcover_max)-1)+0.5)/(length(dataONEperpond$FPcover_max))
dataONEperpond$FPcover_max[dataONEperpond$FPcover_max == 0] <- (0*(length(dataONEperpond$FPcover_max)-1)+0.5)/(length(dataONEperpond$FPcover_max))

dataFP$FPcover_max[dataFP$FPcover_max == 1] <- 0.999
dataFP$FPcover_max[dataFP$FPcover_max == 0] <- 0.001

dataFPsmall$FPcover_max[dataFPsmall$FPcover_max == 1] <- 0.999
dataFPsmall$FPcover_max[dataFPsmall$FPcover_max == 0] <- 0.001

dataFPoutliers$FPcover_max[dataFPoutliers$FPcover_max == 1] <- 0.999
dataFPoutliers$FPcover_max[dataFPoutliers$FPcover_max == 0] <- 0.001

dataFPoutlierssmall$FPcover_max[dataFPoutlierssmall$FPcover_max == 1] <- 0.999
dataFPoutlierssmall$FPcover_max[dataFPoutlierssmall$FPcover_max == 0] <- 0.001

# Plot the different data sets
# Use these later for adding fitted models 
plot(dataONEperpond$FPcover_max ~ dataONEperpond$TOTP_avg,main="dataONEperpond",xlab="Total P (mg/L)",ylab="FP cover",log="x")
plot(dataFP$FPcover_max ~ dataFP$TOTP_avg,main="dataFP",xlab="Total P (mg/L)",ylab="FP cover",log="x")
plot(dataFPsmall$FPcover_max ~ dataFPsmall$TOTP_avg,main="dataFPsmall",xlab="Total P (mg/L)",ylab="FP cover",log="x")
plot(dataFPoutliers$FPcover_max ~ dataFPoutliers$TOTP_avg,main="dataFPoutliers",xlab="Total P (mg/L)",ylab="FP cover",log="x")
plot(dataFPoutlierssmall$FPcover_max ~ dataFPoutlierssmall$TOTP_avg,main="dataFPoutlierssmall",xlab="Total P (mg/L)",ylab="FP cover",log="x")

####################### 
# Beta regression     #
# dataONEperpond      #
# link: logit         #
# Constant dispersion #
#######################
betareg_dataONEperpond_logit <- betareg(FPcover_max ~ TOTP_avg, data=dataONEperpond, link="logit", type="ML")
summary(betareg_dataONEperpond_logit)
# WORKS!

####################### 
# Beta regression     #
# dataFP              #
# link: logit         #
# Constant dispersion #
#######################
betareg_dataFP_logit <- betareg(FPcover_max ~ TOTP_avg, data=dataFP, link="logit")
summary(betareg_dataFP_logit)
# WORKS!

####################### 
# Beta regression     #
# dataFPsmall         #
# link: logit         #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_dataFPsmall_logit <- betareg(formula, data=dataFPsmall, link="logit")
summary(betareg_dataFPsmall_logit)
# WORKS!

####################### 
# Beta regression     #
# dataFPoutliers      #
# link: logit         #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_dataFPoutliers_logit <- betareg(FPcover_max ~ TOTP_avg, data=dataFPoutliers, link="logit")
summary(betareg_dataFPoutliers_logit)
# WORKS!

####################### 
# Beta regression     #
# dataFPoutlierssmall #
# link: logit         #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_dataFPoutlierssmall_logit <- betareg(FPcover_max ~ TOTP_avg, data=dataFPoutlierssmall, link="logit")
summary(betareg_dataFPoutlierssmall_logit)
# works!

####################### 
# Beta regression     #
# dataONEperpond      #
# link: loglog        #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_dataONEperpond_loglog <- betareg(formula, data=dataONEperpond, link="loglog")
summary(betareg_dataONEperpond_loglog) 
# works

####################### 
# Beta regression     #
# dataFP              #
# link: loglog        #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_dataFP_loglog <- betareg(formula, data=dataFP, link="loglog")
summary(betareg_dataFP_loglog) 
# works

####################### 
# Beta regression     #
# dataFPsmall         #
# link: loglog        #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_dataFPsmall_loglog <- betareg(formula, data=dataFPsmall, link="loglog")
summary(betareg_dataFPsmall_loglog) 
# works

####################### 
# Beta regression     #
# dataFPoutliers      #
# link: loglog        #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_dataFPoutliers_loglog <- betareg(formula, data=dataFPoutliers, link="loglog")
summary(betareg_dataFPoutliers_loglog) 
# works

####################### 
# Beta regression     #
# dataFPoutlierssmall #
# link: loglog        #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_dataFPoutlierssmall_loglog <- betareg(formula, data=dataFPoutlierssmall, link="loglog")
summary(betareg_dataFPoutlierssmall_loglog) 
# works

####################### 
# Beta regression     #
# dataONEperpond      #
# link: logit         #
# Variable dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg | TOTP_avg
betareg_dataONEperpond_logit_vardisp <- betareg(formula, data=dataONEperpond, link="logit")
summary(betareg_dataONEperpond_logit_vardisp) 
#   optimization failed to converge

####################### 
# Beta regression     #
# dataFP              #
# link: logit         #
# Variable dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg | TOTP_avg
betareg_dataFP_logit_vardisp <- betareg(formula, data=dataFP, link="logit")
summary(betareg_dataFP_logit_vardisp) 
# works

####################### 
# Beta regression     #
# dataFPsmall         #
# link: logit         #
# Variable dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg | TOTP_avg
betareg_dataFPsmall_logit_vardisp <- betareg(formula, data=dataFPsmall, link="logit")
summary(betareg_dataFPsmall_logit_vardisp) 
# works

####################### 
# Beta regression     #
# dataFPoutliers      #
# link: logit         #
# Variable dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg | TOTP_avg
betareg_dataFPoutliers_logit_vardisp <- betareg(formula, data=dataFPoutliers, link="logit")
summary(betareg_dataFPoutliers_logit_vardisp) 
# works

####################### 
# Beta regression     #
# dataFPoutlierssmall #
# link: logit         #
# Variable dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg | TOTP_avg
betareg_dataFPoutlierssmall_logit_vardisp <- betareg(formula, data=dataFPoutlierssmall, link="logit")
summary(betareg_dataFPoutlierssmall_logit_vardisp) 
# works

####################### 
# Beta regression     #
# Mixed model         #
# dataONEperpond      #
# link: logit         #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataONEperpond_logit <- betamix(formula, link="logit", data=dataONEperpond, k = 2, nstart = 100)
summary(betareg_mix_dataONEperpond_logit) 

####################### 
# Beta regression     #
# Mixed model         #
# dataFP              #
# link: logit         #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataFP_logit <- betamix(formula, link="logit", data=dataFP, k = 2, nstart = 100)
summary(betareg_mix_dataFP_logit) 

####################### 
# Beta regression     #
# Mixed model         #
# dataFPsmall         #
# link: logit         #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataFPsmall_logit <- betamix(formula, link="logit", data=dataFPsmall, k = 2, nstart = 100)
summary(betareg_mix_dataFPsmall_logit) 

####################### 
# Beta regression     #
# Mixed model         #
# dataFPoutliers      #
# link: logit         #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataFPoutliers_logit <- betamix(formula, link="logit", data=dataFPoutliers, k = 2, nstart = 100)
summary(betareg_mix_dataFPoutliers_logit) 

####################### 
# Beta regression     #
# Mixed model         #
# dataFPoutlierssmall #
# link: logit         #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataFPoutlierssmall_logit <- betamix(formula, link="logit", data=dataFPoutlierssmall, k = 2, nstart = 100)
summary(betareg_mix_dataFPoutlierssmall_logit) 









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


