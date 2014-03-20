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

###################
# Beta-regression #
###################
betaregFPsmall <- betareg(FPcover_max ~ TOTP_avg, data = dataFPsmall, type="ML")
summary(betaregFPsmall)

###############################################
# Mixture beta-regression with latent classes #
###############################################
# If you think you have >1 class (groups) but you don't know what they are a priori 

# this example from the package vignette works
data("ReadingSkills", package = "betareg")
rs_mix <- betamix(accuracy ~ iq, data = ReadingSkills, k = 3, extra_components = extraComponent(type = "uniform", coef = 0.99, delta = 0.01), nstart = 10)

# my data 
# no convergence to a suitable mixture
betamix(FPcover_max ~ TOTP_avg, data = dataFPsmall, k = 3, extra_components = extraComponent(type = "uniform", coef = 0.99, delta = 0.01), nstart = 10)
betamix(FPcover_max ~ TOTP_avg, data = dataFPoutlierssmall, k = 3, extra_components = extraComponent(type = "uniform", coef = 0.99, delta = 0.01), nstart = 10)
betamix(FPcover_max ~ TOTP_avg, data = dataFPsmall, k = 2, nstart = 100)
