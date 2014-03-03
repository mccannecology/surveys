############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA #
# Logistic regression                      #
# Data: dataFPsmall (n=54)                 #
# Waterbodies <5 ha with FP present        #
# MJM 2/25/2014                            #
############################################

library(ggplot2)

# logistic regression (generalized linear model, family = binomial)
glmFPsmallTOTPbinomial <- glm(FPcover_max ~ TOTP_avg, family=binomial, data=dataFPsmall)
summary(glmFPsmallTOTPbinomial) 

# trying to extract values/formula to manually plot on a ggplo2 object 
coef(glmFPsmallTOTPbinomial) # Intercept: -0.7962583 # TOTP_avg: 5.7943365 
predict(glmFPsmallTOTPbinomial, type="response") # Good! predicted values are on the 0,1 scale 
exp(coef(glmFPsmallTOTPbinomial)) # Intercept: 0.4510134 # TOTP_avg: 328.4341933 - these are on the original scale - not logit scale 

# plot
ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() + ylim(0,1) + geom_smooth(method="glm", family=binomial,col="red")
ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max)) + scale_x_log10() + geom_point() + ylim(0,1) + geom_smooth(method="glm", family=binomial,col="red")

# same plot but without standard error 
ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() + ylim(0,1) + stat_smooth(method="glm", family=binomial,col="red",se=F)

# multiple the result by 100 for % scale 
logistic <- function(x) {100 * (exp(glmFPsmallTOTPbinomial$coef[1] + glmFPsmallTOTPbinomial$coef[2]*x)) / (1 + exp(glmFPsmallTOTPbinomial$coef[1] + glmFPsmallTOTPbinomial$coef[2]*x))}

# need a function that plots the SE for this line 