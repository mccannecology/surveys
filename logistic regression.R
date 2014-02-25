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

ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() + ylim(0,1) + geom_smooth(method="glm", family=binomial,col="red")

# same plot but without standard error 
ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() + ylim(0,1) + stat_smooth(method="glm", family=binomial,col="red",se=F)
