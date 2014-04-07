############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA #
# Logistic regression                      #
# Data: dataFPsmall (n=54)                 #
# Waterbodies <5 ha with FP present        #
# MJM 3/3/2014                             #
############################################

library(ggplot2)

####################### 
# Logistic Regression #
# dataONEperpond      #
# family: binomial    #
# link: logit         #
#######################
formula <- FPcover_max ~ TOTP_avg
glm_dataONEperpond_binomial_logit <- glm(formula, data=dataONEperpond, family=binomial(link=logit))
summary(glm_dataONEperpond_binomial_logit)
AIC(glm_dataONEperpond_binomial_logit)

# plot fitted model 
plot(dataONEperpond$FPcover_max ~ dataONEperpond$TOTP_avg,main="dataONEperpond",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataONEperpond$TOTP_avg, dataONEperpond$TOTP_avg >0),glm_dataONEperpond_binomial_logit$fitted,type="p",col="red")

####################### 
# Logistic Regression #
# dataFP              #
# family: binomial    #
# link: logit         #
#######################
formula <- FPcover_max ~ TOTP_avg
glm_dataFP_binomial_logit <- glm(formula, data=dataFP, family="binomial",link="logit")
summary(glm_dataFP_binomial_logit)
AIC(glm_dataFP_binomial_logit)

# plot fitted model 
plot(dataFP$FPcover_max ~ dataFP$TOTP_avg,main="dataFP",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFP$TOTP_avg, dataFP$TOTP_avg >0),glm_dataFP_binomial_logit$fitted,type="p",col="red")

####################### 
# Logistic Regression #
# dataFPsmall         #
# family: binomial    #
# link: logit         #
#######################
formula <- FPcover_max ~ TOTP_avg
glm_dataFPsmall_binomial_logit <- glm(formula, data=dataFPsmall, family="binomial",link="logit")
summary(glm_dataFPsmall_binomial_logit)
AIC(glm_dataFPsmall_binomial_logit)

# plot fitted model 
plot(dataFPsmall$FPcover_max ~ dataFPsmall$TOTP_avg,main="dataFPsmall",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPsmall$TOTP_avg, dataFPsmall$TOTP_avg >0),glm_dataFPsmall_binomial_logit$fitted,type="p",col="red")

####################### 
# Logistic Regression #
# dataFPoutliers      #
# family: binomial    #
# link: logit         #
#######################
formula <- FPcover_max ~ TOTP_avg
glm_dataFPoutliers_binomial_logit <- glm(FPcover_max ~ TOTP_avg, data=dataFPoutliers, family="binomial",link="logit")
summary(glm_dataFPoutliers_binomial_logit)
AIC(glm_dataFPoutliers_binomial_logit)

# plot fitted model 
plot(dataFPoutliers$FPcover_max ~ dataFPoutliers$TOTP_avg,main="dataFPoutliers",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPoutliers$TOTP_avg, dataFPoutliers$TOTP_avg >0),glm_dataFPoutliers_binomial_logit$fitted,type="p",col="red")

####################### 
# Logistic Regression #
# dataFPoutlierssmall #
# family: binomial    #
# link: logit         #
#######################
formula <- FPcover_max ~ TOTP_avg
glm_dataFPoutlierssmall_binomial_logit <- glm(FPcover_max ~ TOTP_avg, data=dataFPoutlierssmall, family="binomial",link="logit")
summary(glm_dataFPoutlierssmall_binomial_logit)
AIC(glm_dataFPoutlierssmall_binomial_logit)

# plot fitted model 
plot(dataFPoutlierssmall$FPcover_max ~ dataFPoutlierssmall$TOTP_avg,main="dataFPoutlierssmall",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPoutlierssmall$TOTP_avg, dataFPoutlierssmall$TOTP_avg >0),glm_dataFPoutlierssmall_binomial_logit$fitted,type="p",col="red")










############################ OLD STUFF ####################################


# linear regression 
lmFPsmallTOTP <- lm(FPcover_max ~ TOTP_avg, data=dataFPsmall)
summary(lmFPsmallTOTP) 
AIC(lmFPsmallTOTP)

# logistic regression (generalized linear model, family = binomial)
glmFPsmallTOTPbinomial <- glm(FPcover_max ~ TOTP_avg, family=binomial, data=dataFPsmall)
summary(glmFPsmallTOTPbinomial) 
AIC(glmFPsmallTOTPbinomial) 

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