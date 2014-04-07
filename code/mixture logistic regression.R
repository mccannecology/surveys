############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA #
# Latent class mixture model  regression   #
############################################

library("flexmix")

# re-format data so it is in the form: col 1 is no. successes and col 2 is no. failures
dataONEperpond_flexmix <- cbind(ceiling(dataONEperpond$FPcover_max*100),(100-(ceiling(dataONEperpond$FPcover_max*100))),dataONEperpond$TOTP_avg)
colnames(dataONEperpond_flexmix) <- c("FPcover_max","NotFP","TOTP_avg")
dataONEperpond_flexmix <- as.data.frame(dataONEperpond_flexmix)
dataONEperpond_flexmix <- dataONEperpond_flexmix[complete.cases(dataONEperpond_flexmix),]# remove any NAs

dataFP_flexmix <- cbind(ceiling(dataFP$FPcover_max*100),(100-(ceiling(dataFP$FPcover_max*100))),dataFP$TOTP_avg)
colnames(dataFP_flexmix) <- c("FPcover_max","NotFP","TOTP_avg")

dataFPsmall_flexmix <- cbind(ceiling(dataFPsmall$FPcover_max*100),(100-(ceiling(dataFPsmall$FPcover_max*100))),dataFPsmall$TOTP_avg)
colnames(dataFPsmall_flexmix) <- c("FPcover_max","NotFP","TOTP_avg")

dataFPoutliers_flexmix <- cbind(ceiling(dataFPoutliers$FPcover_max*100),(100-(ceiling(dataFPoutliers$FPcover_max*100))),dataFPoutliers$TOTP_avg)
colnames(dataFPoutliers_flexmix) <- c("FPcover_max","NotFP","TOTP_avg")

dataFPoutlierssmall_flexmix <- cbind(ceiling(dataFPoutlierssmall$FPcover_max*100),(100-(ceiling(dataFPoutlierssmall$FPcover_max*100))),dataFPoutlierssmall$TOTP_avg)
colnames(dataFPoutlierssmall_flexmix) <- c("FPcover_max","NotFP","TOTP_avg")

####################### 
# Latent Mixture      #
# dataONEperpond_flexmix      #
# family: binomial    #
# link: logit         #
#######################
flexmix_dataONEperpond_binomial_logit <- flexmix(cbind(FPcover_max,NotFP) ~ TOTP_avg, data=dataONEperpond_flexmix, k=2, model=FLXMRglm(family="binomial"))
summary(flexmix_dataONEperpond_binomial_logit)
AIC(flexmix_dataONEperpond_binomial_logit)
# WORKS!

# plot fitted model 
plot(dataONEperpond_flexmix$FPcover_max ~ dataONEperpond_flexmix$TOTP_avg,main="dataONEperpond_flexmix",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataONEperpond_flexmix$TOTP_avg, dataONEperpond_flexmix$TOTP_avg >0),flexmix_dataONEperpond_binomial_logit$fitted,type="p",col="red")

####################### 
# Latent Mixture      #
# dataFP              #
# family: binomial    #
# link: logit         #
#######################
formula <- FPcover_max ~ TOTP_avg
flexmix_dataFP_binomial_logit <- flexmix(formula, data=dataFP, k=2, model=FLXMRglm(family="binomial"))
summary(flexmix_dataFP_binomial_logit)
AIC(flexmix_dataFP_binomial_logit)
# WORKS!

# plot fitted model 
plot(dataFP$FPcover_max ~ dataFP$TOTP_avg,main="dataFP",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFP$TOTP_avg, dataFP$TOTP_avg >0),flexmix_dataFP_binomial_logit$fitted,type="p",col="red")

####################### 
# Latent Mixture      #
# dataFPsmall         #
# family: binomial    #
# link: logit         #
#######################
formula <- FPcover_max ~ TOTP_avg
flexmix_dataFPsmall_binomial_logit <- flexmix(formula, data=dataFPsmall, k=2, model=FLXMRglm(family="binomial"))
summary(flexmix_dataFPsmall_binomial_logit)
AIC(flexmix_dataFPsmall_binomial_logit)
# WORKS!

# plot fitted model 
plot(dataFPsmall$FPcover_max ~ dataFPsmall$TOTP_avg,main="dataFPsmall",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPsmall$TOTP_avg, dataFPsmall$TOTP_avg >0),flexmix_dataFPsmall_binomial_logit$fitted,type="p",col="red")

####################### 
# Latent Mixture      #
# dataFPoutliers      #
# family: binomial    #
# link: logit         #
#######################
formula <- FPcover_max ~ TOTP_avg
flexmix_dataFPoutliers_binomial_logit <- flexmix(FPcover_max ~ TOTP_avg, data=dataFPoutliers, k=2, model=FLXMRglm(family="binomial"))
summary(flexmix_dataFPoutliers_binomial_logit)
AIC(flexmix_dataFPoutliers_binomial_logit)
# WORKS!

# plot fitted model 
plot(dataFPoutliers$FPcover_max ~ dataFPoutliers$TOTP_avg,main="dataFPoutliers",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPoutliers$TOTP_avg, dataFPoutliers$TOTP_avg >0),flexmix_dataFPoutliers_binomial_logit$fitted,type="p",col="red")

####################### 
# Latent Mixture      #
# dataFPoutlierssmall #
# family: binomial    #
# link: logit         #
#######################
formula <- FPcover_max ~ TOTP_avg
flexmix_dataFPoutlierssmall_binomial_logit <- flexmix(FPcover_max ~ TOTP_avg, data=dataFPoutlierssmall, k=2, model=FLXMRglm(family="binomial"))
summary(flexmix_dataFPoutlierssmall_binomial_logit)
AIC(flexmix_dataFPoutlierssmall_binomial_logit)
# works!

# plot fitted model 
plot(dataFPoutlierssmall$FPcover_max ~ dataFPoutlierssmall$TOTP_avg,main="dataFPoutlierssmall",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPoutlierssmall$TOTP_avg, dataFPoutlierssmall$TOTP_avg >0),flexmix_dataFPoutlierssmall_binomial_logit$fitted,type="p",col="red")
