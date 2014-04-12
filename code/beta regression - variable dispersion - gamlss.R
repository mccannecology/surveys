############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA #
# Beta regression                          #
# With package gamlss                      #
#                                          #
# Created by MJM 4/10/2014                 #
############################################
library(gamlss)

# Get rid of any 0s and 1s in the dependent (Y) variable
# Transformation suggested by: Smithson M, Verkuilen J (2006). Psychological Methods, 11(1), 54–71
# IMPORTANT: Re-import these data sets when you are done with beta regression analysis 
dataONEperpond$FPcover_max[dataONEperpond$FPcover_max == 1] <- (1*(length(dataONEperpond$FPcover_max)-1)+0.5)/(length(dataONEperpond$FPcover_max))
dataONEperpond$FPcover_max[dataONEperpond$FPcover_max == 0] <- (0*(length(dataONEperpond$FPcover_max)-1)+0.5)/(length(dataONEperpond$FPcover_max))

dataONEperpondoutliers$FPcover_max[dataONEperpondoutliers$FPcover_max == 1] <- (1*(length(dataONEperpondoutliers$FPcover_max)-1)+0.5)/(length(dataONEperpondoutliers$FPcover_max))
dataONEperpondoutliers$FPcover_max[dataONEperpondoutliers$FPcover_max == 0] <- (0*(length(dataONEperpondoutliers$FPcover_max)-1)+0.5)/(length(dataONEperpondoutliers$FPcover_max))

dataFP$FPcover_max[dataFP$FPcover_max == 1] <- (1*(length(dataFP$FPcover_max)-1)+0.5)/(length(dataFP$FPcover_max))
dataFP$FPcover_max[dataFP$FPcover_max == 0] <- (0*(length(dataFP$FPcover_max)-1)+0.5)/(length(dataFP$FPcover_max))

dataFPsmall$FPcover_max[dataFPsmall$FPcover_max == 1] <- (1*(length(dataFPsmall$FPcover_max)-1)+0.5)/(length(dataFPsmall$FPcover_max))
dataFPsmall$FPcover_max[dataFPsmall$FPcover_max == 0] <- (0*(length(dataFPsmall$FPcover_max)-1)+0.5)/(length(dataFPsmall$FPcover_max))

dataFPoutliers$FPcover_max[dataFPoutliers$FPcover_max == 1] <- (1*(length(dataFPoutliers$FPcover_max)-1)+0.5)/(length(dataFPoutliers$FPcover_max))
dataFPoutliers$FPcover_max[dataFPoutliers$FPcover_max == 0] <- (0*(length(dataFPoutliers$FPcover_max)-1)+0.5)/(length(dataFPoutliers$FPcover_max))

dataFPoutlierssmall$FPcover_max[dataFPoutlierssmall$FPcover_max == 1] <- (1*(length(dataFPoutlierssmall$FPcover_max)-1)+0.5)/(length(dataFPoutlierssmall$FPcover_max))
dataFPoutlierssmall$FPcover_max[dataFPoutlierssmall$FPcover_max == 0] <- (0*(length(dataFPoutlierssmall$FPcover_max)-1)+0.5)/(length(dataFPoutlierssmall$FPcover_max))

# Plot the different data sets
# Use these later for adding fitted models 
plot(dataONEperpond$FPcover_max ~ dataONEperpond$TOTP_avg,main="dataONEperpond",xlab="Total P (mg/L)",ylab="FP cover",log="x")
plot(dataONEperpondoutliers$FPcover_max ~ dataONEperpondoutliers$TOTP_avg,main="dataONEperpondoutliers",xlab="Total P (mg/L)",ylab="FP cover",log="x")
plot(dataFP$FPcover_max ~ dataFP$TOTP_avg,main="dataFP",xlab="Total P (mg/L)",ylab="FP cover",log="x")
plot(dataFPsmall$FPcover_max ~ dataFPsmall$TOTP_avg,main="dataFPsmall",xlab="Total P (mg/L)",ylab="FP cover",log="x")
plot(dataFPoutliers$FPcover_max ~ dataFPoutliers$TOTP_avg,main="dataFPoutliers",xlab="Total P (mg/L)",ylab="FP cover",log="x")
plot(dataFPoutlierssmall$FPcover_max ~ dataFPoutlierssmall$TOTP_avg,main="dataFPoutlierssmall",xlab="Total P (mg/L)",ylab="FP cover",log="x")

####################### 
# Beta regression     #
# dataONEperpond      #
# mean link: logit    #
# phi.link: logit     #
# Variable dispersion #
#######################
head(dataONEperpond)

# re-format data so it has no NAs 
dataONEperpond_gamlss <- cbind(dataONEperpond$FPcover_max,dataONEperpond$TOTP_avg)
dataONEperpond_gamlss <- as.data.frame(dataONEperpond_gamlss) # convert to a data frame 
dataONEperpond_gamlss <- dataONEperpond_gamlss[complete.cases(dataONEperpond_gamlss),]# remove any NAs
names(dataONEperpond_gamlss) <- c("FPcover_max","TOTP_avg")
head(dataONEperpond_gamlss)
dataONEperpond_gamlss

# run the betaregression
betareg_dataONEperpond_gamlss_logit_vardisp <- gamlss(FPcover_max ~ TOTP_avg, sigma.formula = ~ TOTP_avg, data=dataONEperpond_gamlss, family=BE)
summary(betareg_dataONEperpond_gamlss_logit_vardisp)
logLik(betareg_dataONEperpond_gamlss_logit_vardisp)

########################## 
# Beta regression        #
# dataONEperpondoutliers #
# mean link: logit       #
# phi.link: logit        #
# Variable dispersion    #
##########################
head(dataONEperpondoutliers)

# re-format data so it has no NAs 
dataONEperpondoutliers_gamlss <- cbind(dataONEperpondoutliers$FPcover_max,dataONEperpondoutliers$TOTP_avg)
dataONEperpondoutliers_gamlss <- as.data.frame(dataONEperpondoutliers_gamlss) # convert to a data frame 
dataONEperpondoutliers_gamlss <- dataONEperpondoutliers_gamlss[complete.cases(dataONEperpondoutliers_gamlss),]# remove any NAs
names(dataONEperpondoutliers_gamlss) <- c("FPcover_max","TOTP_avg")
head(dataONEperpondoutliers_gamlss)
dataONEperpondoutliers_gamlss

# run the betaregression
betareg_dataONEperpondoutliers_gamlss_logit_vardisp <- gamlss(FPcover_max ~ TOTP_avg, sigma.formula = ~ TOTP_avg, data=dataONEperpondoutliers_gamlss, family=BE)
summary(betareg_dataONEperpondoutliers_gamlss_logit_vardisp)
logLik(betareg_dataONEperpondoutliers_gamlss_logit_vardisp)

####################### 
# Beta regression     #
# dataFP              #
# mean link: logit    #
# phi.link: logit     #
# Variable dispersion #
#######################
head(dataFP)

# re-format data so it has no NAs 
dataFP_gamlss <- cbind(dataFP$FPcover_max,dataFP$TOTP_avg)
dataFP_gamlss <- as.data.frame(dataFP_gamlss) # convert to a data frame 
dataFP_gamlss <- dataFP_gamlss[complete.cases(dataFP_gamlss),]# remove any NAs
names(dataFP_gamlss) <- c("FPcover_max","TOTP_avg")
head(dataFP_gamlss)
dataFP_gamlss

# run the betaregression
betareg_dataFP_gamlss_logit_vardisp <- gamlss(FPcover_max ~ TOTP_avg, sigma.formula = ~ TOTP_avg, data=dataFP_gamlss, family=BE)
summary(betareg_dataFP_gamlss_logit_vardisp)
logLik(betareg_dataFP_gamlss_logit_vardisp)

####################### 
# Beta regression     #
# dataFPoutliers      #
# mean link: logit    #
# phi.link: logit     #
# Variable dispersion #
#######################
head(dataFPoutliers)

# re-format data so it has no NAs 
dataFPoutliers_gamlss <- cbind(dataFPoutliers$FPcover_max,dataFPoutliers$TOTP_avg)
dataFPoutliers_gamlss <- as.data.frame(dataFPoutliers_gamlss) # convert to a data frame 
dataFPoutliers_gamlss <- dataFPoutliers_gamlss[complete.cases(dataFPoutliers_gamlss),]# remove any NAs
names(dataFPoutliers_gamlss) <- c("FPcover_max","TOTP_avg")
head(dataFPoutliers_gamlss)
dataFPoutliers_gamlss

# run the betaregression
betareg_dataFPoutliers_gamlss_logit_vardisp <- gamlss(FPcover_max ~ TOTP_avg, sigma.formula = ~ TOTP_avg, data=dataFPoutliers_gamlss, family=BE)
summary(betareg_dataFPoutliers_gamlss_logit_vardisp)
logLik(betareg_dataFPoutliers_gamlss_logit_vardisp)

####################### 
# Beta regression     #
# dataFPsmall         #
# mean link: logit    #
# phi.link: logit     #
# Variable dispersion #
#######################
head(dataFPsmall)

# re-format data so it has no NAs 
dataFPsmall_gamlss <- cbind(dataFPsmall$FPcover_max,dataFPsmall$TOTP_avg)
dataFPsmall_gamlss <- as.data.frame(dataFPsmall_gamlss) # convert to a data frame 
dataFPsmall_gamlss <- dataFPsmall_gamlss[complete.cases(dataFPsmall_gamlss),]# remove any NAs
names(dataFPsmall_gamlss) <- c("FPcover_max","TOTP_avg")
head(dataFPsmall_gamlss)
dataFPsmall_gamlss

# run the betaregression
betareg_dataFPsmall_gamlss_logit_vardisp <- gamlss(FPcover_max ~ TOTP_avg, sigma.formula = ~ TOTP_avg, data=dataFPsmall_gamlss, family=BE)
summary(betareg_dataFPsmall_gamlss_logit_vardisp)
logLik(betareg_dataFPsmall_gamlss_logit_vardisp)

####################### 
# Beta regression     #
# dataFPoutlierssmall #
# mean link: logit    #
# phi.link: logit     #
# Variable dispersion #
#######################
head(dataFPoutlierssmall)

# re-format data so it has no NAs 
dataFPoutlierssmall_gamlss <- cbind(dataFPoutlierssmall$FPcover_max,dataFPoutlierssmall$TOTP_avg)
dataFPoutlierssmall_gamlss <- as.data.frame(dataFPoutlierssmall_gamlss) # convert to a data frame 
dataFPoutlierssmall_gamlss <- dataFPoutlierssmall_gamlss[complete.cases(dataFPoutlierssmall_gamlss),]# remove any NAs
names(dataFPoutlierssmall_gamlss) <- c("FPcover_max","TOTP_avg")
head(dataFPoutlierssmall_gamlss)
dataFPoutlierssmall_gamlss

# run the betaregression
betareg_dataFPoutlierssmall_gamlss_logit_vardisp <- gamlss(FPcover_max ~ TOTP_avg, sigma.formula = ~ TOTP_avg, data=dataFPoutlierssmall_gamlss, family=BE)
summary(betareg_dataFPoutlierssmall_gamlss_logit_vardisp)
logLik(betareg_dataFPoutlierssmall_gamlss_logit_vardisp)

