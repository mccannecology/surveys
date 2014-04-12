############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA #
# Beta regression                          #
#                                          #
# Created by MJM 3/20/2014                 #
############################################
library(betareg)

# Get rid of any 0s and 1s in the dependent (Y) variable
# Transformation suggested by: Smithson M, Verkuilen J (2006). Psychological Methods, 11(1), 54–71
#
# IMPORTANT: Re-import these data sets when you are done with beta regression analysis 
#
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
plot(dataFP$FPcover_max ~ dataFP$TOTP_avg,main="dataFP",xlab="Total P (mg/L)",ylab="FP cover",log="x")
plot(dataFPsmall$FPcover_max ~ dataFPsmall$TOTP_avg,main="dataFPsmall",xlab="Total P (mg/L)",ylab="FP cover",log="x")
plot(dataFPoutliers$FPcover_max ~ dataFPoutliers$TOTP_avg,main="dataFPoutliers",xlab="Total P (mg/L)",ylab="FP cover",log="x")
plot(dataFPoutlierssmall$FPcover_max ~ dataFPoutlierssmall$TOTP_avg,main="dataFPoutlierssmall",xlab="Total P (mg/L)",ylab="FP cover",log="x")

######################## 
# Beta regression      #
# dataONEperpond       #
# link: cauchit        #
# Variable dispersion  #
# quadratic precision  #
########################
# try using this data set that does not include NAs 
head(dataONEperpond_gamlss)

formula <- FPcover_max ~ TOTP_avg | poly(TOTP_avg,2)

# fsmaxit: maximal number of iterations (default = 200) & fstol: convergence tolerance (default = 1e-8)
betareg_dataONEperpond_cauchit_quaddisp <- betareg(formula, data=dataONEperpond_gamlss, link="cauchit",fsmaxit=300,fstol=1e-8) 
summary(betareg_dataONEperpond_cauchit_quaddisp)
AIC(betareg_dataONEperpond_cauchit_quaddisp)

# arrange the plots 
par(mfrow=c(2,1))

# plot fitted model 
plot(dataONEperpond_gamlss$FPcover_max ~ dataONEperpond_gamlss$TOTP_avg,main="dataONEperpond_gamlss - cauchit link",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataONEperpond_gamlss$TOTP_avg, dataONEperpond_gamlss$TOTP_avg >0),betareg_dataONEperpond_cauchit_quaddisp$fitted,type="p",col="red")

# plot the precision
plot(predict(betareg_dataONEperpond_cauchit_quaddisp, type="precision") ~ subset(dataONEperpond_gamlss$TOTP_avg, dataONEperpond_gamlss$TOTP_avg >0),xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

##########################
# Beta regression        #
# dataONEperpondoultiers #
# link: cauchit          #
# Variable dispersion    #
# non-linear precision   #
##########################
# use the data set that does not include NAs 
dataONEperpondoutliers_gamlss

formula <- FPcover_max ~ TOTP_avg | poly(TOTP_avg,2)

# fsmaxit: maximal number of iterations (default = 200) & fstol: convergence tolerance (default = 1e-8)
betareg_dataONEperpondoutliers_cauchit_vardisp_quadratic <- betareg(formula, data=dataONEperpondoutliers_gamlss, link="cauchit",fsmaxit=300,fstol=1e-8) 
summary(betareg_dataONEperpondoutliers_cauchit_vardisp_quadratic)
AIC(betareg_dataONEperpondoutliers_cauchit_vardisp_quadratic)

# arrange the plots 
par(mfrow=c(2,1))

# plot fitted model 
plot(dataONEperpondoutliers_gamlss$FPcover_max ~ dataONEperpondoutliers_gamlss$TOTP_avg,main="dataONEperpondoutliers_gamlss - cauchit link",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataONEperpondoutliers_gamlss$TOTP_avg, dataONEperpondoutliers_gamlss$TOTP_avg >0),betareg_dataONEperpondoutliers_cauchit_vardisp_quadratic$fitted,type="p",col="red")

# plot the precision
plot(predict(betareg_dataONEperpondoutliers_cauchit_vardisp_quadratic, type="precision") ~ subset(dataONEperpondoutliers_gamlss$TOTP_avg, dataONEperpondoutliers_gamlss$TOTP_avg >0),xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

##########################
# Beta regression        #
# dataFP                 #
# link: cauchit          #
# Variable dispersion    #
# non-linear precision   #
##########################
# use the data set that does not include NAs 
dataFP_gamlss

formula <- FPcover_max ~ TOTP_avg | poly(TOTP_avg,2)

# fsmaxit: maximal number of iterations (default = 200) & fstol: convergence tolerance (default = 1e-8)
betareg_dataFP_cauchit_quaddisp <- betareg(formula, data=dataFP_gamlss, link="cauchit",fsmaxit=300,fstol=1) 
summary(betareg_dataFP_cauchit_quaddisp)
AIC(betareg_dataFP_cauchit_quaddisp)

# arrange the plots 
par(mfrow=c(2,1))

# plot fitted model 
plot(dataFP_gamlss$FPcover_max ~ dataFP_gamlss$TOTP_avg,main="dataFP_gamlss - cauchit link",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFP_gamlss$TOTP_avg, dataFP_gamlss$TOTP_avg >0),betareg_dataFP_cauchit_quaddisp$fitted,type="p",col="red")

# plot the precision
plot(predict(betareg_dataFP_cauchit_quaddisp, type="precision") ~ subset(dataFP_gamlss$TOTP_avg, dataFP_gamlss$TOTP_avg >0),xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

##########################
# Beta regression        #
# dataFPoutliers         #
# link: cauchit          #
# Variable dispersion    #
# non-linear precision   #
##########################
# use the data set that does not include NAs 
dataFPoutliers_gamlss

formula <- FPcover_max ~ TOTP_avg | poly(TOTP_avg,2)

# fsmaxit: maximal number of iterations (default = 200) & fstol: convergence tolerance (default = 1e-8)
betareg_dataFPoutliers_cauchit_quaddisp <- betareg(formula, data=dataFPoutliers_gamlss, link="cauchit",fsmaxit=400,fstol=1) 
summary(betareg_dataFPoutliers_cauchit_quaddisp)
AIC(betareg_dataFPoutliers_cauchit_quaddisp)

# arrange the plots 
par(mfrow=c(2,1))

# plot fitted model 
plot(dataFPoutliers_gamlss$FPcover_max ~ dataFPoutliers_gamlss$TOTP_avg,main="dataFPoutliers_gamlss - cauchit link",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPoutliers_gamlss$TOTP_avg, dataFPoutliers_gamlss$TOTP_avg >0),betareg_dataFPoutliers_cauchit_quaddisp$fitted,type="p",col="red")

# plot the precision
plot(predict(betareg_dataFPoutliers_cauchit_quaddisp, type="precision") ~ subset(dataFPoutliers_gamlss$TOTP_avg, dataFPoutliers_gamlss$TOTP_avg >0),xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

##########################
# Beta regression        #
# dataFPsmall            #
# link: cauchit          #
# Variable dispersion    #
# non-linear precision   #
##########################
# use the data set that does not include NAs 
dataFPsmall_gamlss

formula <- FPcover_max ~ TOTP_avg | poly(TOTP_avg,2)

# fsmaxit: maximal number of iterations (default = 200) & fstol: convergence tolerance (default = 1e-8)
betareg_dataFPsmall_cauchit_quaddisp <- betareg(formula, data=dataFPsmall_gamlss, link="cauchit",fsmaxit=200,fstol=1e-8) 
summary(betareg_dataFPsmall_cauchit_quaddisp)
AIC(betareg_dataFPsmall_cauchit_quaddisp)

# arrange the plots 
par(mfrow=c(2,1))

# plot fitted model 
plot(dataFPsmall_gamlss$FPcover_max ~ dataFPsmall_gamlss$TOTP_avg,main="dataFPsmall_gamlss - cauchit link",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPsmall_gamlss$TOTP_avg, dataFPsmall_gamlss$TOTP_avg >0),betareg_dataFPsmall_cauchit_quaddisp$fitted,type="p",col="red")

# plot the precision
plot(predict(betareg_dataFPsmall_cauchit_quaddisp, type="precision") ~ subset(dataFPsmall_gamlss$TOTP_avg, dataFPsmall_gamlss$TOTP_avg >0),xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

##########################
# Beta regression        #
# dataFPoutlierssmall    #
# link: cauchit          #
# Variable dispersion    #
# non-linear precision   #
##########################
# use the data set that does not include NAs 
dataFPoutlierssmall_gamlss

formula <- FPcover_max ~ TOTP_avg | poly(TOTP_avg,2)

# fsmaxit: maximal number of iterations (default = 200) & fstol: convergence tolerance (default = 1e-8)
betareg_dataFPoutlierssmall_cauchit_quaddisp <- betareg(formula, data=dataFPoutlierssmall_gamlss, link="cauchit",fsmaxit=200,fstol=1e-8) 
summary(betareg_dataFPoutlierssmall_cauchit_quaddisp)
AIC(betareg_dataFPoutlierssmall_cauchit_quaddisp)

# arrange the plots 
par(mfrow=c(2,1))

# plot fitted model 
plot(dataFPoutlierssmall_gamlss$FPcover_max ~ dataFPoutlierssmall_gamlss$TOTP_avg,main="dataFPoutlierssmall_gamlss - cauchit link",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPoutlierssmall_gamlss$TOTP_avg, dataFPoutlierssmall_gamlss$TOTP_avg >0),betareg_dataFPoutlierssmall_cauchit_quaddisp$fitted,type="p",col="red")

# plot the precision
plot(predict(betareg_dataFPoutlierssmall_cauchit_quaddisp, type="precision") ~ subset(dataFPoutlierssmall_gamlss$TOTP_avg, dataFPoutlierssmall_gamlss$TOTP_avg >0),xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

