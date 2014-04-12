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

####################### 
# Beta regression     #
# dataONEperpond      #
# link: logit         #
# Variable dispersion #
#######################
formula <-FPcover_max ~ TOTP_avg | TOTP_avg
betareg_dataONEperpond_logit_vardisp <- betareg(formula, data=dataONEperpond, link="logit")
summary(betareg_dataONEperpond_logit_vardisp) 
AIC(betareg_dataONEperpond_logit_vardisp) 
# works 

# arrange the plots 
par(mfrow=c(2,1))

# plot fitted model 
plot(dataONEperpond$FPcover_max ~ dataONEperpond$TOTP_avg,main="dataONEperpond - logit link",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataONEperpond$TOTP_avg, dataONEperpond$TOTP_avg >0),betareg_dataONEperpond_logit_vardisp$fitted,type="p",col="red")

# plot the variable dispersion
plot(predict(betareg_dataONEperpond_logit_vardisp, type="precision") ~ subset(dataONEperpond$TOTP_avg, dataONEperpond$TOTP_avg >0),xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

##########################
# Beta regression        #
# dataONEperpondoutliers #
# link: logit            #
# Variable dispersion    #
##########################
formula <-  FPcover_max ~ TOTP_avg | TOTP_avg
betareg_dataONEperpondoutliers_logit_vardisp <- betareg(formula, data=dataONEperpondoutliers, link="logit")
summary(betareg_dataONEperpondoutliers_logit_vardisp) 
AIC(betareg_dataONEperpondoutliers_logit_vardisp) 
# works 

# arrange the plots 
par(mfrow=c(2,1))

# plot fitted model 
plot(dataONEperpondoutliers$FPcover_max ~ dataONEperpondoutliers$TOTP_avg,main="dataONEperpondoutliers - logit link",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataONEperpondoutliers$TOTP_avg, dataONEperpondoutliers$TOTP_avg >0),betareg_dataONEperpondoutliers_logit_vardisp$fitted,type="p",col="red")

# plot the variable dispersion
plot(predict(betareg_dataONEperpondoutliers_logit_vardisp, type="precision") ~ subset(dataONEperpondoutliers$TOTP_avg, dataONEperpondoutliers$TOTP_avg >0),xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

####################### 
# Beta regression     #
# dataFP              #
# link: logit         #
# Variable dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg | TOTP_avg
betareg_dataFP_logit_vardisp <- betareg(formula, data=dataFP, link="logit")
summary(betareg_dataFP_logit_vardisp) 
AIC(betareg_dataFP_logit_vardisp) 
# works

# arrange the plots 
par(mfrow=c(2,1))

# plot fitted model 
plot(dataFP$FPcover_max ~ dataFP$TOTP_avg,main="dataFP - logit link",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFP$TOTP_avg, dataFP$TOTP_avg >0),betareg_dataFP_logit_vardisp$fitted,type="p",col="red")

# plot the variable dispersion
plot(predict(betareg_dataFP_logit_vardisp, type="precision") ~ subset(dataFP$TOTP_avg, dataFP$TOTP_avg >0),xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

####################### 
# Beta regression     #
# dataFPsmall         #
# link: logit         #
# Variable dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg | TOTP_avg
betareg_dataFPsmall_logit_vardisp <- betareg(formula, data=dataFPsmall, link="logit")
summary(betareg_dataFPsmall_logit_vardisp) 
AIC(betareg_dataFPsmall_logit_vardisp) 
# works

# arrange the plots 
par(mfrow=c(2,1))

# plot fitted model 
plot(dataFPsmall$FPcover_max ~ dataFPsmall$TOTP_avg,main="dataFPsmall - logit link",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPsmall$TOTP_avg, dataFPsmall$TOTP_avg >0),betareg_dataFPsmall_logit_vardisp$fitted,type="p",col="red")

# plot the variable dispersion
plot(predict(betareg_dataFPsmall_logit_vardisp, type="precision") ~ subset(dataFPsmall$TOTP_avg, dataFPsmall$TOTP_avg >0),xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

####################### 
# Beta regression     #
# dataFPoutliers      #
# link: logit         #
# Variable dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg | TOTP_avg
betareg_dataFPoutliers_logit_vardisp <- betareg(formula, data=dataFPoutliers, link="logit")
summary(betareg_dataFPoutliers_logit_vardisp) 
AIC(betareg_dataFPoutliers_logit_vardisp) 
# works

# arrange the plots 
par(mfrow=c(2,1))

# plot fitted model 
plot(dataFPoutliers$FPcover_max ~ dataFPoutliers$TOTP_avg,main="dataFPoutliers - logit link",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPoutliers$TOTP_avg, dataFPoutliers$TOTP_avg >0),betareg_dataFPoutliers_logit_vardisp$fitted,type="p",col="red")

# plot the variable dispersion
plot(predict(betareg_dataFPoutliers_logit_vardisp, type="precision") ~ subset(dataFPoutliers$TOTP_avg, dataFPoutliers$TOTP_avg >0),xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

####################### 
# Beta regression     #
# dataFPoutlierssmall #
# link: logit         #
# Variable dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg | TOTP_avg
betareg_dataFPoutlierssmall_logit_vardisp <- betareg(formula, data=dataFPoutlierssmall, link="logit")
summary(betareg_dataFPoutlierssmall_logit_vardisp) 
AIC(betareg_dataFPoutlierssmall_logit_vardisp) 
# works

# arrange the plots 
par(mfrow=c(2,1))

# plot fitted model 
plot(dataFPoutlierssmall$FPcover_max ~ dataFPoutlierssmall$TOTP_avg,main="dataFPoutlierssmall - logit link",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPoutlierssmall$TOTP_avg, dataFPoutlierssmall$TOTP_avg >0),betareg_dataFPoutlierssmall_logit_vardisp$fitted,type="p",col="red")

# plot the variable dispersion
plot(predict(betareg_dataFPoutlierssmall_logit_vardisp, type="precision") ~ subset(dataFPoutlierssmall$TOTP_avg, dataFPoutlierssmall$TOTP_avg >0),xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

####################### 
# Beta regression     #
# dataONEperpond      #
# link: loglog        #
# linkphi:identity    #
# Variable dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg | TOTP_avg
betareg_dataONEperpond_loglog_linkphi_identity_vardisp <- betareg(formula, data=dataONEperpond, link="loglog",link.phi="identity")
summary(betareg_dataONEperpond_loglog_linkphi_identity_vardisp) 
AIC(betareg_dataONEperpond_loglog_linkphi_identity_vardisp) 
# works 

# arrange the plots 
par(mfrow=c(2,1))

# plot fitted model 
plot(dataONEperpond$FPcover_max ~ dataONEperpond$TOTP_avg,main="dataONEperpond - logloglink",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataONEperpond$TOTP_avg, dataONEperpond$TOTP_avg >0),betareg_dataONEperpond_loglog_linkphi_identity_vardisp$fitted,type="p",col="red")

# plot the variable dispersion
plot(predict(betareg_dataONEperpond_loglog_linkphi_identity_vardisp, type="precision") ~ subset(dataONEperpond$TOTP_avg, dataONEperpond$TOTP_avg >0),xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

##########################
# Beta regression        #
# dataONEperpondoutliers #
# link: loglog           #
# Variable dispersion    #
##########################
formula <-  FPcover_max ~ TOTP_avg | TOTP_avg
betareg_dataONEperpondoutliers_loglog_vardisp <- betareg(formula, data=dataONEperpondoutliers, link="loglog")
summary(betareg_dataONEperpondoutliers_loglog_vardisp) 
AIC(betareg_dataONEperpondoutliers_loglog_vardisp) 
# works 

# arrange the plots 
par(mfrow=c(2,1))

# plot fitted model 
plot(dataONEperpondoutliers$FPcover_max ~ dataONEperpondoutliers$TOTP_avg,main="dataONEperpondoutliers - loglog link",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataONEperpondoutliers$TOTP_avg, dataONEperpondoutliers$TOTP_avg >0),betareg_dataONEperpondoutliers_loglog_vardisp$fitted,type="p",col="red")

# plot the variable dispersion
plot(predict(betareg_dataONEperpondoutliers_loglog_vardisp, type="precision") ~ subset(dataONEperpondoutliers$TOTP_avg, dataONEperpondoutliers$TOTP_avg >0),xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

####################### 
# Beta regression     #
# dataONEperpond      #
# link: loglog        #
# linkphi:sqrt        #
# Variable dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg | TOTP_avg
betareg_dataONEperpond_loglog_sqrt_identity_vardisp <- betareg(formula, data=dataONEperpond, link="loglog",link.phi="sqrt")
summary(betareg_dataONEperpond_loglog_sqrt_identity_vardisp) 
AIC(betareg_dataONEperpond_loglog_sqrt_identity_vardisp) 
# works 

# arrange the plots 
par(mfrow=c(2,1))

# plot fitted model 
plot(dataONEperpond$FPcover_max ~ dataONEperpond$TOTP_avg,main="dataONEperpond - logloglink",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataONEperpond$TOTP_avg, dataONEperpond$TOTP_avg >0),betareg_dataONEperpond_loglog_sqrt_identity_vardisp$fitted,type="p",col="red")

# plot the variable dispersion
plot(predict(betareg_dataONEperpond_loglog_sqrt_identity_vardisp, type="precision") ~ subset(dataONEperpond$TOTP_avg, dataONEperpond$TOTP_avg >0),xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

####################### 
# Beta regression     #
# dataONEperpond      #
# link: loglog        #
# Variable dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg | TOTP_avg
betareg_dataONEperpond_loglog_vardisp <- betareg(formula, data=dataONEperpond, link="loglog")
summary(betareg_dataONEperpond_loglog_vardisp) 
AIC(betareg_dataONEperpond_loglog_vardisp) 
# works 

# arrange the plots 
par(mfrow=c(2,1))

# plot fitted model 
plot(dataONEperpond$FPcover_max ~ dataONEperpond$TOTP_avg,main="dataONEperpond - logloglink",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataONEperpond$TOTP_avg, dataONEperpond$TOTP_avg >0),betareg_dataONEperpond_loglog_vardisp$fitted,type="p",col="red")

# plot the variable dispersion
plot(predict(betareg_dataONEperpond_loglog_vardisp, type="precision") ~ subset(dataONEperpond$TOTP_avg, dataONEperpond$TOTP_avg >0),xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

####################### 
# Beta regression     #
# dataFP              #
# link: loglog        #
# Variable dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg | TOTP_avg
betareg_dataFP_loglog_vardisp <- betareg(formula, data=dataFP, link="loglog")
summary(betareg_dataFP_loglog_vardisp) 
AIC(betareg_dataFP_loglog_vardisp) 
# works

# arrange the plots 
par(mfrow=c(2,1))

# plot fitted model 
plot(dataFP$FPcover_max ~ dataFP$TOTP_avg,main="dataFP - loglog link",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFP$TOTP_avg, dataFP$TOTP_avg >0),betareg_dataFP_loglog_vardisp$fitted,type="p",col="red")

# plot the variable dispersion
plot(predict(betareg_dataFP_loglog_vardisp, type="precision") ~ subset(dataFP$TOTP_avg, dataFP$TOTP_avg >0),xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

####################### 
# Beta regression     #
# dataFPsmall         #
# link: loglog        #
# Variable dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg | TOTP_avg
betareg_dataFPsmall_loglog_vardisp <- betareg(formula, data=dataFPsmall, link="loglog")
summary(betareg_dataFPsmall_loglog_vardisp) 
AIC(betareg_dataFPsmall_loglog_vardisp) 
# works

# arrange the plots 
par(mfrow=c(2,1))

# plot fitted model 
plot(dataFPsmall$FPcover_max ~ dataFPsmall$TOTP_avg,main="dataFPsmall - loglog link",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPsmall$TOTP_avg, dataFPsmall$TOTP_avg >0),betareg_dataFPsmall_loglog_vardisp$fitted,type="p",col="red")

# plot the variable dispersion
plot(predict(betareg_dataFPsmall_loglog_vardisp, type="precision") ~ subset(dataFPsmall$TOTP_avg, dataFPsmall$TOTP_avg >0),xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

####################### 
# Beta regression     #
# dataFPoutliers      #
# link: loglog        #
# Variable dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg | TOTP_avg
betareg_dataFPoutliers_loglog_vardisp <- betareg(formula, data=dataFPoutliers, link="loglog")
summary(betareg_dataFPoutliers_loglog_vardisp) 
AIC(betareg_dataFPoutliers_loglog_vardisp) 
# works

# arrange the plots 
par(mfrow=c(2,1))

# plot fitted model 
plot(dataFPoutliers$FPcover_max ~ dataFPoutliers$TOTP_avg,main="dataFPoutliers - loglog link",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPoutliers$TOTP_avg, dataFPoutliers$TOTP_avg >0),betareg_dataFPoutliers_loglog_vardisp$fitted,type="p",col="red")

# plot the variable dispersion
plot(predict(betareg_dataFPoutliers_loglog_vardisp, type="precision") ~ subset(dataFPoutliers$TOTP_avg, dataFPoutliers$TOTP_avg >0),xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

####################### 
# Beta regression     #
# dataFPoutlierssmall #
# link: loglog        #
# Variable dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg | TOTP_avg
betareg_dataFPoutlierssmall_loglog_vardisp <- betareg(formula, data=dataFPoutlierssmall, link="loglog")
summary(betareg_dataFPoutlierssmall_loglog_vardisp) 
AIC(betareg_dataFPoutlierssmall_loglog_vardisp) 
# works

# arrange the plots 
par(mfrow=c(2,1))

# plot fitted model 
plot(dataFPoutlierssmall$FPcover_max ~ dataFPoutlierssmall$TOTP_avg,main="dataFPoutlierssmall - loglog link",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPoutlierssmall$TOTP_avg, dataFPoutlierssmall$TOTP_avg >0),betareg_dataFPoutlierssmall_loglog_vardisp$fitted,type="p",col="red")

# plot the variable dispersion
plot(predict(betareg_dataFPoutlierssmall_loglog_vardisp, type="precision") ~ subset(dataFPoutlierssmall$TOTP_avg, dataFPoutlierssmall$TOTP_avg >0),xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

