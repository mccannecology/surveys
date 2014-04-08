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
# Constant dispersion #
#######################
betareg_dataONEperpond_logit <- betareg(FPcover_max ~ TOTP_avg, data=dataONEperpond, link="logit", type="ML")
summary(betareg_dataONEperpond_logit)
AIC(betareg_dataONEperpond_logit)
# WORKS!

# plot fitted model 
plot(dataONEperpond$FPcover_max ~ dataONEperpond$TOTP_avg,main="dataONEperpond",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataONEperpond$TOTP_avg, dataONEperpond$TOTP_avg >0),betareg_dataONEperpond_logit$fitted,type="p",col="red")

####################### 
# Beta regression     #
# dataFP              #
# link: logit         #
# Constant dispersion #
#######################
betareg_dataFP_logit <- betareg(FPcover_max ~ TOTP_avg, data=dataFP, link="logit")
summary(betareg_dataFP_logit)
AIC(betareg_dataFP_logit)
# WORKS!

# plot fitted model 
plot(dataFP$FPcover_max ~ dataFP$TOTP_avg,main="dataFP",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFP$TOTP_avg, dataFP$TOTP_avg >0),betareg_dataFP_logit$fitted,type="p",col="red")

####################### 
# Beta regression     #
# dataFPsmall         #
# link: logit         #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_dataFPsmall_logit <- betareg(formula, data=dataFPsmall, link="logit")
summary(betareg_dataFPsmall_logit)
AIC(betareg_dataFPsmall_logit)
# WORKS!

# plot fitted model 
plot(dataFPsmall$FPcover_max ~ dataFPsmall$TOTP_avg,main="dataFPsmall",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPsmall$TOTP_avg, dataFPsmall$TOTP_avg >0),betareg_dataFPsmall_logit$fitted,type="p",col="red")

# plot the dispersion
# not a very interesting plot
plot(predict(betareg_dataFPsmall_logit, type="precision") ~ subset(dataFPsmall$TOTP_avg, dataFPsmall$TOTP_avg >0),xlab="Total P (mg/L)",ylab="Precision (phi)",log="x")

####################### 
# Beta regression     #
# dataFPoutliers      #
# link: logit         #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_dataFPoutliers_logit <- betareg(FPcover_max ~ TOTP_avg, data=dataFPoutliers, link="logit")
summary(betareg_dataFPoutliers_logit)
AIC(betareg_dataFPoutliers_logit)
# WORKS!

# plot fitted model 
plot(dataFPoutliers$FPcover_max ~ dataFPoutliers$TOTP_avg,main="dataFPoutliers",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPoutliers$TOTP_avg, dataFPoutliers$TOTP_avg >0),betareg_dataFPoutliers_logit$fitted,type="p",col="red")

####################### 
# Beta regression     #
# dataFPoutlierssmall #
# link: logit         #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_dataFPoutlierssmall_logit <- betareg(FPcover_max ~ TOTP_avg, data=dataFPoutlierssmall, link="logit")
summary(betareg_dataFPoutlierssmall_logit)
AIC(betareg_dataFPoutlierssmall_logit)
# works!

# plot fitted model 
plot(dataFPoutlierssmall$FPcover_max ~ dataFPoutlierssmall$TOTP_avg,main="dataFPoutlierssmall",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPoutlierssmall$TOTP_avg, dataFPoutlierssmall$TOTP_avg >0),betareg_dataFPoutlierssmall_logit$fitted,type="p",col="red")

####################### 
# Beta regression     #
# dataONEperpond      #
# link: probit        #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_dataONEperpond_probit <- betareg(formula, data=dataONEperpond, link="probit")
summary(betareg_dataONEperpond_probit) 
AIC(betareg_dataONEperpond_probit) 
# works

# plot fitted model 
plot(dataONEperpond$FPcover_max ~ dataONEperpond$TOTP_avg,main="dataONEperpond",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataONEperpond$TOTP_avg, dataONEperpond$TOTP_avg >0),betareg_dataONEperpond_probit$fitted,type="p",col="red")

####################### 
# Beta regression     #
# dataFP              #
# link: probit        #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_dataFP_probit <- betareg(formula, data=dataFP, link="probit")
summary(betareg_dataFP_probit) 
AIC(betareg_dataFP_probit) 
# works

# plot fitted model 
plot(dataFP$FPcover_max ~ dataFP$TOTP_avg,main="dataFP",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFP$TOTP_avg, dataFP$TOTP_avg >0),betareg_dataFP_probit$fitted,type="p",col="red")

####################### 
# Beta regression     #
# dataFPsmall         #
# link: probit        #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_dataFPsmall_probit <- betareg(formula, data=dataFPsmall, link="probit")
summary(betareg_dataFPsmall_probit) 
AIC(betareg_dataFPsmall_probit) 
# works

# plot fitted model 
plot(dataFPsmall$FPcover_max ~ dataFPsmall$TOTP_avg,main="dataFPsmall",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPsmall$TOTP_avg, dataFPsmall$TOTP_avg >0),betareg_dataFPsmall_probit$fitted,type="p",col="red")

####################### 
# Beta regression     #
# dataFPoutliers      #
# link: probit        #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_dataFPoutliers_probit <- betareg(formula, data=dataFPoutliers, link="probit")
summary(betareg_dataFPoutliers_probit) 
AIC(betareg_dataFPoutliers_probit) 
# works

# plot fitted model 
plot(dataFPoutliers$FPcover_max ~ dataFPoutliers$TOTP_avg,main="dataFPoutliers",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPoutliers$TOTP_avg, dataFPoutliers$TOTP_avg >0),betareg_dataFPoutliers_probit$fitted,type="p",col="red")

####################### 
# Beta regression     #
# dataFPoutlierssmall #
# link: probit        #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_dataFPoutlierssmall_probit <- betareg(formula, data=dataFPoutlierssmall, link="probit")
summary(betareg_dataFPoutlierssmall_probit) 
AIC(betareg_dataFPoutlierssmall_probit) 
# works

# plot fitted model 
plot(dataFPoutlierssmall$FPcover_max ~ dataFPoutlierssmall$TOTP_avg,main="dataFPoutlierssmall",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPoutlierssmall$TOTP_avg, dataFPoutlierssmall$TOTP_avg >0),betareg_dataFPoutlierssmall_probit$fitted,type="p",col="red")

####################### 
# Beta regression     #
# dataONEperpond      #
# link: loglog        #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_dataONEperpond_loglog <- betareg(formula, data=dataONEperpond, link="loglog")
summary(betareg_dataONEperpond_loglog) 
AIC(betareg_dataONEperpond_loglog) 
# works

# plot fitted model 
plot(dataONEperpond$FPcover_max ~ dataONEperpond$TOTP_avg,main="dataONEperpond",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataONEperpond$TOTP_avg, dataONEperpond$TOTP_avg >0),betareg_dataONEperpond_loglog$fitted,type="p",col="red")

####################### 
# Beta regression     #
# dataFP              #
# link: loglog        #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_dataFP_loglog <- betareg(formula, data=dataFP, link="loglog")
summary(betareg_dataFP_loglog) 
AIC(betareg_dataFP_loglog) 
# works

# plot fitted model 
plot(dataFP$FPcover_max ~ dataFP$TOTP_avg,main="dataFP",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFP$TOTP_avg, dataFP$TOTP_avg >0),betareg_dataFP_loglog$fitted,type="p",col="red")

####################### 
# Beta regression     #
# dataFPsmall         #
# link: loglog        #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_dataFPsmall_loglog <- betareg(formula, data=dataFPsmall, link="loglog")
summary(betareg_dataFPsmall_loglog) 
AIC(betareg_dataFPsmall_loglog) 
# works

# plot fitted model 
plot(dataFPsmall$FPcover_max ~ dataFPsmall$TOTP_avg,main="dataFPsmall",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPsmall$TOTP_avg, dataFPsmall$TOTP_avg >0),betareg_dataFPsmall_loglog$fitted,type="p",col="red")

####################### 
# Beta regression     #
# dataFPoutliers      #
# link: loglog        #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_dataFPoutliers_loglog <- betareg(formula, data=dataFPoutliers, link="loglog")
summary(betareg_dataFPoutliers_loglog) 
AIC(betareg_dataFPoutliers_loglog) 
# works

# plot fitted model 
plot(dataFPoutliers$FPcover_max ~ dataFPoutliers$TOTP_avg,main="dataFPoutliers",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPoutliers$TOTP_avg, dataFPoutliers$TOTP_avg >0),betareg_dataFPoutliers_loglog$fitted,type="p",col="red")

####################### 
# Beta regression     #
# dataFPoutlierssmall #
# link: loglog        #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_dataFPoutlierssmall_loglog <- betareg(formula, data=dataFPoutlierssmall, link="loglog")
summary(betareg_dataFPoutlierssmall_loglog) 
AIC(betareg_dataFPoutlierssmall_loglog) 
# works

# plot fitted model 
plot(dataFPoutlierssmall$FPcover_max ~ dataFPoutlierssmall$TOTP_avg,main="dataFPoutlierssmall",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPoutlierssmall$TOTP_avg, dataFPoutlierssmall$TOTP_avg >0),betareg_dataFPoutlierssmall_loglog$fitted,type="p",col="red")

####################### 
# Beta regression     #
# dataONEperpond      #
# link: logit         #
# Variable dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg | TOTP_avg
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

####################### 
# Beta regression     #
# Mixed model         #
# dataONEperpondoutliers #
# link: logit         #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataONEperpondoutliers_logit <- betamix(formula, link="logit", data=dataONEperpondoutliers, k = 2, nstart = 100)
summary(betareg_mix_dataONEperpondoutliers_logit) 
logLik(betareg_mix_dataONEperpondoutliers_logit) 
AIC(betareg_mix_dataONEperpondoutliers_logit) 
# two clusters 

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataONEperpondoutliers$beta_logit_cluster <- rep(NA, nrow(dataONEperpondoutliers))
dataONEperpondoutliers_TOTP <- subset(dataONEperpondoutliers, dataONEperpondoutliers$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataONEperpondoutliers_noTOTP <- subset(dataONEperpondoutliers, is.na(dataONEperpondoutliers$TOTP_avg)) # and waterbodies w/o TOTP
dataONEperpondoutliers_TOTP$beta_logit_cluster<-clusters(betareg_mix_dataONEperpondoutliers_logit) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataONEperpondoutliers <- merge(dataONEperpondoutliers_TOTP,dataONEperpondoutliers_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataONEperpondoutliers_TOTP,dataONEperpondoutliers_noTOTP)

# plot 
dataONEperpondoutliers_beta_logit_cluster_plot <- ggplot(dataONEperpondoutliers,aes(x=TOTP_avg,y=FPcover_max,colour=factor(beta_logit_cluster),shape=factor(beta_logit_cluster))) + geom_point(size=3) 
dataONEperpondoutliers_beta_logit_cluster_plot <- dataONEperpondoutliers_beta_logit_cluster_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(beta_logit_cluster))) 
dataONEperpondoutliers_beta_logit_cluster_plot <- dataONEperpondoutliers_beta_logit_cluster_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataONEperpondoutliers_beta_logit_cluster_plot <- dataONEperpondoutliers_beta_logit_cluster_plot + ggtitle("dataONEperpondoutliers - logit link")
dataONEperpondoutliers_beta_logit_cluster_plot <- dataONEperpondoutliers_beta_logit_cluster_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataONEperpondoutliers_beta_logit_cluster_plot <- dataONEperpondoutliers_beta_logit_cluster_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataONEperpondoutliers_beta_logit_cluster_plot <- dataONEperpondoutliers_beta_logit_cluster_plot + theme_classic(base_size=18)
dataONEperpondoutliers_beta_logit_cluster_plot

# save the plot 
ggsave(file="dataONEperpondoutliers_beta_logit_cluster_plot.jpg", dataONEperpondoutliers_beta_logit_cluster_plot, height=8,width=11)

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
logLik(betareg_mix_dataONEperpond_logit) 
AIC(betareg_mix_dataONEperpond_logit) 
# two clusters 

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataONEperpond$beta_logit_cluster <- rep(NA, nrow(dataONEperpond))
dataONEperpond_TOTP <- subset(dataONEperpond, dataONEperpond$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataONEperpond_noTOTP <- subset(dataONEperpond, is.na(dataONEperpond$TOTP_avg)) # and waterbodies w/o TOTP
dataONEperpond_TOTP$beta_logit_cluster<-clusters(betareg_mix_dataONEperpond_logit) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataONEperpond <- merge(dataONEperpond_TOTP,dataONEperpond_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataONEperpond_TOTP,dataONEperpond_noTOTP)

# plot 
dataONEperpond_beta_logit_cluster_plot <- ggplot(dataONEperpond,aes(x=TOTP_avg,y=FPcover_max,colour=factor(beta_logit_cluster),shape=factor(beta_logit_cluster))) + geom_point(size=3) 
dataONEperpond_beta_logit_cluster_plot <- dataONEperpond_beta_logit_cluster_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(beta_logit_cluster))) 
dataONEperpond_beta_logit_cluster_plot <- dataONEperpond_beta_logit_cluster_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataONEperpond_beta_logit_cluster_plot <- dataONEperpond_beta_logit_cluster_plot + ggtitle("dataONEperpond - logit link")
dataONEperpond_beta_logit_cluster_plot <- dataONEperpond_beta_logit_cluster_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataONEperpond_beta_logit_cluster_plot <- dataONEperpond_beta_logit_cluster_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataONEperpond_beta_logit_cluster_plot <- dataONEperpond_beta_logit_cluster_plot + theme_classic(base_size=18)
dataONEperpond_beta_logit_cluster_plot

# save the plot 
ggsave(file="dataONEperpond_beta_logit_cluster_plot.jpg", dataONEperpond_beta_logit_cluster_plot, height=8,width=11)

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
logLik(betareg_mix_dataFP_logit) 
AIC(betareg_mix_dataFP_logit) 
# two clusters 

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataFP$beta_logit_cluster <- rep(NA, nrow(dataFP))
dataFP_TOTP <- subset(dataFP, dataFP$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataFP_noTOTP <- subset(dataFP, is.na(dataFP$TOTP_avg)) # and waterbodies w/o TOTP
dataFP_TOTP$beta_logit_cluster<-clusters(betareg_mix_dataFP_logit) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataFP <- merge(dataFP_TOTP,dataFP_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataFP_TOTP,dataFP_noTOTP)

# plot 
dataFP_beta_logit_cluster_plot <- ggplot(dataFP,aes(x=TOTP_avg,y=FPcover_max,colour=factor(beta_logit_cluster),shape=factor(beta_logit_cluster))) + geom_point(size=3) 
dataFP_beta_logit_cluster_plot <- dataFP_beta_logit_cluster_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(beta_logit_cluster))) 
dataFP_beta_logit_cluster_plot <- dataFP_beta_logit_cluster_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataFP_beta_logit_cluster_plot <- dataFP_beta_logit_cluster_plot + ggtitle("dataFP - logit link")
dataFP_beta_logit_cluster_plot <- dataFP_beta_logit_cluster_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataFP_beta_logit_cluster_plot <- dataFP_beta_logit_cluster_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataFP_beta_logit_cluster_plot <- dataFP_beta_logit_cluster_plot + theme_classic(base_size=18)
dataFP_beta_logit_cluster_plot

# save the plot 
ggsave(file="dataFP_beta_logit_cluster_plot.jpg", dataFP_beta_logit_cluster_plot, height=8,width=11)

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
logLik(betareg_mix_dataFPsmall_logit) 
AIC(betareg_mix_dataFPsmall_logit) 
# one cluster

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataFPsmall$beta_logit_cluster <- rep(NA, nrow(dataFPsmall))
dataFPsmall_TOTP <- subset(dataFPsmall, dataFPsmall$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataFPsmall_noTOTP <- subset(dataFPsmall, is.na(dataFPsmall$TOTP_avg)) # and waterbodies w/o TOTP
dataFPsmall_TOTP$beta_logit_cluster<-clusters(betareg_mix_dataFPsmall_logit) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataFPsmall <- merge(dataFPsmall_TOTP,dataFPsmall_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataFPsmall_TOTP,dataFPsmall_noTOTP)

# plot 
dataFPsmall_beta_logit_cluster_plot <- ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max,colour=factor(beta_logit_cluster),shape=factor(beta_logit_cluster))) + geom_point(size=3) 
dataFPsmall_beta_logit_cluster_plot <- dataFPsmall_beta_logit_cluster_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(beta_logit_cluster))) 
dataFPsmall_beta_logit_cluster_plot <- dataFPsmall_beta_logit_cluster_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataFPsmall_beta_logit_cluster_plot <- dataFPsmall_beta_logit_cluster_plot + ggtitle("dataFPsmall - logit link")
dataFPsmall_beta_logit_cluster_plot <- dataFPsmall_beta_logit_cluster_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataFPsmall_beta_logit_cluster_plot <- dataFPsmall_beta_logit_cluster_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataFPsmall_beta_logit_cluster_plot <- dataFPsmall_beta_logit_cluster_plot + theme_classic(base_size=18)
dataFPsmall_beta_logit_cluster_plot

# save the plot 
ggsave(file="dataFPsmall_beta_logit_cluster_plot.jpg", dataFPsmall_beta_logit_cluster_plot, height=8,width=11)

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
logLik(betareg_mix_dataFPoutliers_logit) 
AIC(betareg_mix_dataFPoutliers_logit) 
# two clusters

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataFPoutliers$beta_logit_cluster <- rep(NA, nrow(dataFPoutliers))
dataFPoutliers_TOTP <- subset(dataFPoutliers, dataFPoutliers$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataFPoutliers_noTOTP <- subset(dataFPoutliers, is.na(dataFPoutliers$TOTP_avg)) # and waterbodies w/o TOTP
dataFPoutliers_TOTP$beta_logit_cluster<-clusters(betareg_mix_dataFPoutliers_logit) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataFPoutliers <- merge(dataFPoutliers_TOTP,dataFPoutliers_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataFPoutliers_TOTP,dataFPoutliers_noTOTP)

# plot 
dataFPoutliers_beta_logit_cluster_plot <- ggplot(dataFPoutliers,aes(x=TOTP_avg,y=FPcover_max,colour=factor(beta_logit_cluster),shape=factor(beta_logit_cluster))) + geom_point(size=3) 
dataFPoutliers_beta_logit_cluster_plot <- dataFPoutliers_beta_logit_cluster_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(beta_logit_cluster))) 
dataFPoutliers_beta_logit_cluster_plot <- dataFPoutliers_beta_logit_cluster_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataFPoutliers_beta_logit_cluster_plot <- dataFPoutliers_beta_logit_cluster_plot + ggtitle("dataFPoutliers - logit link")
dataFPoutliers_beta_logit_cluster_plot <- dataFPoutliers_beta_logit_cluster_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataFPoutliers_beta_logit_cluster_plot <- dataFPoutliers_beta_logit_cluster_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataFPoutliers_beta_logit_cluster_plot <- dataFPoutliers_beta_logit_cluster_plot + theme_classic(base_size=18)
dataFPoutliers_beta_logit_cluster_plot

# save the plot 
ggsave(file="dataFPoutliers_beta_logit_cluster_plot.jpg", dataFPoutliers_beta_logit_cluster_plot, height=8,width=11)

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
logLik(betareg_mix_dataFPoutlierssmall_logit) 
AIC(betareg_mix_dataFPoutlierssmall_logit) 
# one cluster

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataFPoutlierssmall$beta_logit_cluster <- rep(NA, nrow(dataFPoutlierssmall))
dataFPoutlierssmall_TOTP <- subset(dataFPoutlierssmall, dataFPoutlierssmall$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataFPoutlierssmall_noTOTP <- subset(dataFPoutlierssmall, is.na(dataFPoutlierssmall$TOTP_avg)) # and waterbodies w/o TOTP
dataFPoutlierssmall_TOTP$beta_logit_cluster<-clusters(betareg_mix_dataFPoutlierssmall_logit) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataFPoutlierssmall <- merge(dataFPoutlierssmall_TOTP,dataFPoutlierssmall_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataFPoutlierssmall_TOTP,dataFPoutlierssmall_noTOTP)

# plot 
dataFPoutlierssmall_beta_logit_cluster_plot <- ggplot(dataFPoutlierssmall,aes(x=TOTP_avg,y=FPcover_max,colour=factor(beta_logit_cluster),shape=factor(beta_logit_cluster))) + geom_point(size=3) 
dataFPoutlierssmall_beta_logit_cluster_plot <- dataFPoutlierssmall_beta_logit_cluster_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(beta_logit_cluster))) 
dataFPoutlierssmall_beta_logit_cluster_plot <- dataFPoutlierssmall_beta_logit_cluster_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataFPoutlierssmall_beta_logit_cluster_plot <- dataFPoutlierssmall_beta_logit_cluster_plot + ggtitle("dataFPoutlierssmall - logit link")
dataFPoutlierssmall_beta_logit_cluster_plot <- dataFPoutlierssmall_beta_logit_cluster_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataFPoutlierssmall_beta_logit_cluster_plot <- dataFPoutlierssmall_beta_logit_cluster_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataFPoutlierssmall_beta_logit_cluster_plot <- dataFPoutlierssmall_beta_logit_cluster_plot + theme_classic(base_size=18)
dataFPoutlierssmall_beta_logit_cluster_plot

# save the plot 
ggsave(file="dataFPoutlierssmall_beta_logit_cluster_plot.jpg", dataFPoutlierssmall_beta_logit_cluster_plot, height=8,width=11)

####################### 
# Beta regression     #
# Mixed model         #
# dataONEperpond      #
# link: loglog        #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataONEperpond_loglog <- betamix(formula, link="loglog", data=dataONEperpond, k = 2, nstart = 100)
summary(betareg_mix_dataONEperpond_loglog) 
logLik(betareg_mix_dataONEperpond_loglog) 
AIC(betareg_mix_dataONEperpond_loglog) 
# one cluster

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataONEperpond$beta_loglog_cluster <- rep(NA, nrow(dataONEperpond))
dataONEperpond_TOTP <- subset(dataONEperpond, dataONEperpond$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataONEperpond_noTOTP <- subset(dataONEperpond, is.na(dataONEperpond$TOTP_avg)) # and waterbodies w/o TOTP
dataONEperpond_TOTP$beta_loglog_cluster<-clusters(betareg_mix_dataONEperpond_loglog) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataONEperpond <- merge(dataONEperpond_TOTP,dataONEperpond_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataONEperpond_TOTP,dataONEperpond_noTOTP)

# plot 
dataONEperpond_beta_loglog_cluster_plot <- ggplot(dataONEperpond,aes(x=TOTP_avg,y=FPcover_max,colour=factor(beta_loglog_cluster),shape=factor(beta_loglog_cluster))) + geom_point(size=3) 
dataONEperpond_beta_loglog_cluster_plot <- dataONEperpond_beta_loglog_cluster_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(beta_loglog_cluster))) 
dataONEperpond_beta_loglog_cluster_plot <- dataONEperpond_beta_loglog_cluster_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataONEperpond_beta_loglog_cluster_plot <- dataONEperpond_beta_loglog_cluster_plot + ggtitle("dataONEperpond - loglog link")
dataONEperpond_beta_loglog_cluster_plot <- dataONEperpond_beta_loglog_cluster_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataONEperpond_beta_loglog_cluster_plot <- dataONEperpond_beta_loglog_cluster_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataONEperpond_beta_loglog_cluster_plot <- dataONEperpond_beta_loglog_cluster_plot + theme_classic(base_size=18)
dataONEperpond_beta_loglog_cluster_plot

# save the plot 
ggsave(file="dataONEperpond_beta_loglog_cluster_plot.jpg", dataONEperpond_beta_loglog_cluster_plot, height=8,width=11)

####################### 
# Beta regression     #
# Mixed model         #
# dataFP              #
# link: loglog        #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataFP_loglog <- betamix(formula, link="loglog", data=dataFP, k = 2, nstart = 100)
summary(betareg_mix_dataFP_loglog) 
logLik(betareg_mix_dataFP_loglog) 
AIC(betareg_mix_dataFP_loglog) 
# two clusters 

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataFP$beta_loglog_cluster <- rep(NA, nrow(dataFP))
dataFP_TOTP <- subset(dataFP, dataFP$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataFP_noTOTP <- subset(dataFP, is.na(dataFP$TOTP_avg)) # and waterbodies w/o TOTP
dataFP_TOTP$beta_loglog_cluster<-clusters(betareg_mix_dataFP_loglog) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataFP <- merge(dataFP_TOTP,dataFP_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataFP_TOTP,dataFP_noTOTP)

# plot 
dataFP_beta_loglog_cluster_plot <- ggplot(dataFP,aes(x=TOTP_avg,y=FPcover_max,colour=factor(beta_loglog_cluster),shape=factor(beta_loglog_cluster))) + geom_point(size=3) 
dataFP_beta_loglog_cluster_plot <- dataFP_beta_loglog_cluster_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(beta_loglog_cluster))) 
dataFP_beta_loglog_cluster_plot <- dataFP_beta_loglog_cluster_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataFP_beta_loglog_cluster_plot <- dataFP_beta_loglog_cluster_plot + ggtitle("dataFP - loglog link")
dataFP_beta_loglog_cluster_plot <- dataFP_beta_loglog_cluster_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataFP_beta_loglog_cluster_plot <- dataFP_beta_loglog_cluster_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataFP_beta_loglog_cluster_plot <- dataFP_beta_loglog_cluster_plot + theme_classic(base_size=18)
dataFP_beta_loglog_cluster_plot

# save the plot 
ggsave(file="dataFP_beta_loglog_cluster_plot.jpg", dataFP_beta_loglog_cluster_plot, height=8,width=11)

####################### 
# Beta regression     #
# Mixed model         #
# dataFPsmall         #
# link: loglog        #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataFPsmall_loglog <- betamix(formula, link="loglog", data=dataFPsmall, k = 2, nstart = 100)
summary(betareg_mix_dataFPsmall_loglog) 
logLik(betareg_mix_dataFPsmall_loglog) 
AIC(betareg_mix_dataFPsmall_loglog) 
# one cluster

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataFPsmall$beta_loglog_cluster <- rep(NA, nrow(dataFPsmall))
dataFPsmall_TOTP <- subset(dataFPsmall, dataFPsmall$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataFPsmall_noTOTP <- subset(dataFPsmall, is.na(dataFPsmall$TOTP_avg)) # and waterbodies w/o TOTP
dataFPsmall_TOTP$beta_loglog_cluster<-clusters(betareg_mix_dataFPsmall_loglog) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataFPsmall <- merge(dataFPsmall_TOTP,dataFPsmall_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataFPsmall_TOTP,dataFPsmall_noTOTP)

# plot 
dataFPsmall_beta_loglog_cluster_plot <- ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max,colour=factor(beta_loglog_cluster),shape=factor(beta_loglog_cluster))) + geom_point(size=3) 
dataFPsmall_beta_loglog_cluster_plot <- dataFPsmall_beta_loglog_cluster_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(beta_loglog_cluster))) 
dataFPsmall_beta_loglog_cluster_plot <- dataFPsmall_beta_loglog_cluster_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataFPsmall_beta_loglog_cluster_plot <- dataFPsmall_beta_loglog_cluster_plot + ggtitle("dataFPsmall - loglog link")
dataFPsmall_beta_loglog_cluster_plot <- dataFPsmall_beta_loglog_cluster_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataFPsmall_beta_loglog_cluster_plot <- dataFPsmall_beta_loglog_cluster_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataFPsmall_beta_loglog_cluster_plot <- dataFPsmall_beta_loglog_cluster_plot + theme_classic(base_size=18)
dataFPsmall_beta_loglog_cluster_plot

# save the plot 
ggsave(file="dataFPsmall_beta_loglog_cluster_plot.jpg", dataFPsmall_beta_loglog_cluster_plot, height=8,width=11)

####################### 
# Beta regression     #
# Mixed model         #
# dataFPoutliers      #
# link: loglog        #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataFPoutliers_loglog <- betamix(formula, link="loglog", data=dataFPoutliers, k = 2, nstart = 100)
summary(betareg_mix_dataFPoutliers_loglog) 
logLik(betareg_mix_dataFPoutliers_loglog) 
AIC(betareg_mix_dataFPoutliers_loglog) 
# two clusters

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataFPoutliers$beta_loglog_cluster <- rep(NA, nrow(dataFPoutliers))
dataFPoutliers_TOTP <- subset(dataFPoutliers, dataFPoutliers$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataFPoutliers_noTOTP <- subset(dataFPoutliers, is.na(dataFPoutliers$TOTP_avg)) # and waterbodies w/o TOTP
dataFPoutliers_TOTP$beta_loglog_cluster<-clusters(betareg_mix_dataFPoutliers_loglog) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataFPoutliers <- merge(dataFPoutliers_TOTP,dataFPoutliers_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataFPoutliers_TOTP,dataFPoutliers_noTOTP)

# plot 
dataFPoutliers_beta_loglog_cluster_plot <- ggplot(dataFPoutliers,aes(x=TOTP_avg,y=FPcover_max,colour=factor(beta_loglog_cluster),shape=factor(beta_loglog_cluster))) + geom_point(size=3) 
dataFPoutliers_beta_loglog_cluster_plot <- dataFPoutliers_beta_loglog_cluster_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(beta_loglog_cluster))) 
dataFPoutliers_beta_loglog_cluster_plot <- dataFPoutliers_beta_loglog_cluster_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataFPoutliers_beta_loglog_cluster_plot <- dataFPoutliers_beta_loglog_cluster_plot + ggtitle("dataFPoutliers - loglog link")
dataFPoutliers_beta_loglog_cluster_plot <- dataFPoutliers_beta_loglog_cluster_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataFPoutliers_beta_loglog_cluster_plot <- dataFPoutliers_beta_loglog_cluster_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataFPoutliers_beta_loglog_cluster_plot <- dataFPoutliers_beta_loglog_cluster_plot + theme_classic(base_size=18)
dataFPoutliers_beta_loglog_cluster_plot

# save the plot 
ggsave(file="dataFPoutliers_beta_loglog_cluster_plot.jpg", dataFPoutliers_beta_loglog_cluster_plot, height=8,width=11)

####################### 
# Beta regression     #
# Mixed model         #
# dataFPoutlierssmall #
# link: loglog        #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataFPoutlierssmall_loglog <- betamix(formula, link="loglog", data=dataFPoutlierssmall, k = 2, nstart = 100)
summary(betareg_mix_dataFPoutlierssmall_loglog) 
logLik(betareg_mix_dataFPoutlierssmall_loglog) 
AIC(betareg_mix_dataFPoutlierssmall_loglog) 
# two clusters

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataFPoutlierssmall$beta_loglog_cluster <- rep(NA, nrow(dataFPoutlierssmall))
dataFPoutlierssmall_TOTP <- subset(dataFPoutlierssmall, dataFPoutlierssmall$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataFPoutlierssmall_noTOTP <- subset(dataFPoutlierssmall, is.na(dataFPoutlierssmall$TOTP_avg)) # and waterbodies w/o TOTP
dataFPoutlierssmall_TOTP$beta_loglog_cluster<-clusters(betareg_mix_dataFPoutlierssmall_loglog) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataFPoutlierssmall <- merge(dataFPoutlierssmall_TOTP,dataFPoutlierssmall_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataFPoutlierssmall_TOTP,dataFPoutlierssmall_noTOTP)

# plot 
dataFPoutlierssmall_beta_loglog_cluster_plot <- ggplot(dataFPoutlierssmall,aes(x=TOTP_avg,y=FPcover_max,colour=factor(beta_loglog_cluster),shape=factor(beta_loglog_cluster))) + geom_point(size=3) 
dataFPoutlierssmall_beta_loglog_cluster_plot <- dataFPoutlierssmall_beta_loglog_cluster_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(beta_loglog_cluster))) 
dataFPoutlierssmall_beta_loglog_cluster_plot <- dataFPoutlierssmall_beta_loglog_cluster_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataFPoutlierssmall_beta_loglog_cluster_plot <- dataFPoutlierssmall_beta_loglog_cluster_plot + ggtitle("dataFPoutlierssmall - loglog link")
dataFPoutlierssmall_beta_loglog_cluster_plot <- dataFPoutlierssmall_beta_loglog_cluster_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataFPoutlierssmall_beta_loglog_cluster_plot <- dataFPoutlierssmall_beta_loglog_cluster_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataFPoutlierssmall_beta_loglog_cluster_plot <- dataFPoutlierssmall_beta_loglog_cluster_plot + theme_classic(base_size=18)
dataFPoutlierssmall_beta_loglog_cluster_plot

# save the plot 
ggsave(file="dataFPoutlierssmall_beta_loglog_cluster_plot.jpg", dataFPoutlierssmall_beta_loglog_cluster_plot, height=8,width=11)








