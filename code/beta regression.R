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

# add plot 

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

# add plot 

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

# add plot 

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

# add plot 

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

# add plot 

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
# dataONEperpond      #
# link: logit         #
# Constant dispersion #
#######################
formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataONEperpond_logit <- betamix(formula, link="logit", data=dataONEperpond, k = 2, nstart = 100)
summary(betareg_mix_dataONEperpond_logit) 
AIC(betareg_mix_dataONEperpond_logit) 
# one cluster 

# add plot 

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
AIC(betareg_mix_dataFP_logit) 
# two clusters 

# add plot 

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
AIC(betareg_mix_dataFPsmall_logit) 
# one cluster

# add plot 

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
AIC(betareg_mix_dataFPoutliers_logit) 
# two clusters

# add plot 

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
AIC(betareg_mix_dataFPoutlierssmall_logit) 
# one cluster

# add plot 






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


