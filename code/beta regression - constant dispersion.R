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
# Constant dispersion #
#######################
betareg_dataONEperpond_logit <- betareg(FPcover_max ~ TOTP_avg, data=dataONEperpond, link="logit", type="ML")
summary(betareg_dataONEperpond_logit)
AIC(betareg_dataONEperpond_logit)
# WORKS!

# plot fitted model 
plot(dataONEperpond$FPcover_max ~ dataONEperpond$TOTP_avg,main="dataONEperpond",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataONEperpond$TOTP_avg, dataONEperpond$TOTP_avg >0),betareg_dataONEperpond_logit$fitted,type="p",col="red")

########################## 
# Beta regression        #
# dataONEperpondoutliers #
# link: logit            #
# Constant dispersion    #
##########################
betareg_dataONEperpondoutliers_logit <- betareg(FPcover_max ~ TOTP_avg, data=dataONEperpondoutliers, link="logit", type="ML")
summary(betareg_dataONEperpondoutliers_logit)
AIC(betareg_dataONEperpondoutliers_logit)
# WORKS!

# plot fitted model 
plot(dataONEperpondoutliers$FPcover_max ~ dataONEperpondoutliers$TOTP_avg,main="dataONEperpondoutliers",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataONEperpondoutliers$TOTP_avg, dataONEperpondoutliers$TOTP_avg >0),betareg_dataONEperpondoutliers_logit$fitted,type="p",col="red")

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

temp01<-betareg_dataFP_logit$fitted
temp<-cbind(subset(dataFP$TOTP_avg, dataFP$TOTP_avg >0),betareg_dataFP_logit$fitted)

# plot betareg_dataFP_logit with ggplot2 
dataFP_beta_logit <- ggplot(dataFP,aes(x=TOTP_avg,y=FPcover_max)) + geom_point(size=3) 
dataFP_beta_logit <- dataFP_beta_logit + geom_abline(aes(x=subset(dataFP$TOTP_avg, dataFP$TOTP_avg >0),y=betareg_dataFP_logit$fitted))
dataFP_beta_logit

#### problems with adding the second line - probably because I am trying to override the aesthetics 

dataFP_beta_logit <- dataFP_beta_logit + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataFP_beta_logit <- dataFP_beta_logit + ggtitle("dataFP - logit link")
dataFP_beta_logit <- dataFP_beta_logit + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataFP_beta_logit <- dataFP_beta_logit + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataFP_beta_logit <- dataFP_beta_logit + theme_classic(base_size=18)
dataFP_beta_logit

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

########################## 
# Beta regression        #
# dataONEperpondoutliers #
# link: probit           #
# Constant dispersion    #
##########################
betareg_dataONEperpondoutliers_probit <- betareg(FPcover_max ~ TOTP_avg, data=dataONEperpondoutliers, link="probit", type="ML")
summary(betareg_dataONEperpondoutliers_probit)
AIC(betareg_dataONEperpondoutliers_probit)
# WORKS!

# plot fitted model 
plot(dataONEperpondoutliers$FPcover_max ~ dataONEperpondoutliers$TOTP_avg,main="dataONEperpondoutliers",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataONEperpondoutliers$TOTP_avg, dataONEperpondoutliers$TOTP_avg >0),betareg_dataONEperpondoutliers_probit$fitted,type="p",col="red")

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

########################## 
# Beta regression        #
# dataONEperpondoutliers #
# link: loglog           #
# Constant dispersion    #
##########################
betareg_dataONEperpondoutliers_loglog <- betareg(FPcover_max ~ TOTP_avg, data=dataONEperpondoutliers, link="loglog", type="ML")
summary(betareg_dataONEperpondoutliers_loglog)
AIC(betareg_dataONEperpondoutliers_loglog)
# WORKS!

# plot fitted model 
plot(dataONEperpondoutliers$FPcover_max ~ dataONEperpondoutliers$TOTP_avg,main="dataONEperpondoutliers",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataONEperpondoutliers$TOTP_avg, dataONEperpondoutliers$TOTP_avg >0),betareg_dataONEperpondoutliers_loglog$fitted,type="p",col="red")

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

