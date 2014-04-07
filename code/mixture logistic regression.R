############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA #
# Latent class mixture model  regression   #
############################################

library("flexmix")
library(ggplot2)

# re-format data so it is in the form: col 1 is no. successes and col 2 is no. failures
dataONEperpond_flexmix <- cbind(ceiling(dataONEperpond$FPcover_max*100),(100-(ceiling(dataONEperpond$FPcover_max*100))),dataONEperpond$TOTP_avg)
colnames(dataONEperpond_flexmix) <- c("FPcover_max","NotFP","TOTP_avg") # rename columns 
dataONEperpond_flexmix <- as.data.frame(dataONEperpond_flexmix) # convert to a data frame 
dataONEperpond_flexmix <- dataONEperpond_flexmix[complete.cases(dataONEperpond_flexmix),]# remove any NAs

dataFP_flexmix <- cbind(ceiling(dataFP$FPcover_max*100),(100-(ceiling(dataFP$FPcover_max*100))),dataFP$TOTP_avg)
colnames(dataFP_flexmix) <- c("FPcover_max","NotFP","TOTP_avg")
dataFP_flexmix <- as.data.frame(dataFP_flexmix) # convert to a data frame 
dataFP_flexmix <- dataFP_flexmix[complete.cases(dataFP_flexmix),]# remove any NAs

dataFPsmall_flexmix <- cbind(ceiling(dataFPsmall$FPcover_max*100),(100-(ceiling(dataFPsmall$FPcover_max*100))),dataFPsmall$TOTP_avg)
colnames(dataFPsmall_flexmix) <- c("FPcover_max","NotFP","TOTP_avg")
dataFPsmall_flexmix <- as.data.frame(dataFPsmall_flexmix) # convert to a data frame 
dataFPsmall_flexmix <- dataFPsmall_flexmix[complete.cases(dataFPsmall_flexmix),]# remove any NAs

dataFPoutliers_flexmix <- cbind(ceiling(dataFPoutliers$FPcover_max*100),(100-(ceiling(dataFPoutliers$FPcover_max*100))),dataFPoutliers$TOTP_avg)
colnames(dataFPoutliers_flexmix) <- c("FPcover_max","NotFP","TOTP_avg")
dataFPoutliers_flexmix <- as.data.frame(dataFPoutliers_flexmix) # convert to a data frame 
dataFPoutliers_flexmix <- dataFPoutliers_flexmix[complete.cases(dataFPoutliers_flexmix),]# remove any NAs

dataFPoutlierssmall_flexmix <- cbind(ceiling(dataFPoutlierssmall$FPcover_max*100),(100-(ceiling(dataFPoutlierssmall$FPcover_max*100))),dataFPoutlierssmall$TOTP_avg)
colnames(dataFPoutlierssmall_flexmix) <- c("FPcover_max","NotFP","TOTP_avg")
dataFPoutlierssmall_flexmix <- as.data.frame(dataFPoutlierssmall_flexmix) # convert to a data frame 
dataFPoutlierssmall_flexmix <- dataFPoutlierssmall_flexmix[complete.cases(dataFPoutlierssmall_flexmix),]# remove any NAs

########################## 
# Latent Mixture         #
# dataONEperpond_flexmix #
# family: binomial       #
##########################
flexmix_dataONEperpond_binomial <- flexmix(cbind(FPcover_max,NotFP) ~ TOTP_avg, data=dataONEperpond_flexmix, k=2, model=FLXMRglm(family="binomial"))
flexmix_dataONEperpond_binomial
summary(flexmix_dataONEperpond_binomial)
parameters(flexmix_dataONEperpond_binomial,component=1) # logistic regression coefficients - component 1
parameters(flexmix_dataONEperpond_binomial,component=2) # logistic regression coefficients - component 2

dataONEperpond_flexmix$cluster<-clusters(flexmix_dataONEperpond_binomial) # add cluster identities to your original data.frame 

temp <- ggplot(dataONEperpond_flexmix,aes(x=TOTP_avg,y=FPcover_max,shape=factor(cluster))) + geom_point() 
temp <- temp + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(cluster))) 
temp

xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")

geom_text(aes(x=0.015,y=1,label="f)"),size=7)

scale_x_log10()
theme(legend.position="none")
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
scale_y_continuous(breaks=y_breaks,labels=y_labels)
geom_vline(xintercept=0.01972,colour="red",size=1,linetype="longdash") # add vertical line @ lower threshold value 
geom_vline(xintercept=0.2085,colour="red",size=1,linetype="longdash") # add vertical line @ upper threshold value 
theme_classic(base_size=18) + theme(legend.position="none")
theme(axis.title.y=element_blank())



# rootogram of posterior class probabilities
# Peak at prob. 1 = mixture component is well seperated from the other components
# No peak at 1 and/or signiﬁcant mass in the middle of the unit interval indicates overlap with other components.
plot(flexmix_dataONEperpond_binomial)

# still need a method to plot this 
# predicted values for component 1
predict(flexmix_dataONEperpond_binomial)[1]

# predicted values for component 2
predict(flexmix_dataONEperpond_binomial)[2]

# plot fitted model 
plot(dataONEperpond_flexmix$FPcover_max ~ dataONEperpond_flexmix$TOTP_avg,main="dataONEperpond_flexmix",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataONEperpond_flexmix$TOTP_avg, dataONEperpond_flexmix$TOTP_avg >0),flexmix_dataONEperpond_binomial$fitted,type="p",col="red")

####################### 
# Latent Mixture      #
# dataFP              #
# family: binomial    #
#######################
formula <- FPcover_max ~ TOTP_avg
flexmix_dataFP_binomial <- flexmix(formula, data=dataFP, k=2, model=FLXMRglm(family="binomial"))
summary(flexmix_dataFP_binomial)
AIC(flexmix_dataFP_binomial)
# WORKS!

# plot fitted model 
plot(dataFP$FPcover_max ~ dataFP$TOTP_avg,main="dataFP",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFP$TOTP_avg, dataFP$TOTP_avg >0),flexmix_dataFP_binomial$fitted,type="p",col="red")

####################### 
# Latent Mixture      #
# dataFPsmall         #
# family: binomial    #
#######################
formula <- FPcover_max ~ TOTP_avg
flexmix_dataFPsmall_binomial <- flexmix(formula, data=dataFPsmall, k=2, model=FLXMRglm(family="binomial"))
summary(flexmix_dataFPsmall_binomial)
AIC(flexmix_dataFPsmall_binomial)
# WORKS!

# plot fitted model 
plot(dataFPsmall$FPcover_max ~ dataFPsmall$TOTP_avg,main="dataFPsmall",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPsmall$TOTP_avg, dataFPsmall$TOTP_avg >0),flexmix_dataFPsmall_binomial$fitted,type="p",col="red")

####################### 
# Latent Mixture      #
# dataFPoutliers      #
# family: binomial    #
#######################
formula <- FPcover_max ~ TOTP_avg
flexmix_dataFPoutliers_binomial <- flexmix(FPcover_max ~ TOTP_avg, data=dataFPoutliers, k=2, model=FLXMRglm(family="binomial"))
summary(flexmix_dataFPoutliers_binomial)
AIC(flexmix_dataFPoutliers_binomial)
# WORKS!

# plot fitted model 
plot(dataFPoutliers$FPcover_max ~ dataFPoutliers$TOTP_avg,main="dataFPoutliers",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPoutliers$TOTP_avg, dataFPoutliers$TOTP_avg >0),flexmix_dataFPoutliers_binomial$fitted,type="p",col="red")

####################### 
# Latent Mixture      #
# dataFPoutlierssmall #
# family: binomial    #
#######################
formula <- FPcover_max ~ TOTP_avg
flexmix_dataFPoutlierssmall_binomial <- flexmix(FPcover_max ~ TOTP_avg, data=dataFPoutlierssmall, k=2, model=FLXMRglm(family="binomial"))
summary(flexmix_dataFPoutlierssmall_binomial)
AIC(flexmix_dataFPoutlierssmall_binomial)
# works!

# plot fitted model 
plot(dataFPoutlierssmall$FPcover_max ~ dataFPoutlierssmall$TOTP_avg,main="dataFPoutlierssmall",xlab="Total P (mg/L)",ylab="FP cover",log="x")
lines(subset(dataFPoutlierssmall$TOTP_avg, dataFPoutlierssmall$TOTP_avg >0),flexmix_dataFPoutlierssmall_binomial$fitted,type="p",col="red")
