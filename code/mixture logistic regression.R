############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA #
# Latent class mixture model  regression   #
############################################

library(flexmix)
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
dataONEperpond_flexmix[,1:2] <- dataONEperpond_flexmix[,1:2]/100 # convert 0-100 back to 0-1

# plot 
dataONEperpond_flexmix_plot <- ggplot(dataONEperpond_flexmix,aes(x=TOTP_avg,y=FPcover_max,shape=factor(cluster))) + geom_point(size=3) 
dataONEperpond_flexmix_plot <- dataONEperpond_flexmix_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(cluster))) 
dataONEperpond_flexmix_plot <- dataONEperpond_flexmix_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataONEperpond_flexmix_plot <- dataONEperpond_flexmix_plot + ggtitle("dataONEperpond")
dataONEperpond_flexmix_plot <- dataONEperpond_flexmix_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataONEperpond_flexmix_plot <- dataONEperpond_flexmix_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataONEperpond_flexmix_plot <- dataONEperpond_flexmix_plot + theme_classic(base_size=18)
dataONEperpond_flexmix_plot

# save the plot 
ggsave(file="dataONEperpond_flexmix_plot.jpg", dataONEperpond_flexmix_plot, height=8,width=11)

# rootogram of posterior class probabilities
# Peak at prob. 1 = mixture component is well seperated from the other components
# No peak at 1 and/or signiﬁcant mass in the middle of the unit interval indicates overlap with other components.
jpeg("flexmix_dataONEperpond_binomial.jpg")
plot(flexmix_dataONEperpond_binomial,sub="flexmix_dataONEperpond_binomial")
dev.off()

####################### 
# Latent Mixture      #
# dataFP              #
# family: binomial    #
#######################
flexmix_dataFP_binomial <- flexmix(cbind(FPcover_max,NotFP) ~ TOTP_avg, data=dataFP_flexmix, k=2, model=FLXMRglm(family="binomial"))
flexmix_dataFP_binomial
summary(flexmix_dataFP_binomial)
parameters(flexmix_dataFP_binomial,component=1) # logistic regression coefficients - component 1
parameters(flexmix_dataFP_binomial,component=2) # logistic regression coefficients - component 2
dataFP_flexmix$cluster<-clusters(flexmix_dataFP_binomial) # add cluster identities to your original data.frame 
dataFP_flexmix[,1:2] <- dataFP_flexmix[,1:2]/100 # convert 0-100 back to 0-1

# plot 
dataFP_flexmix_plot <- ggplot(dataFP_flexmix,aes(x=TOTP_avg,y=FPcover_max,shape=factor(cluster))) + geom_point(size=3) 
dataFP_flexmix_plot <- dataFP_flexmix_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(cluster))) 
dataFP_flexmix_plot <- dataFP_flexmix_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataFP_flexmix_plot <- dataFP_flexmix_plot + ggtitle("dataFP")
dataFP_flexmix_plot <- dataFP_flexmix_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataFP_flexmix_plot <- dataFP_flexmix_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataFP_flexmix_plot <- dataFP_flexmix_plot + theme_classic(base_size=18)
dataFP_flexmix_plot

# save the plot 
ggsave(file="dataFP_flexmix_plot.jpg", dataFP_flexmix_plot, height=8,width=11)

# rootogram of posterior class probabilities
# Peak at prob. 1 = mixture component is well seperated from the other components
# No peak at 1 and/or signiﬁcant mass in the middle of the unit interval indicates overlap with other components.
jpeg("flexmix_dataFP_binomial.jpg")
plot(flexmix_dataFP_binomial,sub="flexmix_dataFP_binomial")
dev.off()

####################### 
# Latent Mixture      #
# dataFPsmall         #
# family: binomial    #
#######################
flexmix_dataFPsmall_binomial <- flexmix(cbind(FPcover_max,NotFP) ~ TOTP_avg, data=dataFPsmall_flexmix, k=2, model=FLXMRglm(family="binomial"))
flexmix_dataFPsmall_binomial
summary(flexmix_dataFPsmall_binomial)
parameters(flexmix_dataFPsmall_binomial,component=1) # logistic regression coefficients - component 1
parameters(flexmix_dataFPsmall_binomial,component=2) # logistic regression coefficients - component 2
dataFPsmall_flexmix$cluster<-clusters(flexmix_dataFPsmall_binomial) # add cluster identities to your original data.frame 
dataFPsmall_flexmix[,1:2] <- dataFPsmall_flexmix[,1:2]/100 # convert 0-100 back to 0-1

# plot 
dataFPsmall_flexmix_plot <- ggplot(dataFPsmall_flexmix,aes(x=TOTP_avg,y=FPcover_max,shape=factor(cluster))) + geom_point(size=3) 
dataFPsmall_flexmix_plot <- dataFPsmall_flexmix_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(cluster))) 
dataFPsmall_flexmix_plot <- dataFPsmall_flexmix_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataFPsmall_flexmix_plot <- dataFPsmall_flexmix_plot + ggtitle("dataFPsmall")
dataFPsmall_flexmix_plot <- dataFPsmall_flexmix_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataFPsmall_flexmix_plot <- dataFPsmall_flexmix_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataFPsmall_flexmix_plot <- dataFPsmall_flexmix_plot + theme_classic(base_size=18)
dataFPsmall_flexmix_plot

# save the plot 
ggsave(file="dataFPsmall_flexmix_plot.jpg", dataFPsmall_flexmix_plot, height=8,width=11)

# rootogram of posterior class probabilities
# Peak at prob. 1 = mixture component is well seperated from the other components
# No peak at 1 and/or signiﬁcant mass in the middle of the unit interval indicates overlap with other components.
jpeg("flexmix_dataFPsmall_binomial.jpg")
plot(flexmix_dataFPsmall_binomial,sub="flexmix_dataFPsmall_binomial")
dev.off()

####################### 
# Latent Mixture      #
# dataFPoutliers      #
# family: binomial    #
#######################
flexmix_dataFPoutliers_binomial <- flexmix(cbind(FPcover_max,NotFP) ~ TOTP_avg, data=dataFPoutliers_flexmix, k=2, model=FLXMRglm(family="binomial"))
flexmix_dataFPoutliers_binomial
summary(flexmix_dataFPoutliers_binomial)
parameters(flexmix_dataFPoutliers_binomial,component=1) # logistic regression coefficients - component 1
parameters(flexmix_dataFPoutliers_binomial,component=2) # logistic regression coefficients - component 2
dataFPoutliers_flexmix$cluster<-clusters(flexmix_dataFPoutliers_binomial) # add cluster identities to your original data.frame 
dataFPoutliers_flexmix[,1:2] <- dataFPoutliers_flexmix[,1:2]/100 # convert 0-100 back to 0-1

# plot 
dataFPoutliers_flexmix_plot <- ggplot(dataFPoutliers_flexmix,aes(x=TOTP_avg,y=FPcover_max,shape=factor(cluster))) + geom_point(size=3) 
dataFPoutliers_flexmix_plot <- dataFPoutliers_flexmix_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(cluster))) 
dataFPoutliers_flexmix_plot <- dataFPoutliers_flexmix_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataFPoutliers_flexmix_plot <- dataFPoutliers_flexmix_plot + ggtitle("dataFPoutliers")
dataFPoutliers_flexmix_plot <- dataFPoutliers_flexmix_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataFPoutliers_flexmix_plot <- dataFPoutliers_flexmix_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataFPoutliers_flexmix_plot <- dataFPoutliers_flexmix_plot + theme_classic(base_size=18)
dataFPoutliers_flexmix_plot

# save the plot 
ggsave(file="dataFPoutliers_flexmix_plot.jpg", dataFPoutliers_flexmix_plot, height=8,width=11)

# rootogram of posterior class probabilities
# Peak at prob. 1 = mixture component is well seperated from the other components
# No peak at 1 and/or signiﬁcant mass in the middle of the unit interval indicates overlap with other components.
jpeg("flexmix_dataFPoutliers_binomial.jpg")
plot(flexmix_dataFPoutliers_binomial,sub="flexmix_dataFPoutliers_binomial")
dev.off()

####################### 
# Latent Mixture      #
# dataFPoutlierssmall #
# family: binomial    #
#######################
flexmix_dataFPoutlierssmall_binomial <- flexmix(cbind(FPcover_max,NotFP) ~ TOTP_avg, data=dataFPoutlierssmall_flexmix, k=2, model=FLXMRglm(family="binomial"))
flexmix_dataFPoutlierssmall_binomial
summary(flexmix_dataFPoutlierssmall_binomial)
parameters(flexmix_dataFPoutlierssmall_binomial,component=1) # logistic regression coefficients - component 1
parameters(flexmix_dataFPoutlierssmall_binomial,component=2) # logistic regression coefficients - component 2
dataFPoutlierssmall_flexmix$cluster<-clusters(flexmix_dataFPoutlierssmall_binomial) # add cluster identities to your original data.frame 
dataFPoutlierssmall_flexmix[,1:2] <- dataFPoutlierssmall_flexmix[,1:2]/100 # convert 0-100 back to 0-1

# plot 
dataFPoutlierssmall_flexmix_plot <- ggplot(dataFPoutlierssmall_flexmix,aes(x=TOTP_avg,y=FPcover_max,shape=factor(cluster))) + geom_point(size=3) 
dataFPoutlierssmall_flexmix_plot <- dataFPoutlierssmall_flexmix_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(cluster))) 
dataFPoutlierssmall_flexmix_plot <- dataFPoutlierssmall_flexmix_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataFPoutlierssmall_flexmix_plot <- dataFPoutlierssmall_flexmix_plot + ggtitle("dataFPoutlierssmall")
dataFPoutlierssmall_flexmix_plot <- dataFPoutlierssmall_flexmix_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataFPoutlierssmall_flexmix_plot <- dataFPoutlierssmall_flexmix_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataFPoutlierssmall_flexmix_plot <- dataFPoutlierssmall_flexmix_plot + theme_classic(base_size=18)
dataFPoutlierssmall_flexmix_plot

# save the plot 
ggsave(file="dataFPoutlierssmall_flexmix_plot.jpg", dataFPoutlierssmall_flexmix_plot, height=8,width=11)

# rootogram of posterior class probabilities
# Peak at prob. 1 = mixture component is well seperated from the other components
# No peak at 1 and/or signiﬁcant mass in the middle of the unit interval indicates overlap with other components.
jpeg("flexmix_dataFPoutlierssmall_binomial.jpg")
plot(flexmix_dataFPoutlierssmall_binomial,sub="flexmix_dataFPoutlierssmall_binomial")
dev.off()