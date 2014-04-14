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

########################## 
# Beta regression        #
# Mixed model            #
# dataONEperpondoutliers #
# link: logit            #
# Constant dispersion    #
##########################  
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
# w/ fixed components #
# dataONEperpond      #
# link: logit         #
# Constant dispersion #
#######################
# An alternate version where I specify an "extra component" for low FP ponds and have n=3 clusters 

formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataONEperpond_logit_extcomp <- betamix(formula, link="logit", data=dataONEperpond, k = 3, nstart = 100, 
                                                    extra_components = extraComponent(type="uniform",coef=0.01, delta=0.01))

betareg_mix_dataONEperpond_logit_extcomp
summary(betareg_mix_dataONEperpond_logit_extcomp) 
logLik(betareg_mix_dataONEperpond_logit_extcomp) 
AIC(betareg_mix_dataONEperpond_logit_extcomp) 

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataONEperpond$beta_logit_3clusters <- rep(NA, nrow(dataONEperpond))
dataONEperpond_TOTP <- subset(dataONEperpond, dataONEperpond$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataONEperpond_noTOTP <- subset(dataONEperpond, is.na(dataONEperpond$TOTP_avg)) # and waterbodies w/o TOTP
dataONEperpond_TOTP$beta_logit_3clusters<-clusters(betareg_mix_dataONEperpond_logit_extcomp) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataONEperpond <- merge(dataONEperpond_TOTP,dataONEperpond_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataONEperpond_TOTP,dataONEperpond_noTOTP)

# plot 
dataONEperpond_logit_extcomp_cluster_plot <- ggplot(dataONEperpond,aes(x=TOTP_avg,y=FPcover_max,colour=factor(beta_logit_3clusters),shape=factor(beta_logit_3clusters))) + geom_point(size=3) 
dataONEperpond_logit_extcomp_cluster_plot <- dataONEperpond_logit_extcomp_cluster_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(beta_logit_3clusters))) 
dataONEperpond_logit_extcomp_cluster_plot <- dataONEperpond_logit_extcomp_cluster_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataONEperpond_logit_extcomp_cluster_plot <- dataONEperpond_logit_extcomp_cluster_plot + ggtitle("dataONEperpond - logit link")
dataONEperpond_logit_extcomp_cluster_plot <- dataONEperpond_logit_extcomp_cluster_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataONEperpond_logit_extcomp_cluster_plot <- dataONEperpond_logit_extcomp_cluster_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataONEperpond_logit_extcomp_cluster_plot <- dataONEperpond_logit_extcomp_cluster_plot + theme_classic(base_size=18)
dataONEperpond_logit_extcomp_cluster_plot

# save the plot 
ggsave(file="dataONEperpond_logit_extcomp_cluster_plot.jpg", dataONEperpond_logit_extcomp_cluster_plot, height=8,width=11)

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
clusters(betareg_mix_dataFP_logit) 

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataFP$beta_logit_3clusters <- rep(NA, nrow(dataFP))
dataFP_TOTP <- subset(dataFP, dataFP$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataFP_noTOTP <- subset(dataFP, is.na(dataFP$TOTP_avg)) # and waterbodies w/o TOTP
dataFP_TOTP$beta_logit_3clusters<-clusters(betareg_mix_dataFP_logit) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataFP <- merge(dataFP_TOTP,dataFP_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataFP_TOTP,dataFP_noTOTP)

# Problem with my orginal method for adding cluster assignments 
clusters <- clusters(betareg_mix_dataFP_logit)
which(is.na(dataFP$TOTP_avg)) # retuns the indices of that are NA for total P 
# insert NAs into this in the correct spots
clusters <- c(clusters[1:5],NA,clusters[6:65],NA,clusters[66:74],NA,clusters[75:length(clusters)])
# check that it put them in the right spots 
which(is.na(c(clusters[1:5],NA,clusters[6:65],NA,clusters[66:74],NA,clusters[75:length(clusters)])))
which(is.na(clusters))
clusters

# plot 
dataFP_beta_logit_cluster_plot <- ggplot(dataFP,aes(x=TOTP_avg,y=FPcover_max,colour=factor(clusters),shape=factor(clusters))) + geom_point(size=3) 
dataFP_beta_logit_cluster_plot <- dataFP_beta_logit_cluster_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(clusters))) 
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
 
######################## 
# Beta regression      #
# Mixed model          #
# dataFP               #
# initial cluster prob #
# link: logit          #
# Constant dispersion  #
########################
# create the vector for initial cluster assingments 
# if FP cover max >= 50, assign to cluster 1
# if FP cover max < 50, assign to cluster 2 
dataFP$prior_cluster[dataFP$FPcover_max>=0.50]<-1
dataFP$prior_cluster[dataFP$FPcover_max<0.50]<-2
dataFP$prior_cluster <- as.integer(dataFP$prior_cluster)
class(dataFP$prior_cluster)

formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataFP_logit_priorclust <- betamix(formula, link="logit", data=dataFP, k = 2, nstart = 100, cluster=dataFP$prior_cluster)
betareg_mix_dataFP_logit_priorclust
summary(betareg_mix_dataFP_logit_priorclust) 
logLik(betareg_mix_dataFP_logit_priorclust) 
AIC(betareg_mix_dataFP_logit_priorclust) 

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataFP$beta_logit_cluster_prior_cluster <- rep(NA, nrow(dataFP))
dataFP_TOTP <- subset(dataFP, dataFP$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataFP_noTOTP <- subset(dataFP, is.na(dataFP$TOTP_avg)) # and waterbodies w/o TOTP
dataFP_TOTP$beta_logit_cluster_prior_cluster<-clusters(betareg_mix_dataFP_logit_priorclust) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataFP <- merge(dataFP_TOTP,dataFP_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataFP_TOTP,dataFP_noTOTP)

# plot 
dataFP_beta_logit_cluster_prior_cluster_plot <- ggplot(dataFP,aes(x=TOTP_avg,y=FPcover_max,colour=factor(beta_logit_cluster_prior_cluster),shape=factor(beta_logit_cluster_prior_cluster))) + geom_point(size=3) 
dataFP_beta_logit_cluster_prior_cluster_plot <- dataFP_beta_logit_cluster_prior_cluster_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(beta_logit_cluster_prior_cluster))) 
dataFP_beta_logit_cluster_prior_cluster_plot <- dataFP_beta_logit_cluster_prior_cluster_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataFP_beta_logit_cluster_prior_cluster_plot <- dataFP_beta_logit_cluster_prior_cluster_plot + ggtitle("dataFP - logit link")
dataFP_beta_logit_cluster_prior_cluster_plot <- dataFP_beta_logit_cluster_prior_cluster_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataFP_beta_logit_cluster_prior_cluster_plot <- dataFP_beta_logit_cluster_prior_cluster_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataFP_beta_logit_cluster_prior_cluster_plot <- dataFP_beta_logit_cluster_prior_cluster_plot + theme_classic(base_size=18)
dataFP_beta_logit_cluster_prior_cluster_plot

# save the plot 
ggsave(file="dataFP_beta_logit_cluster_prior_cluster_plot.jpg", dataFP_beta_logit_cluster_prior_cluster_plot, height=8,width=11)

######################## 
# Beta regression      #
# Mixed model          #
# dataFP               #
# initial cluster prob #
# link: logit          #
# Constant dispersion  #
# version 2            #
########################
# create the matrix for initial cluster probabilities 
dataFP$prior_cluster1_prob <- dataFP$FPcover_max
dataFP$prior_cluster2_prob <- 1-dataFP$FPcover_max

# This is the one problematic outlier that keeps getting assigned to the not-FP-regime cluster
dataFP$FPcover_max[64]
dataFP$TOTP_avg[64]
# Give that point a 0 % prob. of being in not-FP-regime cluster 
dataFP$prior_cluster1_prob[64] <- 1
dataFP$prior_cluster2_prob[64] <- 0

formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataFP_logit_priorclust2 <- betamix(formula, link="logit", data=dataFP, k = 2, nstart = 100, cluster=cbind(dataFP$prior_cluster1_prob,dataFP$prior_cluster2_prob))
betareg_mix_dataFP_logit_priorclust2
summary(betareg_mix_dataFP_logit_priorclust2) 
logLik(betareg_mix_dataFP_logit_priorclust2) 
AIC(betareg_mix_dataFP_logit_priorclust2) 

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataFP$beta_logit_cluster_prior_clusterv2 <- rep(NA, nrow(dataFP))
dataFP_TOTP <- subset(dataFP, dataFP$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataFP_noTOTP <- subset(dataFP, is.na(dataFP$TOTP_avg)) # and waterbodies w/o TOTP
dataFP_TOTP$beta_logit_cluster_prior_clusterv2<-clusters(betareg_mix_dataFP_logit_priorclust2) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataFP <- merge(dataFP_TOTP,dataFP_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataFP_TOTP,dataFP_noTOTP)

# plot 
dataFP_beta_logit_cluster_prior_clusterv2_plot <- ggplot(dataFP,aes(x=TOTP_avg,y=FPcover_max,colour=factor(beta_logit_cluster_prior_clusterv2),shape=factor(beta_logit_cluster_prior_clusterv2))) + geom_point(size=3) 
dataFP_beta_logit_cluster_prior_clusterv2_plot <- dataFP_beta_logit_cluster_prior_clusterv2_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(beta_logit_cluster_prior_clusterv2))) 
dataFP_beta_logit_cluster_prior_clusterv2_plot <- dataFP_beta_logit_cluster_prior_clusterv2_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataFP_beta_logit_cluster_prior_clusterv2_plot <- dataFP_beta_logit_cluster_prior_clusterv2_plot + ggtitle("dataFP - logit link")
dataFP_beta_logit_cluster_prior_clusterv2_plot <- dataFP_beta_logit_cluster_prior_clusterv2_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataFP_beta_logit_cluster_prior_clusterv2_plot <- dataFP_beta_logit_cluster_prior_clusterv2_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataFP_beta_logit_cluster_prior_clusterv2_plot <- dataFP_beta_logit_cluster_prior_clusterv2_plot + theme_classic(base_size=18)
dataFP_beta_logit_cluster_prior_clusterv2_plot

# save the plot 
ggsave(file="dataFP_beta_logit_cluster_plot.jpg", dataFP_beta_logit_cluster_plot, height=8,width=11)

####################### 
# Beta regression     #
# Mixed model         #
# w/ fixed components #
# dataFP              #
# link: logit         #
# Constant dispersion #
#######################
# An alternate version where I specify an "extra component" for low FP ponds and have n=3 clusters 

formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataFP_logit_extcomp <- betamix(formula, link="logit", data=dataFP, k = 3, nstart = 100, 
                                                    extra_components = extraComponent(type="uniform",coef=0.01, delta=0.01))

betareg_mix_dataFP_logit_extcomp
summary(betareg_mix_dataFP_logit_extcomp) 
clusters(betareg_mix_dataFP_logit_extcomp) 
logLik(betareg_mix_dataFP_logit_extcomp) 
AIC(betareg_mix_dataFP_logit_extcomp) 

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataFP$beta_logit_3clusters <- rep(NA, nrow(dataFP))
dataFP_TOTP <- subset(dataFP, dataFP$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataFP_noTOTP <- subset(dataFP, is.na(dataFP$TOTP_avg)) # and waterbodies w/o TOTP
dataFP_TOTP$beta_logit_3clusters<-clusters(betareg_mix_dataFP_logit_extcomp) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataFP <- merge(dataFP_TOTP,dataFP_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataFP_TOTP,dataFP_noTOTP)

# plot 
dataFP_logit_extcomp_cluster_plot <- ggplot(dataFP,aes(x=TOTP_avg,y=FPcover_max,colour=factor(beta_logit_3clusters),shape=factor(beta_logit_3clusters))) + geom_point(size=3) 
dataFP_logit_extcomp_cluster_plot <- dataFP_logit_extcomp_cluster_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(beta_logit_3clusters))) 
dataFP_logit_extcomp_cluster_plot <- dataFP_logit_extcomp_cluster_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataFP_logit_extcomp_cluster_plot <- dataFP_logit_extcomp_cluster_plot + ggtitle("dataFP - logit link")
dataFP_logit_extcomp_cluster_plot <- dataFP_logit_extcomp_cluster_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataFP_logit_extcomp_cluster_plot <- dataFP_logit_extcomp_cluster_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataFP_logit_extcomp_cluster_plot <- dataFP_logit_extcomp_cluster_plot + theme_classic(base_size=18)
dataFP_logit_extcomp_cluster_plot

# save the plot 
ggsave(file="dataFP_logit_extcomp_cluster_plot.jpg", dataFP_logit_extcomp_cluster_plot, height=8,width=11)

####################### 
# Beta regression     #
# Mixed model         #
# w/ fixed components #
# dataFP              #
# link: logit         #
# Constant dispersion #
#######################
# An alternate version where I specify an "extra component" for low FP ponds and have n=3 clusters 

formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataFP_logit_extcomp2 <- betamix(formula, link="logit", data=dataFP, k = 3, nstart = 100, 
                                            extra_components = extraComponent(type="uniform",coef=0.02, delta=0.01))

betareg_mix_dataFP_logit_extcomp2
summary(betareg_mix_dataFP_logit_extcomp2) 
clusters(betareg_mix_dataFP_logit_extcomp2) 
logLik(betareg_mix_dataFP_logit_extcomp2) 
AIC(betareg_mix_dataFP_logit_extcomp2) 

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataFP$beta_logit_3clusters <- rep(NA, nrow(dataFP))
dataFP_TOTP <- subset(dataFP, dataFP$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataFP_noTOTP <- subset(dataFP, is.na(dataFP$TOTP_avg)) # and waterbodies w/o TOTP
dataFP_TOTP$beta_logit_3clusters<-clusters(betareg_mix_dataFP_logit_extcomp2) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataFP <- merge(dataFP_TOTP,dataFP_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataFP_TOTP,dataFP_noTOTP)

# plot 
dataFP_logit_extcomp2_cluster_plot <- ggplot(dataFP,aes(x=TOTP_avg,y=FPcover_max,colour=factor(beta_logit_3clusters),shape=factor(beta_logit_3clusters))) + geom_point(size=3) 
dataFP_logit_extcomp2_cluster_plot <- dataFP_logit_extcomp2_cluster_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(beta_logit_3clusters))) 
dataFP_logit_extcomp2_cluster_plot <- dataFP_logit_extcomp2_cluster_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataFP_logit_extcomp2_cluster_plot <- dataFP_logit_extcomp2_cluster_plot + ggtitle("dataFP - logit link")
dataFP_logit_extcomp2_cluster_plot <- dataFP_logit_extcomp2_cluster_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataFP_logit_extcomp2_cluster_plot <- dataFP_logit_extcomp2_cluster_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataFP_logit_extcomp2_cluster_plot <- dataFP_logit_extcomp2_cluster_plot + theme_classic(base_size=18)
dataFP_logit_extcomp2_cluster_plot

# save the plot 
ggsave(file="dataFP_logit_extcomp2_cluster_plot.jpg", dataFP_logit_extcomp2_cluster_plot, height=8,width=11)

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
# w/ fixed components #
# dataFPsmall         #
# link: logit         #
# Constant dispersion #
#######################
# An alternate version where I specify an "extra component" for low FP ponds and have n=3 clusters 

formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataFPsmall_logit_extcomp <- betamix(formula, link="logit", data=dataFPsmall, k = 3, nstart = 100, 
                                            extra_components = extraComponent(type="uniform",coef=0.01, delta=0.01))

betareg_mix_dataFPsmall_logit_extcomp
summary(betareg_mix_dataFPsmall_logit_extcomp) 
clusters(betareg_mix_dataFPsmall_logit_extcomp) 
logLik(betareg_mix_dataFPsmall_logit_extcomp) 
AIC(betareg_mix_dataFPsmall_logit_extcomp) 

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataFPsmall$beta_logit_3clusters <- rep(NA, nrow(dataFPsmall))
dataFPsmall_TOTP <- subset(dataFPsmall, dataFPsmall$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataFPsmall_noTOTP <- subset(dataFPsmall, is.na(dataFPsmall$TOTP_avg)) # and waterbodies w/o TOTP
dataFPsmall_TOTP$beta_logit_3clusters<-clusters(betareg_mix_dataFPsmall_logit_extcomp) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataFPsmall <- merge(dataFPsmall_TOTP,dataFPsmall_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataFPsmall_TOTP,dataFPsmall_noTOTP)

# plot 
dataFPsmall_logit_extcomp_cluster_plot <- ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max,colour=factor(beta_logit_3clusters),shape=factor(beta_logit_3clusters))) + geom_point(size=3) 
dataFPsmall_logit_extcomp_cluster_plot <- dataFPsmall_logit_extcomp_cluster_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(beta_logit_3clusters))) 
dataFPsmall_logit_extcomp_cluster_plot <- dataFPsmall_logit_extcomp_cluster_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataFPsmall_logit_extcomp_cluster_plot <- dataFPsmall_logit_extcomp_cluster_plot + ggtitle("dataFPsmall - logit link")
dataFPsmall_logit_extcomp_cluster_plot <- dataFPsmall_logit_extcomp_cluster_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataFPsmall_logit_extcomp_cluster_plot <- dataFPsmall_logit_extcomp_cluster_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataFPsmall_logit_extcomp_cluster_plot <- dataFPsmall_logit_extcomp_cluster_plot + theme_classic(base_size=18)
dataFPsmall_logit_extcomp_cluster_plot

# save the plot 
ggsave(file="dataFPsmall_logit_extcomp_cluster_plot.jpg", dataFPsmall_logit_extcomp_cluster_plot, height=8,width=11)




####################### 
# Beta regression     #
# Mixed model         #
# w/ fixed components #
# dataFPsmall         #
# link: logit         #
# Constant dispersion #
# version 2           # 
#######################
# An alternate version where I specify an "extra component" for low FP ponds and have n=3 clusters 

formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataFPsmall_logit_extcomp2 <- betamix(formula, link="logit", data=dataFPsmall, k = 3, nstart = 100, 
                                                 extra_components = extraComponent(type="uniform",coef=0.02, delta=0.01))

betareg_mix_dataFPsmall_logit_extcomp2
summary(betareg_mix_dataFPsmall_logit_extcomp2) 
clusters(betareg_mix_dataFPsmall_logit_extcomp2) 
logLik(betareg_mix_dataFPsmall_logit_extcomp2) 
AIC(betareg_mix_dataFPsmall_logit_extcomp2) 

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataFPsmall$beta_logit_3clustersv2 <- rep(NA, nrow(dataFPsmall))
dataFPsmall_TOTP <- subset(dataFPsmall, dataFPsmall$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataFPsmall_noTOTP <- subset(dataFPsmall, is.na(dataFPsmall$TOTP_avg)) # and waterbodies w/o TOTP
dataFPsmall_TOTP$beta_logit_3clustersv2<-clusters(betareg_mix_dataFPsmall_logit_extcomp2) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataFPsmall <- merge(dataFPsmall_TOTP,dataFPsmall_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataFPsmall_TOTP,dataFPsmall_noTOTP)

# plot 
dataFPsmall_logit_extcompv2_cluster_plot <- ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max,colour=factor(beta_logit_3clustersv2),shape=factor(beta_logit_3clustersv2))) + geom_point(size=3) 
dataFPsmall_logit_extcompv2_cluster_plot <- dataFPsmall_logit_extcompv2_cluster_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(beta_logit_3clustersv2))) 
dataFPsmall_logit_extcompv2_cluster_plot <- dataFPsmall_logit_extcompv2_cluster_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataFPsmall_logit_extcompv2_cluster_plot <- dataFPsmall_logit_extcompv2_cluster_plot + ggtitle("dataFPsmall - logit link")
dataFPsmall_logit_extcompv2_cluster_plot <- dataFPsmall_logit_extcompv2_cluster_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataFPsmall_logit_extcompv2_cluster_plot <- dataFPsmall_logit_extcompv2_cluster_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataFPsmall_logit_extcompv2_cluster_plot <- dataFPsmall_logit_extcompv2_cluster_plot + theme_classic(base_size=18)
dataFPsmall_logit_extcompv2_cluster_plot

# save the plot 
ggsave(file="dataFPsmall_logit_extcompv2_cluster_plot.jpg", dataFPsmall_logit_extcompv2_cluster_plot, height=8,width=11)


####################### 
# Beta regression     #
# Mixed model         #
# w/ fixed components #
# dataFPsmall         #
# link: logit         #
# Constant dispersion #
# version 3           # 
#######################
# An alternate version where I specify an "extra component" for low FP ponds and have n=3 clusters 

formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataFPsmall_logit_extcomp3 <- betamix(formula, link="logit", data=dataFPsmall, k = 3, nstart = 100, 
                                                  extra_components = extraComponent(type="betareg",coef=list(0.02,10)))

betareg_mix_dataFPsmall_logit_extcomp3
summary(betareg_mix_dataFPsmall_logit_extcomp3) 
clusters(betareg_mix_dataFPsmall_logit_extcomp3) 
logLik(betareg_mix_dataFPsmall_logit_extcomp3) 
AIC(betareg_mix_dataFPsmall_logit_extcomp3) 

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataFPsmall$beta_logit_3clustersv3 <- rep(NA, nrow(dataFPsmall))
dataFPsmall_TOTP <- subset(dataFPsmall, dataFPsmall$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataFPsmall_noTOTP <- subset(dataFPsmall, is.na(dataFPsmall$TOTP_avg)) # and waterbodies w/o TOTP
dataFPsmall_TOTP$beta_logit_3clustersv3<-clusters(betareg_mix_dataFPsmall_logit_extcomp3) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataFPsmall <- merge(dataFPsmall_TOTP,dataFPsmall_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataFPsmall_TOTP,dataFPsmall_noTOTP)

# plot 
dataFPsmall_logit_extcompv3_cluster_plot <- ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max,colour=factor(beta_logit_3clustersv3),shape=factor(beta_logit_3clustersv3))) + geom_point(size=3) 
dataFPsmall_logit_extcompv3_cluster_plot <- dataFPsmall_logit_extcompv3_cluster_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(beta_logit_3clustersv3))) 
dataFPsmall_logit_extcompv3_cluster_plot <- dataFPsmall_logit_extcompv3_cluster_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataFPsmall_logit_extcompv3_cluster_plot <- dataFPsmall_logit_extcompv3_cluster_plot + ggtitle("dataFPsmall - logit link")
dataFPsmall_logit_extcompv3_cluster_plot <- dataFPsmall_logit_extcompv3_cluster_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataFPsmall_logit_extcompv3_cluster_plot <- dataFPsmall_logit_extcompv3_cluster_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataFPsmall_logit_extcompv3_cluster_plot <- dataFPsmall_logit_extcompv3_cluster_plot + theme_classic(base_size=18)
dataFPsmall_logit_extcompv3_cluster_plot

# save the plot 
ggsave(file="dataFPsmall_logit_extcompv3_cluster_plot.jpg", dataFPsmall_logit_extcompv3_cluster_plot, height=8,width=11)


####################### 
# Beta regression     #
# Mixed model         #
# w/ fixed components #
# dataFPsmall         #
# link: logit         #
# Constant dispersion #
# version 4           # 
#######################
# An alternate version where I specify an "extra component" for low FP ponds and have n=3 clusters 

formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataFPsmall_logit_extcomp4 <- betamix(formula, link="logit", data=dataFPsmall, k = 3, nstart = 100, 
                                                  extra_components = extraComponent(type="betareg",coef=list(0.02,100)))

betareg_mix_dataFPsmall_logit_extcomp4
summary(betareg_mix_dataFPsmall_logit_extcomp4) 
clusters(betareg_mix_dataFPsmall_logit_extcomp4) 
logLik(betareg_mix_dataFPsmall_logit_extcomp4) 
AIC(betareg_mix_dataFPsmall_logit_extcomp4) 

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataFPsmall$beta_logit_3clustersv4 <- rep(NA, nrow(dataFPsmall))
dataFPsmall_TOTP <- subset(dataFPsmall, dataFPsmall$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataFPsmall_noTOTP <- subset(dataFPsmall, is.na(dataFPsmall$TOTP_avg)) # and waterbodies w/o TOTP
dataFPsmall_TOTP$beta_logit_3clustersv4<-clusters(betareg_mix_dataFPsmall_logit_extcomp4) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataFPsmall <- merge(dataFPsmall_TOTP,dataFPsmall_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataFPsmall_TOTP,dataFPsmall_noTOTP)

# plot 
dataFPsmall_logit_extcompv4_cluster_plot <- ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max,colour=factor(beta_logit_3clustersv4),shape=factor(beta_logit_3clustersv4))) + geom_point(size=3) 
dataFPsmall_logit_extcompv4_cluster_plot <- dataFPsmall_logit_extcompv4_cluster_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(beta_logit_3clustersv4))) 
dataFPsmall_logit_extcompv4_cluster_plot <- dataFPsmall_logit_extcompv4_cluster_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataFPsmall_logit_extcompv4_cluster_plot <- dataFPsmall_logit_extcompv4_cluster_plot + ggtitle("dataFPsmall - logit link")
dataFPsmall_logit_extcompv4_cluster_plot <- dataFPsmall_logit_extcompv4_cluster_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataFPsmall_logit_extcompv4_cluster_plot <- dataFPsmall_logit_extcompv4_cluster_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataFPsmall_logit_extcompv4_cluster_plot <- dataFPsmall_logit_extcompv4_cluster_plot + theme_classic(base_size=18)
dataFPsmall_logit_extcompv4_cluster_plot

# save the plot 
ggsave(file="dataFPsmall_logit_extcompv4_cluster_plot.jpg", dataFPsmall_logit_extcompv4_cluster_plot, height=8,width=11)

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
# w/ fixed components #
# dataFPoutliers      #
# link: logit         #
# Constant dispersion #
#######################
# An alternate version where I specify an "extra component" for low FP ponds and have n=3 clusters 

formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataFPoutliers_logit_extcomp <- betamix(formula, link="logit", data=dataFPoutliers, k = 3, nstart = 100, 
                                            extra_components = extraComponent(type="uniform",coef=0.01, delta=0.01))

betareg_mix_dataFPoutliers_logit_extcomp
summary(betareg_mix_dataFPoutliers_logit_extcomp) 
logLik(betareg_mix_dataFPoutliers_logit_extcomp) 
AIC(betareg_mix_dataFPoutliers_logit_extcomp) 

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataFPoutliers$beta_logit_3clusters <- rep(NA, nrow(dataFPoutliers))
dataFPoutliers_TOTP <- subset(dataFPoutliers, dataFPoutliers$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataFPoutliers_noTOTP <- subset(dataFPoutliers, is.na(dataFPoutliers$TOTP_avg)) # and waterbodies w/o TOTP
dataFPoutliers_TOTP$beta_logit_3clusters<-clusters(betareg_mix_dataFPoutliers_logit_extcomp) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataFPoutliers <- merge(dataFPoutliers_TOTP,dataFPoutliers_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataFPoutliers_TOTP,dataFPoutliers_noTOTP)

# plot 
dataFPoutliers_logit_extcomp_cluster_plot <- ggplot(dataFPoutliers,aes(x=TOTP_avg,y=FPcover_max,colour=factor(beta_logit_3clusters),shape=factor(beta_logit_3clusters))) + geom_point(size=3) 
dataFPoutliers_logit_extcomp_cluster_plot <- dataFPoutliers_logit_extcomp_cluster_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(beta_logit_3clusters))) 
dataFPoutliers_logit_extcomp_cluster_plot <- dataFPoutliers_logit_extcomp_cluster_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataFPoutliers_logit_extcomp_cluster_plot <- dataFPoutliers_logit_extcomp_cluster_plot + ggtitle("dataFPoutliers - logit link")
dataFPoutliers_logit_extcomp_cluster_plot <- dataFPoutliers_logit_extcomp_cluster_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataFPoutliers_logit_extcomp_cluster_plot <- dataFPoutliers_logit_extcomp_cluster_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataFPoutliers_logit_extcomp_cluster_plot <- dataFPoutliers_logit_extcomp_cluster_plot + theme_classic(base_size=18)
dataFPoutliers_logit_extcomp_cluster_plot

# save the plot 
ggsave(file="dataFPoutliers_logit_extcomp_cluster_plot.jpg", dataFPoutliers_logit_extcomp_cluster_plot, height=8,width=11)


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

########################## 
# Beta regression        #
# Mixed model            #
# dataONEperpondoutliers #
# link: loglog           #
# Constant dispersion    #
##########################
formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataONEperpondoutliers_loglog <- betamix(formula, link="loglog", data=dataONEperpondoutliers, k = 2, nstart = 100)
summary(betareg_mix_dataONEperpondoutliers_loglog) 
logLik(betareg_mix_dataONEperpondoutliers_loglog) 
AIC(betareg_mix_dataONEperpondoutliers_loglog) 

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataONEperpondoutliers$beta_loglog_cluster <- rep(NA, nrow(dataONEperpondoutliers))
dataONEperpondoutliers_TOTP <- subset(dataONEperpondoutliers, dataONEperpondoutliers$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataONEperpondoutliers_noTOTP <- subset(dataONEperpondoutliers, is.na(dataONEperpondoutliers$TOTP_avg)) # and waterbodies w/o TOTP
dataONEperpondoutliers_TOTP$beta_loglog_cluster<-clusters(betareg_mix_dataONEperpondoutliers_loglog) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataONEperpondoutliers <- merge(dataONEperpondoutliers_TOTP,dataONEperpondoutliers_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataONEperpondoutliers_TOTP,dataONEperpondoutliers_noTOTP)

# plot 
dataONEperpondoutliers_beta_loglog_cluster_plot <- ggplot(dataONEperpondoutliers,aes(x=TOTP_avg,y=FPcover_max,colour=factor(beta_loglog_cluster),shape=factor(beta_loglog_cluster))) + geom_point(size=3) 
dataONEperpondoutliers_beta_loglog_cluster_plot <- dataONEperpondoutliers_beta_loglog_cluster_plot + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(beta_loglog_cluster))) 
dataONEperpondoutliers_beta_loglog_cluster_plot <- dataONEperpondoutliers_beta_loglog_cluster_plot + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
dataONEperpondoutliers_beta_loglog_cluster_plot <- dataONEperpondoutliers_beta_loglog_cluster_plot + ggtitle("dataONEperpondoutliers - loglog link")
dataONEperpondoutliers_beta_loglog_cluster_plot <- dataONEperpondoutliers_beta_loglog_cluster_plot + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
dataONEperpondoutliers_beta_loglog_cluster_plot <- dataONEperpondoutliers_beta_loglog_cluster_plot + scale_y_continuous(breaks=y_breaks,labels=y_labels)
dataONEperpondoutliers_beta_loglog_cluster_plot <- dataONEperpondoutliers_beta_loglog_cluster_plot + theme_classic(base_size=18)
dataONEperpondoutliers_beta_loglog_cluster_plot

# save the plot 
ggsave(file="dataONEperpondoutliers_beta_loglog_cluster_plot.jpg", dataONEperpondoutliers_beta_loglog_cluster_plot, height=8,width=11)

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








