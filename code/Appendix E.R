######################################################################
# Manuscript I: Appendix E                                           #
#                                                                    #
# Eight waterbodies had shifts between FP states                     #
# Sensitivity of results to different estimates of FP cover          # 
#                                                                    #
# SCENARIOS:                                                         #
# a) max FP estimate (same as main text)                             #
# b) min FP estimate                                                 #
# c) FP shifts removed                                               #
######################################################################
library(ggplot2)
library(gridExtra)
library(diptest) # Hartigan's dip test for bimodality
library(betareg)

#############################################################################################################
# REMOVING "SHIFTS"
#############################################################################################################
dataFP_noshifts <- subset(dataFP, waterbody != "Betty Allen Twin Pond")
dataFP_noshifts <- subset(dataFP_noshifts, waterbody != "Birch Pond")
dataFP_noshifts <- subset(dataFP_noshifts, waterbody != "Mills Pond, Lower")
dataFP_noshifts <- subset(dataFP_noshifts, waterbody != "Mill Pond  South Basin")
dataFP_noshifts <- subset(dataFP_noshifts, waterbody != "Robinson Pond ")
dataFP_noshifts <- subset(dataFP_noshifts, waterbody != "Sound Avenue Pond")
dataFP_noshifts <- subset(dataFP_noshifts, waterbody != "Tilleys Pond")
dataFP_noshifts <- subset(dataFP_noshifts, waterbody != "Young's Pond")
nrow(dataFP_noshifts)

write.csv(dataFP_noshifts, file = "FP ponds less than 5ha no shifts.csv", row.names = FALSE )

###################
# Fig E1a         #
# Appendix E      #
# re-do Fig.2     #
# FPcover_max     #
################### 
AppE1_a <- Fig02
AppE1_a <- AppE1_a + scale_y_continuous(limits=c(0,80),expand=c(0,0))
AppE1_a <- AppE1_a + geom_text(aes(x=5,y=78,label="a)"),size=7) # add pane label
AppE1_a

dip.test(dataFP$FPcover_max)

# dataFP - waterbodies with floating plants 
cl.res03 <- kmeans(dataFP$FPcover_max, 2, centers=c(min(dataFP$FPcover_max), max(dataFP$FPcover_max)))
n03 <- length(dataFP$FPcover_max)
# variance reduction score = within cluster SS / total sum of squares
# value ranges 0 to 1; low score "indicates an informative split"
vrs03 <- (sum(cl.res03$withinss) / (n03-1)) / var(dataFP$FPcover_max) 
# weighted variance reduction score
# variance reduction independent of cluster sizes
# vale ranges 0 or greater; "low score reflects bimodality"
wvrs03 <- (sum(cl.res03$withinss / cl.res03$size) / 2) / ( ((n03-1)/n03) * var(dataFP$FPcover_max)) 
groups03 <- cl.res03$cluster

vrs03
wvrs03

###################
# Fig E1b         #
# Appendix E      #
# re-do Fig.2     #
# min. FP value   #
# low_est         #
################### 
dataFP$FPcover_low_est_percent <- dataFP$FPcover_low_est*100 

AppE1_b <- ggplot(dataFP,aes(FPcover_low_est_percent)) + stat_bin(binwidth=20,right=TRUE,col="black")
AppE1_b <- AppE1_b + ylab("Frequency") + xlab("Floating plant cover (%)")
AppE1_b <- AppE1_b + scale_x_continuous(limits=c(0,101),breaks=c(0,20,40,60,80,100),expand=c(0,0)) + scale_y_continuous(limits=c(0,80),expand=c(0,0))
AppE1_b <- AppE1_b + theme_classic(base_size=18)
AppE1_b <- AppE1_b + theme(axis.title.y=element_blank())
AppE1_b <- AppE1_b + geom_text(aes(x=5,y=78,label="b)"),size=7) # add pane label
AppE1_b

dip.test(dataFP$FPcover_low_est)

# dataFP - waterbodies with floating plants 
cl.res04 <- kmeans(dataFP$FPcover_low_est, 2, centers=c(min(dataFP$FPcover_low_est), max(dataFP$FPcover_low_est)))
n04 <- length(dataFP$FPcover_low_est)
# variance reduction score = within cluster SS / total sum of squares
# value ranges 0 to 1; low score "indicates an informative split"
vrs04 <- (sum(cl.res04$withinss) / (n04-1)) / var(dataFP$FPcover_low_est) 
# weighted variance reduction score
# variance reduction independent of cluster sizes
# vale ranges 0 or greater; "low score reflects bimodality"
wvrs04 <- (sum(cl.res04$withinss / cl.res04$size) / 2) / ( ((n04-1)/n04) * var(dataFP$FPcover_low_est)) # 
groups04 <- cl.res04$cluster

vrs04
wvrs04

###################
# Fig E1c         #
# Appendix E      #
# re-do Fig.2     #
# 8 removed       #
################### 
dataFP_noshifts$FPcover_low_est_percent <- dataFP_noshifts$FPcover_max*100 

AppE1_c <- ggplot(dataFP_noshifts,aes(FPcover_low_est_percent)) + stat_bin(binwidth=20,right=TRUE,col="black")
AppE1_c <- AppE1_c + ylab("Frequency") + xlab("Floating plant cover (%)")
AppE1_c <- AppE1_c + scale_x_continuous(limits=c(0,101),breaks=c(0,20,40,60,80,100),expand=c(0,0)) + scale_y_continuous(limits=c(0,80),expand=c(0,0))
AppE1_c <- AppE1_c + theme_classic(base_size=18)
AppE1_c <- AppE1_c + theme(axis.title.y=element_blank())
AppE1_c <- AppE1_c + geom_text(aes(x=5,y=78,label="c)"),size=7) # add pane label
AppE1_c

dip.test(dataFP_noshifts$FPcover_max)

# dataFP_noshifts - waterbodies with floating plants 
cl.res05 <- kmeans(dataFP_noshifts$FPcover_max, 2, centers=c(min(dataFP_noshifts$FPcover_max), max(dataFP_noshifts$FPcover_max)))
n05 <- length(dataFP_noshifts$FPcover_max)
# variance reduction score = within cluster SS / total sum of squares
# value ranges 0 to 1; low score "indicates an informative split"
vrs05 <- (sum(cl.res05$withinss) / (n05-1)) / var(dataFP_noshifts$FPcover_max) #
# weighted variance reduction score
# variance reduction independent of cluster sizes
# vale ranges 0 or greater; "low score reflects bimodality"
wvrs05 <- (sum(cl.res05$withinss / cl.res05$size) / 2) / ( ((n05-1)/n05) * var(dataFP_noshifts$FPcover_max)) # 
groups05 <- cl.res05$cluster

vrs05
wvrs05

#################
# ARRANGE 
#################
appendixE_freq <- arrangeGrob(AppE1_a,AppE1_b,AppE1_c,ncol=3,nrow=1) #grid.arrange does not work with ggsave()
appendixE_freq

ggsave(file="appendixE_freq.jpg", appendixE_freq, height=5,width=15)


###################################################
# number of waterbodies (dataONEperpond) with FP cover> 66.666%
nrow(subset(dataONEperpond, dataONEperpond$FPcover_low_est >= 0.66666)) # 12 






##########################
# dataFP max FP estimate # 
##########################
##########
# FigE2a #
##########
# Beta reg.    
AppE2_a <- Fig3a
AppE2_a
##########
# FigE2b #
##########
# Laten class Beta reg.    
AppE2_b <- ggplot(dataFP,aes(x=TOTP_avg,y=FPcover_max,shape=factor(beta_logit_cluster_prior_clusterv3))) + stat_smooth(method=glm, family=binomial, se=F,colour="blue",size=1) + geom_point(size=2) 
AppE2_b <- AppE2_b + scale_shape_manual(values=c(1,19),name="Cluster")
AppE2_b <- AppE2_b + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
AppE2_b <- AppE2_b + geom_text(aes(x=0.0075,y=1,label="b)"),size=7)
AppE2_b <- AppE2_b + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
AppE2_b <- AppE2_b + scale_y_continuous(breaks=y_breaks,labels=y_labels)
AppE2_b <- AppE2_b + theme_classic(base_size=18)
AppE2_b <- AppE2_b + theme(legend.position=c(0.85,0.3))
AppE2_b 

###################
# beta regress    #
# FPcover_low_est #
################### 
##########
# FigE2c #
##########
dataFP$FPcover_low_est

# get rid of your 0s and 1s
dataFP$FPcover_low_est[dataFP$FPcover_low_est == 1] <- (1*(length(dataFP$FPcover_low_est)-1)+0.5)/(length(dataFP$FPcover_low_est))
dataFP$FPcover_low_est[dataFP$FPcover_low_est == 0] <- (0*(length(dataFP$FPcover_low_est)-1)+0.5)/(length(dataFP$FPcover_low_est))

# fit the model 
betareg_dataFP_low_est_logit <- betareg(FPcover_low_est ~ TOTP_avg, data=dataFP, link="logit")
summary(betareg_dataFP_low_est_logit)
betareg_dataFP_low_est_logit$fitted
logLik(betareg_dataFP_low_est_logit)
AIC(betareg_dataFP_low_est_logit)

# add the fitted values to my original data set for plotting 
# will need to work around NAs 
AppE2_c_data <- cbind(dataFP$TOTP_avg,dataFP$FPcover_max)
AppE2_c_data <- AppE2_c_data[complete.cases(AppE2_c_data),]
AppE2_c_data <- cbind(AppE2_c_data,betareg_dataFP_low_est_logit$fitted)
AppE2_c_data <- as.data.frame(AppE2_c_data)
names(AppE2_c_data) <- c("TOTP_avg","FPcover_max","fitted")
AppE2_c_data

# with color 
library(ggplot2)
AppE2_c <- ggplot(data=AppE2_c_data, aes(x=TOTP_avg,y=FPcover_max)) + geom_point(size=2) 
AppE2_c <- AppE2_c + geom_line(aes(x=TOTP_avg,y=fitted),colour="blue",size=1)
AppE2_c <- AppE2_c + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
AppE2_c <- AppE2_c + geom_text(aes(x=0.0075,y=1,label="c)"),size=7)
AppE2_c <- AppE2_c + scale_x_log10() 
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
AppE2_c <- AppE2_c + scale_y_continuous(breaks=y_breaks,labels=y_labels)
AppE2_c <- AppE2_c + theme_classic(base_size=18)
AppE2_c <- AppE2_c + theme(axis.title.x=element_blank())
AppE2_c 

##########
# FigE2d #
##########
###################
# Mixture         #
# Beta regression #
# k = 2 clusters  #
# FPcover_low_est #
###################
dataFP$FPcover_low_est

# get rid of your 0s and 1s
dataFP$FPcover_low_est[dataFP$FPcover_low_est == 1] <- (1*(length(dataFP$FPcover_low_est)-1)+0.5)/(length(dataFP$FPcover_low_est))
dataFP$FPcover_low_est[dataFP$FPcover_low_est == 0] <- (0*(length(dataFP$FPcover_low_est)-1)+0.5)/(length(dataFP$FPcover_low_est))

# create the matrix for initial cluster probabilities 
dataFP$prior_cluster1_probv3_low_est <- sqrt(dataFP$FPcover_low_est)
dataFP$prior_cluster2_probv3_low_est <- 1-sqrt(dataFP$FPcover_low_est)

# This is the one problematic outlier that keeps getting assigned to the not-FP-regime cluster
dataFP$FPcover_low_est[64]
dataFP$TOTP_avg[64]

# Give that point a 0 % prob. of being in not-FP-regime cluster 
dataFP$prior_cluster1_probv3_low_est[64] <- 1
dataFP$prior_cluster2_probv3_low_est[64] <- 0

# These are the initial cluster probabilities 
dataFP$prior_cluster1_probv3_low_est 
dataFP$prior_cluster2_probv3_low_est

formula <- FPcover_low_est ~ TOTP_avg
betareg_mix_dataFP_logit_priorclust3_low_est <- betamix(formula, link="logit", data=dataFP, k = 2, nstart = 100, cluster=cbind(dataFP$prior_cluster1_probv3_low_est,dataFP$prior_cluster2_probv3_low_est))
betareg_mix_dataFP_logit_priorclust3_low_est
summary(betareg_mix_dataFP_logit_priorclust3_low_est) 
logLik(betareg_mix_dataFP_logit_priorclust3_low_est) 
AIC(betareg_mix_dataFP_logit_priorclust3_low_est) 
clusters(betareg_mix_dataFP_logit_priorclust3_low_est) 

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataFP$beta_logit_cluster_prior_clusterv3_low_est <- rep(NA, nrow(dataFP))
dataFP_TOTP <- subset(dataFP, dataFP$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataFP_noTOTP <- subset(dataFP, is.na(dataFP$TOTP_avg)) # and waterbodies w/o TOTP
dataFP_TOTP$beta_logit_cluster_prior_clusterv3_low_est<-clusters(betareg_mix_dataFP_logit_priorclust3_low_est) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataFP <- merge(dataFP_TOTP,dataFP_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataFP_TOTP,dataFP_noTOTP)

# plot 
AppE2_d <- ggplot(dataFP,aes(x=TOTP_avg,y=FPcover_low_est,shape=factor(beta_logit_cluster_prior_clusterv3_low_est))) 
AppE2_d <- AppE2_d + stat_smooth(method=glm, family=binomial, se=F, size=1) 
AppE2_d <- AppE2_d + geom_point(size=2) 
AppE2_d <- AppE2_d + scale_shape_manual(values=c(1,19),name="Cluster")
AppE2_d <- AppE2_d + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
AppE2_d <- AppE2_d + geom_text(aes(x=0.0075,y=1,label="d)"),size=7)
AppE2_d <- AppE2_d + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
AppE2_d <- AppE2_d + scale_y_continuous(breaks=y_breaks,labels=y_labels)
AppE2_d <- AppE2_d + theme_classic()
AppE2_d <- AppE2_d + theme(legend.position=c(1,0.3))
AppE2_d <- AppE2_d + theme(axis.title.y=element_blank())
AppE2_d 

##########
# FigE2e #
##########
###################
# Beta regression #
# 8 removed       #
################### 
dataFP_noshifts$FPcover_max

# get rid of your 0s and 1s
dataFP_noshifts$FPcover_max[dataFP_noshifts$FPcover_max == 1] <- (1*(length(dataFP_noshifts$FPcover_max)-1)+0.5)/(length(dataFP_noshifts$FPcover_max))
dataFP_noshifts$FPcover_max[dataFP_noshifts$FPcover_max == 0] <- (0*(length(dataFP_noshifts$FPcover_max)-1)+0.5)/(length(dataFP_noshifts$FPcover_max))

# fit the model 
betareg_dataFP_noshifts_logit <- betareg(FPcover_max ~ TOTP_avg, data=dataFP_noshifts, link="logit")
summary(betareg_dataFP_noshifts_logit)
betareg_dataFP_noshifts_logit$fitted
logLik(betareg_dataFP_noshifts_logit)
AIC(betareg_dataFP_noshifts_logit)

# add the fitted values to my original data set for plotting 
# will need to work around NAs 
AppE2_e_data <- cbind(dataFP_noshifts$TOTP_avg,dataFP_noshifts$FPcover_max)
AppE2_e_data <- AppE2_e_data[complete.cases(AppE2_e_data),]
AppE2_e_data <- cbind(AppE2_e_data,betareg_dataFP_noshifts_logit$fitted)
AppE2_e_data <- as.data.frame(AppE2_e_data)
names(AppE2_e_data) <- c("TOTP_avg","FPcover_max","fitted")
AppE2_e_data

# with color 
library(ggplot2)
AppE2_e <- ggplot(data=AppE2_e_data, aes(x=TOTP_avg,y=FPcover_max)) + geom_point(size=2) 
AppE2_e <- AppE2_e + geom_line(aes(x=TOTP_avg,y=fitted),colour="blue",size=1)
AppE2_e <- AppE2_e + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
AppE2_e <- AppE2_e + geom_text(aes(x=0.0075,y=1,label="e)"),size=7)
AppE2_e <- AppE2_e + scale_x_log10() 
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
AppE2_e <- AppE2_e + scale_y_continuous(breaks=y_breaks,labels=y_labels)
AppE2_e <- AppE2_e + theme_classic(base_size=18)
AppE2_e <- AppE2_e + theme(axis.title.x=element_blank())
AppE2_e 

##########
# FigE2f #
##########
###################
# Mixture         #
# Beta regression #
# k = 2 clusters  #
# 8 shifts removed#
###################
# get rid of your 0s and 1s
dataFP_noshifts$FPcover_max[dataFP_noshifts$FPcover_max == 1] <- (1*(length(dataFP_noshifts$FPcover_max)-1)+0.5)/(length(dataFP_noshifts$FPcover_max))
dataFP_noshifts$FPcover_max[dataFP_noshifts$FPcover_max == 0] <- (0*(length(dataFP_noshifts$FPcover_max)-1)+0.5)/(length(dataFP_noshifts$FPcover_max))

# create the matrix for initial cluster probabilities 
dataFP_noshifts$prior_cluster1_prob <- sqrt(dataFP_noshifts$FPcover_max)
dataFP_noshifts$prior_cluster2_prob <- 1-sqrt(dataFP_noshifts$FPcover_max)

# This is the one problematic outlier that keeps getting assigned to the not-FP-regime cluster
dataFP_noshifts$FPcover_max[60]
dataFP_noshifts$TOTP_avg[60]

# Give that point a 0 % prob. of being in not-FP-regime cluster 
dataFP_noshifts$prior_cluster1_prob[60] <- 1
dataFP_noshifts$prior_cluster2_prob[60] <- 0

# These are the initial cluster probabilities 
dataFP_noshifts$prior_cluster1_prob 
dataFP_noshifts$prior_cluster2_prob

formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataFP_noshifts_logit_priorclust3 <- betamix(formula, link="logit", data=dataFP_noshifts, k = 2, nstart = 100, cluster=cbind(dataFP_noshifts$prior_cluster1_prob,dataFP_noshifts$prior_cluster2_prob))
betareg_mix_dataFP_noshifts_logit_priorclust3
summary(betareg_mix_dataFP_noshifts_logit_priorclust3) 
logLik(betareg_mix_dataFP_noshifts_logit_priorclust3) 
AIC(betareg_mix_dataFP_noshifts_logit_priorclust3) 
clusters(betareg_mix_dataFP_noshifts_logit_priorclust3) 

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataFP_noshifts$beta_logit_cluster_prior_cluster <- rep(NA, nrow(dataFP_noshifts))
dataFP_noshifts_TOTP <- subset(dataFP_noshifts, dataFP_noshifts$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataFP_noshifts_noTOTP <- subset(dataFP_noshifts, is.na(dataFP_noshifts$TOTP_avg)) # and waterbodies w/o TOTP
dataFP_noshifts_TOTP$beta_logit_cluster_prior_cluster<-clusters(betareg_mix_dataFP_noshifts_logit_priorclust3) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataFP_noshifts <- merge(dataFP_noshifts_TOTP,dataFP_noshifts_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataFP_noshifts_TOTP,dataFP_noshifts_noTOTP)

# plot it 
AppE2_f <- ggplot(dataFP_noshifts,aes(x=TOTP_avg,y=FPcover_max,shape=factor(beta_logit_cluster_prior_cluster))) 
AppE2_f <- AppE2_f + stat_smooth(method=glm, family=binomial, se=F, size=1) 
AppE2_f <- AppE2_f+ geom_point(size=2) 
AppE2_f <- AppE2_f + scale_shape_manual(values=c(1,19),name="Cluster")
AppE2_f <- AppE2_f + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
AppE2_f <- AppE2_f + geom_text(aes(x=0.0075,y=1,label="f)"),size=7)
AppE2_f <- AppE2_f + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
AppE2_f <- AppE2_f + scale_y_continuous(breaks=y_breaks,labels=y_labels)
AppE2_f <- AppE2_f + theme_classic()
AppE2_f <- AppE2_f + theme(legend.position=c(0.85,0.3))
AppE2_f <- AppE2_f + theme(axis.title.y=element_blank())
AppE2_f 



###################
# ARRANGING PLOTS #
###################
appendixE <- arrangeGrob(AppE2_a, AppE2_b, AppE2_c, AppE2_d, AppE2_e, AppE2_f, ncol=2,nrow=3) #grid.arrange does not work with ggsave()
appendixE 
ggsave(file="appendixE.jpg", appendixE, height=11,width=11)
