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
# Appendix E      #
# re-do Fig.2     #
# FPcover_max     #
################### 
E1 <- Fig02
E1 <- E1 + scale_y_continuous(limits=c(0,80),expand=c(0,0))
E1 <- E1 + geom_text(aes(x=5,y=78,label="a)"),size=7) # add pane label
E1

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
# Appendix E      #
# re-do Fig.2     #
# min. FP value   #
# low_est         #
################### 
dataFP$FPcover_low_est_percent <- dataFP$FPcover_low_est*100 

E2 <- ggplot(dataFP,aes(FPcover_low_est_percent)) + stat_bin(binwidth=20,right=TRUE,col="black")
E2 <- E2 + ylab("Frequency") + xlab("Floating plant cover (%)")
E2 <- E2 + scale_x_continuous(limits=c(0,101),breaks=c(0,20,40,60,80,100),expand=c(0,0)) + scale_y_continuous(limits=c(0,80),expand=c(0,0))
E2 <- E2 + theme_classic(base_size=18)
E2 <- E2 + theme(axis.title.y=element_blank())
E2 <- E2 + geom_text(aes(x=5,y=78,label="b)"),size=7) # add pane label
E2

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
# Appendix E      #
# re-do Fig.2     #
# 8 removed       #
################### 
dataFP_noshifts$FPcover_low_est_percent <- dataFP_noshifts$FPcover_max*100 

E3 <- ggplot(dataFP_noshifts,aes(FPcover_low_est_percent)) + stat_bin(binwidth=20,right=TRUE,col="black")
E3 <- E3 + ylab("Frequency") + xlab("Floating plant cover (%)")
E3 <- E3 + scale_x_continuous(limits=c(0,101),breaks=c(0,20,40,60,80,100),expand=c(0,0)) + scale_y_continuous(limits=c(0,80),expand=c(0,0))
E3 <- E3 + theme_classic(base_size=18)
E3 <- E3 + theme(axis.title.y=element_blank())
E3 <- E3 + geom_text(aes(x=5,y=78,label="c)"),size=7) # add pane label
E3

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
appendixE_freq <- arrangeGrob(E1,E2,E3,ncol=3,nrow=1) #grid.arrange does not work with ggsave()
appendixE_freq

ggsave(file="appendixE_freq.jpg", appendixE_freq, height=5,width=15)


###################################################3
# number of waterbodies (dataONEperpond) with FP cover> 66.666%
nrow(subset(dataONEperpond, dataONEperpond$FPcover_low_est >= 0.66666)) # 12 

###################
# Appendix E      #
# EMPIRICAL       #
# "LINEAR"        #
# (LOGISTIC)      #
# FPcover_low_est #
################### 
appE_b1 <- ggplot(data=dataFP, aes(x=TOTP_avg,y=FPcover_low_est)) + geom_point(size=2) 
appE_b1 <- appE_b1 + stat_smooth(method=glm, family=binomial, se=F)
appE_b1 <- appE_b1 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
appE_b1 <- appE_b1 + geom_text(aes(x=0.0075,y=1,label="d)"),size=7)
appE_b1 <- appE_b1 + scale_x_log10() 
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
appE_b1 <- appE_b1 + scale_y_continuous(breaks=y_breaks,labels=y_labels)
appE_b1 <- appE_b1 + theme_classic()
appE_b1 

glmFPsmallTOTPbinomial_low_est <- glm(FPcover_low_est ~ TOTP_avg, family=binomial, data=dataFP)
summary(glmFPsmallTOTPbinomial_low_est) 
AIC(glmFPsmallTOTPbinomial_low_est) 


###################
# Appendix E      #
# EMPIRICAL       #
# SEGMENTED       #
# THRESHOLD       #
# FPcover_low_est #
###################
# generates your breakpoint 1st
breaks <- dataFP$TOTP_avg[which(dataFP$TOTP_avg >= 0.00001 & dataFP$TOTP_avg <= 0.2)]    # create a vector to hold potential breakpoints 

mse <- numeric(length(breaks)) # create a blank vector to hold MSE     

for(i in 1:length(breaks)){ # loop over all of the potential breakpoints & actually try them out in a lm()
  piecewise <- glm(FPcover_low_est ~ TOTP_avg*(TOTP_avg < breaks[i]) + TOTP_avg*(TOTP_avg>=breaks[i]), family = binomial, data=dataFP)
  mse[i] <- summary(piecewise)[4] # If this is a lm() I should index [6], if it's a glm() I should index [4]
}
mse <- as.numeric(mse) # converts list to numeric 
breakpoint03<-breaks[which(mse==min(mse))] # picks the breakpoint with the lowest mse
breakpoint03 # 0.03004

# Add new variables to the data frame just for plotting the segmented data in ggplot2
# segmented data: TOTP_avg < breakpoint02 and TOTP_avg >= breakpoint03
dataFP$breakpoint03 <- ifelse(dataFP$TOTP_avg <= breakpoint03, "below", "above")

segmented.glmFPsmallTOTPbinomial_low_est <- glm(FPcover_low_est ~ TOTP_avg*(TOTP_avg<breakpoint03) + TOTP_avg*(TOTP_avg>=breakpoint03), family=binomial, data=dataFP)
summary(segmented.glmFPsmallTOTPbinomial_low_est)     
-2*logLik(segmented.glmFPsmallTOTPbinomial_low_est)[1]+2*5 # calculate the actual AIC for this model 
rm(breaks,mse,breakpoint03,piecewise) # clean up your workspace 

# plot it 
appE_b2 <- ggplot(data=dataFP, aes(x=TOTP_avg,y=FPcover_low_est)) + geom_point(size=2) 
appE_b2 <- appE_b2 + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(breakpoint03)))
appE_b2 <- appE_b2 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
appE_b2 <- appE_b2 + geom_text(aes(x=0.0075,y=1,label="e)"),size=7)
appE_b2 <- appE_b2 + geom_vline(xintercept=breakpoint03,colour="red",size=1,linetype="longdash") # add vertical line @ threshold value 
appE_b2 <- appE_b2 + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
appE_b2 <- appE_b2 + scale_y_continuous(breaks=y_breaks,labels=y_labels)
appE_b2 <- appE_b2 + theme_classic() + theme(legend.position="none")
appE_b2

###################
# Appendix E      #
# EMPIRICAL       #
# Mixture         #
# Beta regression #
# k = 2 clusters  #
# ALT. STATES     #
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
appE_b3 <- ggplot(dataFP,aes(x=TOTP_avg,y=FPcover_low_est,shape=factor(beta_logit_cluster_prior_clusterv3_low_est))) + stat_smooth(method=glm, family=binomial, se=F) + geom_point(size=2) 
appE_b3 <- appE_b3 + scale_shape_manual(values=c(1,19),name="Cluster")
appE_b3 <- appE_b3 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
appE_b3 <- appE_b3 + geom_text(aes(x=0.0075,y=1,label="f)"),size=7)
appE_b3 <- appE_b3 + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
appE_b3 <- appE_b3 + scale_y_continuous(breaks=y_breaks,labels=y_labels)
appE_b3 <- appE_b3 + geom_vline(xintercept=0.02234,colour="red",size=1,linetype="longdash") # add vertical line @ lower threshold value 
appE_b3 <- appE_b3 + geom_vline(xintercept=0.457171,colour="red",size=1,linetype="longdash") # add vertical line @ upper threshold value 
appE_b3 <- appE_b3 + theme_classic()
appE_b3 <- appE_b3 + theme(legend.position=c(1,0.3))
appE_b3 <- appE_b3 + theme(axis.title.y=element_blank())
appE_b3 

###################
# Appendix E      #
# EMPIRICAL       #
# "LINEAR"        #
# (LOGISTIC)      #
# 8 removed       #
################### 
glmFPsmallTOTPbinomial_no_shifts <- glm(FPcover_max ~ TOTP_avg, family=binomial, data=dataFP_noshifts)
summary(glmFPsmallTOTPbinomial_no_shifts) 
AIC(glmFPsmallTOTPbinomial_no_shifts) 

appE_c1 <- ggplot(data=dataFP_noshifts, aes(x=TOTP_avg,y=FPcover_max)) + geom_point(size=2) 
appE_c1 <- appE_c1 + stat_smooth(method=glm, family=binomial, se=F)
appE_c1 <- appE_c1 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
appE_c1 <- appE_c1 + geom_text(aes(x=0.0075,y=1,label="g)"),size=7)
appE_c1 <- appE_c1 + scale_x_log10() 
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
appE_c1 <- appE_c1 + scale_y_continuous(breaks=y_breaks,labels=y_labels)
appE_c1 <- appE_c1 + theme_classic()
appE_c1 

###################
# Appendix E      #
# EMPIRICAL       #
# SEGMENTED       #
# THRESHOLD       #
# 8 removed       #
###################
# generates your breakpoint 1st

breaks <- dataFP_noshifts$TOTP_avg[which(dataFP_noshifts$TOTP_avg >= 0.00001 & dataFP_noshifts$TOTP_avg <= 0.2)]    # create a vector to hold potential breakpoints 
mse <- numeric(length(breaks)) # create a blank vector to hold MSE     
for(i in 1:length(breaks)){ # loop over all of the potential breakpoints & actually try them out in a lm()
  piecewise <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg < breaks[i]) + TOTP_avg*(TOTP_avg>=breaks[i]), family = binomial, data=dataFP_noshifts)
  mse[i] <- summary(piecewise)[4] # If this is a lm() I should index [6], if it's a glm() I should index [4]
}
mse <- as.numeric(mse) # converts list to numeric 
breakpoint04<-breaks[which(mse==min(mse))] # picks the breakpoint with the lowest mse
breakpoint04 # 0.03004

# Add new variables to the data frame just for plotting the segmented data in ggplot2
# segmented data: TOTP_avg < breakpoint02 and TOTP_avg >= breakpoint02 
dataFP_noshifts$breakpoint04 <- ifelse(dataFP_noshifts$TOTP_avg <= breakpoint04, "below", "above")

segmented.glmFPsmallTOTPbinomial_noshifts <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg<breakpoint04) + TOTP_avg*(TOTP_avg>=breakpoint04), family=binomial, data=dataFP_noshifts)
summary(segmented.glmFPsmallTOTPbinomial_noshifts)    
-2*logLik(segmented.glmFPsmallTOTPbinomial_noshifts)[1]+2*5 # calculate the actual AIC for this model 
rm(breaks,mse,breakpoint03,piecewise) # clean up your workspace 

# plot it
appE_c2 <- ggplot(data=dataFP_noshifts, aes(x=TOTP_avg,y=FPcover_max)) + geom_point(size=2) 
appE_c2  <- appE_c2  + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(breakpoint04)))
appE_c2  <- appE_c2  + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
appE_c2  <- appE_c2  + geom_text(aes(x=0.0075,y=1,label="h)"),size=7)
appE_c2  <- appE_c2  + geom_vline(xintercept=breakpoint04,colour="red",size=1,linetype="longdash") # add vertical line @ threshold value 
appE_c2  <- appE_c2  + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
appE_c2  <- appE_c2  + scale_y_continuous(breaks=y_breaks,labels=y_labels)
appE_c2  <- appE_c2  + theme_classic() + theme(legend.position="none")
appE_c2 

###################
# Appendix E      #
# EMPIRICAL       #
# Mixture         #
# Beta regression #
# k = 2 clusters  #
# ALT. STATES     #
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
appE_c3 <- ggplot(dataFP_noshifts,aes(x=TOTP_avg,y=FPcover_max,shape=factor(beta_logit_cluster_prior_cluster))) + stat_smooth(method=glm, family=binomial, se=F) + geom_point(size=2) 
appE_c3 <- appE_c3 + scale_shape_manual(values=c(1,19),name="Cluster")
appE_c3 <- appE_c3 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
appE_c3 <- appE_c3 + geom_text(aes(x=0.0075,y=1,label="i)"),size=7)
appE_c3 <- appE_c3 + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
appE_c3 <- appE_c3 + scale_y_continuous(breaks=y_breaks,labels=y_labels)
appE_c3 <- appE_c3 + geom_vline(xintercept=0.02234,colour="red",size=1,linetype="longdash") # add vertical line @ lower threshold value 
appE_c3 <- appE_c3 + geom_vline(xintercept=0.2085,colour="red",size=1,linetype="longdash") # add vertical line @ upper threshold value 
appE_c3 <- appE_c3 + theme_classic()
appE_c3 <- appE_c3 + theme(legend.position=c(0.85,0.3))
appE_c3 <- appE_c3 + theme(axis.title.y=element_blank())
appE_c3 


########################################
# A few small tweaks to original plots #
########################################
# I may need to remove the original plot labels 
appE_a1 <- ggplot(data=dataFP, aes(x=TOTP_avg,y=FPcover_max)) + geom_point(size=2) 
appE_a1 <- appE_a1 + stat_smooth(method=glm, family=binomial, se=F)
appE_a1 <- appE_a1 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
appE_a1 <- appE_a1 + geom_text(aes(x=0.0075,y=1,label="a)"),size=7)
appE_a1 <- appE_a1 + scale_x_log10() 
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
appE_a1 <- appE_a1 + scale_y_continuous(breaks=y_breaks,labels=y_labels)
appE_a1 <- appE_a1 + theme_classic()
appE_a1

appE_a2 <- ggplot(data=dataFP, aes(x=TOTP_avg,y=FPcover_max)) + geom_point(size=2) 
appE_a2 <- appE_a2 + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(breakpoint03)))
appE_a2 <- appE_a2 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
appE_a2 <- appE_a2 + geom_text(aes(x=0.0075,y=1,label="b)"),size=7)
appE_a2 <- appE_a2 + geom_vline(xintercept=breakpoint,colour="red",size=1,linetype="longdash") # add vertical line @ threshold value 
appE_a2 <- appE_a2 + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
appE_a2 <- appE_a2 + scale_y_continuous(breaks=y_breaks,labels=y_labels)
appE_a2 <- appE_a2 + theme_classic() + theme(legend.position="none")
appE_a2

appE_a3 <- ggplot(dataFP,aes(x=TOTP_avg,y=FPcover_max,shape=factor(beta_logit_cluster_prior_clusterv3))) + stat_smooth(method=glm, family=binomial, se=F) + geom_point(size=2) 
appE_a3 <- appE_a3 + scale_shape_manual(values=c(1,19),name="Cluster")
appE_a3 <- appE_a3 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
appE_a3 <- appE_a3 + geom_text(aes(x=0.0075,y=1,label="c)"),size=7)
appE_a3 <- appE_a3 + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
appE_a3 <- appE_a3 + scale_y_continuous(breaks=y_breaks,labels=y_labels)
appE_a3 <- appE_a3 + geom_vline(xintercept=0.01972,colour="red",size=1,linetype="longdash") # add vertical line @ lower threshold value 
appE_a3 <- appE_a3 + geom_vline(xintercept=0.2085,colour="red",size=1,linetype="longdash") # add vertical line @ upper threshold value 
appE_a3 <- appE_a3 + theme_classic(base_size=18)
appE_a3 <- appE_a3 + theme(legend.position=c(0.85,0.3))
appE_a3 <- appE_a3 + theme(axis.title.y=element_blank())
appE_a3 

###################
# ARRANGING PLOTS #
###################
appendixE <- arrangeGrob(appE_a1, appE_a2, appE_a3,appE_b1,appE_b2,appE_b3,appE_c1,appE_c2,appE_c3,ncol=3,nrow=3) #grid.arrange does not work with ggsave()
appendixE 
ggsave(file="appendixE.jpg", appendixE, height=11,width=11)
