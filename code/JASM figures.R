##############################
# Figures for JASM 2014 talk #
##############################

library(ggplot2)
library(gridExtra)

# make a new variable on the data frame - convert proportion to precentage FP cover 
dataONEperpond$FPcover_max_percent <- dataONEperpond$FPcover_max*100 
dataFP$FPcover_max_percent <- dataFP$FPcover_max*100 
dataFPsmall$FPcover_max_percent <- dataFPsmall$FPcover_max*100 


##################
# Surfacea area  #
# dataFP         #
##################
JASM_SA <-ggplot(data=dataFP, aes(x=surfacearea_ha,y=FPcover_max_percent)) + geom_point(size=3) 
JASM_SA <- JASM_SA + scale_x_log10()
JASM_SA <- JASM_SA + geom_vline(xintercept = 5,colour="red",size=1,linetype="longdash") 
JASM_SA <- JASM_SA + ylab("Floating plant cover (%)") 
JASM_SA <- JASM_SA + xlab("Surface area (ha)")
JASM_SA <- JASM_SA + theme_classic(base_size=18)
JASM_SA

ggsave(file="JASM_SA", JASM_SA, height=5,width=5)


##################
# FP frequency   #
# dataONEperpond #
##################
JASMfig_All <- ggplot(dataONEperpond,aes(FPcover_max_percent)) 
JASMfig_All <- JASMfig_All + stat_bin(binwidth=20,right=F,col="white")
JASMfig_All <- JASMfig_All + ylab("Frequency") 
JASMfig_All <- JASMfig_All + xlab("Floating plant cover (%)")
JASMfig_All <- JASMfig_All + scale_x_continuous(limits=c(0,101),breaks=c(0,20,40,60,80,100),expand=c(0,0)) 
JASMfig_All <- JASMfig_All + scale_y_continuous(limits=c(0,200),expand=c(0,0))
JASMfig_All <- JASMfig_All + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank()) # remove x-axis values & ticks
JASMfig_All <- JASMfig_All + theme_classic(base_size=18)
JASMfig_All

ggsave(file="JASMfig_All.jpg", JASMfig_All, height=5,width=5)

##################
# FP frequency   #
# dataFP         #
##################
JASMfig_FP <- ggplot(dataFP,aes(FPcover_max_percent)) 
JASMfig_FP <- JASMfig_FP + stat_bin(binwidth=20,right=TRUE,col="white")
JASMfig_FP <- JASMfig_FP + ylab("Frequency") 
JASMfig_FP <- JASMfig_FP + xlab("Floating plant cover (%)")
JASMfig_FP <- JASMfig_FP + scale_x_continuous(limits=c(0,101),breaks=c(0,20,40,60,80,100),expand=c(0,0)) 
JASMfig_FP <- JASMfig_FP + scale_y_continuous(limits=c(0,70),expand=c(0,0))
JASMfig_FP <- JASMfig_FP + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank()) # remove x-axis values & ticks
JASMfig_FP <- JASMfig_FP + theme_classic(base_size=18)
JASMfig_FP

ggsave(file="JASMfig_FP.jpg", JASMfig_FP, height=5,width=5)

##################
# FP frequency   #
# dataFPsmall    #
################## 
JASMfig_FPsmall <- ggplot(dataFPsmall,aes(FPcover_max_percent)) 
JASMfig_FPsmall <- JASMfig_FPsmall + stat_bin(binwidth=20,right=TRUE,col="white")
JASMfig_FPsmall <- JASMfig_FPsmall + ylab("Frequency") 
JASMfig_FPsmall <- JASMfig_FPsmall + xlab("Floating plant cover (%)")
JASMfig_FPsmall <- JASMfig_FPsmall + scale_x_continuous(limits=c(0,101),breaks=c(0,20,40,60,80,100),expand=c(0,0)) 
JASMfig_FPsmall <- JASMfig_FPsmall + scale_y_continuous(limits=c(0,25),expand=c(0,0))
JASMfig_FPsmall <- JASMfig_FPsmall + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank()) # remove x-axis values & ticks
JASMfig_FPsmall <- JASMfig_FPsmall + theme_classic(base_size=18)
JASMfig_FPsmall

ggsave(file="JASMfig_FPsmall.jpg", JASMfig_FPsmall, height=5,width=5)

###########################
# FP frequency            #
# White on black versions #
# needs theme_black.R     #
###########################
JASMfig_FP <- ggplot(dataFP,aes(FPcover_max_percent)) 
JASMfig_FP <- JASMfig_FP + stat_bin(binwidth=20,right=TRUE,col="black",colour="white")
JASMfig_FP <- JASMfig_FP + stat_density(colour="blue",fill=NA)
JASMfig_FP <- JASMfig_FP + ylab("Frequency") 
JASMfig_FP <- JASMfig_FP + xlab("Floating plant cover (%)")
JASMfig_FP <- JASMfig_FP + scale_x_continuous(limits=c(0,101),breaks=c(0,20,40,60,80,100),expand=c(0,0)) 
JASMfig_FP <- JASMfig_FP + scale_y_continuous(limits=c(0,72),expand=c(0,0))
JASMfig_FP <- JASMfig_FP + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank()) # remove x-axis values & ticks
JASMfig_FP <- JASMfig_FP + theme_black()
JASMfig_FP

ggsave(file="JASMfig - FP ponds.jpg", JASMfig_FP, height=8,width=8)


################
# Fig. 1a      #
# HYPOTHETICAL #
# LINEAR       #
################ 
JASM_a1 <- ggplot(data.frame(x = c(0, 0.5)), aes(x)) # set-up blank plot 
JASM_a1 <- JASM_a1 + geom_segment(aes(x=0,y=1,xend=0.5,yend=100),size=1) # add linear line
JASM_a1 <- JASM_a1 + xlab(" ") # label axes 
JASM_a1 <- JASM_a1 + ylab("Floating plant cover (%)") # label axes 
JASM_a1 <- JASM_a1 + theme_classic(base_size=18)
JASM_a1 <- JASM_a1 + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) # remove x-axis values & ticks
JASM_a1

################
# Fig. 1b      #
# HYPOTHETICAL #
# SEGMENTED    # 
# THRESHOLD    #
################
JASM_a2 <- ggplot(data.frame(x = c(0, 0.5)), aes(x)) # set-up blank plot 
JASM_a2 <- JASM_a2 + geom_segment(aes(x=0,y=1,xend=0.25,yend=10),size=1) + geom_segment(aes(x=0.25,y=90,xend=0.5,yend=100),size=1) # add two segments 
JASM_a2 <- JASM_a2 + geom_vline(xintercept=0.25,colour="red",size=1,linetype="longdash") # add vertical line @ threshold value 
JASM_a2 <- JASM_a2 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)") # label axes 
JASM_a2 <- JASM_a2 + theme_classic(base_size=18)
JASM_a2 <- JASM_a2 + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) # remove x-axis values & ticks
JASM_a2 <- JASM_a2 + theme(axis.title.y=element_blank())
JASM_a2

################
# Fig. 1c      #
# HYPOTHETICAL #
# OVERLAPPING  #
# ALT. STATES  #
################
# create a blank plot - but you cannot view it until you add a line 
JASM_a3 <- ggplot(data.frame(x = c(0, 0.5)), aes(x)) # set-up blank plot 
JASM_a3 <- JASM_a3 + geom_segment(aes(x=0,y=1,xend=0.35,yend=12),size=1) + geom_segment(aes(x=0.15,y=88,xend=0.5,yend=100),size=1) # add two segments 
JASM_a3 <- JASM_a3 + geom_vline(xintercept=0.15,colour="red",size=1,linetype="longdash") # add vertical line @ 1st threshold value 
JASM_a3 <- JASM_a3 + geom_vline(xintercept=0.35,colour="red",size=1,linetype="longdash") # add vertical line @ 2nd threshold value 
JASM_a3 <- JASM_a3 + xlab(" ") + ylab("Floating plant cover(%)") # label axes 
JASM_a3 <- JASM_a3 + theme_classic(base_size=18)
JASM_a3 <- JASM_a3 + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) # remove x-axis values & ticks
JASM_a3 <- JASM_a3 + theme(axis.title.y=element_blank())
JASM_a3


JASM_totalP <- arrangeGrob(JASM_a1,JASM_a2,JASM_a3,ncol=3,nrow=1) #grid.arrange does not work with ggsave()
JASM_totalP
ggsave(file="JASM_totalP.jpg", JASM_totalP, height=4,width=11)


##############
# TOTP vs FP #
# beta reg   #
##############
# Get rid of any 0s and 1s in the dependent (Y) variable
dataFP$FPcover_max[dataFP$FPcover_max == 1] <- (1*(length(dataFP$FPcover_max)-1)+0.5)/(length(dataFP$FPcover_max))
dataFP$FPcover_max[dataFP$FPcover_max == 0] <- (0*(length(dataFP$FPcover_max)-1)+0.5)/(length(dataFP$FPcover_max))

# add the fitted values to my original data set for plotting 
# will need to work around NAs 
Fig3data <- cbind(dataFP$TOTP_avg,dataFP$FPcover_max)
Fig3data <- Fig3data[complete.cases(Fig3data),]
Fig3data <- cbind(Fig3data,betareg_dataFP_logit$fitted)
Fig3data <- as.data.frame(Fig3data)
names(Fig3data) <- c("TOTP_avg","FPcover_max","fitted")
Fig3data

############
# raw data #
# no model #
############
JASM_TOTPFP <- ggplot(data=Fig3data, aes(x=TOTP_avg,y=FPcover_max)) + geom_point(size=2) 
JASM_TOTPFP <- JASM_TOTPFP + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
JASM_TOTPFP <- JASM_TOTPFP + scale_x_log10() 
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
JASM_TOTPFP <- JASM_TOTPFP + scale_y_continuous(breaks=y_breaks,labels=y_labels)
JASM_TOTPFP <- JASM_TOTPFP + theme_classic(base_size=18)
JASM_TOTPFP <- JASM_TOTPFP + geom_vline(xintercept=0.01972,colour="red",size=1,linetype="longdash") # add vertical line @ lower threshold value 
JASM_TOTPFP <- JASM_TOTPFP + geom_vline(xintercept=0.2085,colour="red",size=1,linetype="longdash") # add vertical line @ upper threshold value 
JASM_TOTPFP 

ggsave(file="JASM_TOTPFP.jpg", JASM_TOTPFP, height=5,width=5)

###################
# Beta regression #
###################
JASM_beta01 <- ggplot(data=Fig3data, aes(x=TOTP_avg,y=FPcover_max)) + geom_point(size=2) 
JASM_beta01 <- JASM_beta01 + geom_line(aes(x=TOTP_avg,y=fitted),colour="blue",size=1)
JASM_beta01 <- JASM_beta01 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
JASM_beta01 <- JASM_beta01 + scale_x_log10() 
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
JASM_beta01 <- JASM_beta01 + scale_y_continuous(breaks=y_breaks,labels=y_labels)
JASM_beta01 <- JASM_beta01 + theme_classic(base_size=18)
JASM_beta01 

ggsave(file="JASM_beta01.jpg", JASM_beta01, height=5,width=5)

###################
# Mixture         # 
# Beta regression #
###################
library(betareg)

# create the matrix for initial cluster probabilities 
dataFP$prior_cluster1 <- sqrt(dataFP$FPcover_max)
dataFP$prior_cluster2 <- 1-sqrt(dataFP$FPcover_max)

# This is the one problematic outlier that keeps getting assigned to the not-FP-regime cluster
dataFP$FPcover_max[64]
dataFP$TOTP_avg[64]

# Give that point a 0 % prob. of being in not-FP-regime cluster 
dataFP$prior_cluster1[64] <- 1
dataFP$prior_cluster2[64] <- 0

# These are the initial cluster probabilities 
dataFP$prior_cluster1 
dataFP$prior_cluster2

# The real model 
formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataFP_logit_priorcluster <- betamix(formula, link="logit", data=dataFP, k = 2, nstart = 100, cluster=cbind(dataFP$prior_cluster1,dataFP$prior_cluster2))
betareg_mix_dataFP_logit_priorcluster
summary(betareg_mix_dataFP_logit_priorcluster) 
logLik(betareg_mix_dataFP_logit_priorcluster) 
AIC(betareg_mix_dataFP_logit_priorcluster) 
clusters(betareg_mix_dataFP_logit_priorcluster) 

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataFP$betareg_mix_dataFP_logit_priorcluster <- rep(NA, nrow(dataFP))
dataFP_TOTP <- subset(dataFP, dataFP$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataFP_noTOTP <- subset(dataFP, is.na(dataFP$TOTP_avg)) # and waterbodies w/o TOTP
dataFP_TOTP$betareg_mix_dataFP_logit_priorcluster<-clusters(betareg_mix_dataFP_logit_priorcluster) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataFP <- merge(dataFP_TOTP,dataFP_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataFP_TOTP,dataFP_noTOTP)

JAMS_beta02 <- ggplot(dataFP,aes(x=TOTP_avg,y=FPcover_max,shape=factor(betareg_mix_dataFP_logit_priorcluster))) + stat_smooth(method=glm, family=binomial, se=F,colour="blue",size=1) + geom_point(size=2) 
JAMS_beta02 <- JAMS_beta02 + scale_shape_manual(values=c(1,19),name="Cluster")
JAMS_beta02 <- JAMS_beta02 + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
JAMS_beta02 <- JAMS_beta02 + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
JAMS_beta02 <- JAMS_beta02 + scale_y_continuous(breaks=y_breaks,labels=y_labels)
JAMS_beta02 <- JAMS_beta02 + geom_vline(xintercept=0.01972,colour="red",size=1,linetype="longdash") # add vertical line @ lower threshold value 
JAMS_beta02 <- JAMS_beta02 + geom_vline(xintercept=0.2085,colour="red",size=1,linetype="longdash") # add vertical line @ upper threshold value 
JAMS_beta02 <- JAMS_beta02 + theme_classic(base_size=18)
JAMS_beta02 <- JAMS_beta02 + theme(legend.position=c(0.85,0.3))
JAMS_beta02 

ggsave(file="JASM_beta02.jpg", JAMS_beta02, height=5,width=5)

