###############
# Appendix D  #
#             #
# dataFPsmall #
#     vs      #
# dataFP      #
###############

library(ggplot2)
library(gridExtra)

##########################
# Frequency distribution #
##########################
# the original
dataFP$FPcover_max_percent <- dataFP$FPcover_max*100 

Fig01appenda <- ggplot(dataFP,aes(FPcover_max_percent)) + stat_bin(binwidth=20,right=TRUE,col="black")
Fig01appenda <- Fig01appenda + ylab("Frequency") + xlab("Floating plant cover (%)")
Fig01appenda <- Fig01appenda + scale_x_continuous(limits=c(0,101),breaks=c(0,20,40,60,80,100),expand=c(0,0)) + scale_y_continuous(limits=c(0,75),expand=c(0,0))
Fig01appenda <- Fig01appenda + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank()) # remove x-axis values & ticks
Fig01appenda <- Fig01appenda + theme_classic(base_size=18)
Fig01appenda <- Fig01appenda + geom_text(aes(x=5,y=73,label="a)"),size=7)
Fig01appenda

# make a new variable on the data frame - convert proportion to precentage FP cover 
dataFPsmall$FPcover_max_percent <- dataFPsmall$FPcover_max*100 

Fig01appendb <- ggplot(dataFPsmall,aes(FPcover_max_percent)) + stat_bin(binwidth=20,right=TRUE,col="black")
Fig01appendb <- Fig01appendb + ylab("Frequency") + xlab("Floating plant cover (%)")
Fig01appendb <- Fig01appendb + scale_x_continuous(limits=c(0,101),breaks=c(0,20,40,60,80,100),expand=c(0,0)) + scale_y_continuous(limits=c(0,27),expand=c(0,0))
Fig01appendb <- Fig01appendb + theme_classic(base_size=18)
Fig01appendb <- Fig01appendb + geom_text(aes(x=5,y=26,label="b)"),size=7)
Fig01appendb

# arrange # save 
Appendix_D_Fig1 <- arrangeGrob(Fig01appenda, Fig01appendb,ncol=1,nrow=2) #grid.arrange does not work with ggsave()
Appendix_D_Fig1
ggsave(file="Appendix_D_Fig1.pdf", Appendix_D_Fig1, height=10,width=5)
ggsave(file="Appendix_D_Fig1.png", Appendix_D_Fig1, height=10,width=5)
ggsave(file="Appendix_D_Fig1.jpg", Appendix_D_Fig1, height=10,width=5)


##############
# Model fits #
##############

################
# Beta regress #
# dataFPsmall  # 
################ 
# get rid of 0s and 1s 
dataFPsmall$FPcover_max[dataFPsmall$FPcover_max == 1] <- (1*(length(dataFPsmall$FPcover_max)-1)+0.5)/(length(dataFPsmall$FPcover_max))
dataFPsmall$FPcover_max[dataFPsmall$FPcover_max == 0] <- (0*(length(dataFPsmall$FPcover_max)-1)+0.5)/(length(dataFPsmall$FPcover_max))

library(betareg)

# re-run the beta regression
formula <- FPcover_max ~ TOTP_avg
betareg_dataFPsmall_logit <- betareg(formula, data=dataFPsmall, link="logit")
summary(betareg_dataFPsmall_logit)
AIC(betareg_dataFPsmall_logit)
logLik(betareg_dataFPsmall_logit)

# add the fitted values to my original data set for plotting 
# will need to work around NAs 
AppD2_c_data <- cbind(dataFPsmall$TOTP_avg,dataFPsmall$FPcover_max)
AppD2_c_data <- AppD2_c_data[complete.cases(AppD2_c_data),]
AppD2_c_data <- cbind(AppD2_c_data,betareg_dataFPsmall_logit$fitted)
AppD2_c_data <- as.data.frame(AppD2_c_data)
names(AppD2_c_data) <- c("TOTP_avg","FPcover_max","fitted")
AppD2_c_data

# with color 
library(ggplot2)
AppD2_c <- ggplot(data=AppD2_data, aes(x=TOTP_avg,y=FPcover_max)) + geom_point(size=2) 
AppD2_c <- AppD2_c + geom_line(aes(x=TOTP_avg,y=fitted),colour="blue",size=1)
AppD2_c <- AppD2_c + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
AppD2_c <- AppD2_c + geom_text(aes(x=0.0075,y=1,label="c)"),size=7)
AppD2_c <- AppD2_c + scale_x_log10() 
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
AppD2_c <- AppD2_c + scale_y_continuous(breaks=y_breaks,labels=y_labels)
AppD2_c <- AppD2_c + theme_classic(base_size=18)
AppD2_c <- AppD2_c + theme(axis.title.x=element_blank())
AppD2_c 

################
# Fig. 1f      #
# EMPIRICAL    #
# OVERLAPPING  #
# ALT. STATES  #
################
dataFPsmall$prior_cluster1_probv3 <- sqrt(dataFPsmall$FPcover_max)
dataFPsmall$prior_cluster2_probv3 <- 1-sqrt(dataFPsmall$FPcover_max)

# This is the one problematic outlier that keeps getting assigned to the not-FP-regime cluster
dataFPsmall$FPcover_max[33]
dataFPsmall$TOTP_avg[33]

# Give that point a 0 % prob. of being in not-FP-regime cluster 
dataFPsmall$prior_cluster1_probv3[33] <- 1
dataFPsmall$prior_cluster2_probv3[33] <- 0

# These are the initial cluster probabilities 
dataFPsmall$prior_cluster1_probv3 
dataFPsmall$prior_cluster2_probv3

formula <- FPcover_max ~ TOTP_avg
betareg_mix_dataFPsmall_logit_priorclust3 <- betamix(formula, link="logit", data=dataFPsmall, k = 2, nstart = 100, cluster=cbind(dataFPsmall$prior_cluster1_probv3,dataFPsmall$prior_cluster2_probv3))
betareg_mix_dataFPsmall_logit_priorclust3
summary(betareg_mix_dataFPsmall_logit_priorclust3) 
logLik(betareg_mix_dataFPsmall_logit_priorclust3) 
AIC(betareg_mix_dataFPsmall_logit_priorclust3) 
clusters(betareg_mix_dataFPsmall_logit_priorclust3) 

# add cluster assignments to the original data frame 
# need to deal with the fact that there are 6 missing values of TotP
dataFPsmall$beta_logit_cluster_prior_clusterv3 <- rep(NA, nrow(dataFPsmall))
dataFPsmall_TOTP <- subset(dataFPsmall, dataFPsmall$TOTP_avg > 0) # split the dataframe into waterbodies w/ TOTP
dataFPsmall_noTOTP <- subset(dataFPsmall, is.na(dataFPsmall$TOTP_avg)) # and waterbodies w/o TOTP
dataFPsmall_TOTP$beta_logit_cluster_prior_clusterv3<-clusters(betareg_mix_dataFPsmall_logit_priorclust3) # add cluster identities to the data.frame of waterbodies w/ TOTP
dataFPsmall <- merge(dataFPsmall_TOTP,dataFPsmall_noTOTP,all.x=T,all.y=T) # add the dataframes back together 
rm(dataFPsmall_TOTP,dataFPsmall_noTOTP)

# colour
AppD2_d <- ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max,shape=factor(beta_logit_cluster_prior_clusterv3))) 
AppD2_d <- AppD2_d + stat_smooth(method=glm, family=binomial, se=F, size=1) 
AppD2_d <- AppD2_d + geom_point(size=2) 
AppD2_d <- AppD2_d + scale_shape_manual(values=c(1,19),name="Cluster")
AppD2_d <- AppD2_d + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
AppD2_d <- AppD2_d + geom_text(aes(x=0.0095,y=1,label="d)"),size=7)
AppD2_d <- AppD2_d + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
AppD2_d <- AppD2_d + scale_y_continuous(breaks=y_breaks,labels=y_labels)
AppD2_d <- AppD2_d + theme_classic(base_size=18)
AppD2_d <- AppD2_d + theme(legend.position=c(0.95,0.2))
AppD2_d <- AppD2_d + theme(axis.title.y=element_blank())
AppD2_d 

###################
# ARRANGING PLOTS #
###################
# original plots 
AppD2_a <- Fig3a
AppD2_b <- ggplot(dataFP,aes(x=TOTP_avg,y=FPcover_max,shape=factor(beta_logit_cluster_prior_clusterv3))) + stat_smooth(method=glm, family=binomial, se=F,colour="blue",size=1) + geom_point(size=2) 
AppD2_b <- AppD2_b + scale_shape_manual(values=c(1,19),name="Cluster")
AppD2_b <- AppD2_b + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
AppD2_b <- AppD2_b + geom_text(aes(x=0.0075,y=1,label="b)"),size=7)
AppD2_b <- AppD2_b + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
AppD2_b <- AppD2_b + scale_y_continuous(breaks=y_breaks,labels=y_labels)
AppD2_b <- AppD2_b + theme_classic(base_size=18)
AppD2_b <- AppD2_b + theme(legend.position=c(0.85,0.3))
AppD2_b  


Appendix_D_Fig2 <- arrangeGrob(AppD2_a,AppD2_b,AppD2_c,AppD2_d,ncol=2,nrow=2) #grid.arrange does not work with ggsave()
Appendix_D_Fig2
ggsave(file="Appendix_D_Fig2.pdf", Appendix_D_Fig2, height=8,width=8)
ggsave(file="Appendix_D_Fig2.png", Appendix_D_Fig2, height=8,width=8)
ggsave(file="Appendix_D_Fig2.jpg", Appendix_D_Fig2, height=8,width=8)
