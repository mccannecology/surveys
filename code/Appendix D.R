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
# Fig. 1d      #
# EMPIRICAL    #
# "LINEAR"     #
# (LOGISTIC)   #
################ 
formula <- FPcover_max ~ TOTP_avg
glm_dataFPsmall_binomial_logit <- glm(formula, data=dataFPsmall, family=binomial(link=logit))
summary(glm_dataFPsmall_binomial_logit)
logLik(glm_dataFPsmall_binomial_logit)
AIC(glm_dataFPsmall_binomial_logit)

# colour
b1append <- ggplot(data=dataFPsmall, aes(x=TOTP_avg,y=FPcover_max)) + geom_point(size=2) 
b1append <- b1append + stat_smooth(method=glm, family=binomial, se=F)
b1append <- b1append + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
b1append <- b1append + geom_text(aes(x=0.0095,y=1,label="d)"),size=7)
b1append <- b1append + scale_x_log10() 
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
b1append <- b1append + scale_y_continuous(breaks=y_breaks,labels=y_labels)
b1append <- b1append + theme_classic(base_size=18)
b1append 

################
# Fig. 1e      #
# EMPIRICAL    #
# SEGMENTED    #
# THRESHOLD    #
################
# use "segmented logistic.R" first 
# generates your breakpoint 
# breakpoint is used below and is defined in the segmented logistic script 

breaks <- dataFPsmall$TOTP_avg[which(dataFPsmall$TOTP_avg >= 0.00001 & dataFPsmall$TOTP_avg <= 0.5)]    # create a vector to hold potential breakpoints 

mse <- numeric(length(breaks)) # create a blank vector to hold MSE     

for(i in 1:length(breaks)){ # loop over all of the potential breakpoints & actually try them out in a lm()
  piecewise <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg < breaks[i]) + TOTP_avg*(TOTP_avg>=breaks[i]), family=binomial(link=logit), data=dataFPsmall)
  mse[i] <- summary(piecewise)[4] # If this is a lm() I should index [6], if it's a glm() I should index [4]
}

mse <- as.numeric(mse) # converts list to numeric 
breakpoint<-breaks[which(mse==min(mse))] # picks the breakpoint with the lowest mse
breakpoint # returns the breakpoint 

# re-run the glm() using this breakpoint 
segmented_dataFPsmall_binomial_logit <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg<breakpoint) + TOTP_avg*(TOTP_avg>=breakpoint), family=binomial(link=logit), data=dataFPsmall)
summary(segmented_dataFPsmall_binomial_logit)     
AIC(segmented_dataFPsmall_binomial_logit) # gives you the incorrect AIC - does not account for estimating the breakpoint parameter 
logLik(segmented_dataFPsmall_binomial_logit)
-2*logLik(segmented_dataFPsmall_binomial_logit)[1]+2*5 # calculate the actual AIC for this model 
breakpoint

# Add new variables to the data frame 
# used to plot the segmented data in ggplot2
dataFPsmall$breakpoint <- ifelse(dataFPsmall$TOTP_avg <= breakpoint, "below", "above")

# colour
b2append <- ggplot(data=dataFPsmall, aes(x=TOTP_avg,y=FPcover_max)) + geom_point(size=2) 
b2append <- b2append + stat_smooth(method=glm, family=binomial, se=F, aes(fill=factor(breakpoint)))
b2append <- b2append + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
b2append <- b2append + geom_text(aes(x=0.0095,y=1,label="e)"),size=7)
b2append <- b2append + geom_vline(xintercept=breakpoint,colour="red",size=1,linetype="longdash") # add vertical line @ threshold value
b2append <- b2append + scale_x_log10()
b2append <- b2append + theme(legend.position="none")
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
b2append <- b2append + scale_y_continuous(breaks=y_breaks,labels=y_labels)
b2append <- b2append + theme_classic(base_size=18) + theme(legend.position="none")
b2append <- b2append + theme(axis.title.y=element_blank())
b2append

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
b3append <- ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max,shape=factor(beta_logit_cluster_prior_clusterv3))) + stat_smooth(method=glm, family=binomial, se=F) + geom_point(size=2) 
b3append <- b3append + scale_shape_manual(values=c(1,19),name="Cluster")
b3append <- b3append + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
b3append <- b3append + geom_text(aes(x=0.0095,y=1,label="f)"),size=7)
b3append <- b3append + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
b3append <- b3append + scale_y_continuous(breaks=y_breaks,labels=y_labels)
b3append <- b3append + geom_vline(xintercept=0.01972,colour="red",size=1,linetype="longdash") # add vertical line @ lower threshold value 
b3append <- b3append + geom_vline(xintercept=0.2085,colour="red",size=1,linetype="longdash") # add vertical line @ upper threshold value 
b3append <- b3append + theme_classic(base_size=18)
b3append <- b3append + theme(legend.position=c(0.95,0.2))
b3append <- b3append + theme(axis.title.y=element_blank())
b3append 

###################
# ARRANGING PLOTS #
###################
# original plots 
appE_a1
appE_a2
appE_a3

# or 

appD_a1
appD_a2
appD_a3


Appendix_D_Fig2 <- arrangeGrob(appD_a1,appD_a2,appD_a3,b1append,b2append,b3append,ncol=3,nrow=2) #grid.arrange does not work with ggsave()
Appendix_D_Fig2
ggsave(file="Appendix_D_Fig2.pdf", Appendix_D_Fig2, height=8,width=11)
ggsave(file="Appendix_D_Fig2.png", Appendix_D_Fig2, height=8,width=11)
ggsave(file="Appendix_D_Fig2.jpg", Appendix_D_Fig2, height=8,width=11)
