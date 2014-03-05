######################################################################
# Manuscript I: Appendix D                                           #
#                                                                    #
# Sensitivity of results to different estimates of FP cover          # 
######################################################################
library(ggplot2)
library(gridExtra)
library(diptest) # Hartigan's dip test for bimodalit y

###################
# Appendix D      #
# re-do Fig.2     #
# FPcover_max     #
################### 
D1 <- Fig02
D1 <- D1 + scale_y_continuous(limits=c(0,30),expand=c(0,0))
D1 <- D1 + geom_text(aes(x=10,y=29,label="a)"),size=7) # add pane label
D1

dip.test(dataFPsmall$FPcover_max)

# dataFPsmall - waterbodies with floating plants 
cl.res03 <- kmeans(dataFPsmall$FPcover_max, 2, centers=c(min(dataFPsmall$FPcover_max), max(dataFPsmall$FPcover_max)))
n03 <- length(dataFPsmall$FPcover_max)
# variance reduction score = within cluster SS / total sum of squares
# value ranges 0 to 1; low score "indicates an informative split"
vrs03 <- (sum(cl.res03$withinss) / (n03-1)) / var(dataFPsmall$FPcover_max) # 0.09097248
# weighted variance reduction score
# variance reduction independent of cluster sizes
# vale ranges 0 or greater; "low score reflects bimodality"
wvrs03 <- (sum(cl.res03$withinss / cl.res03$size) / 2) / ( ((n03-1)/n03) * var(dataFPsmall$FPcover_max)) # 0.1201015
groups03 <- cl.res03$cluster

###################
# Appendix D      #
# re-do Fig.2     #
# low_est         #
################### 
dataFPsmall$FPcover_low_est_percent <- dataFPsmall$FPcover_low_est*100 

D2 <- ggplot(dataFPsmall,aes(FPcover_low_est_percent)) + stat_bin(binwidth=20,right=TRUE,col="black")
D2 <- D2 + ylab("Frequency") + xlab("Floating plant cover (%)")
D2 <- D2 + scale_x_continuous(limits=c(0,101),breaks=c(0,20,40,60,80,100),expand=c(0,0)) + scale_y_continuous(limits=c(0,30),expand=c(0,0))
D2 <- D2 + theme_classic(base_size=18)
D2 <- D2 + theme(axis.title.y=element_blank())
D2 <- D2 + geom_text(aes(x=10,y=29,label="b)"),size=7) # add pane label
D2

dip.test(dataFPsmall$FPcover_low_est)
# D = 0.0926 
# p-value = 0.0007187

# dataFPsmall - waterbodies with floating plants 
cl.res04 <- kmeans(dataFPsmall$FPcover_low_est, 2, centers=c(min(dataFPsmall$FPcover_low_est), max(dataFPsmall$FPcover_low_est)))
n04 <- length(dataFPsmall$FPcover_low_est)
# variance reduction score = within cluster SS / total sum of squares
# value ranges 0 to 1; low score "indicates an informative split"
vrs04 <- (sum(cl.res04$withinss) / (n04-1)) / var(dataFPsmall$FPcover_low_est) # 0.1721161
# weighted variance reduction score
# variance reduction independent of cluster sizes
# vale ranges 0 or greater; "low score reflects bimodality"
wvrs04 <- (sum(cl.res04$withinss / cl.res04$size) / 2) / ( ((n04-1)/n04) * var(dataFPsmall$FPcover_low_est)) # 
groups04 <- cl.res04$cluster

###################
# Appendix D      #
# re-do Fig.2     #
# 8 removed       #
################### 
dataFPsmall_noshifts$FPcover_low_est_percent <- dataFPsmall_noshifts$FPcover_max*100 

D3 <- ggplot(dataFPsmall_noshifts,aes(FPcover_low_est_percent)) + stat_bin(binwidth=20,right=TRUE,col="black")
D3 <- D3 + ylab("Frequency") + xlab("Floating plant cover (%)")
D3 <- D3 + scale_x_continuous(limits=c(0,101),breaks=c(0,20,40,60,80,100),expand=c(0,0)) + scale_y_continuous(limits=c(0,30),expand=c(0,0))
D3 <- D3 + theme_classic(base_size=18)
D3 <- D3 + theme(axis.title.y=element_blank())
D3 <- D3 + geom_text(aes(x=10,y=29,label="c)"),size=7) # add pane label
D3

dip.test(dataFPsmall_noshifts$FPcover_max)
# D = 0.1087 
# p-value = 9.645e-05

# dataFPsmall_noshifts - waterbodies with floating plants 
cl.res05 <- kmeans(dataFPsmall_noshifts$FPcover_max, 2, centers=c(min(dataFPsmall_noshifts$FPcover_max), max(dataFPsmall_noshifts$FPcover_max)))
n05 <- length(dataFPsmall_noshifts$FPcover_max)
# variance reduction score = within cluster SS / total sum of squares
# value ranges 0 to 1; low score "indicates an informative split"
vrs05 <- (sum(cl.res05$withinss) / (n05-1)) / var(dataFPsmall_noshifts$FPcover_max) # 0.1721161
# weighted variance reduction score
# variance reduction independent of cluster sizes
# vale ranges 0 or greater; "low score reflects bimodality"
wvrs05 <- (sum(cl.res05$withinss / cl.res05$size) / 2) / ( ((n05-1)/n05) * var(dataFPsmall_noshifts$FPcover_max)) # 
groups05 <- cl.res05$cluster


#################
# ARRANGE 
#################
appendixD_freq <- arrangeGrob(D1,D2,D3,ncol=3,nrow=1) #grid.arrange does not work with ggsave()
appendixD_freq



###################################################3

# number of waterbodies (dataONEperpond) with FP cover> 66.666%
nrow(subset(dataONEperpond, dataONEperpond$FPcover_low_est >= 0.66666)) # 12 

###################
# Appendix D      #
# EMPIRICAL       #
# "LINEAR"        #
# (LOGISTIC)      #
# FPcover_low_est #
################### 
appD_b1 <- ggplot(data=dataFPsmall, aes(x=TOTP_avg,y=FPcover_low_est)) + geom_point() 
appD_b1 <- appD_b1 + stat_smooth(method=glm, family=binomial, se=F)
appD_b1 <- appD_b1 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
appD_b1 <- appD_b1 + geom_text(aes(x=0.01,y=1,label="d)"),size=7)
appD_b1 <- appD_b1 + scale_x_log10() 
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
appD_b1 <- appD_b1 + scale_y_continuous(breaks=y_breaks,labels=y_labels)
appD_b1 

glmFPsmallTOTPbinomial_low_est <- glm(FPcover_low_est ~ TOTP_avg, family=binomial, data=dataFPsmall)
summary(glmFPsmallTOTPbinomial_low_est) 
AIC(glmFPsmallTOTPbinomial_low_est) 


###################
# Appendix D      #
# EMPIRICAL       #
# SEGMENTED       #
# THRESHOLD       #
# FPcover_low_est #
###################
# generates your breakpoint 1st

breaks <- dataFPsmall$TOTP_avg[which(dataFPsmall$TOTP_avg >= 0.00001 & dataFPsmall$TOTP_avg <= 0.2)]    # create a vector to hold potential breakpoints 
mse <- numeric(length(breaks)) # create a blank vector to hold MSE     
for(i in 1:length(breaks)){ # loop over all of the potential breakpoints & actually try them out in a lm()
  piecewise <- glm(FPcover_low_est ~ TOTP_avg*(TOTP_avg < breaks[i]) + TOTP_avg*(TOTP_avg>=breaks[i]), family = binomial, data=dataFPsmall)
  mse[i] <- summary(piecewise)[4] # If this is a lm() I should index [6], if it's a glm() I should index [4]
}
mse <- as.numeric(mse) # converts list to numeric 
breakpoint03<-breaks[which(mse==min(mse))] # picks the breakpoint with the lowest mse
breakpoint03 # 0.03004

# Add new variables to the data frame just for plotting the segmented data in ggplot2
# segmented data: TOTP_avg < breakpoint02 and TOTP_avg >= breakpoint03
dataFPsmall$breakpoint03 <- ifelse(dataFPsmall$TOTP_avg <= breakpoint03, "below", "above")

segmented.glmFPsmallTOTPbinomial_low_est <- glm(FPcover_low_est ~ TOTP_avg*(TOTP_avg<breakpoint03) + TOTP_avg*(TOTP_avg>=breakpoint03), family=binomial, data=dataFPsmall)
summary(segmented.glmFPsmallTOTPbinomial_low_est)     
rm(breaks,mse,breakpoint03,piecewise) # clean up your workspace 

# Look at each segment separately 

# segment 1
dataFPsmallseg1_low_est <- subset(dataFPsmall, TOTP_avg < 0.03004)
segmented.glmFPsmallTOTPbinomial_low_est.seg1 <- glm(FPcover_max ~ TOTP_avg, family=binomial, data=dataFPsmallseg1_low_est)
summary(segmented.glmFPsmallTOTPbinomial_low_est.seg1)     

# segment 2
dataFPsmallseg2_low_est <- subset(dataFPsmall, TOTP_avg >= 0.03004)
segmented.glmFPsmallTOTPbinomial_low_est.seg2 <- glm(FPcover_max ~ TOTP_avg, family=binomial, data=dataFPsmallseg2_low_est)
summary(segmented.glmFPsmallTOTPbinomial_low_est.seg2)     

# plot it 
appD_b2 <- ggplot(data=dataFPsmall, aes(x=TOTP_avg,y=FPcover_low_est)) + geom_point() 
appD_b2 <- appD_b2 + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(breakpoint03)))
appD_b2 <- appD_b2 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
appD_b2 <- appD_b2 + geom_text(aes(x=0.01,y=1,label="e)"),size=7)
appD_b2 <- appD_b2 + geom_vline(xintercept=breakpoint02,colour="red",size=1,linetype="longdash") # add vertical line @ threshold value 
appD_b2 <- appD_b2 + scale_x_log10()
appD_b2 <- appD_b2 + theme(legend.position="none")
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
appD_b2 <- appD_b2 + scale_y_continuous(breaks=y_breaks,labels=y_labels)
appD_b2

###################
# Appendix D      #
# EMPIRICAL       #
# OVERLAPPING     #
# ALT. STATES     #
# FPcover_low_est #
###################
# partition data: FPcover_max >= 0.6666 and FPcover_max < 0.6666 
dataFPsmall$third_appD <- ifelse(dataFPsmall$FPcover_low_est >= 0.6666, "greater", "lesser")

glmFPsmallupperthird_low_est <- glm(FPcover_low_est ~ TOTP_avg, family = binomial, data = subset(dataFPsmall, FPcover_low_est >= 0.6666)) 
glmFPsmalllowerthird_low_est <- glm(FPcover_low_est ~ TOTP_avg, family = binomial, data = subset(dataFPsmall, FPcover_low_est < 0.6666)) 
summary(glmFPsmallupperthird_low_est)
summary(glmFPsmalllowerthird_low_est)



appD_b3 <- ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_low_est)) + geom_point() 
appD_b3 <- appD_b3 + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(third_appD))) 
appD_b3 <- appD_b3 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
appD_b3 <- appD_b3 + geom_text(aes(x=0.01,y=1,label="f)"),size=7)
appD_b3 <- appD_b3 + scale_x_log10()
appD_b3 <- appD_b3 + theme(legend.position="none")
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
appD_b3 <- appD_b3 + scale_y_continuous(breaks=y_breaks,labels=y_labels)
appD_b3 <- appD_b3 + geom_vline(xintercept=0.02234,colour="red",size=1,linetype="longdash") # add vertical line @ lower threshold value 
appD_b3 <- appD_b3 + geom_vline(xintercept=0.45717,colour="red",size=1,linetype="longdash") # add vertical line @ upper threshold value 
appD_b3 




#############################################################################################################
# REMOVING "SHIFTS"
#############################################################################################################
dataFPsmall_noshifts <- subset(dataFPsmall, waterbody != "Betty Allen Twin Pond")
dataFPsmall_noshifts <- subset(dataFPsmall_noshifts, waterbody != "Birch Pond")
dataFPsmall_noshifts <- subset(dataFPsmall_noshifts, waterbody != "Mills Pond, Lower")
dataFPsmall_noshifts <- subset(dataFPsmall_noshifts, waterbody != "Mill Pond  South Basin")
dataFPsmall_noshifts <- subset(dataFPsmall_noshifts, waterbody != "Robinson Pond ")
dataFPsmall_noshifts <- subset(dataFPsmall_noshifts, waterbody != "Sound Avenue Pond")
dataFPsmall_noshifts <- subset(dataFPsmall_noshifts, waterbody != "Tilleys Pond")
dataFPsmall_noshifts <- subset(dataFPsmall_noshifts, waterbody != "Young's Pond")
nrow(dataFPsmall_noshifts)

write.csv(dataFPsmall_noshifts, file = "FP ponds less than 5ha no shifts.csv", row.names = FALSE )

###################
# Appendix D      #
# EMPIRICAL       #
# "LINEAR"        #
# (LOGISTIC)      #
# 8 removed       #
################### 
glmFPsmallTOTPbinomial_no_shifts <- glm(FPcover_max ~ TOTP_avg, family=binomial, data=dataFPsmall_noshifts)
summary(glmFPsmallTOTPbinomial_no_shifts) 
AIC(glmFPsmallTOTPbinomial_no_shifts) 

appD_c1 <- ggplot(data=dataFPsmall_noshifts, aes(x=TOTP_avg,y=FPcover_max)) + geom_point() 
appD_c1 <- appD_c1 + stat_smooth(method=glm, family=binomial, se=F)
appD_c1 <- appD_c1 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
appD_c1 <- appD_c1 + geom_text(aes(x=0.01,y=1,label="g)"),size=7)
appD_c1 <- appD_c1 + scale_x_log10() 
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
appD_c1 <- appD_c1 + scale_y_continuous(breaks=y_breaks,labels=y_labels)
appD_c1 

###################
# Appendix D      #
# EMPIRICAL       #
# SEGMENTED       #
# THRESHOLD       #
# 8 removed       #
###################
# generates your breakpoint 1st

breaks <- dataFPsmall_noshifts$TOTP_avg[which(dataFPsmall_noshifts$TOTP_avg >= 0.00001 & dataFPsmall_noshifts$TOTP_avg <= 0.2)]    # create a vector to hold potential breakpoints 
mse <- numeric(length(breaks)) # create a blank vector to hold MSE     
for(i in 1:length(breaks)){ # loop over all of the potential breakpoints & actually try them out in a lm()
  piecewise <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg < breaks[i]) + TOTP_avg*(TOTP_avg>=breaks[i]), family = binomial, data=dataFPsmall_noshifts)
  mse[i] <- summary(piecewise)[4] # If this is a lm() I should index [6], if it's a glm() I should index [4]
}
mse <- as.numeric(mse) # converts list to numeric 
breakpoint04<-breaks[which(mse==min(mse))] # picks the breakpoint with the lowest mse
breakpoint04 # 0.03004

# Add new variables to the data frame just for plotting the segmented data in ggplot2
# segmented data: TOTP_avg < breakpoint02 and TOTP_avg >= breakpoint02 
dataFPsmall_noshifts$breakpoint04 <- ifelse(dataFPsmall_noshifts$TOTP_avg <= breakpoint04, "below", "above")

segmented.glmFPsmallTOTPbinomial_noshifts <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg<breakpoint04) + TOTP_avg*(TOTP_avg>=breakpoint04), family=binomial, data=dataFPsmall_noshifts)
summary(segmented.glmFPsmallTOTPbinomial_noshifts)     
rm(breaks,mse,breakpoint03,piecewise) # clean up your workspace 

# Look at each segment separately 

# segment 1
dataFPsmall_noshifts_seg01 <- subset(dataFPsmall_noshifts, TOTP_avg < 0.03004)
segmented.glmFPsmallTOTPbinomial_noshifts.seg1 <- glm(FPcover_max ~ TOTP_avg, family=binomial, data=dataFPsmall_noshifts_seg01)
summary(segmented.glmFPsmallTOTPbinomial_noshifts.seg1)     

# segment 2
dataFPsmall_noshifts_seg02 <- subset(dataFPsmall_noshifts, TOTP_avg >= 0.03004)
segmented.glmFPsmallTOTPbinomial_noshifts.seg2 <- glm(FPcover_max ~ TOTP_avg, family=binomial, data=dataFPsmall_noshifts_seg02)
summary(segmented.glmFPsmallTOTPbinomial_noshifts.seg2)     

# plot it
appD_c2 <- ggplot(data=dataFPsmall_noshifts, aes(x=TOTP_avg,y=FPcover_max)) + geom_point() 
appD_c2  <- appD_c2  + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(breakpoint04)))
appD_c2  <- appD_c2  + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
appD_c2  <- appD_c2  + geom_text(aes(x=0.01,y=1,label="h)"),size=7)
appD_c2  <- appD_c2  + geom_vline(xintercept=breakpoint02,colour="red",size=1,linetype="longdash") # add vertical line @ threshold value 
appD_c2  <- appD_c2  + scale_x_log10()
appD_c2  <- appD_c2  + theme(legend.position="none")
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
appD_c2  <- appD_c2  + scale_y_continuous(breaks=y_breaks,labels=y_labels)
appD_c2 

###################
# Appendix D      #
# EMPIRICAL       #
# OVERLAPPING     #
# ALT. STATES     #
# 8 removed       #
###################
# partition data: FPcover_max >= 0.6666 and FPcover_max < 0.6666 
dataFPsmall_noshifts$third_appD <- ifelse(dataFPsmall_noshifts$FPcover_max >= 0.6666, "greater", "lesser")

glmFPsmallupperthird_noshifts <- glm(FPcover_max ~ TOTP_avg, family = binomial, data = subset(dataFPsmall_noshifts, FPcover_max >= 0.6666)) 
glmFPsmalllowerthird_noshifts <- glm(FPcover_max ~ TOTP_avg, family = binomial, data = subset(dataFPsmall_noshifts, FPcover_max < 0.6666)) 
summary(glmFPsmallupperthird_noshifts)
summary(glmFPsmalllowerthird_noshifts)

# plot it 
appD_c3 <- ggplot(dataFPsmall_noshifts,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() 
appD_c3 <- appD_c3 + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(third_appD))) 
appD_c3 <- appD_c3 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
appD_c3 <- appD_c3 + geom_text(aes(x=0.01,y=1,label="i)"),size=7)
appD_c3 <- appD_c3 + scale_x_log10()
appD_c3 <- appD_c3 + theme(legend.position="none")
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
appD_c3 <- appD_c3 + scale_y_continuous(breaks=y_breaks,labels=y_labels)
appD_c3 <- appD_c3 + geom_vline(xintercept=0.02234,colour="red",size=1,linetype="longdash") # add vertical line @ lower threshold value 
appD_c3 <- appD_c3 + geom_vline(xintercept=0.20642,colour="red",size=1,linetype="longdash") # add vertical line @ upper threshold value 
appD_c3 


########################################
# A few small tweaks to original plots #
########################################
# I may need to remove the original plot labels 
appD_a1 <- ggplot(data=dataFPsmall, aes(x=TOTP_avg,y=FPcover_max)) + geom_point() 
appD_a1 <- appD_a1 + stat_smooth(method=glm, family=binomial, se=F)
appD_a1 <- appD_a1 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
appD_a1 <- appD_a1 + geom_text(aes(x=0.01,y=1,label="a)"),size=7)
appD_a1 <- appD_a1 + scale_x_log10() 
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
appD_a1 <- appD_a1 + scale_y_continuous(breaks=y_breaks,labels=y_labels)
appD_a1 <- appD_a1 + geom_text(aes(x=0.01,y=1,label="a)"),size=7) 
appD_a1

appD_a2 <- ggplot(data=dataFPsmall, aes(x=TOTP_avg,y=FPcover_max)) + geom_point() 
appD_a2 <- appD_a2 + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(breakpoint02)))
appD_a2 <- appD_a2 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
appD_a2 <- appD_a2 + geom_text(aes(x=0.01,y=1,label="b)"),size=7)
appD_a2 <- appD_a2 + geom_vline(xintercept=breakpoint02,colour="red",size=1,linetype="longdash") # add vertical line @ threshold value 
appD_a2 <- appD_a2 + scale_x_log10()
appD_a2 <- appD_a2 + theme(legend.position="none")
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
appD_a2 <- appD_a2 + scale_y_continuous(breaks=y_breaks,labels=y_labels)
appD_a2

appD_a3 <- ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() 
appD_a3 <- appD_a3 + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(third))) 
appD_a3 <- appD_a3 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
appD_a3 <- appD_a3 + geom_text(aes(x=0.01,y=1,label="c)"),size=7)
appD_a3 <- appD_a3 + scale_x_log10()
appD_a3 <- appD_a3 + theme(legend.position="none")
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
appD_a3 <- appD_a3 + scale_y_continuous(breaks=y_breaks,labels=y_labels)
appD_a3 <- appD_a3 + geom_vline(xintercept=0.01972,colour="red",size=1,linetype="longdash") # add vertical line @ lower threshold value 
appD_a3 <- appD_a3 + geom_vline(xintercept=0.2085,colour="red",size=1,linetype="longdash") # add vertical line @ upper threshold value 
appD_a3 

###################
# ARRANGING PLOTS #
###################
appendixD <- arrangeGrob(appD_a1, appD_a2, appD_a3,appD_b1,appD_b2,appD_b3,appD_c1,appD_c2,appD_c3,ncol=3,nrow=3) #grid.arrange does not work with ggsave()
appendixD 
