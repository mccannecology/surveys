######################################################################
# Manuscript I: Appendix D                                           #
#                                                                    #
# Sensitivity of results to different estimates of FP cover          # 
######################################################################

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
# segmented data: TOTP_avg < breakpoint02 and TOTP_avg >= breakpoint02 
dataFPsmall$breakpoint03 <- ifelse(dataFPsmall$TOTP_avg <= breakpoint03, "below", "above")

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
appendixE <- arrangeGrob(appD_a1, appD_a2, appD_a3,appD_b1,appD_b2,appD_b3,appD_c1,appD_c2,appD_c3,ncol=3,nrow=3) #grid.arrange does not work with ggsave()
appendixE 
