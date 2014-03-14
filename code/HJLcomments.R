#############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA  #
#                                           #
# 3/13/14 - response to HJL comments        #
#############################################
library(ggplot2)
###########################################################################################
# FPcover ~ TOTP - include 8 waterbodies that shift as separate points in Fig. 1
###########################################################################################
dataSHIFTStwice = read.csv("survey analysis shiftsoccurtwice.csv") # import data matrix 

# plot the data 
j <- ggplot(data=dataSHIFTStwice, aes(x=TOTP_avg,y=FPcover_max)) + geom_point(size=2) 
j <- j + xlab("Total P (mg/L)") + ylab("Proportion FP cover")
j <- j + scale_x_log10() 
j <- j + theme_classic(base_size=18) 
j

###########################################################################################
# FPcover ~ TOTP - all waterbodies - even w/o FP 
###########################################################################################
k <- ggplot(data=dataSHIFTStwice, aes(x=TOTP_avg,y=FPcover_max)) + geom_point(size=2) 
k <- k + xlab("Total P (mg/L)") + ylab("Proportion FP cover")
k <- k + scale_x_log10() 
k <- k + theme_classic(base_size=18) 
k

###########################################################################################
# Fit mixed distribution to frequency data - Fig. 2 - compare two- vs. one-distribution
###########################################################################################

###########################################################################################
# Fit beta distribution to frequency data - Fig. 2 - compare uniform vs. urn 
###########################################################################################
# qqplot(x, y, plot.it = TRUE, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), ...)
library(MASS)

fitdistr(dataFPsmall$FPcover_max,"beta",list(shape1=1,shape2=1))
### I get a different error message depending the values for shape1 and shape2 used

###########################################################################################
# Include ponds w/o FP in analyses - Fig. 1
###########################################################################################
# plot the data 
c <- ggplot(data=dataONEperpond, aes(x=TOTP_avg,y=FPcover_max)) + geom_point(shape=1) 
c <- c + xlab("Total P (mg/L)") + ylab("Proportion FP cover")
c <- c + scale_x_log10() 
c

# logistic regression
glmFPTOTPbinomial <- glm(FPcover_max ~ TOTP_avg, family=binomial, data=dataONEperpond)
summary(glmFPTOTPbinomial) 

# segmented logistic regression
breaks <- dataONEperpond$TOTP_avg[which(dataONEperpond$TOTP_avg >= 0.00001 & dataONEperpond$TOTP_avg <= 0.2)]    # create a vector to hold potential breakpoints 
mse <- numeric(length(breaks)) # create a blank vector to hold MSE     
for(i in 1:length(breaks)){ # loop over all of the potential breakpoints & actually try them out in a lm()
  piecewise <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg < breaks[i]) + TOTP_avg*(TOTP_avg>=breaks[i]), family = binomial, data=dataONEperpond)
  mse[i] <- summary(piecewise)[4] # If this is a lm() I should index [6], if it's a glm() I should index [4]
}
mse <- as.numeric(mse) # converts list to numeric 
breakpoint05<-breaks[which(mse==min(mse))] # picks the breakpoint with the lowest mse
breakpoint05

# Add new variables to the data frame - used to plot the segmented data in ggplot2
dataONEperpond$breakpoint05 <- ifelse(dataONEperpond$TOTP_avg <= breakpoint05, "below", "above")

# re-run the glm() using this breakpoint 
segmented.glmFPsmallTOTPbinomial2 <- glm(FPcover_max ~ TOTP_avg*(TOTP_avg<breakpoint05) + TOTP_avg*(TOTP_avg>=breakpoint05), family=binomial, data=dataONEperpond)
summary(segmented.glmFPsmallTOTPbinomial)     
rm(breaks,mse,breakpoint05,piecewise) # clean up your workspace 

# plot the segmented logistic regression - dataONEperpond
b2allponds <- ggplot(data=dataONEperpond, aes(x=TOTP_avg,y=FPcover_max)) + geom_point() 
b2allponds <- b2allponds + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(breakpoint05)))
b2allponds <- b2allponds + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
b2allponds <- b2allponds + geom_vline(xintercept=breakpoint02,colour="red",size=1,linetype="longdash") # add vertical line @ threshold value 
b2allponds <- b2allponds + scale_x_log10()
b2allponds <- b2allponds + theme(legend.position="none")
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
b2allponds <- b2allponds + scale_y_continuous(breaks=y_breaks,labels=y_labels)
b2allponds <- b2allponds + theme_classic(base_size=18) + theme(legend.position="none")
b2allponds <- b2allponds + theme(axis.title.y=element_blank())
b2allponds

# partitioned logistic regression 
# Add new variables to the data frame just for plotting the partitioned data in ggplot2
# partition data: FPcover_max >= 0.6666 and FPcover_max < 0.6666 
dataONEperpond$third <- ifelse(dataONEperpond$FPcover_max >= 0.6666, "greater", "lesser")

# partitioned 33:67 y-variable 
glmupperthird <- glm(FPcover_max ~ TOTP_avg, family = binomial, data = subset(dataONEperpond, dataONEperpond$FPcover_max >= 0.6666)) 
glmlowerthird <- glm(FPcover_max ~ TOTP_avg, family = binomial, data = subset(dataONEperpond, dataONEperpond$FPcover_max < 0.6666)) 
summary(glmupperthird)
summary(glmlowerthird)

# plot it
b3allponds <- ggplot(dataONEperpond,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() 
b3allponds <- b3allponds + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(third))) 
b3allponds <- b3allponds + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
b3allponds <- b3allponds + scale_x_log10()
b3allponds <- b3allponds + theme(legend.position="none")
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
b3allponds <- b3allponds + scale_y_continuous(breaks=y_breaks,labels=y_labels)
b3allponds <- b3allponds + geom_vline(xintercept=0.01972,colour="red",size=1,linetype="longdash") # add vertical line @ lower threshold value 
b3allponds <- b3allponds + geom_vline(xintercept=0.2151,colour="red",size=1,linetype="longdash") # add vertical line @ upper threshold value 
b3allponds <- b3allponds + theme_classic(base_size=18) + theme(legend.position="none")
b3allponds <- b3allponds + theme(axis.title.y=element_blank())
b3allponds 


###########################################################################################
# Include ponds w/o FP in analyses - Fig. 2
###########################################################################################
dataONEperpond$FPcover_max_percent <- dataONEperpond$FPcover_max*100 
hist(dataONEperpond$FPcover_max_percent)
e <- ggplot(data=dataONEperpond, aes(x=FPcover_max, fill=as.factor(FP_species_richness))) + geom_bar(stat="bin", binwidth = 0.1) 
e