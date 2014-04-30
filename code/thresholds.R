##############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA   #
# Threshold & breakpoint detection           #
# UPDATED: 04/30/2014 by MJ McCann           #
##############################################

library(strucchange)
library(changepoint)
library(tree)

############################################################################################### 
# Package: tree 
###############################################################################################
# This approach is dumb
# tree() will always find a split! 

# threshold, with package: tree 
treeFPsmallTOTP <- tree(dataFPsmall$FPcover_max ~ dataFPsmall$TOTP_avg)
treeFPsmallTOTP

# threshold, with package: tree 
treeFPTOTP <- tree(dataFP$FPcover_max ~ dataFP$TOTP_avg)
treeFPTOTP
plot(treeFPTOTP)
text(treeFPTOTP)


############################################################################################### 
# Package: strucchange 
###############################################################################################
# threshold, with package: strucchange 
breakpointFPsmallTOTP <- breakpoints(FPcover_max ~ TOTP_avg, h=5, data=dataFPsmall)
summary(breakpointFPsmallTOTP)

# returns breakpoints, but this is with y = Total P --- Not what I want 
breakpoints(dataFP$TOTP_avg ~ dataFP$FPcover_max)

# does not return any breakpoints 
breakpoints(dataFP$FPcover_max ~ dataFP$TOTP_avg)
summary(breakpoints(dataFP$FPcover_max ~ dataFP$TOTP_avg))

breakpoints(dataFP$FPcover_max ~ log(dataFP$TOTP_avg))
summary(breakpoints(dataFP$FPcover_max ~ log(dataFP$TOTP_avg)))

breakpoints(dataFP$FPcover_max ~ dataFP$TOTN_avg)
breakpoints(dataFP$FPcover_max ~ log(dataFP$TOTN_avg))

breakpoints(dataFP$FPcover_max ~ dataFP$surfacearea_ha)
breakpoints(dataFP$FPcover_max ~ log(dataFP$surfacearea_ha))


############################################################################################### 
# Package: changepoint 
############################################################################################### 

##################### 
# try it for total P 
#####################
dataBREAK_P <- dataFP[,c("TOTP_avg","FPcover_max")]
dataBREAK_P <- na.omit(dataBREAK_P) # remove cases with missing values
dataBREAK_P <- dataBREAK_P[order(dataBREAK_P$TOTP_avg),] # order the dataframe by TOTP (lowest to highest)
totP <- dataBREAK_P[,"FPcover_max"] # a vector of only FPcover_max - ordered by TOTP (lowest to highest)
ansmeantotP <- cpt.mean(totP)
jpeg("FPcover_max - TOT P - change point.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(ansmeantotP,cpt.col='blue',ylab="maximum FP cover", xlab="TOTAL P (ordered values)")
dev.off()
print(ansmeantotP)
# this might be good! - I cannot find any change points in FPcover_max ~ TOTP_avg

##########################
# now, try it for total N
##########################

dataBREAK_N <- dataFP[,c("TOTN_avg","FPcover_max")]
dataBREAK_N <- na.omit(dataBREAK_N) # remove cases with missing values
dataBREAK_N <- dataBREAK_N[order(dataBREAK_N$TOTN_avg),] # order the dataframe by TOTN (lowest to highest)
totN <- dataBREAK_N[,"FPcover_max"] # a vector of only FPcover_max - ordered by TOTN (lowest to highest)
ansmeantotN <- cpt.mean(totN)
jpeg("FPcover_max - TOT N - change point.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(ansmeantotN,cpt.col='blue',ylab="maximum FP cover", xlab="TOTAL N (ordered values)")
dev.off()
print(ansmeantotN)
# Again, no change points found in FPcover_max ~ TOTN_avg

##############################
# now, try it for surface area
##############################

dataBREAK_SA <- dataFP[,c("surfacearea_ha","FPcover_max")]
dataBREAK_SA <- na.omit(dataBREAK_SA) # remove cases with missing values
dataBREAK_SA <- dataBREAK_SA[order(dataBREAK_SA$surfacearea_ha),] # order the dataframe by surfacea area (lowest to highest)
SA <- dataBREAK_SA[,"FPcover_max"] # a vector of only FPcover_max - ordered by surfacea area (lowest to highest)
ansmeanSA <- cpt.mean(SA)
jpeg("FPcover_max - surface area - change point.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(ansmeanSA,cpt.col='blue',ylab="maximum FP cover", xlab="Surface area (ordered values)")
dev.off()
print(ansmeanSA)

cpt.mean(SA,penalty=AIC)


# Cool! this seems right
# Maximum no. of cpts   : 1  # Changepoint Locations : 18 
# location 18: surface area = 0.4 ha, FPcover_max = 1.0
# location 19: surface area = 0.4 ha, FPcover_max = 0.0288 

############################################################################################### 
# Package: bcp
###############################################################################################

##############################
# try it for total P
##############################
plot.bcp(bcp(totP))
# only gives one change point 

##############################
# try it for total N
##############################
plot.bcp(bcp(totN))
# only gives one change point 

##############################
# try it for surface area
##############################
plot.bcp(bcp(SA))
# gives a lot of differnt change points with pretty high posterior probabilities 

############################################################################################################################## 
# SIGNIFICANT ZERO CROSSINGS - Package: SiZer
##############################################################################################################################
library(SiZer)

# All Ponds #
# subset data so it is only complete cases 
dataBREAK_P_all <- dataONEperpond[,c("TOTP_avg","FPcover_max")]
dataBREAK_P_all <- na.omit(dataBREAK_P_all) # remove cases with missing values

SiZer(x = dataBREAK_P_all$TOTP_avg, y = dataBREAK_P_all$FPcover_max)
plot.SiZer(SiZer(x = dataBREAK_P_all$TOTP_avg, y = dataBREAK_P_all$FPcover_max))


# FP Ponds #
# subset data so it is only complete cases 
dataBREAK_P <- dataFP[,c("TOTP_avg","FPcover_max")]
dataBREAK_P <- na.omit(dataBREAK_P) # remove cases with missing values

SiZer(x = dataBREAK_P$TOTP_avg, y = dataBREAK_P$FPcover_max)
plot.SiZer(SiZer(x = dataBREAK_P$TOTP_avg, y = dataBREAK_P$FPcover_max))

