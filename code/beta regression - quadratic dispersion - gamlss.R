############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA #
# Beta regression                          #
# With package gamlss                      #
#                                          #
# Created by MJM 4/10/2014                 #
############################################
library(gamlss)

# Get rid of any 0s and 1s in the dependent (Y) variable
# Transformation suggested by: Smithson M, Verkuilen J (2006). Psychological Methods, 11(1), 54–71
# IMPORTANT: Re-import these data sets when you are done with beta regression analysis 
dataONEperpond$FPcover_max[dataONEperpond$FPcover_max == 1] <- (1*(length(dataONEperpond$FPcover_max)-1)+0.5)/(length(dataONEperpond$FPcover_max))
dataONEperpond$FPcover_max[dataONEperpond$FPcover_max == 0] <- (0*(length(dataONEperpond$FPcover_max)-1)+0.5)/(length(dataONEperpond$FPcover_max))

dataFP$FPcover_max[dataFP$FPcover_max == 1] <- (1*(length(dataFP$FPcover_max)-1)+0.5)/(length(dataFP$FPcover_max))
dataFP$FPcover_max[dataFP$FPcover_max == 0] <- (0*(length(dataFP$FPcover_max)-1)+0.5)/(length(dataFP$FPcover_max))

dataFPsmall$FPcover_max[dataFPsmall$FPcover_max == 1] <- (1*(length(dataFPsmall$FPcover_max)-1)+0.5)/(length(dataFPsmall$FPcover_max))
dataFPsmall$FPcover_max[dataFPsmall$FPcover_max == 0] <- (0*(length(dataFPsmall$FPcover_max)-1)+0.5)/(length(dataFPsmall$FPcover_max))

dataONEperpondoutliers$FPcover_max[dataONEperpondoutliers$FPcover_max == 1] <- (1*(length(dataONEperpondoutliers$FPcover_max)-1)+0.5)/(length(dataONEperpondoutliers$FPcover_max))
dataONEperpondoutliers$FPcover_max[dataONEperpondoutliers$FPcover_max == 0] <- (0*(length(dataONEperpondoutliers$FPcover_max)-1)+0.5)/(length(dataONEperpondoutliers$FPcover_max))

dataFPoutliers$FPcover_max[dataFPoutliers$FPcover_max == 1] <- (1*(length(dataFPoutliers$FPcover_max)-1)+0.5)/(length(dataFPoutliers$FPcover_max))
dataFPoutliers$FPcover_max[dataFPoutliers$FPcover_max == 0] <- (0*(length(dataFPoutliers$FPcover_max)-1)+0.5)/(length(dataFPoutliers$FPcover_max))

dataFPoutlierssmall$FPcover_max[dataFPoutlierssmall$FPcover_max == 1] <- (1*(length(dataFPoutlierssmall$FPcover_max)-1)+0.5)/(length(dataFPoutlierssmall$FPcover_max))
dataFPoutlierssmall$FPcover_max[dataFPoutlierssmall$FPcover_max == 0] <- (0*(length(dataFPoutlierssmall$FPcover_max)-1)+0.5)/(length(dataFPoutlierssmall$FPcover_max))


# Plot the different data sets
# Use these later for adding fitted models 
plot(dataONEperpond$FPcover_max ~ dataONEperpond$TOTP_avg,main="dataONEperpond",xlab="Total P (mg/L)",ylab="FP cover",log="x")
plot(dataONEperpondoutliers$FPcover_max ~ dataONEperpondoutliers$TOTP_avg,main="dataONEperpondoutliers",xlab="Total P (mg/L)",ylab="FP cover",log="x")
plot(dataFP$FPcover_max ~ dataFP$TOTP_avg,main="dataFP",xlab="Total P (mg/L)",ylab="FP cover",log="x")
plot(dataFPsmall$FPcover_max ~ dataFPsmall$TOTP_avg,main="dataFPsmall",xlab="Total P (mg/L)",ylab="FP cover",log="x")
plot(dataFPoutliers$FPcover_max ~ dataFPoutliers$TOTP_avg,main="dataFPoutliers",xlab="Total P (mg/L)",ylab="FP cover",log="x")
plot(dataFPoutlierssmall$FPcover_max ~ dataFPoutlierssmall$TOTP_avg,main="dataFPoutlierssmall",xlab="Total P (mg/L)",ylab="FP cover",log="x")

##########################################
# Parameters of gamlss beta distribution #
# mu & sigma != mu & phi of betareg      #
# Sigma is (0,1) - unlike phi            # 
##########################################
# Try 
hist(rBE(100, mu=.3, sigma=.001))
hist(rBE(100, mu=.3, sigma=.9))

# or 
plot(function(y) dBE(y, mu=.1 ,sigma=.5), 0.001, .999)
plot(function(y) dBE(y, mu=.5 ,sigma=.5), 0.001, .999)
plot(function(y) dBE(y, mu=.5 ,sigma=.2), 0.001, .999)

####################### 
# Beta regression     #
# dataONEperpond      #
# link: logit         #
# Variable dispersion #
# quadratic dispersion#
#######################
head(dataONEperpond)

# re-format data so it has no NAs 
dataONEperpond_gamlss <- cbind(dataONEperpond$FPcover_max,dataONEperpond$TOTP_avg)
dataONEperpond_gamlss <- as.data.frame(dataONEperpond_gamlss) # convert to a data frame 
dataONEperpond_gamlss <- dataONEperpond_gamlss[complete.cases(dataONEperpond_gamlss),]# remove any NAs
names(dataONEperpond_gamlss) <- c("FPcover_max","TOTP_avg")
head(dataONEperpond_gamlss)
dataONEperpond_gamlss



gamlss(FPcover_max ~ TOTP_avg, sigma.formula = ~ vc(TOTP_avg), data=dataONEperpond_gamlss, family=BE)
# Error in eval(expr, envir, enclos) : could not find function "vc"

gamlss(FPcover_max ~ TOTP_avg, sigma.formula = ~ cs(TOTP_avg), data=dataONEperpond_gamlss, family=BE)
# Error in RS() : The global deviance is increasing 

gamlss(FPcover_max ~ TOTP_avg, sigma.formula = fp(FPcover_maxx ~ TOTP_avg), data=dataONEperpond_gamlss, family=BE)
# Error in array(x, c(length(x), 1L), if (!is.null(names(x))) list(names(x),  : invalid first argument
                                                                   
gamlss(FPcover_max ~ TOTP_avg, sigma.formula = ~ fp(TOTP_avg), data=dataONEperpond_gamlss, family=BE)
# Error in gamlss.fp(data[["fp(TOTP_avg)"]], z, w, 2) : object 'powerbest' not found

gamlss(FPcover_max ~ TOTP_avg, sigma.formula = ~ poly(TOTP_avg,2), data=dataONEperpond_gamlss, family=BE(mu.link="identity",sigma.link="identity"))
# Error in dBE(y, mu, sigma, log = TRUE) : mu must be between 0 and 1 

gamlss(FPcover_max ~ TOTP_avg, sigma.formula = TOTP_avg ~ poly(TOTP_avg,2), data=dataONEperpond_gamlss, family=BE)
#Error in RS() : The global deviance is increasing 
# Try different steps for the parameters or the model maybe inappropriate
# In addition: Warning message:
# In glim.fit(f = sigma.object, X = sigma.X, y = y, w = w, fv = sigma,  :
# The deviance has increased in an inner iteration for sigma
# If persist, try different steps or model maybe inappropriate

gamlss(FPcover_max ~ TOTP_avg, sigma.formula = ~ poly(TOTP_avg,2), data=dataONEperpond_gamlss, family=BE(mu.link="log",sigma.link="log"))
# Error in dBE(y, mu, sigma, log = TRUE) : mu must be between 0 and 1 

gamlss(FPcover_max ~ TOTP_avg, sigma.formula = ~ poly(TOTP_avg,2), data=dataONEperpond_gamlss, family=BE(mu.link="logit",sigma.link="logit"))
# Error in RS() : The global deviance is increasing 
# Try different steps for the parameters or the model maybe inappropriate
# In addition: Warning message:
# In glim.fit(f = sigma.object, X = sigma.X, y = y, w = w, fv = sigma,  :
# The deviance has increased in an inner iteration for sigma
# If persist, try different steps or model maybe inappropriate

gamlss(FPcover_max ~ TOTP_avg, sigma.formula = ~ poly(TOTP_avg,2), data=dataONEperpond_gamlss, family=BE)
# Error in RS() : The global deviance is increasing 
# Try different steps for the parameters or the model maybe inappropriate

gamlss(FPcover_max ~ TOTP_avg, sigma.formula = ~ pb(TOTP_avg), data=dataONEperpond_gamlss, family=BE)
# Error in RS() : The global deviance is increasing 
# Try different steps for the parameters or the model maybe inappropriate

########################## 
# Beta regression        #
# dataONEperpondoutliers #
# link: logit            #
# Variable dispersion    #
# quadratic dispersion   #
##########################

########################## 
# Beta regression        #
# dataFP                 #
# link: logit            #
# Variable dispersion    #
# quadratic dispersion   #
##########################

########################## 
# Beta regression        #
# dataFPoutliers         #
# link: logit            #
# Variable dispersion    #
# quadratic dispersion   #
##########################

########################## 
# Beta regression        #
# dataFPsmall            #
# link: logit            #
# Variable dispersion    #
# quadratic dispersion   #
##########################

########################## 
# Beta regression        #
# dataFPsmalloutliers    #
# link: logit            #
# Variable dispersion    #
# quadratic dispersion   #
##########################