############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA #
# Partitioned logistic regression          #
# Data: dataFPsmall (n=54)                 #
# Waterbodies <5 ha with FP present        #
# MJM 2/25/2014                            #
############################################

library(ggplot2)

# Add new variables to the data frame just for plotting the partitioned data in ggplot2
# partition data: FPcover_max >= 0.5 and FPcover_max < 0.5 
dataFPsmall$half <- ifelse(dataFPsmall$FPcover_max >= 0.5, "greater", "lesser")
# partition data: FPcover_max >= 0.6666 and FPcover_max < 0.6666 
dataFPsmall$third <- ifelse(dataFPsmall$FPcover_max >= 0.6666, "greater", "lesser")

# partitioned 50:50 y-variable 
glmFPsmallupper50 <- glm(FPcover_max ~ TOTP_avg, family = binomial, data = subset(dataFPsmall, FPcover_max >= 0.50)) 
glmFPsmalllower50 <- glm(FPcover_max ~ TOTP_avg, family = binomial, data = subset(dataFPsmall, FPcover_max < 0.50)) 
summary(glmFPsmallupper50)
summary(glmFPsmalllower50)
ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() + stat_smooth(method=glm, family=binomial, aes(fill=factor(half)))

# partitioned 33:67 y-variable 
glmFPsmallupperthird <- glm(FPcover_max ~ TOTP_avg, family = binomial, data = subset(dataFPsmall, FPcover_max >= 0.6666)) 
glmFPsmalllowerthird <- glm(FPcover_max ~ TOTP_avg, family = binomial, data = subset(dataFPsmall, FPcover_max < 0.6666)) 
summary(glmFPsmallupperthird)
summary(glmFPsmalllowerthird)
ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() + stat_smooth(method=glm, family=binomial, aes(fill=factor(third)))