##############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA   #
# Bimodality tests                           #
# UPDATED: 02/24/2014 by MJ McCann           #
##############################################

###############################################################################################
# LOAD SOME PACKAGES 
###############################################################################################
# plotting 
library(ggplot2) 

# bimodality 
library(diptest) # Hartigan's dip test for bimodalit y
library(bimodalitytest) # Implements a Likelihoodratiotest to test whether a normal mixture is bimodal - does not work with my data
library(mixtools) # A collection of R functions for analyzing ???nite mixture models.
library(ADGofTest) # Anderson-Darling test - whether a sample comes from a given probability distribution
library(mclust) # Normal Mixture Modeling for Model-Based Clustering, Classification, and Density Estimation

# non-parametric threshold detection 
library(ecp) # multivariate changepoint  -- really only 2 options in this - "divisive" or "agglomerative" "hierarchical estimation algorithm"
library(changepoint) # univariate time series -- probably want to use cpt.mean() -- 
library(cpm) # univariate time series - may only work for time series 
library(strucchange)
library(bcp) # Bayesian analysis of change point 

# hybrid approach 
library(SiZer)

###############################################################################################
# HARTIGAN'S DIP TEST STATISTIC FOR UNIMODALITY 
###############################################################################################
# http://en.wikipedia.org/wiki/Bimodal_distribution
# The values for the dip statistic values range between 0 to 1. (Source: Wiki)
# Values less than 0.05 indicating significant bimodality (Source: Wiki) #MJM note: not sure if they're refering to p-values here
# Values greater than 0.05 but less than 0.10 suggesting bimodality with marginal significance. (Source: Wiki)
# Can also get p-values for the dip statistic (package: diptest)

dip.test(dataONEperpond$FPcover_max) # Compute Hartigans' dip statistic Dn, and  P-value for  test for unimodality, by interpolating tabulated quantiles of sqrt(nDn)
# D = 0.0348, p-value = 0.08394
# alternative hypothesis: non-unimodal, i.e., at least bimodal

dip.test(dataFP$FPcover_max) # Compute Hartigans' dip statistic Dn, and  P-value for  test for unimodality, by interpolating tabulated quantiles of sqrt(nDn)
# D = 0.0707, p-value = 0.0006063
# alternative hypothesis: non-unimodal, i.e., at least bimodal

dip.test(dataFPsmall$FPcover_max) # Compute Hartigans' dip statistic Dn, and  P-value for  test for unimodality, by interpolating tabulated quantiles of sqrt(nDn)
# D = 0.1296, p-value < 2.2e-16
# alternative hypothesis: non-unimodal, i.e., at least bimodal

###############################################################################################
# K-MEANS CLUSTERING - see Hellwig et al. 2010 - for the meaing of the statistics in here
###############################################################################################
# dataONEperond - all waterbodies 
cl.res01 <- kmeans(dataONEperpond$FPcover_max, 2, centers=c(min(dataONEperpond$FPcover_max), max(dataONEperpond$FPcover_max)))
n01 <- length(dataONEperpond$FPcover_max)
# variance reduction score = within cluster SS / total sum of squares
# value ranges 0 to 1; low score "indicates an informative split"
vrs01 <- (sum(cl.res01$withinss) / (n01-1)) / var(dataONEperpond$FPcover_max) # 0.08085627
# weighted variance reduction score
# variance reduction independent of cluster sizes
# vale ranges 0 or greater; "low score reflects bimodality"
wvrs01 <- (sum(cl.res01$withinss / cl.res01$size) / 2) / ( ((n01-1)/n01) * var(dataONEperpond$FPcover_max)) # 0.1740212
groups01 <- cl.res01$cluster

# dataFP - waterbodies with floating plants 
cl.res02 <- kmeans(dataFP$FPcover_max, 2, centers=c(min(dataFP$FPcover_max), max(dataFP$FPcover_max)))
n02 <- length(dataFP$FPcover_max)
# variance reduction score = within cluster SS / total sum of squares
# value ranges 0 to 1; low score "indicates an informative split"
vrs02 <- (sum(cl.res02$withinss) / (n02-1)) / var(dataFP$FPcover_max) # 0.09008887
# weighted variance reduction score
# variance reduction independent of cluster sizes
# vale ranges 0 or greater; "low score reflects bimodality"
wvrs02 <- (sum(cl.res02$withinss / cl.res02$size) / 2) / ( ((n02-1)/n02) * var(dataFP$FPcover_max)) # 0.1194556
groups02 <- cl.res02$cluster

# dataFPsmall - waterbodies with floating plants 
cl.res03 <- kmeans(dataFPsmall$FPcover_max, 2, centers=c(min(dataFPsmall$FPcover_max), max(dataFPsmall$FPcover_max)))
n03 <- length(dataFPsmall$FPcover_max)
# variance reduction score = within cluster SS / total sum of squares
# value ranges 0 to 1; low score "indicates an informative split"
vrs03 <- (sum(cl.res03$withinss) / (n03-1)) / var(dataFPsmall$FPcover_max) # 0.1099593
# weighted variance reduction score
# variance reduction independent of cluster sizes
# vale ranges 0 or greater; "low score reflects bimodality"
wvrs03 <- (sum(cl.res03$withinss / cl.res03$size) / 2) / ( ((n03-1)/n03) * var(dataFPsmall$FPcover_max)) # 0.1082916
groups03 <- cl.res03$cluster

vrs03
wvrs03

###############################################################################################
# K-MEANS CLUSTERING - determining optimal number of clusters 
###############################################################################################
# http://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters
# http://www.statmethods.net/advstats/cluster.html
# http://www.mattpeeples.net/kmeans.html
# Assess optimal cluster number by looking at a scree plot 

#############
# All ponds #
#############
mydata <- dataONEperpond$FPcover_max
# within cluster sum of squares
wss <- sum((mydata - mean(mydata))^2) # within cluster sum of squares
# get the WSS for each cluster size 
for (i in 2:15) wss[i] <- sum(kmeans(mydata, centers=i)$withinss) 
# plot the WSS for each # of clusters
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 
# looks like a within cluster sum of squares (WSS) reaches its minimum at k=3 or 4 clusters 

############
# FP ponds #
############
mydata02  <- dataFP$FPcover_max
# within cluster sum of squares
wss <- sum((mydata02 - mean(mydata02))^2) 
# get the WSS for each cluster size 
for (i in 2:15) wss[i] <- sum(kmeans(mydata02, centers=i)$withinss) 
# plot the WSS for each # of clusters
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")  
# looks like a within cluster sum of squares (WSS) reaches its minimum at k=3 or 4 clusters 


###############################################################################################
# ANDERSON-DARLING TEST: Whether a sample comes from a given probability distribution 
###############################################################################################
# if p > 0.5, then fail to reject the null hypothsis that the sample cames from the given probability distribution 
ad.test(dataONEperpond$FPcover_max, pnorm)$statistic
# AD = 55.31475
ad.test(dataONEperpond$FPcover_max, pnorm)$p.value
# p = 2.985075e-06
# not normal

ad.test(dataONEperpond$FPcover_max, punif)$statistic
# AD = Inf
ad.test(dataONEperpond$FPcover_max, punif)$p.value
# p = 2.985075e-06 
# not uniform

ad.test(dataFP$FPcover_max, pnorm)$statistic
# AD = 21.07342
ad.test(dataFP$FPcover_max, pnorm)$p.value
# p = 6.185567e-06
# not normal

ad.test(dataFP$FPcover_max, punif)$statistic 
# AD = Inf 
ad.test(dataFP$FPcover_max, punif)$p.value
# p = 6.185567e-06
# not uniform

###############################################################################################
# BIMODALITY TEST: R package bimodalitytest 
###############################################################################################
# This function performs the likelihood ratio test for a given dataset
# Tests the null hypothesis whether a two componente normal mixture is bimodal
# Therefore it calculates the maximum-likelihood estimators for the restricted 
# and non restricted parameter space and returns for example the
# likelihoodratio and the p-value.

mydata <- dataONEperpond$FPcover_max
bimodality.test(mydata)
# Error in log(Sumry$n) : non-numeric argument to mathematical function 

bimodality.test(dataONEperpond$FPcover_max)
# Error in log(Sumry$n) : non-numeric argument to mathematical function

bimodality.test(dataONEperpond$FPcover_max, move_data=T)
# Error in log(Sumry$n) : non-numeric argument to mathematical function

dataNEW <- dataONEperpond$FPcover_max
class(dataNEW)
summary(dataNEW)
length(dataNEW)

bimodality.test(dataNEW, start_vec = NA, equal_sigmas = F, N=10)
#  Error in log(Sumry$n) : non-numeric argument to mathematical function

###############################################################################################
# MODEL-BASED CLUSTERING: R package Mclust 
###############################################################################################
# http://www.stat.washington.edu/research/reports/2012/tr597.pdf

# All ponds #
ONE.clust <- Mclust(dataONEperpond$FPcover_max)
summary(ONE.clust, parameters = T)
plot(ONE.clust)
# best model has 4 components
# I guess this agrees with k-means scree plot

# FP ponds #
FP.clust <- Mclust(dataFP$FPcover_max)
summary(FP.clust, parameters = T)
plot(FP.clust)
# best model has 4 components
# I guess this agrees with k-means scree plot

###############################################################################################
# Bimodality index 
###############################################################################################
# Hellwig et al. 2010 BMC Bioinformatics 
# Wang et al. 2009 recommend BI = 1.1 as a cutoff to select bimodally distributed genes 

# All ponds #
ONE.clustTWO <- Mclust(dataONEperpond$FPcover_max, G=2)
summary(ONE.clustTWO,parameters = T)
# standardized distance between the two clusters - # need to calculate the means and standard deviations of each cluster 
d <- (0.8940272 - 0.0219641)/(sqrt(0.006800489)) # = 10.57494
# proportion of observations in first component - # not hard - can copy from summary(ONE.clustTWO)
p <- 178/201 # = 0.8855721
# bimodality index 
BI <- sqrt(p*(1-p))*d # = 3.366322 - NOT BIMODAL
  
# FP ponds #
FP.clustTWO <- Mclust(dataFP$FPcover_max, G=2)
summary(FP.clustTWO,parameters = T)
# VERY SUPRISING CLUSTER MEANS... 
# standardized distance between the two clusters - # need to calculate the means and standard deviations of each cluster 
d <- (0.448939203 - 0.003264964)/(sqrt(1.643690e-01)) # = 1.099278
# proportion of observations in first component - # not hard - can copy from summary(ONE.clustTWO)
p <- 43/97 # = 0.443299
# bimodality index 
BI <- sqrt(p*(1-p))*d # = 0.5460934 - BIMODAL 
