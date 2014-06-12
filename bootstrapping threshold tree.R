########################
# Detecting Threshold  #
# Bootstrap est. of CI #  
########################
# load your packages 
library(tree)
library(boot)
library(stringr)

# example found here:
# http://www.statmethods.net/advstats/bootstrapping.html

# define function to obtain threshold 
threshold <- function(formula,data,i){
  d <- data[i,] # allows boot to select sample
  tree <- tree(formula , data=d) # fit the regression tree 
  a <- tree$frame[1,5][1] # pull out the first split in the data 
  b <- str_replace_all(a,"<","") # remove equality signs
  c <- as.numeric(b) # convert to a number
  return(c)
}

###########################
# FP cover ~ Surface area 
###########################
# bootstrap
results <- boot(data=dataFP, statistic=threshold,R=1000,formula=FPcover_max~surfacearea_ha)
# view results 
results
summary(results)
# get 95% CI 
boot.ci(results, type="all")
boot.ci(results, type="bca")


###########################
# FP cover ~ Total P 
###########################
# bootstrap
results_TOTP <- boot(data=dataFP, statistic=threshold,R=1000,formula=FPcover_max~TOTP_avg)
# view results 
results_TOTP
summary(results_TOTP)
# get 95% CI 
boot.ci(results_TOTP, type="all")
boot.ci(results_TOTP, type="bca")
