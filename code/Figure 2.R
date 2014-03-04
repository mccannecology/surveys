#########################################################
# Manuscript I: Figure 2                                #
# histogram of floating plant cover                     #
# small (<5ha) ponds with floating plants present       #
#########################################################

# Set working directory 
setwd("C:/Users/Mike/Desktop/Dropbox/CT & LI  Duckweed Surveys/Survey Analysis All") 

library(ggplot2)

# make a new variable on the data frame - convert proportion to precentage FP cover 
dataFPsmall$FPcover_max_percent <- dataFPsmall$FPcover_max*100 

Fig02 <- ggplot(dataFPsmall,aes(FPcover_max_percent)) + stat_bin(binwidth=20,right=TRUE,col="black")

Fig02 <- Fig02 + ylab("Frequency") + xlab("Floating plant cover (%)")

Fig02 <- Fig02 + scale_x_continuous(limits=c(0,101),breaks=c(0,20,40,60,80,100),expand=c(0,0)) + scale_y_continuous(limits=c(0,25),expand=c(0,0))

Fig02 <- Fig02 + theme_classic(base_size=18)

Fig02




#Fig02 <- Fig02 + theme_bw()
#Fig02 <- Fig02 + theme_minimal()

# save your plot in the working directory 
ggsave(file="Figure 2 - histogram - FP cover max - small FP ponds.jpg")
