#########################################################
# Manuscript I: Figure 2                                #
# histogram of floating plant cover                     #
# small (<5ha) ponds with floating plants present       #
#########################################################

# Set working directory 
setwd("C:/Users/Mike/Desktop/Dropbox/CT & LI  Duckweed Surveys/Survey Analysis All") 

library(ggplot2)
library(gridExtra)

###############
#             #
# dataFPsmall #
#             #
###############
# make a new variable on the data frame - convert proportion to precentage FP cover 
dataFPsmall$FPcover_max_percent <- dataFPsmall$FPcover_max*100 

Fig02append <- ggplot(dataFPsmall,aes(FPcover_max_percent)) + stat_bin(binwidth=20,right=TRUE,col="black")
Fig02append <- Fig02append + ylab("Frequency") + xlab("Floating plant cover (%)")
Fig02append <- Fig02append + scale_x_continuous(limits=c(0,101),breaks=c(0,20,40,60,80,100),expand=c(0,0)) + scale_y_continuous(limits=c(0,27),expand=c(0,0))
Fig02append <- Fig02append + theme_classic(base_size=18)
Fig02append

################
# SAVING PLOTS #
################
ggsave(file="Fig02appendix.pdf", Fig02append, height=5,width=5)
ggsave(file="Fig02appendix.png", Fig02append, height=5,width=5)
ggsave(file="Fig02appendix.jpg", Fig02append, height=5,width=5)



