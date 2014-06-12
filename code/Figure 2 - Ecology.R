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
#   dataFP    #
#             #
###############
# make a new variable on the data frame - convert proportion to precentage FP cover 
dataFP$FPcover_max_percent <- dataFP$FPcover_max*100 

Fig02 <- ggplot(dataFP,aes(FPcover_max_percent)) + stat_bin(binwidth=20,right=TRUE,col="black")
Fig02 <- Fig02 + ylab("Frequency") + xlab("Floating plant cover (%)")
Fig02 <- Fig02 + scale_x_continuous(limits=c(0,101),breaks=c(0,20,40,60,80,100),expand=c(0,0)) + scale_y_continuous(limits=c(0,72),expand=c(0,0))
Fig02 <- Fig02 + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank()) # remove x-axis values & ticks
Fig02 <- Fig02 + theme_classic(base_size=18)
Fig02


# Save the plots 
ggsave(file="Figure 02.pdf", Fig02, height=5,width=5)
ggsave(file="Figure 02.png", Fig02, height=5,width=5)
ggsave(file="Figure 02.jpg", Fig02, height=5,width=5)




# Figures for JASM 2014 talk 
JASMfig_FP <- ggplot(dataFP,aes(FPcover_max_percent)) 
JASMfig_FP <- JASMfig_FP + stat_bin(binwidth=20,right=TRUE,col="black",colour="white")
JASMfig_FP <- JASMfig_FP + stat_density(colour="blue",fill=NA)
JASMfig_FP <- JASMfig_FP + ylab("Frequency") 
JASMfig_FP <- JASMfig_FP + xlab("Floating plant cover (%)")
JASMfig_FP <- JASMfig_FP + scale_x_continuous(limits=c(0,101),breaks=c(0,20,40,60,80,100),expand=c(0,0)) 
JASMfig_FP <- JASMfig_FP + scale_y_continuous(limits=c(0,72),expand=c(0,0))
JASMfig_FP <- JASMfig_FP + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank()) # remove x-axis values & ticks
JASMfig_FP <- JASMfig_FP + theme_black()
JASMfig_FP

ggsave(file="JASMfig - FP ponds.jpg", JASMfig_FP, height=8,width=8)


