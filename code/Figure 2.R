#########################################################
# Manuscript I: Figure 2                                #
# histogram of floating plant cover                     #
# small (<5ha) ponds with floating plants present       #
#########################################################

# Set working directory 
setwd("C:/Users/Mike/Desktop/Dropbox/CT & LI  Duckweed Surveys/Survey Analysis All") 

library(ggplot2)

###############
#             #
#   dataFP    #
#             #
###############
# make a new variable on the data frame - convert proportion to precentage FP cover 
dataFP$FPcover_max_percent <- dataFP$FPcover_max*100 

Fig02a <- ggplot(dataFP,aes(FPcover_max_percent)) + stat_bin(binwidth=20,right=TRUE,col="black")
Fig02a <- Fig02a + ylab("Frequency") + xlab("Floating plant cover (%)")
Fig02a <- Fig02a + scale_x_continuous(limits=c(0,101),breaks=c(0,20,40,60,80,100),expand=c(0,0)) + scale_y_continuous(limits=c(0,76),expand=c(0,0))
Fig02a <- Fig02a + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank()) # remove x-axis values & ticks
Fig02a <- Fig02a + geom_text(aes(x=4,y=74,label="a)"),size=7) # add pane label
Fig02a <- Fig02a + theme_classic(base_size=18)
Fig02a

###############
#             #
# dataFPsmall #
#             #
###############
# make a new variable on the data frame - convert proportion to precentage FP cover 
dataFPsmall$FPcover_max_percent <- dataFPsmall$FPcover_max*100 

Fig02b <- ggplot(dataFPsmall,aes(FPcover_max_percent)) + stat_bin(binwidth=20,right=TRUE,col="black")
Fig02b <- Fig02b + ylab("Frequency") + xlab("Floating plant cover (%)")
Fig02b <- Fig02b + scale_x_continuous(limits=c(0,101),breaks=c(0,20,40,60,80,100),expand=c(0,0)) + scale_y_continuous(limits=c(0,27),expand=c(0,0))
Fig02b <- Fig02b + geom_text(aes(x=4,y=26,label="b)"),size=7) # add pane label
Fig02b <- Fig02b + theme_classic(base_size=18)
Fig02b

###################
# ARRANGING PLOTS #
###################
Fig02 <- arrangeGrob(Fig02a,Fig02b,ncol=1,nrow=2) #grid.arrange does not work with ggsave()
Fig02
ggsave(file="Figure 02.pdf", Fig02, height=11,width=8)
ggsave(file="Figure 02.png", Fig02, height=11,width=8)
ggsave(file="Figure 02.jpg", Fig02, height=11,width=8)



