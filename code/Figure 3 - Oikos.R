######################################################################
# Manuscript I: Appendix C                                           #
#                                                                    #
# Scatterplot of waterbody size and floating plant cover             #
######################################################################
library(ggplot2)

# make a new variable on the data frame - convert proportion to precentage FP cover 
dataFP$FPcover_max_percent <- dataFP$FPcover_max*100 

appendixC <-ggplot(data=dataFP, aes(x=surfacearea_ha,y=FPcover_max_percent)) 
appendixC <- appendixC + geom_point() 
appendixC <- appendixC + scale_x_log10()
appendixC <- appendixC + geom_vline(xintercept = 0.425,colour="black",linetype="longdash") 
appendixC <- appendixC + geom_vline(xintercept = 4.095,colour="black",linetype="longdash") 
appendixC <- appendixC + geom_vline(xintercept = 1.74,colour="black",linetype="solid") 
appendixC <- appendixC + ylab("Floating plant cover (%)") + xlab("Surface area (ha)")
appendixC <- appendixC + theme_classic(base_size=12)
appendixC

ggsave(file="scatterplot - log surface area ~ FP cover max - FP ponds - threshold&CI.jpg", appendixC, height=8,width=8,units="cm",dpi=1200)
