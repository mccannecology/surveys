######################################################################
# Manuscript I: Appendix C                                           #
#                                                                    #
# Scatterplot of waterbody size and floating plant cover             #
######################################################################

# make a new variable on the data frame - convert proportion to precentage FP cover 
dataFP$FPcover_max_percent <- dataFP$FPcover_max*100 

appendixC <-ggplot(data=dataFP, aes(x=surfacearea_ha,y=FPcover_max_percent)) + geom_point(size=3) 

appendixC <- appendixC + scale_x_log10()

appendixC <- appendixC + geom_vline(xintercept = 5,colour="red",size=1,linetype="longdash") 

appendixC <- appendixC + ylab("Floating plant cover (%)") + xlab("Surface area (ha)")

appendixC <- appendixC + theme_classic(base_size=18)

appendixC

ggsave(file="scatterplot - log surface area ~ FP cover max - FP ponds - line at 5ha.jpg")
