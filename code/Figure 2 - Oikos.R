######################################################################
# Manuscript I: Appendix B                                           #
#                                                                    #
# Floating plant richness in all surveyed waterbodies                #
######################################################################

# convert FP_species_richness to a factor 
dataONEperpond$FP_species_richness <- as.factor(dataONEperpond$FP_species_richness)
appendixB <- ggplot(data=dataONEperpond, aes(x=FP_species_richness)) + geom_bar(width=0.9, position = position_dodge(width=1)) # plot 
appendixB <- appendixB + xlab("Floating Plant Species Richness") + ylab("Frequency")
appendixB <- appendixB + theme_classic(base_size=12)
appendixB
ggsave(file="histogram - FP species richness - all ponds.jpg", appendixB, height=8,width=8,units="cm",dpi=1200)
