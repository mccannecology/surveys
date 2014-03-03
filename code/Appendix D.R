######################################################################
# Manuscript I: Appendix D                                           #
#                                                                    #
# Comparison of linear and logistic regression                       #
# Total P and floating plant cover                                   # 
######################################################################

# without Total P values >0.5 mg/L 
appendixD <- ggplot(dataFPoutlierssmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() 

appendixD <- appendixD + stat_smooth(method="glm", family=binomial,col="red", se=F) 

appendixD <- appendixD + stat_smooth(method="lm",col="blue", se=F)

appendixD <- appendixD + xlab("Total phosphorus (mg/L)") + ylab("Floating plant cover(%)")

appendixD <- appendixD + ylim(0,1) 

y_breaks <- seq(0,1,0.25)

y_labels <- as.character(y_breaks*100)

appendixD <- appendixD + scale_y_continuous(breaks=y_breaks,labels=y_labels)

appendixD
