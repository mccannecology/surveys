######################################################################
# Manuscript I: Appendix E                                           #
#                                                                    #
# Comparison of linear and logistic regression                       #
# Total P and floating plant cover                                   # 
######################################################################

library(ggplot2)

# dataFPsmall - no outliers removed 
appendixE <- ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point(size=3) 

appendixE <- appendixE + stat_smooth(method="glm", family=binomial,col="red", se=F) 

appendixE <- appendixE + stat_smooth(method="lm",col="blue", se=F)

appendixE <- appendixE + xlab("Total phosphorus (mg/L)") + ylab("Floating plant cover(%)")

appendixE <- appendixE + ylim(0,1) 

y_breaks <- seq(0,1,0.25)

y_labels <- as.character(y_breaks*100)

appendixE <- appendixE + scale_y_continuous(breaks=y_breaks,labels=y_labels)

appendixE <- appendixE + scale_x_log10() 

appendixE <- appendixE + theme_classic(base_size=18)

appendixE


# without Total P values >0.5 mg/L 
appendixE <- ggplot(dataFPoutlierssmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() 

appendixE <- appendixE + stat_smooth(method="glm", family=binomial,col="red", se=F) 

appendixE <- appendixE + stat_smooth(method="lm",col="blue", se=F)

appendixE <- appendixE + xlab("Total phosphorus (mg/L)") + ylab("Floating plant cover(%)")

appendixE <- appendixE + ylim(0,1) 

y_breaks <- seq(0,1,0.25)

y_labels <- as.character(y_breaks*100)

appendixE <- appendixE + scale_y_continuous(breaks=y_breaks,labels=y_labels)

appendixE
