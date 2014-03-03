######################################################################
# Manuscript I: Appendix E                                           #
#                                                                    #
# Sensitivity of results to different estimates of FP cover          # 
######################################################################

# number of waterbodies (dataONEperpond) with FP cover> 66.666%
nrow(subset(dataONEperpond, dataONEperpond$FPcover_low_est >= 0.66666)) # 12 

# total P ~ FP cover 
# without 8 waterbodies that had a shift between FP and SAV regimes 
appendixE <- ggplot(data=dataFPsmall, aes(x=TOTP_avg,y=FPcover_low_est)) + geom_point() 
appendixE <- appendixE + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
appendixE <- appendixE + scale_x_log10() 
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
appendixE <- appendixE + scale_y_continuous(breaks=y_breaks,labels=y_labels)
appendixE 


# generate b1 - b3 

# table 1 of fits 