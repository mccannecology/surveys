#######################
# Segmented           #
# Beta regression     #
# dataONEperpond      #
# link: logit         #
# Variable dispersion #
#######################

# this analysis was not successfully completed - MJM 4/12/2014

breakpoint01 <- 0.01850
breakpoint02 <- 0.20850

formula <- FPcover_max ~ (TOTP_avg*(TOTP_avg<breakpoint01) + TOTP_avg*(TOTP_avg>=breakpoint01 & TOTP_avg<breakpoint02) + TOTP_avg*(TOTP_avg>=breakpoint02))|
  (TOTP_avg*(TOTP_avg<breakpoint01) + TOTP_avg*(TOTP_avg>=breakpoint01 & TOTP_avg<breakpoint02) + TOTP_avg*(TOTP_avg>=breakpoint02))
  
segmented_beta_dataONEperpond_logit_vardisp <- betareg(formula, data=dataONEperpond, link="logit")
# Error in optim(par = start, fn = loglikfun, gr = gradfun, method = method,: non-finite value supplied by optim

summary(segmented_beta_dataONEperpond_logit_vardisp) 
AIC(segmented_beta_dataONEperpond_logit_vardisp) 


######################
# Package: Segmented #
######################
library(segmented)

# here is the beta regression object that I want to segment 
betareg_dataFP_logit

segmented(betareg_dataFP_logit, seg.Z=~TOTP_avg, psi=list(TOTP_avg=c(0.05)), data = dataFPsmall)


