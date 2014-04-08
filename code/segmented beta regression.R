#######################
# Segmented           #
# Beta regression     #
# dataONEperpond      #
# link: logit         #
# Variable dispersion #
#######################
breakpoint01 <- 0.01850
breakpoint02 <- 0.20850

formula <- FPcover_max ~ (TOTP_avg*(TOTP_avg<breakpoint01) + TOTP_avg*(TOTP_avg>=breakpoint01 & TOTP_avg<breakpoint02) + TOTP_avg*(TOTP_avg>=breakpoint02))|
  (TOTP_avg*(TOTP_avg<breakpoint01) + TOTP_avg*(TOTP_avg>=breakpoint01 & TOTP_avg<breakpoint02) + TOTP_avg*(TOTP_avg>=breakpoint02))
  
segmented_beta_dataONEperpond_logit_vardisp <- betareg(formula, data=dataONEperpond, link="logit")
# Error in optim(par = start, fn = loglikfun, gr = gradfun, method = method,: non-finite value supplied by optim

summary(segmented_beta_dataONEperpond_logit_vardisp) 
AIC(segmented_beta_dataONEperpond_logit_vardisp) 






