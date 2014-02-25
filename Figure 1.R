######################################################################
# Manuscript I: Figure 1                                             #
# 2 x 3 panes                                                        #
# top row: hypothetical relationship between total P and FP cover    #
# bottom row: data and model fits of total P and FP cover            #
######################################################################
# GOAL: make the 6 plots  
# Then arrange plots using either plot.arrange() or grid.arrange()

library(ggplot2)
library(gridExtra)

# make a new variable on the data frame - convert proportion to precentage FP cover 
dataFPsmall$FPcover_max_percent <- dataFPsmall$FPcover_max*100 

################
# Fig. 1a      #
# HYPOTHETICAL #
# LINEAR       #
################ 
a1 <- ggplot(data.frame(x = c(0, 0.5)), aes(x)) # set-up blank plot 
a1 <- a1 + scale_x_log10() # x-axis on log scale
a1 <- a1 + geom_segment(aes(x=0,y=1,xend=0.5,yend=100),size=1) # add linear line
a1 <- a1 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)") # label axes 
a1 <- a1 + geom_text(aes(x=0.01,y=100,label="a)"),size=7) # add pane label
a1

################
# Fig. 1b      #
# HYPOTHETICAL #
# SEGMENTED    # 
# THRESHOLD    #
################
a2 <- ggplot(data.frame(x = c(0, 0.5)), aes(x)) # set-up blank plot 
a2 <- a2 + scale_x_log10() # x-axis on log scale
a2 <- a2 + geom_segment(aes(x=0,y=1,xend=0.25,yend=10),size=1) + geom_segment(aes(x=0.25,y=90,xend=0.5,yend=100),size=1) # add two segments 
a2 <- a2 + geom_vline(xintercept=0.25,colour="red",size=1) # add vertical line @ threshold value 
a2 <- a2 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)") # label axes 
a2 <- a2 + geom_text(aes(x=0.01,y=100,label="b)"),size=7) # add pane label 
a2

################
# Fig. 1c      #
# HYPOTHETICAL #
# OVERLAPPING  #
# ALT. STATES  #
################
# create a blank plot - but you cannot view it until you add a line 
a3 <- ggplot(data.frame(x = c(0, 0.5)), aes(x)) # set-up blank plot 
a3 <- a3 + scale_x_log10() # x-axis on log scale
a3 <- a3 + geom_segment(aes(x=0,y=1,xend=0.3,yend=10),size=1) + geom_segment(aes(x=0.2,y=90,xend=0.5,yend=100),size=1) # add two segments 
a3 <- a3 + geom_vline(xintercept=0.20,colour="red",size=1) # add vertical line @ 1st threshold value 
a3 <- a3 + geom_vline(xintercept=0.30,colour="red",size=1) # add vertical line @ 2nd threshold value 
a3 <- a3 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)") # label axes 
a3 <- a3 + geom_text(aes(x=0.01,y=100,label="c)"),size=7) # add pane label 
a3

################
# Fig. 1d      #
# EMPIRICAL    #
# "LINEAR"     #
# (LOGISTIC)   #
################ 
b1 <- ggplot(data=dataFPsmall, aes(x=TOTP_avg,y=as.factor(FPcover_max_percent))) + geom_point(shape=1) +geom_smooth(method="glm",family=binomial)
b1 <- ggplot(data=dataFPsmall, aes(x=TOTP_avg,y=FPcover_max_percent)) + geom_point(shape=1) 
b1 <- b1 + scale_x_log10()
b1 <- b1 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
b1 <- b1 + geom_text(aes(x=0.01,y=100,label="d)"),size=7)

b1 <- b1 + geom_smooth(method="glm", family=quasibinomial) # add logistic regression here - problem: y(0,100) not (0,1) # may need to covert to "factor

b1  

################
# Fig. 1e      #
# EMPIRICAL    #
# SEGMENTED    #
# THRESHOLD    #
################
b2 <- ggplot(data=dataFPsmall, aes(x=TOTP_avg,y=FPcover_max_percent)) + geom_point(shape=1) 
b2 <- b2 + scale_x_log10()
b2 <- b2 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
b2 <- b2 + geom_text(aes(x=0.01,y=100,label="e)"),size=7)
b2  
  
################
# Fig. 1f      #
# EMPIRICAL    #
# OVERLAPPING  #
# ALT. STATES  #
################
b3 <- ggplot(data=dataFPsmall, aes(x=TOTP_avg,y=FPcover_max_percent)) + geom_point(shape=1) 
b3 <- b3 + scale_x_log10()
b3 <- b3 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
b3 <- b3 + geom_text(aes(x=0.01,y=100,label="f)"),size=7)
b3  
  
###################
# ARRANGING PLOTS #
###################
Fig01 <- arrangeGrob(a1,a2,a3,b1,b2,b3,ncol=3,nrow=2) #grid.arrange does not work with ggsave()
ggsave(file="Figure 01.pdf", Fig02)
