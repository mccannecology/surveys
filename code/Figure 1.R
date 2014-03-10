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
a1 <- a1 + geom_segment(aes(x=0,y=1,xend=0.5,yend=100),size=1) # add linear line
a1 <- a1 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)") # label axes 
a1 <- a1 + geom_text(aes(x=0.01,y=100,label="a)"),size=7) # add pane label
a1 <- a1 + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) # remove x-axis values & ticks
a1 <- a1 + theme_classic(base_size=18)
a1 <- a1 + theme(axis.title.x=element_blank()) 
a1

################
# Fig. 1b      #
# HYPOTHETICAL #
# SEGMENTED    # 
# THRESHOLD    #
################
a2 <- ggplot(data.frame(x = c(0, 0.5)), aes(x)) # set-up blank plot 
a2 <- a2 + geom_segment(aes(x=0,y=1,xend=0.25,yend=10),size=1) + geom_segment(aes(x=0.25,y=90,xend=0.5,yend=100),size=1) # add two segments 
a2 <- a2 + geom_vline(xintercept=0.25,colour="red",size=1,linetype="longdash") # add vertical line @ threshold value 
a2 <- a2 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)") # label axes 
a2 <- a2 + geom_text(aes(x=0.01,y=100,label="b)"),size=7) # add pane label 
a2 <- a2 + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) # remove x-axis values & ticks
a2 <- a2 + theme_classic(base_size=18)
a2 <- a2 + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank())
a2

################
# Fig. 1c      #
# HYPOTHETICAL #
# OVERLAPPING  #
# ALT. STATES  #
################
# create a blank plot - but you cannot view it until you add a line 
a3 <- ggplot(data.frame(x = c(0, 0.5)), aes(x)) # set-up blank plot 
a3 <- a3 + geom_segment(aes(x=0,y=1,xend=0.35,yend=12),size=1) + geom_segment(aes(x=0.15,y=88,xend=0.5,yend=100),size=1) # add two segments 
a3 <- a3 + geom_vline(xintercept=0.15,colour="red",size=1,linetype="longdash") # add vertical line @ 1st threshold value 
a3 <- a3 + geom_vline(xintercept=0.35,colour="red",size=1,linetype="longdash") # add vertical line @ 2nd threshold value 
a3 <- a3 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)") # label axes 
a3 <- a3 + geom_text(aes(x=0.01,y=100,label="c)"),size=7) # add pane label 
a3 <- a3 + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) # remove x-axis values & ticks
a3 <- a3 + theme_classic(base_size=18)
a3 <- a3 + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank())
a3


################
# Fig. 1d      #
# EMPIRICAL    #
# "LINEAR"     #
# (LOGISTIC)   #
################ 
b1 <- ggplot(data=dataFPsmall, aes(x=TOTP_avg,y=FPcover_max)) + geom_point() 
b1 <- b1 + stat_smooth(method=glm, family=binomial, se=F)
b1 <- b1 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
b1 <- b1 + geom_text(aes(x=0.015,y=1,label="d)"),size=7)
b1 <- b1 + scale_x_log10() 
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
b1 <- b1 + scale_y_continuous(breaks=y_breaks,labels=y_labels)
b1 <- b1 + theme_classic(base_size=18)
b1 

################
# Fig. 1e      #
# EMPIRICAL    #
# SEGMENTED    #
# THRESHOLD    #
################
# use "segmented logistic.R" first 
# generates your breakpoint 

b2 <- ggplot(data=dataFPsmall, aes(x=TOTP_avg,y=FPcover_max)) + geom_point() 
b2 <- b2 + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(breakpoint02)))
b2 <- b2 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
b2 <- b2 + geom_text(aes(x=0.015,y=1,label="e)"),size=7)
b2 <- b2 + geom_vline(xintercept=breakpoint02,colour="red",size=1,linetype="longdash") # add vertical line @ threshold value 
b2 <- b2 + scale_x_log10()
b2 <- b2 + theme(legend.position="none")
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
b2 <- b2 + scale_y_continuous(breaks=y_breaks,labels=y_labels)
b2 <- b2 + theme_classic(base_size=18) + theme(legend.position="none")
b2 <- b2 + theme(axis.title.y=element_blank())
b2

################
# Fig. 1f      #
# EMPIRICAL    #
# OVERLAPPING  #
# ALT. STATES  #
################
b3 <- ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max)) + geom_point() 
b3 <- b3 + stat_smooth(method=glm, family=binomial, se=F,aes(fill=factor(third))) 
b3 <- b3 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
b3 <- b3 + geom_text(aes(x=0.015,y=1,label="f)"),size=7)
b3 <- b3 + scale_x_log10()
b3 <- b3 + theme(legend.position="none")
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
b3 <- b3 + scale_y_continuous(breaks=y_breaks,labels=y_labels)
b3 <- b3 + geom_vline(xintercept=0.01972,colour="red",size=1,linetype="longdash") # add vertical line @ lower threshold value 
b3 <- b3 + geom_vline(xintercept=0.2085,colour="red",size=1,linetype="longdash") # add vertical line @ upper threshold value 
b3 <- b3 + theme_classic(base_size=18) + theme(legend.position="none")
b3 <- b3 + theme(axis.title.y=element_blank())
b3 

###################
# ARRANGING PLOTS #
###################
Fig01 <- arrangeGrob(a1,a2,a3,b1,b2,b3,ncol=3,nrow=2) #grid.arrange does not work with ggsave()
Fig01
ggsave(file="Figure 01.pdf", Fig01, height=8,width=11)
ggsave(file="Figure 01.png", Fig01, height=8,width=11)
