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
# colour
b1append <- ggplot(data=dataFPsmall, aes(x=TOTP_avg,y=FPcover_max)) + geom_point(size=2) 
b1append <- b1append + stat_smooth(method=glm, family=binomial, se=F)
b1append <- b1append + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
b1append <- b1append + geom_text(aes(x=0.0075,y=1,label="a)"),size=7)
b1append <- b1append + scale_x_log10() 
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
b1append <- b1append + scale_y_continuous(breaks=y_breaks,labels=y_labels)
b1append <- b1append + theme_classic(base_size=18)
b1append 

################
# Fig. 1e      #
# EMPIRICAL    #
# SEGMENTED    #
# THRESHOLD    #
################
# use "segmented logistic.R" first 
# generates your breakpoint 
# breakpoint is used below and is defined in the segmented logistic script 

# colour
b2append <- ggplot(data=dataFPsmall, aes(x=TOTP_avg,y=FPcover_max)) + geom_point(size=2) 
b2append <- b2append + stat_smooth(method=glm, family=binomial, se=F, aes(fill=factor(breakpoint)))
b2append <- b2append + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
b2append <- b2append + geom_text(aes(x=0.0075,y=1,label="b)"),size=7)
b2append <- b2append + geom_vline(xintercept=breakpoint,colour="red",size=1,linetype="longdash") # add vertical line @ threshold value
b2append <- b2append + scale_x_log10()
b2append <- b2append + theme(legend.position="none")
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
b2append <- b2append + scale_y_continuous(breaks=y_breaks,labels=y_labels)
b2append <- b2append + theme_classic(base_size=18) + theme(legend.position="none")
b2append <- b2append + theme(axis.title.y=element_blank())
b2append

################
# Fig. 1f      #
# EMPIRICAL    #
# OVERLAPPING  #
# ALT. STATES  #
################

# colour
b3append <- ggplot(dataFPsmall,aes(x=TOTP_avg,y=FPcover_max,shape=factor(beta_logit_cluster_prior_clusterv3))) + stat_smooth(method=glm, family=binomial, se=F) + geom_point(size=2) 
b3append <- b3append + scale_shape_manual(values=c(1,19),name="Cluster")
b3append <- b3append + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
b3append <- b3append + geom_text(aes(x=0.0075,y=1,label="c)"),size=7)
b3append <- b3append + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
b3append <- b3append + scale_y_continuous(breaks=y_breaks,labels=y_labels)
b3append <- b3append + geom_vline(xintercept=0.01972,colour="red",size=1,linetype="longdash") # add vertical line @ lower threshold value 
b3append <- b3append + geom_vline(xintercept=0.2085,colour="red",size=1,linetype="longdash") # add vertical line @ upper threshold value 
b3append <- b3append + theme_classic(base_size=18)
b3append <- b3append + theme(legend.position=c(0.85,0.3))
b3append <- b3append + theme(axis.title.y=element_blank())
b3append 

###################
# ARRANGING PLOTS #
###################
Fig01append <- arrangeGrob(b1append,b2append,b3append,ncol=3,nrow=1) #grid.arrange does not work with ggsave()
Fig01append
ggsave(file="Fig01appendix.pdf", Fig01append, height=8,width=11)
ggsave(file="Fig01appendix.png", Fig01append, height=8,width=11)
ggsave(file="Fig01appendix.jpg", Fig01append, height=8,width=11)
