######################################################################
# Manuscript I: Figure 1                                             #
# 1 x 2 panes                                                        #
# data and model fits of total P and FP cover                        #
######################################################################
# GOAL: make the 2 plots  
# Then arrange plots using either plot.arrange() or grid.arrange()

library(ggplot2)
library(gridExtra)

# make a new variable on the data frame - convert proportion to precentage FP cover 
dataFP$FPcover_max_percent <- dataFP$FPcover_max*100 

################
# Fig. 3a      #
# EMPIRICAL    #
# Beta reg.    #
################
# with color 
Fig3a <- ggplot(data=Fig3data, aes(x=TOTP_avg,y=FPcover_max)) + geom_point(size=2) 
Fig3a <- Fig3a + geom_line(aes(x=TOTP_avg,y=fitted),colour="blue",size=1)
Fig3a <- Fig3a + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
Fig3a <- Fig3a + geom_text(aes(x=0.0075,y=1,label="a)"),size=7)
Fig3a <- Fig3a + scale_x_log10() 
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
Fig3a <- Fig3a + scale_y_continuous(breaks=y_breaks,labels=y_labels)
Fig3a <- Fig3a + theme_classic(base_size=18)
Fig3a <- Fig3a + theme(axis.title.x=element_blank())
Fig3a 

################
# Fig. 3b      #
# EMPIRICAL    #
# Laten class  #
# Beta reg.    #
################
# this figure got generated in the "beta regression - mixture.R" script 
dataFP_beta_logit_cluster_prior_clusterv3_plot

# With color 
Fig3b <- ggplot(dataFP,aes(x=TOTP_avg,y=FPcover_max,shape=factor(beta_logit_cluster_prior_clusterv3))) + stat_smooth(method=glm, family=binomial, se=F,colour="blue",size=1) + geom_point(size=2) 
Fig3b <- Fig3b + scale_shape_manual(values=c(1,19),name="Cluster")
Fig3b <- Fig3b + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
Fig3b <- Fig3b + geom_text(aes(x=0.0075,y=1,label="b)"),size=7)
Fig3b <- Fig3b + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
Fig3b <- Fig3b + scale_y_continuous(breaks=y_breaks,labels=y_labels)
Fig3b <- Fig3b + geom_vline(xintercept=0.01972,colour="red",size=1,linetype="longdash") # add vertical line @ lower threshold value 
Fig3b <- Fig3b + geom_vline(xintercept=0.2085,colour="red",size=1,linetype="longdash") # add vertical line @ upper threshold value 
Fig3b <- Fig3b + theme_classic(base_size=18)
Fig3b <- Fig3b + theme(legend.position=c(0.85,0.3))
Fig3b 

# black and white 
Fig3b <- ggplot(dataFP,aes(x=TOTP_avg,y=FPcover_max,shape=factor(beta_logit_cluster_prior_clusterv3))) + stat_smooth(method=glm, family=binomial, se=F, colour="black",size=1) + geom_point(size=2) 
Fig3b <- Fig3b + scale_shape_manual(values=c(1,19),name="Cluster")
Fig3b <- Fig3b + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)")
Fig3b <- Fig3b + geom_text(aes(x=0.0075,y=1,label="f)"),size=7)
Fig3b <- Fig3b + scale_x_log10()
y_breaks <- seq(0,1,0.25)
y_labels <- as.character(y_breaks*100)
Fig3b <- Fig3b + scale_y_continuous(breaks=y_breaks,labels=y_labels)
Fig3b <- Fig3b + geom_vline(xintercept=0.01972,colour="grey50",size=1,linetype="longdash") # add vertical line @ lower threshold value 
Fig3b <- Fig3b + geom_vline(xintercept=0.2085,colour="grey50",size=1,linetype="longdash") # add vertical line @ upper threshold value 
Fig3b <- Fig3b + theme_classic(base_size=18)
Fig3b <- Fig3b + theme(legend.position=c(0.85,0.3))
Fig3b 

###################
# ARRANGING PLOTS #
###################
Fig03 <- arrangeGrob(Fig3a,Fig3b,ncol=1,nrow=2) #grid.arrange does not work with ggsave()
Fig03
ggsave(file="Figure 03.pdf", Fig03, height=12,width=6)
ggsave(file="Figure 03.png", Fig03, height=12,width=6)
ggsave(file="Figure 03.jpg", Fig03, height=12,width=6)
