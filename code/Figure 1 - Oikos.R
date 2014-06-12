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

################
# Fig. 1a      #
# HYPOTHETICAL #
# LINEAR       #
################ 
a1 <- ggplot(data.frame(x = c(0, 0.5)), aes(x)) # set-up blank plot 
a1 <- a1 + geom_segment(aes(x=0,y=1,xend=0.5,yend=100)) # add linear line
a1 <- a1 + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)") # label axes 
a1 <- a1 + geom_text(aes(x=0.01,y=100,label="a)"),size=4) # add pane label
a1 <- a1 + theme_classic(base_size=12)
a1 <- a1 + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) # remove x-axis values & ticks
a1

################
# Fig. 1b      #
# HYPOTHETICAL #
# SEGMENTED    # 
# THRESHOLD    #
################
a2 <- ggplot(data.frame(x = c(0, 0.5)), aes(x)) # set-up blank plot 
a2 <- a2 + geom_segment(aes(x=0,y=1,xend=0.25,yend=10)) + geom_segment(aes(x=0.25,y=90,xend=0.5,yend=100)) # add two segments 
a2 <- a2 + geom_vline(xintercept=0.25,colour="black",linetype="longdash") # add vertical line @ threshold value 
a2 <- a2 + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)") # label axes 
a2 <- a2 + geom_text(aes(x=0.01,y=100,label="b)"),size=4) # add pane label 
a2 <- a2 + theme_classic(base_size=12)
a2 <- a2 + theme(axis.title.y=element_blank())
a2 <- a2 + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) # remove x-axis values & ticks
a2

################
# Fig. 1c      #
# HYPOTHETICAL #
# OVERLAPPING  #
# ALT. STATES  #
################
# create a blank plot - but you cannot view it until you add a line 
a3 <- ggplot(data.frame(x = c(0, 0.5)), aes(x)) # set-up blank plot 
a3 <- a3 + geom_segment(aes(x=0,y=1,xend=0.35,yend=12)) + geom_segment(aes(x=0.15,y=88,xend=0.5,yend=100)) # add two segments 
a3 <- a3 + geom_vline(xintercept=0.15,colour="black",linetype="longdash") # add vertical line @ 1st threshold value 
a3 <- a3 + geom_vline(xintercept=0.35,colour="black",linetype="longdash") # add vertical line @ 2nd threshold value 
a3 <- a3 + xlab("Total P (mg/L)") + ylab("Floating plant cover (%)") # label axes 
a3 <- a3 + geom_text(aes(x=0.01,y=100,label="c)"),size=4) # add pane label 
a3 <- a3 + theme_classic(base_size=12)
a3 <- a3 + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) # remove x-axis values & ticks
a3 <- a3 + theme(axis.title.y=element_blank())
a3


###################
# ARRANGING PLOTS #
###################
Fig01 <- arrangeGrob(a1,a2,a3,ncol=3,nrow=1) #grid.arrange does not work with ggsave()
Fig01
ggsave(file="Figure 01.pdf", Fig01, height=4,width=11)
ggsave(file="Figure 01.png", Fig01, height=4,width=11)
ggsave(file="Figure 01.jpg", Fig01, height=5.53,width=16.6, units="cm",dpi=1200)
