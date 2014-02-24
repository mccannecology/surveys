######################################################################
# Manuscript I: Figure 1                                             #
# 2 x 3 panes                                                        #
# top row: hypothetical relationship between total P and FP cover    #
# bottom row: data and model fits of total P and FP cover            #
######################################################################
# GOAL: make the 6 plots  
# Then arrange plots using either plot.arrange() or grid.arrange()

library(ggplot2)

################
# HYPOTHETICAL #
# LINEAR       #
################ 
a1 <- ggplot(data.frame(x = c(0, 0.5)), aes(x)) 
a1 <- a1 + geom_segment(aes(x=0,y=1,xend=0.5,yend=100),size=1)
a1 <- a1 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
a1

################
# HYPOTHETICAL #
# SEGMENTED    #
################
a2 <- ggplot(data.frame(x = c(0, 0.5)), aes(x)) 
a2 <- a2 + geom_segment(aes(x=0,y=1,xend=0.25,yend=10),size=1) + geom_segment(aes(x=0.25,y=90,xend=0.5,yend=100),size=1)
a2 <- a2 + geom_vline(xintercept=0.25,colour="red",size=1)
a2 <- a2 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
a2

################
# HYPOTHETICAL #
# OVERLAPPING  #
################
# create a blank plot - but you cannot view it until you add a line 
a3 <- ggplot(data.frame(x = c(0, 0.5)), aes(x)) 
a3 <- a3 + geom_segment(aes(x=0,y=1,xend=0.3,yend=10),size=1) + geom_segment(aes(x=0.2,y=90,xend=0.5,yend=100),size=1)
a3 <- a3 + geom_vline(xintercept=0.20,colour="red",size=1)
a3 <- a3 + geom_vline(xintercept=0.30,colour="red",size=1)
a3 <- a3 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
a3

###################
# ARRANGING PLOTS #
###################
