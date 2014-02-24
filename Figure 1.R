######################################################################
# Manuscript I: Figure 1                                             #
# 2 x 3 panes                                                        #
# top row: hypothetical relationship between total P and FP cover    #
# bottom row: data and model fits of total P and FP cover            #
######################################################################

library(ggplot2)

##########
# LINEAR #
##########
# create a blank plot - but you cannot view it until you add a line 
a1 <- ggplot(data.frame(x = c(0, 0.5)), aes(x)) 

# describe a function here 
linear <- function(x) {200*x + 1}

# add the function to the plot 
a1 <- a1 + stat_function(fun = linear)
a1 <- a1 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
a1

#############
# SEGMENTED #
#############
# generate some segmented data 
x <- seq(0,0.5,0.001)
y <- numeric(501)
y[1:250] <- seq(1,10,0.036)
y[251:501] <- seq(90,100,0.03984064)
y
df<-data.frame(x,y) # put them together into a dataframe

# create a blank plot - but you cannot view it until you add a line 
a2 <- ggplot(data=df,aes(x=x,y=y)) + geom_line()
a2 <- a2 + geom_vline(xintercept=0.25,colour="red",size=1)
a2 <- a2 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
a2

###############
# OVERLAPPING #
###############
# generate some overlapping data 

lineI <- function(x) {.015*x + 1}
lineII <- function(x) {.015*x + 85}

xI <- seq(0,0.30,5e-04)
xII <- seq(0.2,0.5,5e-04)
x<-append(xI,xII)
length(x) #1202 

zI <- rep("line1",601)
zII <- rep("line2",601)
z<-append(zI,zII)
length(z) #1202 

yI<-NULL
yII<-NULL 

for(i in 1:602) {
  yI[i] <- lineI(i)
}

for(i in 603:1202) {
  yII <- append(yII,lineII(i))
}

y<-append(yI,yII)

length(y)

df<-data.frame(x,y,z) # put them together into a dataframe

# looks OK, except for one weird little point 

# create a blank plot - but you cannot view it until you add a line 
a3 <- ggplot(data=df,aes(x=x,y=y)) + geom_line()
a3 <- a3 + geom_vline(xintercept=0.25,colour="red",size=1)
a3 <- a3 + xlab("Total P (mg/L)") + ylab("Floating plant cover(%)")
a3


a3 <- ggplot(data.frame(x = c(0, 0.5)), aes(x)) 
a3 <- a3 + geom_segment(aes(x=0,y=1,xend=0.3,yend=10)) + geom_segment(aes(x=0.2,y=90,xend=0.5,yend=100))
a3




# make the 6 plots then 
# arrange plots using either plot.arrange() or grid.arrange()