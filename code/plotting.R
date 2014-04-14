#############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA  #
# Plots and histograms                      #
# UPDATED: 02/24/2013 by MJ McCann          #
#############################################

###############################################################################################
# TO DO
###############################################################################################
# Fix-up scatterplots with a third variable plotted as color, shape, size, etc. to look nice 
# plot error bars (SE) on scatterplots of total N and total P 
# error bars on FP cover? - min. and max.?

###############################################################################################
# LOAD SOME PACKAGES
###############################################################################################
library(ggplot2)

###############################################################################################
# FUNCTION FOR NICE HISTOGRAMS 
###############################################################################################
# SOURCE: http://mikelove.wordpress.com/2011/03/30/r-one-liner-histogram-for-integers/
int.hist = function(x,ylab="Frequency",...) {
  barplot(table(factor(x,levels=min(x):max(x))),space=0,xaxt="n",ylab=ylab,...);axis(1)
}

###################################
# HISTOGRAM: FPcover_max
###################################

# histogram: FPcover_max - all ponds
ggplot(data=dataONEperpond, aes(x=FPcover_max)) + geom_bar(stat="bin", binwidth = 0.1) + labs(title = "All ponds (n=201)")
ggsave(file="histogram - FP cover max - all ponds.jpg")

# histogram: FPcover_max - with FP_species_richness labelled - all ponds
e <- ggplot(data=dataONEperpond, aes(x=FPcover_max, fill=as.factor(FP_species_richness))) + geom_bar(stat="bin", binwidth = 0.1) + labs(title = "All ponds (n=201)")
ggsave(file="histogram - FP cover max - labelled by FP species richness - all ponds.jpg")


# histogram: FPcover_max - with FP_species_richness labelled - FP ponds
J <- ggplot(data=dataFP, aes(x=FPcover_max, fill=as.factor(FP_species_richness))) + geom_bar(stat="bin", binwidth = 0.1) + labs(title = "FP ponds (n=97)")
J <- J + scale_fill_grey(name="FP species richness")
J <- J + labs(x="Proportion FP cover", y = "Frequency")
ggsave(file="histogram - FP cover max - labelled by FP species richness - FP ponds.jpg")

#########
# I want FP richness = 0 to show up 

# histogram: FPcover_max - with FP_species_richness labelled - all ponds
h <- ggplot(data=dataONEperpond, aes(x=FPcover_max, fill=as.factor(FP_species_richness))) + geom_bar(stat="bin", binwidth = 0.1) + labs(title = "All ponds (n=201)")
h <- h + scale_fill_grey(name="FP species richness",start=0.8,end=0.2)
h <- h + labs(x="Proportion FP cover", y = "Frequency")
h <- h + theme_classic()
h
ggsave(file="histogram - FP cover max - labelled by FP species richness - all ponds - b.jpg")


# histogram: FPcover_max - with FP_species_richness labelled - FPsmall ponds
I <- ggplot(data=dataFPsmall, aes(x=FPcover_max, fill=as.factor(FP_species_richness))) + geom_bar(stat="bin", binwidth = 0.1) + labs(title = "FP ponds (n=54)")
I <- I + scale_fill_grey(name="FP species richness",start=0.8,end=0.2)
I <- I + labs(x="Proportion FP cover", y = "Frequency")
I <- I + theme_classic()
I
ggsave(file="histogram - FP cover max - labelled by FP species richness - FPsmall ponds.jpg")


# Histogram: FPcovermax - with FP_species_richness labelled
# facet - dataONEperpond and dataFPsmall 
dataONEperpond$id <- "All waterbodies (n=201)"
dataFPsmall$id <- "Waterbodies <5 ha with Floating Plants Present (n=54)"
df_all <- rbind(dataONEperpond,dataFPsmall)
Q <- ggplot(df_all, aes(x=FPcover_max, fill=as.factor(FP_species_richness))) + geom_bar(stat="bin", binwidth = 0.1) + facet_wrap(~id)
Q <- Q + scale_fill_grey(name="Floating plant species richness",start=0.8,end=0.2)
Q <- Q + labs(x="Proportion FP cover", y = "Frequency")
Q + theme_classic() # this changes the theme to a classic look 
Q + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) # this gets a little closer to starting the plot @ the origin (0,0)
# I don't like how I don't get my second y-axis 

ggsave(file="histogram - FP cover max - labelled by FP species richness - All ponds vs FPsmall ponds.jpg")


# histogram: FPcover_max - FP ponds 
ggplot(data=dataFP, aes(x=FPcover_max)) + geom_bar(stat="bin", binwidth = 0.1) + labs(title = "Ponds w/ Floating Plants (n=97)")
ggsave(file="histogram - FP cover max - FP ponds.jpg")

###
######
######## Figure 2
######
###

# histogram: FPcover_max - small (<5ha), FP ponds 
dataFPsmall$FPcover_max_percent <- dataFPsmall$FPcover_max*100 # make a new variable that is a data frame 

P <- ggplot(dataFPsmall,aes(FPcover_max_percent)) + geom_histogram(binwidth=20)
P <- P + ylab("Frequency") + xlab("Floating plant cover (%)")


P <- ggplot(dataFPsmall,aes(FPcover_max_percent)) + stat_bin(binwidth=20,right=TRUE,col="black")
P <- P + ylab("Frequency") + xlab("Floating plant cover (%)")
P <- P + scale_x_continuous(breaks=c(0,20,40,60,80,100))
P <- P + xlim(0,100)
P
ggsave(file="Figure 2 - histogram - FP cover max - small FP ponds.jpg")

###
######
######## Figure 2
######
###


###################################
# HISTOGRAM: FP_species_richness
###################################

# histogram: FP species richness - all ponds
ggplot(data=dataONEperpond, aes(x=FP_species_richness)) + geom_bar(stat="bin", binwidth=1) + labs(title = "All ponds (n=203)")
# convert FP_species_richness to a factor 
dataONEperpond$FP_species_richness <- as.factor(dataONEperpond$FP_species_richness)
a <- ggplot(data=dataONEperpond, aes(x=FP_species_richness)) + geom_bar(width=0.9, position = position_dodge(width=1))
a + xlab("Floating Plant Species Richness") + ylab("Frequency")
ggsave(file="histogram - FP species richness - all ponds - b.jpg")

# another plot of the histogram with the baic plot functions 
jpeg("FP_species_richness histogram - all ponds - one per pond.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
int.hist(dataONEperpond$FP_species_richness, xlab = "Floating Plant Species Richness", main = "All Ponds (n=201)")
dev.off()

########################################
# SCATTER PLOT: FPcover_max ~ TOTP_avg
########################################

# scatterplot: FPcover_max ~ TOTP_avg - FP ponds 
# ggplot2 
# with error bars for TOTP 
b <- ggplot(data=dataFP, aes(x=TOTP_avg,y=FPcover_max)) + geom_point(shape=1) + labs(title = "Ponds w/ Floating Plants (n=93)")
b <- b + geom_errorbarh(aes(xmin=TOTP_avg - TOTP_SE,xmax= TOTP_avg + TOTP_SE)) 
b <- b + xlab("Total P (mg/L)") + ylab("Proportion FP cover")
ggsave(file="scatterplot - totP ~ FP cover max - FP ponds - error bars.jpg")

# scatterplot: FPcover_max ~ TOTP_avg - FP ponds 
# ggplot2 
# instead of taking the log of total P, I can just plot total P on a log scale 
b + scale_x_log10()
ggsave(file="scatterplot - totP ~ FP cover max - FP ponds - error bars - x log scale.jpg")

# scatterplot: FPcover_max ~ TOTP_avg - small FP ponds (<5 ha)
# ggplot2 
# with error bars for TOTP 
ggplot(data=dataFPsmall, aes(x=TOTP_avg,y=FPcover_max)) + geom_point(shape=1) + 
  labs(title = "Small Ponds(<5ha) w/ Floating Plants (n=54)") + geom_errorbarh(aes(xmin=TOTP_avg - TOTP_SE,xmax= TOTP_avg + TOTP_SE)) + xlab("Total P (mg/L)") + ylab("Proportion FP cover")
ggsave(file="scatterplot - totP ~ FP cover max - small FP ponds - error bars.jpg")

# scatterplot: FPcover_max ~ TOTP_avg - FP ponds 
# basic graphics 
jpeg("TOTP ~ FPcover_max - FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(dataFP$TOTP_avg,dataFP$FPcover_max, xlab="Total P (mg/L)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=93)")
dev.off()

# scatterplot: FPcover_max ~ TOTP_avg - FP ponds - CAES data  
# basic graphics 
jpeg("TOTP ~ FPcover_max - CAESFP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(dataCAESFP$TOTP_avg,dataCAESFP$FPcover_max, xlab="Total P (mg/L)", ylab="Proportion FP cover", main="CAES: Ponds w/ Floating Plants (n=64)")
dev.off()

# scatterplot: FPcover_max ~ TOTP_avg - FP ponds 
# label points with surface area 
# basic graphics 
jpeg("TOTP ~ FPcover_max - SA labels - FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(dataFP$TOTP_avg,dataFP$FPcover_max, xlab="Total P (mg/L)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=93)")
text(dataFP$TOTP_avg,dataFP$FPcover_max,labels=dataFP$surfacearea_ha,cex=0.7,pos=1)
dev.off()

# scatterplot: FPcover_max ~ TOTP_avg - FP ponds 
# label points with date nutrient sample collected
# basic graphics
jpeg("TOTP ~ FPcover_max - dateNUTR labels - FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(dataFP$TOTP_avg,dataFP$FPcover_max, xlab="Total P (mg/L)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=93)")
text(dataFP$TOTP_avg,dataFP$FPcover_max,labels=dataFP$dateNUTR,cex=0.7,pos=1,srt=45)
dev.off()

# scatterplot: FPcover_max ~ log(TOTP_avg) - FP ponds 
# LOG TOT P 
# ggplot2 
# with error bars for TOTP 
# Not sure if these error bars are plotted on the correct scale 
c <- ggplot(data=dataFP, aes(x=log(TOTP_avg),y=FPcover_max)) + geom_point(shape=1) + labs(title = "Ponds w/ Floating Plants (n=93)")
c <- c + geom_errorbarh(aes(xmin=TOTP_avg-TOTP_SE,xmax=TOTP_avg+TOTP_SE)) 
c <- c + xlab("log Total P (mg/L)") + ylab("Proportion FP cover")
c
ggsave(file="scatterplot - log(totP) ~ FP cover max - FP ponds.jpg")

# scatterplot: FPcover_max ~ log(TOTP_avg) - small FP ponds 
# Error bars for TOTAL P 
D <- ggplot(data=dataFPsmall, aes(x=TOTP_avg,y=FPcover_max)) + geom_point(shape=1) + labs(title = "Ponds <5 ha w/ Floating Plants (n=54)")
D <- D + scale_x_log10()
D <- D + geom_errorbarh(aes(xmin=TOTP_avg-TOTP_SE,xmax=TOTP_avg+TOTP_SE)) 
D <- D + xlab("Total P (mg/L)") + ylab("Proportion FP cover")
D

# scatterplot: FPcover_max ~ log(TOTP_avg) - small FP ponds 
# labelled with yea r
G <- ggplot(data=dataFPsmall,aes(x=TOTP_avg, y=FPcover_max)) + geom_point(aes(colour=year),size=4)
G <- G + labs(title = "Ponds <5 ha w/ Floating Plants (n=54)")
G <- G + scale_x_log10()
G


dataFP$size[dataFP$surfacearea_ha<=5] <- "<5ha"
dataFP$size[dataFP$surfacearea_ha>5] <- ">5ha"

G2 <- ggplot(data=dataFP,aes(x=TOTP_avg, y=FPcover_max)) + geom_point(aes(colour=size),size=4)
G2 <- G2 + labs(title = "Ponds w/ Floating Plants (n=99)")
G2 <- G2 + scale_x_log10()
G2


# scatterplot: FPcover_max ~ log(TOTP_avg) - small FP ponds 
# Error bars for FP cover
# I still need to check if these are the right error bars to use 
E <- ggplot(data=dataFPsmall, aes(x=TOTP_avg,y=FPcover_avg)) + geom_point(shape=1) + labs(title = "Ponds <5 ha w/ Floating Plants (n=54)")
E <- E + scale_x_log10()
E <- E + geom_errorbar(aes(ymin=FPcover_min,ymax=FPcover_max)) 
E <- E + xlab("Total P (mg/L)") + ylab("Proportion FP cover")
E

# scatterplot: FPcover_max ~ log(TOTP_avg) - FP ponds 
# LOG TOT P 
# basic graphics 
jpeg("log(TOTP) ~ FPcover_max - FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(log(dataFP$TOTP_avg),dataFP$FPcover_max, xlab="log Total P (mg/L)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=93)")
dev.off()

# scatterplot: FPcover_max ~ log(TOTP_avg) - SMALL FP ponds 
# LOG TOT P 
# basic graphics 
jpeg("log(TOTP) ~ FPcover_max - small FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(log(dataFPsmall$TOTP_avg),dataFPsmall$FPcover_max, xlab="log Total P (mg/L)", ylab="Proportion FP cover", main="Ponds <5 ha w/ Floating Plants (n=54)")
text(log(dataFPsmall$TOTP_avg),dataFPsmall$FPcover_max,labels=dataFPsmall$waterbody,cex=0.7,pos=1)
dev.off()

# Interactively label points - click near it and it will get labeled 
plot(log(dataFPsmall$TOTP_avg),dataFPsmall$FPcover_max, xlab="log Total P (mg/L)", ylab="Proportion FP cover", main="Ponds <5 ha w/ Floating Plants (n=54)")
identify(log(dataFPsmall$TOTP_avg),dataFPsmall$FPcover_max,labels=dataFPsmall$waterbody)

# scatterplot: FPcover_max ~ log(TOTP_avg) - EXTRA SMALL FP ponds 
# LOG TOT P 
# basic graphics 
jpeg("log(TOTP) ~ FPcover_max - extra small FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(log(dataFPextrasmall$TOTP_avg),dataFPextrasmall$FPcover_max, xlab="log Total P (mg/L)", ylab="Proportion FP cover", main="Ponds <=0.4 ha w/ Floating Plants (n=19)")
dev.off()

# scatterplot: FPcover_max ~ log(TOTP_avg) - CAES data only 
# LOG TOT P 
# basic graphics 
jpeg("log(TOTP) ~ FPcover_max - CAESFP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(log(dataCAESFP$TOTP_avg),dataCAESFP$FPcover_max, xlab="log Total P (mg/L)", ylab="Proportion FP cover", main="CAES: Ponds w/ Floating Plants (n=64)")
dev.off()

# scatterplot: FPcover_max ~ log(TOTP_avg) - FP ponds 
# LOG TOT P 
# label points with surface area 
# basic graphics 
jpeg("log(TOTP) ~ FPcover_max - SA labels - FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(log(dataFP$TOTP_avg),dataFP$FPcover_max, xlab="log Total P (mg/L)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=93)")
text(log(dataFP$TOTP_avg),dataFP$FPcover_max,labels=dataFP$surfacearea_ha,cex=0.7,pos=1)
dev.off()

# scatterplot: FPcover_max ~ log(TOTP_avg) - FP ponds 
# LOG TOT P 
# label points with date nutrients collected 
# basic graphics
jpeg("log(TOTP) ~ FPcover_max - dateNUTR labels - FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(log(dataFP$TOTP_avg),dataFP$FPcover_max, xlab="log Total P (mg/L)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=93)")
text(log(dataFP$TOTP_avg),dataFP$FPcover_max,labels=dataFP$dateNUTR,cex=0.7,pos=1,srt=45)
dev.off()

# scatterplot: FPcover_max ~ log(TOTP_avg) - FP ponds 
# LOG TOT P 
# label points with year 
# basic graphics
jpeg("log(TOTP) ~ FPcover_max - year labels - FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(log(dataFP$TOTP_avg),dataFP$FPcover_max, xlab="log Total P (mg/L)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=93)")
text(log(dataFP$TOTP_avg),dataFP$FPcover_max,labels=dataFP$year,cex=0.7,pos=1)
dev.off()

# add straight line to plot
abline(lm(dataFP$FPcover_max~log(dataFP$TOTP_avg)))
# add lowess fit to plot 
lines(lowess(log(dataFP$TOTP_avg),dataFP$FPcover_max,delta=0.01))

########################################
# SCATTER PLOT: FPcover_max ~ TOTN_avg
########################################

# scatterplot: FPcover_max ~ TOTN_avg - FP ponds 
ggplot(data=dataFP, aes(x=TOTN_avg,y=FPcover_max)) + geom_point(shape=1) + labs(title = "Ponds w/ Floating Plants (n=35)")
ggsave(file="scatterplot - totN ~ FP cover max - FP ponds.jpg")

jpeg("TOTN ~ FPcover_max - FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(dataFP$TOTN_avg,dataFP$FPcover_max, xlab="Total N (mg/L)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=35)")
dev.off()

jpeg("TOTN ~ FPcover_max - year labels - FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(dataFP$TOTN_avg,dataFP$FPcover_max, xlab="Total N (mg/L)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=35)")
text(dataFP$TOTN_avg,dataFP$FPcover_max,labels=dataFP$year,cex=0.7,pos=1)
dev.off()

jpeg("TOTN ~ FPcover_max - waterbody labels - FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(dataFP$TOTN_avg,dataFP$FPcover_max, xlab="Total N (mg/L)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=35)")
text(dataFP$TOTN_avg,dataFP$FPcover_max,labels=dataFP$waterbody,cex=0.3,pos=1,srt=45)
dev.off()

jpeg("TOTN ~ FPcover_max - SA labels - FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(dataFP$TOTN_avg,dataFP$FPcover_max, xlab="Total N (mg/L)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=35)")
text(dataFP$TOTN_avg,dataFP$FPcover_max,labels=dataFP$surfacearea_ha,cex=0.7,pos=1)
dev.off()

# scatterplot: FPcover_max ~ TOTN_avg - FP ponds 
# size/color of points indicate surface area 
ggplot(data=dataFP, aes(x=TOTN_avg,y=FPcover_max)) + labs(title = "Ponds w/ Floating Plants (n=35)") + geom_point(aes(size = surfacearea_ha))
ggplot(data=dataFP, aes(x=TOTN_avg,y=FPcover_max)) + labs(title = "Ponds w/ Floating Plants (n=35)") + geom_point(aes(size = surfacearea_ha)) + scale_area()
ggsave(file="scatterplot - totN ~ FP cover max - surfacea area - FP ponds.jpg")
f <- ggplot(data=dataFP, aes(x=TOTN_avg,y=FPcover_max)) + labs(title = "Ponds w/ Floating Plants (n=35)") + geom_point(aes(size = 3, colour = surfacearea_ha))
f + scale_colour_gradient()
f+scale_colour_gradient(limits=c(0,179),low="red")
ggsave(file="scatterplot - totN ~ FP cover max - surfacea area - FP pondsb.jpg")

# scatterplot: FPcover_max ~ log(TOTN_avg) - FP ponds 
d <- ggplot(data=dataFP, aes(x=log(TOTN_avg),y=FPcover_max)) + geom_point(shape=1) + labs(title = "Ponds w/ Floating Plants (n=35)")
d <- d + geom_errorbarh(aes(xmin=TOTN_SE,xmax=TOTN_SE)) 
d <- d + xlab("log Total N (mg/L)") + ylab("Proportion FP cover")
ggsave(file="scatterplot - log(totN) ~ FP cover max - FP ponds.jpg")

jpeg("log(TOTN) ~ FPcover_max - FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(log(dataFP$TOTN_avg),dataFP$FPcover_max, xlab="log Total N (mg/L)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=35)")
dev.off()

# log TOTN 
# label points with date nutrients collected
jpeg("log(TOTN) ~ FPcover_max - dateNUTR collected labels - FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(log(dataFP$TOTN_avg),dataFP$FPcover_max, xlab="log Total N (mg/L)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=35)")
text(log(dataFP$TOTN_avg),dataFP$FPcover_max,labels=dataFP$dateNUTR,cex=0.3,pos=4,srt=45)
dev.off()

#####################################################
# SCATTER PLOT: FPcover_max ~ TOTN_avg * TOTP_avg
#####################################################

# scatterplot: FPcover_max ~ TOTN_avg * TOTP_avg - FP ponds 
ggplot(data=dataFP, aes(x=TOTN_avg,y=TOTP_avg)) + geom_point(shape=1) + labs(title = "Ponds w/ Floating Plants (n=35)") + geom_point(aes(size = FPcover_max))
ggsave(file="scatterplot - totN and totP ~ FP cover max - FP ponds.jpg")
ggplot(data=dataFP, aes(x=TOTN_avg,y=TOTP_avg)) + geom_point(shape=1) + labs(title = "Ponds w/ Floating Plants (n=35)") + geom_point(aes(size = 3, colour = FPcover_max))
ggsave(file="scatterplot - totN and totP ~ FP cover max - FP pondsb.jpg")
# labels the points with their regime (e.g., FP, SAV, etc.)
ggplot(data=dataFP, aes(x=TOTN_avg,y=TOTP_avg)) + geom_point(shape=1) + labs(title = "Ponds w/ Floating Plants (n=35)") + geom_point() + geom_text(aes(label=regime))

# scatterplot: FPcover_max ~ totNtotP_avg - FP ponds 
ggplot(data=dataFP, aes(x=totNtotP_avg,y=FPcover_max)) + geom_point(shape=1) + labs(title = "Ponds w/ Floating Plants (n=35)") 
ggsave(file="scatterplot - totNtotP ~ FP cover max - FP ponds.jpg")

jpeg("logTOTN ~ logTOTP - FPcover_max labels - FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(log(dataFP$TOTN_avg),log(dataFP$TOTP_avg), xlab="log Total N (mg/L)", ylab="log Total P (mg/L)", main="Ponds w/ Floating Plants (n=35)")
text(log(dataFP$TOTN_avg),log(dataFP$TOTP_avg),labels=dataFP$FPcover_max,cex=0.7,pos=1)
dev.off()

jpeg("FPcover_max ~ logTOTN-logTOTP - FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot((log(dataFP$TOTN_avg)/log(dataFP$TOTP_avg)),dataFP$FPcover_max, xlab="log Total N:log Total P (mg/L)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=35)")
dev.off()

jpeg("FPcover_max ~ TOTN-TOTP - FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot((dataFP$TOTN_avg/dataFP$TOTP_avg),dataFP$FPcover_max, xlab="Total N:Total P (mg/L)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=35)")
dev.off()

#####################################################
# SCATTER PLOT: FPcover_max ~ depth_max_m
#####################################################
jpeg("maxdepth ~ FPcover_max - FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(dataFP$depth_max_m,dataFP$FPcover_max, xlab="Maximum depth (m)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=97)")
dev.off()

##########################################################
# SCATTER PLOT: FPcover_max ~ depth_max_m * surface area 
##########################################################
ggplot(data=dataFP,aes(x=surfacearea_ha, y=depth_max_m)) + geom_point(aes(colour=FPcover_max),size=5)
# OR 
ggplot(data=dataFP,aes(x=surfacearea_ha, y=depth_max_m)) + geom_text(label = dataFP$FPcover_max,size=5)

#####################################################
# SCATTER PLOT: FPcover_max ~ shoreline_development 
#####################################################
jpeg("shorelinedevelopment ~ FPcover_max - FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(dataFP$shoreline_development,dataFP$FPcover_max, xlab="Shoreline development index", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=97)")
dev.off()

#####################################################
# SCATTER PLOT: surface area_ha ~ depth_max_m
#####################################################
jpeg("surfacea area ~ maxdepth - FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(dataFP$depth_max_m,dataFP$surfacearea_ha, xlab="Maximum depth (m)", ylab="Surfacea area (ha)", main="Ponds w/ Floating Plants (n=97)")
dev.off()

# or
plot(dataFP$surfacearea_ha, dataFP$depth_max_m, ylab="Maximum depth (m)", xlab="Surfacea area (ha)", main="Ponds w/ Floating Plants (n=97)")

# pearson correlation coefficient
cor(dataFP$surfacearea_ha, dataFP$depth_max_m)

#####################################################
# SCATTER PLOT: surface area_ha ~ shoreline_development
#####################################################
jpeg("surfacea area ~ shorelinedevelopment - FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(dataFP$shoreline_development,dataFP$surfacearea_ha, xlab="Shoreline development index", ylab="Surfacea area (ha)", main="Ponds w/ Floating Plants (n=97)")
dev.off()

# or
plot(dataFP$surfacearea_ha, dataFP$shoreline_development, ylab="Shoreline development index", xlab="Surfacea area (ha)", main="Ponds w/ Floating Plants (n=97)")

# pearson correlation coefficient
cor(dataFP$surfacearea_ha, dataFP$shoreline_development)

#####################################################
# SCATTER PLOT: FPcover_max ~ surface area_ha
#####################################################
# pearson correlation coefficient
cor(dataFP$surfacearea_ha,dataFP$FPcover_max)

# scatterplot: FPcover_max ~ log(surface area) - FP ponds 
ggplot(data=dataFP, aes(x=log(surfacearea_ha),y=FPcover_max)) + geom_point(shape=1) + labs(title = "Ponds w/ Floating Plants (n=97)") 
ggsave(file="scatterplot - log(surface area) ~ FP cover max - FP ponds.jpg")

# scatterplot: FPcover_max ~ surface area - FP ponds 
ggplot(data=dataFP, aes(x=surfacearea_ha,y=FPcover_max)) + geom_point(shape=16,size=3) + labs(title = "Ponds w/ Floating Plants (n=97)") + scale_x_log10() +
  geom_vline(xintercept = 5) + ylab("Proportion FP cover") + xlab("Surface area (ha)")
ggsave(file="scatterplot - log surface area ~ FP cover max - FP ponds - line at 5ha.jpg")

# scatterplot: FPcover_max ~ surface area - FP ponds - x-axis on log scale 
ggplot(data=dataFP, aes(x=surfacearea_ha,y=FPcover_max)) + geom_point(shape=1) + labs(title = "Ponds w/ Floating Plants (n=97)") 
ggsave(file="scatterplot - surface area ~ FP cover max - FP ponds.jpg")

jpeg("surfacearea ~ FPcover_max - FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(dataFP$surfacearea_ha,dataFP$FPcover_max, xlab="Surface area (ha)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=97)")
dev.off()
# plot the treshold line from package changepoint cpt.mean
abline(v=0.4,col=3,lty=3)

scatterplot(dataFP$FPcover_max ~ dataFP$surfacearea_ha)

jpeg("log(surfacearea) ~ FPcover_max - FP ponds.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
plot(log(dataFP$surfacearea_ha),dataFP$FPcover_max, xlab="log Surface area (ha)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=97)")
dev.off()
# plot the treshold line from package changepoint cpt.mean
abline(v=log(0.4),col=3,lty=3)

#####################################################
# combine scatter plots 
#####################################################

jpeg("figure 02.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
par(mfrow=c(3,1))
plot(log(dataFP$TOTP_avg),dataFP$FPcover_max, xlab="log Total P (mg/L)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=93)")
plot(log(dataFP$TOTN_avg),dataFP$FPcover_max, xlab="log Total N (mg/L)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=35)")
plot((dataFP$TOTN_avg/dataFP$TOTP_avg),dataFP$FPcover_max, xlab="Total N:Total P (mg/L)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=35)")
dev.off()

jpeg("figure 02b.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=250)
par(mfrow=c(1,3))
plot(log(dataFP$TOTP_avg),dataFP$FPcover_max, xlab="log Total P (mg/L)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=93)")
plot(log(dataFP$TOTN_avg),dataFP$FPcover_max, xlab="log Total N (mg/L)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=35)")
plot((dataFP$TOTN_avg/dataFP$TOTP_avg),dataFP$FPcover_max, xlab="Total N:Total P (mg/L)", ylab="Proportion FP cover", main="Ponds w/ Floating Plants (n=35)")
dev.off()


