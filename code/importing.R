##############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA   #
# Importing data sets                        #
# UPDATED: 02/24/2014 by MJ McCann           #
##############################################

# Creates the followind data frames: 
# data (n = 415)                  all surveys (including CAES 2004 surveys when FP were not measured)
# dataONEperpond (n = 205)        surveys combined to one per waterbody
# dataCAES (n = 170)              waterbodies surveyed by CAES
# dataFP (n = 99)                 waterbodies with floating plants present 
# dataCAESFP (n = 64)             waterbodies surveyed by CAES - with floating plants present 
# dataFPoutliers (n = 97)         waterbodies with floating plants present - total P < 0.5 mg/L (2 waterbodies removed)
# dataFPsmall (n = 54)            waterbodies with floating plants present and <5 ha. surface area 
# dataFPextrasmall (n = 19)       waterbodies with floating plants present and <=0.4 ha. surface area 
# dataFPoutlierssmall (n = 52)    waterbodies with floating plants present and <5 ha. surface area - total P < 0.5 mg/L (2 waterbodies removed)

###############################################################################################
# SAVING EXCEL FILES FOR IMPORTING 
# save main database ("survey analysis all xx-xx-20xx.xls") as "allsurveys+combined.csv" 
# open up allsurveys+combined.csv
# CTRL + F and replace all "-" with ""
###############################################################################################

###############################################################################################
# IMPORTING & SETTING THINGS UP
###############################################################################################
rm(list=ls()) # clear your workspace 

# Not sure that I need to set working directory when this is a project on git 
setwd("C:/Users/Mike/Desktop/Dropbox/surveys")
# setwd(choose.dir()) # interactively choose the directory that you want 

list.files() # see the files/folders inside your working directory 

data = read.csv("allsurveys+combined.csv") # import data matrix 

# check to see the classes of your variables 
# tells you if variables are numerical, factors, integers, etc.
# and it can be a problem if your variables are factors and you try to do histograms or scatterplots 
str(data)

#############################################################################################################
# GETTING A "ONEPERPOND" FILE / DATAFRAME
# using the "combined" values, where I averaged multiple surveys into one observation in Excel 
#############################################################################################################
# if a waterbody has multiple surveys, choose the "combined" value 
dataONEperpond <- subset(data, mult_survey == "N" | mult_survey == "Y" & year == "comb")

# remove 2004 CAES surveys - these did not record presence/absence of FP
dataONEperpond <- subset(dataONEperpond, year != "2004")

# save the oneperpond dataframe to file  
write.csv(dataONEperpond, file = "oneperpond.csv", row.names = FALSE)

#############################################################################################################
# REMOVING "OUTLIER"
# get rid of Private Pond (5) Lower/West - TOTP_avg ~ 1.4 mg/L & FPcover_max ~ 1
# get rid of Scriner Pond - TOTP_avg ~ 0.5 mg /L & FPcover_max ~ 0.71 
#############################################################################################################
dataONEperpondoutliers <- subset(dataONEperpond, waterbody != "Scribner Pond")
dataONEperpondoutliers <- subset(dataONEperpondoutliers, waterbody != "Private Pond (5) Lower/West")

#############################################################################################################
# GETTING A "FLOATING PLANTS PRESENT" FILE / DATAFRAME
# only deal with ponds that have floating plants present 
#############################################################################################################
# an alternative way to subset that I won't use this time 
# dataFP <- dataONEperpond[which(FP_species_richness > 0),]

dataFP <- subset(dataONEperpond, FP_presence == "Y" | FP_presence == "Y/N")

write.csv(dataFP, file = "FP ponds only.csv", row.names = FALSE )                 

#############################################################################################################
# GETTING A "SMALL PONDS ONLY" FILE / DATAFRAME 
# only deal with ponds that have floating plants present and are small (<5 ha)
#############################################################################################################
# an alternative way to subset that I won't use this time 
# dataFP <- dataONEperpond[which(FP_species_richness > 0),]

dataFPsmall <- subset(dataFP, surfacearea_ha <= 5.0)

write.csv(dataFPsmall, file = "FP ponds less than 5ha.csv", row.names = FALSE )      

#############################################################################################################
# GETTING A "CAES" FILE / DATAFRAME
#############################################################################################################
# choose waterbodies surveyed by CAES with only one survey OR 
# if a waterbody has multiple surveys, choose the "combined" value 
dataCAES <- subset(data, data_source == "CAES" & mult_survey == "N" | data_source == "CAES" & mult_survey == "Y" & year == "comb")

# remove 2004 CAES surveys - these did not record presence/absence of FP
dataCAES <- subset(dataCAES, year != "2004")

# save the oneperpond dataframe to file  
write.csv(dataCAES, file = "dataCAES.csv", row.names = FALSE)

#############################################################################################################
# GETTING A "CAESFP" FILE / DATAFRAME
#############################################################################################################
dataCAESFP <- subset(dataCAES, FP_presence == "Y" | FP_presence == "Y/N")

write.csv(dataCAESFP, file = "CAES FP ponds only.csv", row.names = FALSE )                 

#############################################################################################################
# REMOVING "OUTLIER"
# get rid of Private Pond (5) Lower/West - TOTP_avg ~ 1.4 mg/L & FPcover_max ~ 1
# get rid of Scriner Pond - TOTP_avg ~ 0.5 mg /L & FPcover_max ~ 0.71 
#############################################################################################################
dataFPoutliers <- subset(dataFP, waterbody != "Scribner Pond")
dataFPoutliers <- subset(dataFPoutliers, waterbody != "Private Pond (5) Lower/West")

#############################################################################################################
# REMOVING "OUTLIER" - for small ponds 
# get rid of Private Pond (5) Lower/West - TOTP_avg ~ 1.4 mg/L & FPcover_max ~ 1
# get rid of Scriner Pond - TOTP_avg ~ 0.5 mg /L & FPcover_max ~ 0.71 
#############################################################################################################
dataFPoutlierssmall <- subset(dataFPsmall, waterbody != "Scribner Pond")
dataFPoutlierssmall <- subset(dataFPoutlierssmall, waterbody != "Private Pond (5) Lower/West")


#############################################################################################################
# GETTING AN  EXTRA"SMALL PONDS ONLY" FILE / DATAFRAME 
# only deal with ponds that have floating plants present and are small (<0.4 ha)
#############################################################################################################
dataFPextrasmall <- subset(dataFPsmall, surfacearea_ha <= 0.4)

write.csv(dataFPextrasmall, file = "FP ponds less than 0_4ha.csv", row.names = FALSE ) 

