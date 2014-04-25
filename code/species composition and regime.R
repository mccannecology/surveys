######################################################################
# Table: FP species richness, composition, frequency, and FP regime  #
#                                                                    #
#                                                                    #
######################################################################
library(plyr)

# This is the data frame to use 
# Check that it hasn't been modified since it was originally imported (or just re-import)
head(dataONEperpond)

# Floating plant species richness
dataONEperpond$FP_species_richness

# Function that takes the 0,1s in each floating plant column
# and converts them to a string of the species names 
# x in this function will be dataONEperpond 
# if there are no floating plants present, then nothing is returned 
fun <- function(x) {  
  composition <- NULL 
  for (i in 26:35) {
    if(x[,i] == 1) {
      composition <- paste(composition,names(x)[i],sep=" ")
    } 
  }
  return(composition)
}

# build a vector of all of the species composition names 
sp_composition <- NULL 
for (i in 1:nrow(dataONEperpond)){
  sp_composition <- append(sp_composition,fun(dataONEperpond[i,]))
}
sp_composition

# assign those species composition names to dataONEperpond 
dataONEperpond$FP_species_composition[dataONEperpond$FP_species_richness > 0] <- sp_composition

# re-name species composition labels 
# convert to 1 or 2 letters 
# combine things that are the same species - e.g., Wolffia and Wolffia brasiliensis 
dataONEperpond$FP_species_composition2[dataONEperpond$FP_species_composition == " azolla"] <- "A" 
dataONEperpond$FP_species_composition2[dataONEperpond$FP_species_composition == " azolla wolffia"] <- "A,W"
dataONEperpond$FP_species_composition2[dataONEperpond$FP_species_composition == " lemna_minor lemna_trisulca spirodela_polyrhiz"] <- "LM,LT,SP"
dataONEperpond$FP_species_composition2[dataONEperpond$FP_species_composition == " lemna_minor riccia spirodela_polyrhiza wolffia_brasiliensis"] <- "LM,R,SP,W"
dataONEperpond$FP_species_composition2[dataONEperpond$FP_species_composition == " lemna_minor spirodela_polyrhiza wolffia"] <- "LM,SP,W"
dataONEperpond$FP_species_composition2[dataONEperpond$FP_species_composition == " lemna_minor wolffia"] <- "LM,W"
dataONEperpond$FP_species_composition2[dataONEperpond$FP_species_composition == " lemna_trisulca riccia"] <- "LT,R"
dataONEperpond$FP_species_composition2[dataONEperpond$FP_species_composition == " spirodela_polyrhiza"] <- "SP"
dataONEperpond$FP_species_composition2[dataONEperpond$FP_species_composition == " spirodela_polyrhiza wolffia_brasiliensis"] <- "SP,W"
dataONEperpond$FP_species_composition2[dataONEperpond$FP_species_composition == " wolffia_brasiliensis"] <- "W"
dataONEperpond$FP_species_composition2[dataONEperpond$FP_species_composition == " azolla lemna_minor spirodela_polyrhiza wolffia"] <- "A,LM,SP,W"
dataONEperpond$FP_species_composition2[dataONEperpond$FP_species_composition == " lemna_minor"] <- "LM"
dataONEperpond$FP_species_composition2[dataONEperpond$FP_species_composition == " lemna_minor lemna_valdiviana spirodela_polyrhiza wolffia_brasiliensis"] <- "LM,LV,SP,W"
dataONEperpond$FP_species_composition2[dataONEperpond$FP_species_composition == " lemna_minor spirodela_polyrhiza "] <- "LM, SP"
dataONEperpond$FP_species_composition2[dataONEperpond$FP_species_composition == " lemna_minor spirodela_polyrhiza wolffia_brasiliensis"] <- "LM,SP,W"
dataONEperpond$FP_species_composition2[dataONEperpond$FP_species_composition == " lemna_minor wolffia_brasiliensis"] <- "LM,W"
dataONEperpond$FP_species_composition2[dataONEperpond$FP_species_composition == " lemna_trisulca spirodela_polyrhiza wolffia"] <- "LT,SP,W"
dataONEperpond$FP_species_composition2[dataONEperpond$FP_species_composition == " spirodela_polyrhiza wolffia "] <- "SP,W"
dataONEperpond$FP_species_composition2[dataONEperpond$FP_species_composition == " wolffia"] <- "W"

# Make a table of the different FP compositions 
FP_comp_freq <- table(dataONEperpond$FP_species_composition2)

# Add a variable to dataONEperpond that indicates if a waterbody is FP regime or not 
dataONEperpond$FP_regime[dataONEperpond$FPcover_max >= 0.66667] <- "FP"
dataONEperpond$FP_regime[dataONEperpond$FPcover_max < 0.66667] <- "notFP"

# using basic R to make a table
# structure is strange 
temp <- as.data.frame(table(dataONEperpond$FP_species_composition2,dataONEperpond$FP_regime))

# re-organize the table into a data frame 
temp <- as.data.frame(cbind(as.vector(temp[1:13,1]),as.numeric(temp[1:13,3]),as.numeric(temp[14:26,3])))
names(temp) <- c("composition","FP","notFP")

# fix the classes of the data frame columns 
temp$FP <- as.numeric(levels(temp$FP))[temp$FP]
temp$notFP <- as.numeric(levels(temp$notFP))[temp$notFP]

# add the FP richness column 
temp$richness <- c(1,4,2,1,4,4,3,2,2,3,1,2,1)
temp <- temp[order(temp$richness,decreasing=T),]
temp

###################
# Permutation     #
# Set up the data # 
###################
temp2 <- data.frame(temp,row.names=NULL)

temp2$group <- NA

# combine all of the species richness = 4
temp2[14,1] <- NA
temp2[14,2] <- 1
temp2[14,3] <- 2
temp2[14,4] <- 4 
temp2[14,5] <- "richness4"

temp2 <- temp2[-1,]
temp2 <- temp2[-1,]
temp2 <- temp2[-1,]

# combine all of the species richness = 3
temp2[12,1] <- NA
temp2[12,2] <- 7
temp2[12,3] <- 12
temp2[12,4] <- 3 
temp2[12,5] <- "richness3"

temp2 <- temp2[-1,]
temp2 <- temp2[-1,]

row.names(temp2) <- seq(1,nrow(temp2),1)

# combine all of the species richness = 2 
temp2[11,1] <- NA
temp2[11,2] <- 3
temp2[11,3] <- 14
temp2[11,4] <- 2
temp2[11,5] <- "richness2"

temp2 <- temp2[-1,]
temp2 <- temp2[-1,]
temp2 <- temp2[-1,]
temp2 <- temp2[-1,]

row.names(temp2) <- seq(1,nrow(temp2),1)

# keep the species richness = 1 separate 
# remove azolla 
temp2 <- temp2[-1,]

# rename the single species 
temp2$group[1] <- "LM"
temp2$group[2] <- "SP"
temp2$group[3] <- "W"

# remoe the composition column
temp2 <- temp2[,-1]
temp2 <- temp2[,-3]

temp2

# observed FP 
obsvFP <- sum(temp2$FP)
# observed notFP
obsvnotFP <- sum(temp2$notFP)

###### PROBLEM #######
# there are fewer observations than the nrow(dataFP)


# create vector of FP regimes to sample from
states <- c(rep("FP",obsvFP),rep("notFP",obsvnotFP))


