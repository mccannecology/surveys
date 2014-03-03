##############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA   #
# Summary stats                              #
# UPDATED: 03/03/2014 by MJ McCann           #
##############################################

# stats on waterbody sizes in my surveys 
summary(subset(dataONEperpond, dataONEperpond$data_source == "MJM"))

# stats on waterbody sizes in CAES surveys 
summary(subset(dataONEperpond, dataONEperpond$data_source == "CAES" | dataONEperpond$data_source == "CAES+MJM"))

# dates of waterbody surveys - me and CAES 
# I think it may be easier to just look through the .xls or .csv file for this one 
summary(dataONEperpond$date) # problem with this: some surveys have already been combined 
summary(data$date) 

# number of waterbodies (dataONEperpond) from CAES - #183
nrow(subset(dataONEperpond, dataONEperpond$data_source == "CAES" |dataONEperpond$data_source == "CAES/MJM"))

# number of waterbodies (dataONEperpond) from me - #21
nrow(subset(dataONEperpond, dataONEperpond$data_source == "MJM"))

# number of waterbodies without floating plants 
nrow(dataONEperpond) - nrow(dataFP)

# number of waterbodies (dataONEperpond) with FP cover> 66.666%
nrow(subset(dataONEperpond, dataONEperpond$FPcover_max >= 0.66666))

# number of waterbodies (dataONEperpond) with FP cover> 1%
nrow(subset(dataONEperpond, dataONEperpond$FPcover_max <= 0.01))

# number of waterbodies (dataONEperpond) with surface area < 5 ha
nrow(subset(dataONEperpond, dataONEperpond$surfacearea_ha <= 5)) # 79

# number of waterbodies (dataONEperpond) with surface area < 10 ha
nrow(subset(dataONEperpond, dataONEperpond$surfacearea_ha <= 10)) # 107

# number of waterbodies (dataONEperpond) with surface area < 50 ha
nrow(subset(dataONEperpond, dataONEperpond$surfacearea_ha <= 50)) # 169

# number of wataerbodies without FP (dataONEperpond)
nrow(subset(dataONEperpond, dataONEperpond$FP_species_richness == 0))

# largest waterbody with FP cover > 66.66% (dataONEperpond)
summary(subset(dataONEperpond, dataONEperpond$FPcover_max >= 0.66666))

# 20 waterbodies in dataFPsmall Were based on a combination of surveys 
combineddataFPsmall <- subset(dataFPsmall, dataFPsmall$year == "comb")
combineddataFPsmall$waterbody 

d# Is FP richness = 0 more likely then we would expect? 
# I did the G-test in Excel and the answer is Yes. (But this might not really be an interesting question)