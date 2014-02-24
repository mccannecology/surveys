# Manuscript Figure 1
# 2 x 3 panes 
# top row: hypothetical relationship between total P and FP cover 
# bottom row: data and model fits of total P and FP cover 

# Set working directory 
setwd("C:/Users/Mike/Desktop/Dropbox/CT & LI  Duckweed Surveys/Survey Analysis All") 

library(ggplot2)

# create a blank plot - but you cannot view it until you add a line 
f <- ggplot(data.frame(x = c(0, 100)), aes(x)) 

# describe a function here 
linear <- function(x) {x}

# add the function to the plot 
f + stat_function(fun = linear)


# make the 6 plots then 
# arrange plots using either plot.arrange() or grid.arrange()