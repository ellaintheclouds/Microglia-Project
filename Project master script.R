# 0 Set up ---------------------------------------------------------------------
#setwd("C:/Users/El Richardson/OneDrive - Lancaster University/Biomedicine/Year 3
#      /387 Project/Lab work/Project Working Directory")

# Install packages
if (!require("car", quietly = TRUE)){install.packages("car")}
if (!require("AICcmodavg", quietly = TRUE)){install.packages("AICcmodavg")} 
if (!require("ggplot2", quietly = TRUE)){install.packages("ggplot2")} 
if (!require("gridExtra", quietly = TRUE)){install.packages("gridExtra")} 
if (!require("ggsignif", quietly = TRUE)){install.packages("ggsignif")} 

# Libraries for statistics
library(car) # for Levene's test
library(AICcmodavg) # for Akaike information criterion (AIC) test for models

# Libraries for plotting
library(ggplot2) # for creating plots
library(gridExtra) # allows multiple graphs to be arranged nicely in one image
library(ggsignif) # draws asterisks on ggplot plots to indicate significance


# 1 Data processing ------------------------------------------------------------
source("data processing function.R")
process_data("Input/microglia_data.csv")


# 2 Statistics -----------------------------------------------------------------
source("statistics script.R")


# 3 Significance testing -------------------------------------------------------
source("significance script.R")


# 4 Creating plots -------------------------------------------------------------
# Plotting
source("bar plot script.R")

# Arranging plots for publication
source("plot arrange script.R")
