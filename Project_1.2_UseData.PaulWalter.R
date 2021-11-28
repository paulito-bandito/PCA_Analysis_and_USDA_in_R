# ================================================================
# AUTHOR: Paul Walter
# DATE:   11/28/2021 	 
# ASSIGNMENT: Final Project, create usable data for my linear model
# ================================================================

#Load software packages 

library(dplyr)
library(ggplot2)
library(corrplot)
library(psych)
library(car)
library(broom)
library(foreign)

# ================================================================
# Next, load your data:
# ================================================================

#Load spreadsheet into data frame 

#Set your working directory, so R knows where to look for files. Choose the LAB2 folder you just made.
# setwd(choose.dir())
# setwd("d:\\rfiles\\myfiles")


#Set the number of numeric digits to work with
FOLDER_LOCATION = "data/refined2/"
options(digits = 9)
dataToUse <- read.csv("data/refined2/startingData.csv")
#This reads your csv file into R and names it "data"
attach(dataToUse) 

# ================================================================
# We will start our analysis in R at Step 2: Identify explanatory 
# (i.e., independent) variables [x].
# ================================================================

#Calculate correlation between variables  23 - 35
correlation_P <- cor(select(dataToUse,7, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35), use="complete.obs", method="pearson") 
write.csv(correlation_P, "data/refined2/correlation_P.csv") 
corrplot(correlation_P, type = "upper", order = "hclust", method = "color", 
         tl.cex = 0.7, tl.col = "black", tl.srt = 45)

print("Correlation plot written to file.")
# # ================================================================
# # Open up the spreadsheet "correlation_P.csv" and save is as an 
# # Excel Workbook (.xlsx) use the Conditional Formatting tip 
# # (see Step 2 above) to explore the correlation between variables.
# # ================================================================
# 
# Section blank on purpose: read comment above. 
