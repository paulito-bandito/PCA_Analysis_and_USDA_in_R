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

options(digits = 9)
dataToUse <- read.csv("data/refined2/startingData.csv")
#This reads your csv file into R and names it "data"
attach(dataToUse) 

names(dataToUse)



# ================================================================
# We will start our analysis in R at Step 2: Identify explanatory 
# (i.e., independent) variables [x].
# ================================================================

#Calculate correlation between variables  23 - 35
correlation_P <- cor(select(dataToUse,7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22), use="complete.obs", method="pearson")
write.csv(correlation_P, "data/refined2/correlation_P.3.csv")
corrplot(correlation_P, type = "upper", order = "hclust", method = "color",
         tl.cex = 0.7, tl.col = "black", tl.srt = 45)

print("Correlation plot written to file.")
# # ================================================================
# # Open up the spreadsheet "correlation_P.csv" and save is as an 
# # Excel Workbook (.xlsx) use the Conditional Formatting tip 
# # (see Step 2 above) to explore the correlation between variables.
# # ================================================================
  
  # Section blank on purpose: read comment above. 

# # ================================================================
# # Next, run your initial linear regression model with all the 
# # independent variables left after Step 2. 
# # ================================================================
# 
# 	#Initial Regression model, name "modela"
# 	#First, insert all non-corrrelated variables

model_a <- lm( PctFFood ~  population + fpl_100_score +  sing_parent_fam_score + black_score +  hisp_score +
                           dropout_score + no_car_score + rent_occup_score + crowding_score + nonemp_score +
                           highneeds_score + foreignb_score,
               data = dataToUse)




summary(model_a) 


dataToUse$residuals <- residuals(model_a)
dataToUse$fitted <- fitted(model_a)

# Test normality of residuals with Shapiro-Wilk's W test
# in order to understand the p-value you have to understand what the corresponding statistical test is actually testing.
#
# In case of the Shapiro-Wilk Normality Test the null hypothesis is the underlying data has a normal distribution. The p-value then measures (more or less) how likely this is. Often we accept the null hypothesis if the p-value is greater or equal than 0.05. This means that in only 5% of the cases we reject the null hypothesis although it would be correct (Type I error).
shapiro.test(dataToUse$residuals)
