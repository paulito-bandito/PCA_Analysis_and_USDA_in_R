# ================================================================
# AUTHOR: Paul Walter
# DATE:   10/27/2021 	 
# ASSIGNMENT: Lab 4: Time Series
# LEARNING OBJECTIVES:
#   -	Review the concepts of regression.
#   -	Review the concept of classic hypothesis testing.
#   -	Develop a statistical model using a linear regression to describe the relationship between dependent and independent variables.
#   -	Critically analyze the statistical model you developed. 
# ================================================================

#Load software packages 


	library(dplyr)
	library(ggplot2)
	library(corrplot)
	library(psych)
	library(car)
	library(broom)
	library(foreign)
  library(forecast)
  
  # checking for skewness
  # @ see https://www.datanovia.com/en/lessons/transform-data-to-normal-distribution-in-r/
  library(moments)

  # Pivot Tables (wider)
  library(tidyr)


  # PCA
  library("FactoMineR")
  library("factoextra")


  # ================================================================
  # METHODS
  # ================================================================@ 
  
  ACCEPTABLE_SKEW = 1
    
  NUMERIC_VARIABLES = c('per 1,000 pop', 'Percent', 'Count', 'Dollars/capita', 'Dollars/store', "Dollars")
  
  # skewness(aboriginal_non_ratio, na.rm = TRUE)
  checkSkewness <- function( vectorToCheck ) {
    # last line is the return statement
    #print(vectorToCheck)
    skewness(vectorToCheck, na.rm = TRUE)
  }
  
  # ================================================================
  # LOAD DATA
  # ================================================================@ 
  
  usdaData = read.csv("data/refined/StateAndCountyData_pivoted.colsRemoved.csv") 
  #socialDeprivationIndex = read.csv("data/refined/ACS2015_countyallvars.csv") 
  
  # ================================================================
  # CORRELATION
  # ================================================================@ 
  filteredData <- usdaData[,5:ncol(usdaData)]
  
  correlation_P <- cor(filteredData, use="complete.obs", method="pearson") 
  write.csv(correlation_P, "data/refined/correlation_usdaData.csv") 
 