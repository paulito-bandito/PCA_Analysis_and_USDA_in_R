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

  # ================================================================
  # METHODS
  # ================================================================@ 
  
  ACCEPTABLE_SKEW = 1
    
  NUMERIC_VARIABLES = c('per 1,000 pop', 'Percent', 'Count', 'Dollars/capita', 'Dollars/store')
  
  # skewness(aboriginal_non_ratio, na.rm = TRUE)
  checkSkewness <- function( vectorToCheck ) {
    # last line is the return statement
    #print(vectorToCheck)
    skewness(vectorToCheck, na.rm = TRUE)
  }
  
  # ================================================================
  # IDENTIFY VARIABLES (By Year and Type)
  #
  #   @see https://www.statology.org/filter-rows-r/
  # ================================================================ 


	
  #Set the number of numeric digits to work with
	options(digits = 9)
  
  variableList <- read.csv("data/VariableList.csv") 
	
  filteredByType <- filter(variableList, Units %in% NUMERIC_VARIABLES )
  
  filteredByYear <- filter(filteredByType, grepl('2015', Variable_Name) )
  
  filteredByYear_VariableCode = filteredByYear$Variable_Code
  
  filteredByYear_VariableCode
  
  # ================================================================
  # FILTER DATA BY SELECTED VARIABLES
  # ================================================================ 
  dataStateAndCounty <- read.csv("data/StateAndCountyData.csv") 
  dataStateAndCountyFiltered <- filter( dataStateAndCounty, Variable_Code %in% filteredByYear_VariableCode)
  
  # ================================================================
  # PIVOT THE DATA
  #
  #   The data has a list of all the variables as rows, we need these as columns
  #
  #   @see https://tidyr.tidyverse.org/articles/pivot.html#wider
  # ================================================================
  pivoted <- pivot_wider(dataStateAndCountyFiltered, names_from = Variable_Code , values_from = Value)
  #write.csv(pivoted, "data/output_pivoted.csv")
  # ================================================================
  # FILTER DATA BY ACCEPTABLE SKEWNESS
  # ================================================================ 
  
  # ignore the first 4 columns
  newdata <- pivoted[,4:ncol(pivoted)]
  newdata
  
  # apply to all the columns https://stackoverflow.com/questions/7303322/apply-function-to-each-column-in-a-data-frame-observing-each-columns-existing-da
  sapply(newdata, checkSkewness )
  
  # lapply(dataStateAndCountyFiltered$Value, checkSkewness)
  #by_county <- dataStateAndCountyFiltered %>% group_by(County)