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

# ================================================================
# Methods
# ================================================================@ 

NUMERIC_VARIABLES = c('per 1,000 pop', 'Percent', 'Count', 'Dollars/capita', 'Dollars/store')

# skewness(aboriginal_non_ratio, na.rm = TRUE)
checkSkewness <- function( vectorToCheck ) {
  # last line is the return statement
  skewness(vectorToCheck, na.rm = TRUE)
}

filterRows()


  # ================================================================
  # Find Variables
  #
  #   @see https://www.statology.org/filter-rows-r/
  # ================================================================ 


	
  #Set the number of numeric digits to work with
	options(digits = 9)
  
  variableList <- read.csv("data/VariableList.csv") 
	
  attach(variableList)
  
	
  # FILTER ON YEAR
  #   variableList %>% filter(Units == 'Percent')
  #   variableList %>% filter(grepl('2016', Variable_Name) & Units == 'Percent') 
  filteredByType <- filter(variableList, Units %in% NUMERIC_VARIABLES )
  
  filteredByYear <- filter(filteredByType, grepl('2016', Variable_Name) )
  
  #variableList %>% filter(Units == '# per 1,000 pop' & grepl('2016', Variable_Name)) 
  ## 
  
  filteredByYear
  
  # ================================================================
  # Get found  Variables
  # 
  #   NOTE:   We can also filter for rows where the eye color is in a list of colors:
  #   
  #           starwars %>% filter(eye_color %in% c('blue', 'yellow', 'red'))
  # ================================================================ 
  dataStateAndCounty <- read.csv("data/StateAndCountyData.csv") 
  dataStateAndCounty %>% select(Variable_Code %in% variableListFiltered$Variable_Code)
  
  