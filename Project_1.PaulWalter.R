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
# Find Variables
#
#   @see https://www.statology.org/filter-rows-r/
# ================================================================@ 


	
  #Set the number of numeric digits to work with
	options(digits = 9)
  
  variableList <- read.csv("data/VariableList.csv") 
	
  attach(variableList)
  names(variableList)
	#variableList %>% filter(Units == 'Percent')
  variableList %>% filter(grepl('2015', Variable_Name) & Units == 'Percent') 