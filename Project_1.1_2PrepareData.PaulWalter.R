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

  # 
  library("zoo")

  # PCA
  library("FactoMineR")
  library("factoextra")


  # ================================================================
  # METHODS
  # ================================================================@ 
  
  ACCEPTABLE_SKEW = 1
    
  # skewness(aboriginal_non_ratio, na.rm = TRUE)
  checkSkewness <- function( vectorToCheck ) {
    # last line is the return statement
    #print(vectorToCheck)
    skewness(vectorToCheck, na.rm = TRUE)
  }
  
  openFilterByColNamesAndPivot <- function( csvToOpenData, vectorOfColNames, isVerbose = FALSE ) {
    
    variableList <- read.csv(csvToOpenData) 
    
    variableList <- filter(variableList, Variable_Code %in%  vectorOfColNames)
    
    if( isVerbose){
      print(variableList)
    }
    
    filteredByYear_VariableCode = variableList$Variable_Code
    
    if( isVerbose){
      print(filteredByYear_VariableCode)
    }
    
    # ================================================================
    # PIVOT THE DATA
    #
    #   The data has a list of all the variables as rows, we need these as columns
    #
    #   @see https://tidyr.tidyverse.org/articles/pivot.html#wider
    # ================================================================
    pivoted <- pivot_wider(variableList, names_from = Variable_Code , values_from = Value)
  }
  
  interpInterveningYears <- function( vectorOfData ){
    #@ https://stackoverflow.com/questions/33186316/linear-interpolate-missing-values-in-time-series
    
    #  @ see R example in RStudio after you type ?approx()
    
    # z <- zoo(c(2, NA, NA, NA, NA, 3), c(1, 3, 4, 6, 7, 8))
    # z
    # 
    # ## use equidistant spacing
    # na.approx(z, 1:6)
    
    
    # dont' change
    newVect = vectorOfData[c("FFRPTH11","FFRPTH12","FFRPTH13","FFRPTH14","FFRPTH15","FFRPTH16")]
    
    #z <- zoo(newVect, c(1, 6, 2, 3, 4, 5))

    ## use equidistant spacing
    approximated = na.approx(newVect, 1:6)
    
    print(approximated[3])
    
    # vectorOfData[1, 4] <-
    vectorOfData[9] = 0.555
    
    print(vectorOfData)
    
    vectorOfData
  }
  
  #Set the number of numeric digits to work with
  options(digits = 9)
  
  # get most of the variables
  #pivoted2015 = openFilterTwiceAndPivot( "data/VariableList.csv", "data/StateAndCountyData.csv", "2015", NUMERIC_VARIABLES, TRUE);
  pivoted = openFilterByColNamesAndPivot( "data/StateAndCountyData.csv",  c("FFRPTH11", "FFRPTH16"), FALSE);
  pivoted
  
  # write.csv(pivoted, "data/StateAndCountyData_pivoted.2.csv")
  # 
  # # get variables that have to be derived (the counts, minus the populationin the county)
  # #pivoted2016 = openFilterTwiceAndPivot( "data/VariableList.csv", "data/StateAndCountyData.csv", "2016", c("Count"), FALSE);
  # #pivotedPopulationPerCounty = openFilterAndPivot( "data/SupplementalDataCounty.csv", "2016", FALSE);
  # socialDeprivationIndex = read.csv("data/ACS2015_countyallvars.csv") 
  # 
  # merged_files<-merge(x=pivoted2015,y=socialDeprivationIndex,by="FIPS",all.x=FALSE, all.y=FALSE)
  # 
  # merged_files
  # 
  # 
  # ================================================================
  # Add a new column for Fast Food pct per 1000
  #
  #   example column:
  #   Fast-food restaurants/1,000 pop, 2011	Restaurant Availability and Expenditures	RESTAURANTS	Fast-food	FFRPTH11	CNTY10	# per 1,000 pop
  #
  # ================================================================

  # HYPOTHESIS
  # If a country has high Social Deprivation Indices, it will also have a higher ratio of Fast
  # Food to Grocery stores as well.
  #

  # add for fast food FFRPTH**  bewtween 2011 & 2016
  pivoted = mutate(pivoted, FFRPTH12 = NA )
  pivoted = mutate(pivoted, FFRPTH13 = NA )
  pivoted = mutate(pivoted, FFRPTH14 = NA )
  pivoted = mutate(pivoted, FFRPTH15 = NA )
  
  pivoted
  # 
  # 
  # # Add for grocery store GROC** bewtween 2011 & 2016
  # 
  # # dependentVars = mutate(dependentVars, GROC12 = NA ) 
  # # dependentVars = mutate(dependentVars, GROC13 = NA ) 
  # # dependentVars = mutate(dependentVars, GROC14 = NA ) 
  # # dependentVars = mutate(dependentVars, GROC15 = NA ) 
  # 
  # dependentVars
  # 
  # # ================================================================
  # # Interpolate between the 2011 & 2016 Fast food columns to find 2015
  # # ================================================================ 
  # # @see https://stackoverflow.com/questions/33186316/linear-interpolate-missing-values-in-time-series
  # 
  # 
  
  APPLY_TO_ROWS = 1
  newVect = apply( pivoted, APPLY_TO_ROWS, interpInterveningYears )
  write.csv(pivoted, "data/newVect.csv")
  
  