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
  library(forecast)
  

  # Pivot Tables (wider)
  library(tidyr)


  # ================================================================
  # METHODS
  # ================================================================
  
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
  
  #Set the number of numeric digits to work with
  options(digits = 9)
  
  # get most of the variables
  #pivoted2015 = openFilterTwiceAndPivot( "data/VariableList.csv", "data/StateAndCountyData.csv", "2015", NUMERIC_VARIABLES, TRUE);
  pivoted = openFilterByColNamesAndPivot( "data/StateAndCountyData.csv",  c("FFRPTH16", "GROCPTH16"), FALSE);
  
  # ================================================================
  # Add a new column for Fast Food pct per 1000
  # ================================================================

  pivoted = mutate(pivoted, PctFFood =  FFRPTH16 / ( GROCPTH16 + FFRPTH16 ) ) # TODO: interpolate these values
  
  # Clean any NA values, TODO: create a method that is tolerant of zeros
  pivoted = na.omit(pivoted)
  
  # ================================================================
  # Merge the Social Deprivation Index variables into this dataset
  # ================================================================
  
  socialDeprivationIndex = read.csv("data/ACS2015_countyallvars.csv")
  
  merged_files<-merge(x=pivoted,y=socialDeprivationIndex,by="FIPS",all.x=FALSE, all.y=FALSE)
  
  # ================================================================
  # Write to file
  # ================================================================
  write.csv(merged_files, "data/refined2/startingData.csv")
  print("Test file created at 'data/refined2/startingData.csv'")
  