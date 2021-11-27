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
  
  interpInterveningYears <- function( tableToUse, varsToInterpolate ){
    #@ https://stackoverflow.com/questions/33186316/linear-interpolate-missing-values-in-time-series
    
    #  @ see R example in RStudio after you type ?approx()
    
    # z <- zoo(c(2, NA, NA, NA, NA, 3), c(1, 3, 4, 6, 7, 8))
    # z
    # 
    # ## use equidistant spacing
    # na.approx(z, 1:6)
    
    
    numVars <- length(varsToInterpolate)
    s <- seq(1, numVars)
    
    tableToUse # return statement
  }
  
  #Set the number of numeric digits to work with
  options(digits = 9)
  
  # get most of the variables
  #pivoted2015 = openFilterTwiceAndPivot( "data/VariableList.csv", "data/StateAndCountyData.csv", "2015", NUMERIC_VARIABLES, TRUE);
  pivoted = openFilterByColNamesAndPivot( "data/StateAndCountyData.csv",  c("FFRPTH11", "FFRPTH16"), FALSE);
  
  
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
  dependentVars = merged_files[,"FFRPTH11"]
  dependentVars = mutate(dependentVars, FFRPTH12 = NA )
  dependentVars = mutate(dependentVars, FFRPTH13 = NA )
  dependentVars = mutate(dependentVars, FFRPTH14 = NA )
  dependentVars = mutate(dependentVars, FFRPTH15 = NA )
  #dependentVars  = mutate(dependentVars,  merged_files[,"FFRPTH11"] )
  dependentVars
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
  # # merged_files = sapply( merged_files, interpInterveningYears, c("FFRPTH11", "FFRPTH12", "FFRPTH13", "FFRPTH14", "FFRPTH15", "FFRPTH16") )
  # # merged_files
  # 
  # # ================================================================
  # # Interpolate between the 2011 & 2016 Grocery Store columns to find 2015
  # # ================================================================ 
  # 
  # 
  # # ignore the first 4 columns
  # # newdata <- pivoted[,4:ncol(pivoted)]
  # # newdata <- na.omit(newdata) # Get rid of rows with no data.
  # # 
  # #sapply(newdata, checkSkewness )
  # 
  # # PCA=princomp(newdata,
  # #              scale = TRUE, # this will scale the values that are too big.
  # #              cor = TRUE, 
  # #              scores = TRUE
  # # )
  # # 
  # # 
  # # summary(PCA)
  # # loadings(PCA)
  # # 
  # # 
  # # ## make a scree plot
  # # PCA.var <- PCA$sdev^2
  # # PCA.var.per <- round(PCA.var/sum(PCA.var)*100, 8) #To compute variance for a given PC, the eigenvalue for that PC must be divided by the sum of eigenvalues
  # # 
  # # barplot(PCA.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")
  # 
  # # apply to all the columns https://stackoverflow.com/questions/7303322/apply-function-to-each-column-in-a-data-frame-observing-each-columns-existing-da
  # #newdata2 <-sapply(newdata, log10 )
  # #newdata2 <- na.omit(newdata2)
  # #write.csv(newdata2, "data/output_newdata2.csv")
  # #newdata2
  # #sapply(newdata2, checkSkewness )
  # 
  # 
  # # lapply(dataStateAndCountyFiltered$Value, checkSkewness)
  # #by_county <- dataStateAndCountyFiltered %>% group_by(County)