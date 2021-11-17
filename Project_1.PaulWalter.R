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
    
  NUMERIC_VARIABLES = c('per 1,000 pop', 'Percent', 'Count', 'Dollars/capita', 'Dollars/store')
  
  # skewness(aboriginal_non_ratio, na.rm = TRUE)
  checkSkewness <- function( vectorToCheck ) {
    # last line is the return statement
    #print(vectorToCheck)
    skewness(vectorToCheck, na.rm = TRUE)
  }
  
  # OPEN, FILTER, and PIVOT.
  openFilterAndPivot <- function( csvToOpenVariable, csvToOpenData, textInVariablesToSearch ) {
    
    # ================================================================
    # IDENTIFY VARIABLES (By Year and Type)
    #
    #   @see https://www.statology.org/filter-rows-r/
    # ================================================================ 
    variableList <- read.csv(csvToOpenVariable) 
    
    filteredByType <- filter(variableList, Units %in% NUMERIC_VARIABLES )
    
    filteredByYear <- filter(filteredByType, grepl(textInVariablesToSearch, Variable_Name) )
    
    print(filteredByYear)
    
    filteredByYear_VariableCode = filteredByYear$Variable_Code
    
    filteredByYear_VariableCode
    
    # ================================================================
    # FILTER DATA BY SELECTED VARIABLES
    # ================================================================ 
    dataStateAndCounty <- read.csv(csvToOpenData) 
    dataStateAndCountyFiltered <- filter( dataStateAndCounty, Variable_Code %in% filteredByYear_VariableCode)
    
    # ================================================================
    # PIVOT THE DATA
    #
    #   The data has a list of all the variables as rows, we need these as columns
    #
    #   @see https://tidyr.tidyverse.org/articles/pivot.html#wider
    # ================================================================
    pivoted <- pivot_wider(dataStateAndCountyFiltered, names_from = Variable_Code , values_from = Value)
    # pivoted <- na.omit(pivoted)
    # write.csv(pivoted, "data/output_pivoted.csv")
  }
  
  
  #Set the number of numeric digits to work with
  options(digits = 9)
  
  #pivoted = openFilterAndPivot( "data/VariableList.csv", "data/StateAndCountyData.csv", "2015");
  
  pivoted = openFilterAndPivot( "data/VariableList.csv", "data/StateAndCountyData.csv", "2016");
  
  # ================================================================
  # FILTER DATA BY ACCEPTABLE SKEWNESS
  # ================================================================ 
  
  # ignore the first 4 columns
  newdata <- pivoted[,4:ncol(pivoted)]
  newdata <- na.omit(newdata) # Get rid of rows with no data.
  
  #sapply(newdata, checkSkewness )
  
  # PCA=princomp(newdata,
  #              scale = TRUE, # this will scale the values that are too big.
  #              cor = TRUE, 
  #              scores = TRUE
  # )
  # 
  # 
  # summary(PCA)
  # loadings(PCA)
  # 
  # 
  # ## make a scree plot
  # PCA.var <- PCA$sdev^2
  # PCA.var.per <- round(PCA.var/sum(PCA.var)*100, 8) #To compute variance for a given PC, the eigenvalue for that PC must be divided by the sum of eigenvalues
  # 
  # barplot(PCA.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")
  
  # apply to all the columns https://stackoverflow.com/questions/7303322/apply-function-to-each-column-in-a-data-frame-observing-each-columns-existing-da
  #newdata2 <-sapply(newdata, log10 )
  #newdata2 <- na.omit(newdata2)
  #write.csv(newdata2, "data/output_newdata2.csv")
  #newdata2
  #sapply(newdata2, checkSkewness )
  
  
  # lapply(dataStateAndCountyFiltered$Value, checkSkewness)
  #by_county <- dataStateAndCountyFiltered %>% group_by(County)