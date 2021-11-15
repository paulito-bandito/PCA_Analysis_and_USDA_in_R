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
# Next, load your data:
# ================================================================

	#Load spreadsheet into data frame 
	
  #Set your working directory, so R knows where to look for files. Choose the LAB2 folder you just made.
	# setwd(choose.dir())
  # setwd("d:\\rfiles\\myfiles")

	
  #Set the number of numeric digits to work with
	options(digits = 9)
	data <- read.csv("data/census_2016_calgary_data_clean1.csv") 
	
	#This reads your csv file into R and names it "data"
	attach(data)
	
	# NOTE: What is a large skewness?
	#
	#     @ see https://www.spcforexcel.com/knowledge/basic-statistics/are-skewness-and-kurtosis-useful-statistics
	#
	#     So, when is the skewness too much? The rule of thumb seems to be: If the skewness 
	#     is between -0.5 and 0.5, the data are fairly symmetrical. If the 
	#     skewness is between -1 and - 0.5 or between 0.5 and 1, the data 
	#     are moderately skewed. If the skewness is less than -1 or 
	#     greater than 1, the data are highly skewed.
	#   
	skewness(aboriginal_non_ratio, na.rm = TRUE)
	skewness(age_0_14_prop, na.rm = TRUE)
	skewness(age_15_64_prop, na.rm = TRUE)
	skewness(age_65_up_prop, na.rm = TRUE)
	skewness(area_sq_km, na.rm = TRUE)
	skewness(citizen_non_ratio, na.rm = TRUE)
	skewness(employed_prop, na.rm = TRUE)
	skewness(english_prop, na.rm = TRUE)
	skewness(french_prop, na.rm = TRUE)
	skewness(gender_ratio, na.rm = TRUE)
	skewness(HH_Size, na.rm = TRUE)
	skewness(HS_prop, na.rm = TRUE)
	skewness(married_prop, na.rm = TRUE)
	skewness(med_hh_income, na.rm = TRUE)
	skewness(no_dip_prop, na.rm = TRUE)
	skewness(owner_prop, na.rm = TRUE)
	skewness(pop, na.rm = TRUE)
	skewness(pop_density, na.rm = TRUE)
	skewness(post_sec_prop, na.rm = TRUE)
	skewness(private_dwellings, na.rm = TRUE)
	skewness(renter_prop, na.rm = TRUE)
	skewness(single_prop, na.rm = TRUE)
	skewness(unemployed_prop, na.rm = TRUE)
  
  # transfomrations
  popLog10 = log10(pop);
  skewness(popLog10, na.rm = TRUE)
  
  private_dwellingsLog10 = log10(private_dwellings);
  skewness(private_dwellingsLog10, na.rm = TRUE)
  
  area_sq_kmLog10 = log10(area_sq_km);
  skewness(area_sq_kmLog10, na.rm = TRUE)
  
  post_sec_propLog10 = log10(max(post_sec_prop+1) - post_sec_prop) 
  skewness(post_sec_propLog10, na.rm = TRUE)
  
  citizen_non_ratioLog10 = log10(citizen_non_ratio);
  skewness(citizen_non_ratioLog10, na.rm = TRUE)
  
  aboriginal_non_ratioLog10 = log10(aboriginal_non_ratio);
  skewness(aboriginal_non_ratioLog10, na.rm = TRUE)
  
  gender_ratioLog10 = log10(gender_ratio);
  skewness(gender_ratioLog10, na.rm = TRUE)
  
  single_propLog10 = log10(single_prop);
  skewness(single_propLog10, na.rm = TRUE)
  
  married_propLog10 = log10(max(married_prop+1) - married_prop) 
  skewness(married_propLog10, na.rm = TRUE)
  
  HH_SizeLog10 = sqrt(max(HH_Size+1) - HH_Size)
  skewness(HH_SizeLog10, na.rm = TRUE)
  
  pop_densityLog10 = sqrt(max(pop_density+1) - pop_density)
  skewness(pop_densityLog10, na.rm = TRUE)
  
  med_hh_incomeSqrt = sqrt(max(med_hh_income+1) - med_hh_income)
  skewness(med_hh_incomeSqrt, na.rm = TRUE)
  
  owner_propLog10 = log10(max(owner_prop+1) - owner_prop) 
  skewness(owner_propLog10, na.rm = TRUE)
  
  renter_propLog10 = log10(max(renter_prop+1) - renter_prop) 
  skewness(renter_propLog10, na.rm = TRUE)
  
  english_propLog10 = log10(max(english_prop+1) - english_prop) 
  skewness(english_propLog10, na.rm = TRUE)
  
  french_propLog10 = log10(french_prop) 
  skewness(french_propLog10, na.rm = TRUE)
  
  age_15_64_propLog10 = log10(age_15_64_prop) 
  skewness(age_15_64_propLog10, na.rm = TRUE)
  
  age_65_up_propSqrt = sqrt(age_65_up_prop) 
  skewness(age_65_up_propLog10, na.rm = TRUE)
  
  no_dip_propLog10 = log10(no_dip_prop) 
  skewness(no_dip_propLog10, na.rm = TRUE)
  
  
  # Removed these ones because their skew isn't appropriate
  # 
	#   popLog10, private_dwellingsLog10, area_sq_km, gender_ratio, pop_density were wrong scale, 
  #   med_hh_income was too, owner_prop too skewed, renter_prop, english_prop,  was too skewed, 
	
	# Principal component analysis
	PCA=princomp(~ 
	               aboriginal_non_ratioLog10 + 
	               age_0_14_prop+ 
	               age_15_64_propLog10+ 
	               age_65_up_propSqrt+
	               citizen_non_ratioLog10+
	               employed_prop+
	               french_propLog10 +
	               HH_SizeLog10+
	               HS_prop+
	               no_dip_propLog10+
	               married_propLog10+
	               post_sec_propLog10 +
	               single_propLog10+
	               unemployed_prop,
	               scale = TRUE, # this will scale the values that are too big.
	               cor = TRUE, 
	               scores = TRUE
	                )
	
	
	summary(PCA)
	loadings(PCA)
	
	
	 ## make a scree plot
	 PCA.var <- PCA$sdev^2
	 PCA.var.per <- round(PCA.var/sum(PCA.var)*100, 8) #To compute variance for a given PC, the eigenvalue for that PC must be divided by the sum of eigenvalues
	 
	 barplot(PCA.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")
	 
	 # PRINT OUT EIGENVALUES (PCA$sdev^2) & The explained variance.
	 # write.csv( PCA.var.per, "PCA.var.per.csv")
	 # write.csv( PCA.var, "PCA.var.csv")
	 
	#scores used for linear regression
	PCA$scores
	View(PCA$scores)
	# 
	#export scores to excel
	write.csv(PCA$scores, "scores.csv")
	#copy the column "med_hh_income" and insert it into table "scores.csv",
	#rename and save as a text file " scores_income.txt".
	# 
	# #PCA model
	# #upload your scores and Median Household Income data.
	pcatable<-read.table("scores_income.txt",header=T)
	attach(pcatable)
	names(pcatable)
	# 
	#fit a multiple linear regression model
	#insert PCA components selected for analysis, use backwards selection model, removing one at a time
	#no need to start with a Pearson correlation command as in previous labs
	# inmodel=lm(med_hh_income ~ Comp.1 + Comp.2 + Comp.3+ Comp.4+ Comp.5+ Comp.6+ Comp.7+ Comp.8+ Comp.9+ Comp.10+ Comp.11+ Comp.12+ Comp.13+ Comp.14, data=pcatable)
	# 
	# # removed Comp.11
	# inmodel=lm(med_hh_income ~ Comp.1 + Comp.2 + Comp.3+ Comp.4+ Comp.5+ Comp.6+ Comp.7+ Comp.8+ Comp.9+ Comp.10+ Comp.12+ Comp.13+ Comp.14, data=pcatable)
	# 
	# # removed Comp.13
	# inmodel=lm(med_hh_income ~ Comp.1 + Comp.2 + Comp.3+ Comp.4+ Comp.5+ Comp.6+ Comp.7+ Comp.8+ Comp.9+ Comp.10+ Comp.12+ Comp.14, data=pcatable)
	# 
	# # removed Comp.4
	# inmodel=lm(med_hh_income ~ Comp.1 + Comp.2 + Comp.3+ Comp.5+ Comp.6+ Comp.7+ Comp.8+ Comp.9+ Comp.10+ Comp.12+ Comp.14, data=pcatable)
	# 
	# # removed Comp.12
	# inmodel=lm(med_hh_income ~ Comp.1 + Comp.2 + Comp.3+ Comp.5+ Comp.6+ Comp.7+ Comp.8+ Comp.9+ Comp.10+ Comp.14, data=pcatable)
	# 
	# # removed Comp.8
	# inmodel=lm(med_hh_income ~ Comp.1 + Comp.2 + Comp.3+ Comp.5+ Comp.6+ Comp.7+ Comp.9+ Comp.10+ Comp.14, data=pcatable)
	# 
	# # removed Comp.5
	# inmodel=lm(med_hh_income ~ Comp.1 + Comp.2 + Comp.3+ Comp.6+ Comp.7+ Comp.9+ Comp.10+ Comp.14, data=pcatable)
	# 
	# removed Comp.10
	#inmodel=lm(med_hh_income ~ Comp.1 + Comp.2 + Comp.3+ Comp.6+ Comp.7+ Comp.9+ Comp.14, data=pcatable)
	
	model3=lm(med_hh_income ~ Comp.1 + Comp.2 + Comp.3+ Comp.4+ Comp.5+ Comp.6+ Comp.7, data=pcatable)
	
	
	summary(model3)
	#continue producing model iterations and hypothesis testing until only significant components remain.
	# 
	
	model1 <- lm(med_hh_income ~ owner_prop + HH_Size + post_sec_prop + citizen_non_ratio, 
	             data = data)
	
	model3Summary = summary(model3)
	model1Summary = summary(model1)
	
	# 2)  Determine the goodness-of-fit of your Model 1 to the data (R2), discuss the fit and 
	#     whether or not, based on this R2 value, your Model 1 is sufficient in describing your 
	#     dependent variable.
	
	
	
	model1Summary$r.squared 
	model1Summary$adj.r.squared
	skewness(model1Summary$residuals, na.rm = TRUE)
	model1Fstatistic = model1Summary$fstatistic
	critVal1 = qf(0.05, model1Fstatistic["numdf"], model1Fstatistic["dendf"], lower.tail=FALSE)
	paste("Model 1 fStatistic is ", model1Fstatistic["value"], ", critical value is: ", critVal1)
	
	model3Summary$r.squared 
	model3Summary$adj.r.squared
	skewness(model3Summary$residuals, na.rm = TRUE)
	model1Summary$fstatistic
	model3Fstatistic = model3Summary$fstatistic
	critVal3 = qf(0.05, model3Fstatistic["numdf"], model3Fstatistic["dendf"], lower.tail=FALSE)
	paste("Model 1 fStatistic is ", model3Fstatistic["value"], ", critical value is: ", critVal3)
	
	
