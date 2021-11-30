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
# correlation_P <- cor(select(dataToUse,7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 22), use="complete.obs", method="pearson") 
# write.csv(correlation_P, "data/refined2/correlation_P.2.csv") 
# corrplot(correlation_P, type = "upper", order = "hclust", method = "color", 
#          tl.cex = 0.7, tl.col = "black", tl.srt = 45)
# 
# print("Correlation plot written to file.")
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

# model_a <- lm( PctFFood ~ population + fpl_100_score +  sing_parent_fam_score + black_score +  hisp_score +
#                  dropout_score + no_car_score + rent_occup_score + crowding_score + nonemp_score + 
#                  highneeds_score + hisp_score,
#                data = dataToUse)

# # nonemp_score
# model_a <- lm( PctFFood ~ population + fpl_100_score +  sing_parent_fam_score + black_score +  hisp_score +
#                  dropout_score + no_car_score + rent_occup_score + crowding_score + 
#                  highneeds_score + hisp_score,
#                data = dataToUse)

# # nonemp_score
# model_a <- lm( PctFFood ~ population + fpl_100_score +  sing_parent_fam_score + black_score +  hisp_score +
#                  dropout_score + no_car_score + rent_occup_score + crowding_score + 
#                  highneeds_score + hisp_score,
#                data = dataToUse)

# # no_car_score
# model_a <- lm( PctFFood ~ population + fpl_100_score +  sing_parent_fam_score + black_score +  hisp_score +
#                  dropout_score +  + rent_occup_score + crowding_score + 
#                  highneeds_score + hisp_score,
#                data = dataToUse)

# # sing_parent_fam_score
# model_a <- lm( PctFFood ~ population + fpl_100_score  + black_score +  hisp_score +
#                  dropout_score +  + rent_occup_score + crowding_score + 
#                  highneeds_score + hisp_score,
#                data = dataToUse)

# # sing_parent_fam_score
# model_a <- lm( PctFFood ~ fpl_100_score  + black_score +  hisp_score +
#                  dropout_score +  + rent_occup_score + crowding_score + 
#                  highneeds_score + hisp_score,
#                data = dataToUse)

# # crowding_score
# model_a <- lm( PctFFood ~ fpl_100_score  + black_score +  hisp_score +
#                  dropout_score +  + rent_occup_score + 
#                  highneeds_score + hisp_score,
#                data = dataToUse)

# fpl_100_score
model_a <- lm( PctFFood ~  black_score +  dropout_score + highneeds_score + hisp_score + rent_occup_score,
               data = dataToUse)

        # Call:
        #   lm(formula = PctFFood ~ black_score + hisp_score + dropout_score + 
        #        +rent_occup_score + highneeds_score + hisp_score, data = dataToUse)
        # 
        # Residuals:
        #   Min         1Q     Median         3Q        Max 
        # -0.8614892 -0.0585029  0.0455472  0.1178558  0.4048290 
        # 
        # Coefficients:
        #   Estimate   Std. Error  t value   Pr(>|t|)    
        # (Intercept)       0.664976752  0.010192649 65.24081 < 2.22e-16 ***
        #   black_score       0.001408952  0.000124115 11.35197 < 2.22e-16 ***
        #   hisp_score        0.000900298  0.000141526  6.36137 2.2943e-10 ***
        #   dropout_score    -0.000646961  0.000126273 -5.12350 3.1829e-07 ***
        #   rent_occup_score  0.000996103  0.000169695  5.86994 4.8207e-09 ***
        #   highneeds_score  -0.000556035  0.000108132 -5.14218 2.8845e-07 ***
        #   ---
        #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
        # 
        # Residual standard error: 0.192956 on 3098 degrees of freedom
        # Multiple R-squared:  0.104655,	Adjusted R-squared:  0.10321 
        # F-statistic: 72.4239 on 5 and 3098 DF,  p-value: < 2.22e-16


summary(model_a) 


# dataToUse$residuals <- residuals(model_a)
# dataToUse$fitted <- fitted(model_a)
# 
# # Test normality of residuals with Shapiro-Wilk's W test
# # in order to understand the p-value you have to understand what the corresponding statistical test is actually testing.
# # 
# # In case of the Shapiro-Wilk Normality Test the null hypothesis is the underlying data has a normal distribution. The p-value then measures (more or less) how likely this is. Often we accept the null hypothesis if the p-value is greater or equal than 0.05. This means that in only 5% of the cases we reject the null hypothesis although it would be correct (Type I error).
# shapiro.test(dataToUse$residuals)
