# Assignment: ASSIGNMENT 7
# Name: Rahmanzai, Saima
# Date: 2021-04-29


##.	Student Survey
#a.	As a data science intern with newly learned knowledge in skills in statistical correlation and R programming, 
##you will analyze the results of a survey recently given to college students. You learn that the research question 
##being investigated is: “Is there a significant relationship between the amount of time spent reading and the time 
##spent watching television?” You are also interested if there are other significant relationships that can be 
##discovered? The survey data is located in this StudentSurvey.csv file.

options(max.print=10000)


## Set the working directory to the root of your DSC 520 directory
##setwd("/home/jdoe/Workspaces/dsc520")

setwd("C:\\Users\\Saima\\Class_Assignments\\dsc520")

Stdnt_Srvy_df <- read.csv("data/student-survey.csv")
Stdnt_Srvy_df

str(Stdnt_Srvy_df)

Stdnt_Srvy_df$TimeReading=as.numeric(as.character(Stdnt_Srvy_df$TimeReading))
Stdnt_Srvy_df$TimeTV=as.numeric(as.character(Stdnt_Srvy_df$TimeTV))
str(Stdnt_Srvy_df)

##I changed the above fields into numerics vs. int as can perform a lot more math on them.


#i.	Use R to calculate the covariance of the Survey variables and provide an explanation of why you would use this 
##calculation and what the results indicate.

cor(Stdnt_Srvy_df$TimeReading,Stdnt_Srvy_df$TimeTV)
##or use the below command:
cor.test(Stdnt_Srvy_df$TimeReading,Stdnt_Srvy_df$TimeTV, method = ("pearson"), use = "complete.obs")

cor.test(Stdnt_Srvy_df$TimeReading,Stdnt_Srvy_df$Happiness, method = ("pearson"), use = "complete.obs")

cor.test(Stdnt_Srvy_df$TimeTV,Stdnt_Srvy_df$Happiness, method = ("pearson"), use = "complete.obs")

cor.test(Stdnt_Srvy_df$TimeReading,Stdnt_Srvy_df$Gender, method = ("pearson"), use = "complete.obs")

cor.test(Stdnt_Srvy_df$TimeTV,Stdnt_Srvy_df$Gender, method = ("pearson"), use = "complete.obs")

##Covariance is a measure of correlation.  Covariance can be used to measure the linear relationship between two 
##variables in a dataset.  A positive covariance value indicates a positive linear relationship between variables, a negative
##value represents the negative linear relationship, Around zero shows no relationship between them.

##The above shows that there is negative but strong correlation between reading and watching TV (-.883), followed by another negative but 
##a little less strong relationship between reading and happiness (-0.434), a positive close to strong (0.636) between happiness and
## watching TV.  There appears to be no or close to no correlation between reading and gender (-0.089).  
##I used pearson correlation as the data appears to be linear for most of these variables (accept Gender) and is normally distributed.  
##I did that by running additional statistical techiques including looking at scatterplots, performing ggqqplot and Shapiro tests as shown below:

##Scatterplots
####Graphs and the relevant statistical measures often work better in tandem.  It is crucial to graph as well to see the relationships between variables better.

library(ggplot2)

ggplot(Stdnt_Srvy_df) +
    aes(x = TimeTV, y = TimeReading) +
    geom_point(colour = "#0c4c8a") +
    theme_minimal()

## The above scatterplot between TimeTV and TimeReading shows a fairly strong negative relationship as sloping top left to bottom right.
ggplot(Stdnt_Srvy_df) +    
    aes(x = Happiness, y = TimeReading) +
    geom_point(colour = "#0c4c8a") +
    theme_minimal()
## The above scatterplot between Happiness and TimeReading also shows somewhat a negative relationship as sloping top left to bottom right but 
## data points are more scattered.  Makes sense as the correlation coefficient above for these two variables is -0.434 compared to
## -0.883 which is much stronger and data more in closer to the straight line between TimeTV and TimeReading.  

ggplot(Stdnt_Srvy_df) +    
    aes(x = Happiness, y = TimeTV) +
    geom_point(colour = "#0c4c8a") +
    theme_minimal()
##The above scatterplot between Happiness and TimeTV shows a positive relationship and data sloping up from lower left to top right. The correlation
##coefficient also suggests this positive relationship with 0.636. However not very strong.  The scatterplot also shows spread of data around the path.

library(ggplot2)
ggplot(Stdnt_Srvy_df) +
    aes(x = Gender, y = TimeReading) +
    geom_point(colour = "#0c4c8a") +
    theme_minimal()

##I created a scatterplot between Gender and TimeReading.  There does not appear to be any linearity to the data. the coefficient correlation (Pearson method)
##shows -0.08 so hints towards not having a relationship.  However, Gender appears to be ordinal data.  We will run Spearman test
## as well as Pearson is not a good measure for nonlinear or ordinal data.

cor.test(Stdnt_Srvy_df$TimeReading,Stdnt_Srvy_df$Gender, method = ("spearman"), use = "complete.obs", exact = FALSE)
##It gives almost the same measurement as Pearson (-0.088 vs. -0.089)



ggplot(Stdnt_Srvy_df) +
    aes(x = Gender, y = TimeTV) +
    geom_point(colour = "#0c4c8a") +
    theme_minimal()

cor.test(Stdnt_Srvy_df$TimeTV,Stdnt_Srvy_df$Gender, method = ("spearman"), use = "complete.obs", exact = FALSE)


##I created a scatterplot between Gender and TimeTV.  Similar to the Gender and Timereading, there does not appear to be 
##a good relationship between them.  

#ii.	Examine the Survey data variables. What measurement is being used for the variables? Explain what effect 
## changing the measurement being used for the variables would have on the covariance calculation. 
##Would this be a problem? Explain and provide a better alternative if needed.

str(Stdnt_Srvy_df)
head(Stdnt_Srvy_df)


##Variables can be categorical,  continuous or ordinal, etc.  We need to select the correlation measurement based on
##the type of data.  Usually Pearson correlation is used for parametric linear relationships and continuous variables.  
##There are others like Spearman and Kendall correlations that are nonparametric alternatives to Pearson’s correlation
##and for ordinal data.  Gender appears to be ordinal data while others are continous.  Pearson’s is an excellent 
##choice when you have data for a pair of variables and the relationship follows a straight line. If your data do not meet
##both of those requirements, it’s time to find a different correlation measure. Spearman’s rho is an excellent choice when
##you have ordinal data because Pearson’s is not appropriate. Choosing not a fitting correlation test will not identify a strong
## relationship the two variables may have. In addition to ordinal datat, if the data is curvilinear, it confuses Pearson 
##and it underestimates the relationship’s strength.  NOTE:  As the structure command shows, I changed the type of TimeTV and 
##Timereading to Numeric from Integer for more ease to apply mathematical techniques.



#iii.	Choose the type of correlation test to perform, explain why you chose this test, and make a prediction if the 
##test yields a positive or negative correlation?

##As tested above, before we look at the type of correlations to use, we should alsolook at the plots of our variables to get an idea of what 
##to expect. In particular, we need to determine if it's reasonable to assume that our variables have linear 
##relationships. I ran the scatterplot test above and the ggqqplot and Shapiro commands below that confirms that TimeTV and TimeReading, TimeTV
## and Happiness, Happiness and Time Reading have linear relationships and hence Pearsons correlation fits better.
## There appears to be no relationship when comparing with Gender so we will not use that variable for relationship with other variables.  
library("ggpubr")
ggqqplot(Stdnt_Srvy_df$TimeReading, ylab = "Time Spent Reading")
ggqqplot(Stdnt_Srvy_df$TimeTV, ylab = "Time Spent Watching TV")
ggqqplot(Stdnt_Srvy_df$Happiness, ylab = "Happiness")
ggqqplot(Stdnt_Srvy_df$Gender, ylab = "Gender")


shapiro.test(Stdnt_Srvy_df$TimeTV)
shapiro.test(Stdnt_Srvy_df$TimeReading)

##Again note that, if the data are not normally distributed, it’s recommended to use the non-parametric correlation, including Spearman and Kendall 
##rank-based correlation tests.

##The shapiro test for both TimeTV and Timereading shows if p value is greater than the significance level of .05 implying that the distribution of the data
## is not significantly different from normal distribution.  In other words, we can assume normality.



######Higher values of the t-value, also called t-score, indicate that a large difference exists between the two sample sets. 
#######The smaller the t-value, the more similarity exists between the two sample sets.
#######A large t-score indicates that the groups are different.
#######A small t-score indicates that the groups are similar.



#    iv.	Perform a correlation analysis of:
#    1.	All variables

##Correlation matrix is used to analyze the correlation between multiple variables at the same time.

cor(Stdnt_Srvy_df, method = "pearson", use = "complete.obs")


##The above code creates the correlation matrix where in the table above correlations coefficients between the possible 
##pairs of all variables are shown


# remove gender as categorical variable
##library(tidyverse)
##dat <- Stdnt_Srvy_df %>%
##    select(-Gender)


##library(Hmisc)
##dat.matrix <- as.matrix(dat)
##dat.matrix
##str(dat.matrix)



#2.	A single correlation between two a pair of the variables

##These were performed above but created below:


cor(Stdnt_Srvy_df$TimeReading,Stdnt_Srvy_df$TimeTV)
##or use the below command:
cor.test(Stdnt_Srvy_df$TimeReading,Stdnt_Srvy_df$TimeTV, method = ("pearson"), use = "complete.obs")

cor.test(Stdnt_Srvy_df$TimeReading,Stdnt_Srvy_df$Happiness, method = ("pearson"), use = "complete.obs")

cor.test(Stdnt_Srvy_df$TimeTV,Stdnt_Srvy_df$Happiness, method = ("pearson"), use = "complete.obs")

##NOTE: I did not recreate the Gender correlation as we proved above as there appears to be not a good relationship with
##other variables and therefore I will not consider in my model.


#3.	Repeat your correlation test in step 2 but set the confidence interval at 99%

##library(Rmisc)

##CI(Stdnt_Srvy_df$TimeReading,
##   ci=0.99)

##CI(Stdnt_Srvy_df$TimeTV,
##   ci=0.99)

##CI = 99% for Variables TimeReading vs. TimeTV below:

cor.test(Stdnt_Srvy_df$TimeReading, Stdnt_Srvy_df$TimeTV,
         alternative = c("greater"),
         method = c("pearson"),
         exact = NULL, conf.level = 0.99, continuity = FALSE)


##CI = 99% for Variables TimeReading vs. Happiness below:

cor.test(Stdnt_Srvy_df$TimeReading, Stdnt_Srvy_df$Happiness,
         alternative = c("greater"),
         method = c("pearson"),
         exact = NULL, conf.level = 0.99, continuity = FALSE)


##CI = 99% for Variables TimeTV vs. Happiness below:

cor.test(Stdnt_Srvy_df$TimeTV, Stdnt_Srvy_df$Happiness,
         alternative = c("greater"),
         method = c("pearson"),
         exact = NULL, conf.level = 0.99, continuity = FALSE)


##The confidence interval is the range of values that you expect your estimate to fall between a 
##certain percentage of the time if you run your experiment again or re-sample the population in the same way.
##The confidence level is the percentage of times you expect to reproduce an estimate between the upper and 
##lower bounds of the confidence interval, and is set by the alpha value. The alpha value, or the threshold for 
##statistical significance, is arbitrary – which value you use depends on your field of study. In most cases, 
##researchers use an alpha of 0.05, which means that there is a less than 5% chance that the data being tested 
##could have occurred under the null hypothesis. As well as the larger is the sample, the narrower is the confidence 
##interval.Intuitively, the more observations we have, the better our estimates will be.

#4.	Describe what the calculations in the correlation matrix suggest about the relationship between the variables. 
##Be specific with your explanation.

cor(Stdnt_Srvy_df, method = "pearson", use = "complete.obs")


### As shown above, a correlation matrix is a matrix that represents the pair correlation of all the variables. 
### The cor () function returns a correlation matrix. The only difference with the bivariate correlation is we don't 
###need to specify which variables. By default, R computes the correlation between all the variables.
###The bivariate Pearson Correlation produces a sample correlation coefficient, r, which measures the strength and direction of linear 
###relationships between pairs of continuous variables.
###You can see perfect correlation of 1 when a variable is compared with itself.e.g. TimeReading compared to TimeReading is
###1.00 etc.  At a glance, we can see strong negative correlation between TimeReading and TimeTV, Next stronger negative correlation
###is TimeTV and Happiness which is a positive correlation of 0.636, followed by a negative correlation of Happiness with 
###timeReading (-0.434).  Gender as you can see does not have a great correlation with other variables but with itself.



#v.	Calculate the correlation coefficient and the coefficient of determination, describe what you conclude about the
##results.


##Correlation coefficients help quantify mutual relationships or connections between two things. How close is the data to 
## the line of best fit?  If points are far away, r (correlation coefficient) is close to 0.  If very close to the line and moving upwards, it is close to +1,
## and if it is close to the line and sloping downwards, r is close to -1.  In other words, This correlation coefficient is 
## a single number that measures both the strength and direction of the linear relationship between two variables. Values can range from -1 to +1
## The greater the absolute value of the correlation coefficient, the stronger the relationship.  The extreme values of -1 and 1 indicate a perfectly linear 
## relationship where a change in one variable is accompanied by a perfectly consistent change in the other. A coefficient of zero represents no linear relationship. 
## As one variable increases, there is no tendency in the other variable to either increase or decrease. The sign of the correlation coefficient represents the direction of the relationship.
## Positive coefficients indicate that when the value of one variable increases, the value of the other variable also tends to increase. Positive relationships produce an upward slope on a scatterplot.
## Negative coefficients represent cases when the value of one variable increases, the value of the other variable tends to decrease. Negative relationships produce a downward slope.


### Coefficient of Determination R2 tells how good is the model.  It measures how well the predicted values match the observed values.
### +1 indicates that the predictions match the observations perfectly.  R2=0, indicates that the predictions are as good as random guesses around the mean of the observed values
#### Negative R2 indicates that the predictions are worse than random. Since R2 indicates the distance of points  from 1:1 line, it does
##depend on the magnitude of the numbers (unlike r2)

head(Stdnt_Srvy_df)


model <- lm(TimeReading~TimeTV+Happiness, data=Stdnt_Srvy_df)
summary(model)
##This means that 80.7% of the variation in the TimeReading can be explained by the number of TimeTV and happiness.

##OR ALSO

summary(model)$r.squared
## gives the same number of coefficient of determination.

#vi.	Based on your analysis can you say that watching more TV caused students to read less? Explain.

###Correct.  The coefficient of determination validates that (80.7% of the variation in TimeReading can be explained by the two variables)
###as well as all the other statistical measures we performed above.
###There is a strong negative correlation between TimeTV and TimeReading.  The scatterplots also shows this relationship between them



#vii.	Pick three variables and perform a partial correlation, documenting which variable you are “controlling”.
##Explain how this changes your interpretation and explanation of the results.

library(ppcor)
cor(Stdnt_Srvy_df$TimeReading,Stdnt_Srvy_df$TimeTV)
##ggplot(Stdnt_Srvy_df, aes(x=TimeReading, TimeTV)) +
##    geom_point(color = "darkblue") +
 ##   geom_smooth(formula = y~x, method = lm, se=FALSE, color="red", linetype = "dashed", size=0.8)+
##    xlim(0,20)+
##    ylim(0,8)+
##    labs(x="Time Spent Reading", y= "Time Spent Watching TV", title = "Correlation Plot")

pcor.test(x=Stdnt_Srvy_df$TimeReading, y=Stdnt_Srvy_df$TimeTV, z=Stdnt_Srvy_df$Happiness)

## The p value is low that means the two variables (TimeReading and TimeTV) are partially correlated.
## Control variable is Happiness.  The results show that the estimate value of -0.87 Partial Correlation shows a strong but opposite direction 
## correlation and the pValue is also and hence highly statistically significant.  Happiness is a mediating variable and
## partially explains the correlation between the TimeReading and TimeTV variables.
 

#Include all of your answers in an R Markdown report. Refer to the example template presented as a guide.  If you are submitting a file to the assignment link, it still needs to be a PDF.















