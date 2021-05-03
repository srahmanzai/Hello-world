# Assignment: ASSIGNMENT 5
# Name: Rahmanzai, Saima
# Date: 2021-04-27

## Set the working directory to the root of your DSC 520 directory
##setwd("/home/jdoe/Workspaces/dsc520")

options(max.print=10000)

setwd("C:\\Users\\Saima\\Class_Assignments\\dsc520")


## Load the `data/r4ds/heights.csv` to
heights_df <- read.csv("data/r4ds/heights.csv")
head(heights_df)

## Using `cor()` compute correlation coefficients for
## height vs. earn

cor(heights_df$height,heights_df$earn)

### age vs. earn

cor(heights_df$age,heights_df$earn)

### ed vs. earn
cor(heights_df$ed,heights_df$earn)

## Spurious correlation
## The following is data on US spending on science, space, and technology in millions of today's dollars
## and Suicides by hanging strangulation and suffocation for the years 1999 to 2009
## Compute the correlation between these variables
tech_spending <- c(18079, 18594, 19753, 20734, 20831, 23029, 23597, 23584, 25525, 27731, 29449)
suicides <- c(5427, 5688, 6198, 6462, 6635, 7336, 7248, 7491, 8161, 8578, 9000)

##cor() computes the correlation coefficient
##cor.test() test for association/correlation between paired samples. It returns both the correlation coefficient 
##and the significance level(or p-value) of the correlation
##use = "complete.obs" removes missing values

cor(tech_spending, suicides, method = c("pearson"), use = "complete.obs")

cor.test(tech_spending, suicides, method=c("pearson"), use = "complete.obs")

## The above can be visualized in a graph as well using the below commands

library("ggpubr")
ggqqplot(tech_spending, ylab = "US Spending on Science, space, and technology")
ggqqplot(suicides, ylab = "Suicides by hanging, strangulation, and suffocation")
##above shows both data come from normal distribution so the pearson correlation works.

shapiro.test(tech_spending)
##This test shows if p value is greater than the significance level of .05 implying that the distribution of the data
## is not significantly different from normal distribution.  In other words, we can assume normality as the p value is 0.6285
## for tech_spending.


shapiro.test(suicides)
##the p value for suicides is 0.8674 which is also greater than the significance level of .05 implying that the distribution
##of data is not significantly different from normal distribution.




theDF <- data.frame(tech_spending, suicides)

theDF

library(ggplot2)
g<-ggplot(theDF, aes(x=suicides,y=tech_spending)) + geom_point()+ geom_smooth(formula = y~x, method='lm')
g + xlab("Suicides by hanging, strangulation, and suffocation") +
    ylab("US Spending on Science, Space, and Technology") +
    ggtitle("US Spending on Science, Space & Technology Vs. Suicides Correlation")


##theDF <- data.frame(tech_spending, suicides)

##theDF

##library("ggpubr")


##ggscatter(theDF, x = "suicides", y = "tech_spending", 
##          add = "reg.line", conf.int = TRUE, 
##          cor.coef = TRUE, cor.method = "pearson",
##          xlab = "Suicides", ylab = "US Spending",
##          geom_smooth(formula = y~x))







##The relationship is linear. In the situation where the scatter plots show curved patterns, we are dealing with nonlinear association between the two variables.
## We ran Use Shapiro-Wilk normality test and the ggqplot test for normality.  Appears to be a normal distribution.
