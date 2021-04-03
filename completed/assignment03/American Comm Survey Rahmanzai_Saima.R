# Assignment: American Community Survey Week3
# Name: Rahmanzai, Saima
# Date: 2021-03-31

## I set the max.print option as when I read the data file, it omitted 11 rows with an error statement as it reached "max"
## I researched it and found an answer on stackoverflow to setup max.print command above first thing in the script so it is initialized

options(max.print=10000)

##	For this exercise, you will use the dataset, 2014 American Community Survey. y
## You will need to load and activate the ggplot2 package. 
library(ggplot2)
theme_set(theme_minimal())

## Set the working directory to the root of your DSC 520 directory
setwd("C:\\Users\\Saima\\Class_Assignments\\dsc520")


## Load the file acs-14-1yr-s0201.csv located under the Data folder
survey_df<- read.csv("data/acs-14-1yr-s0201.csv")
survey_df

## i.	What are the elements in your data (including the categories and data types)?

## unique returns a vector, data frame or array like x but with duplicate elements/rows removed.

categories <- unique(survey_df)
categories

## ii.	Please provide the output from the following functions: str(); nrow(); ncol()

str(survey_df)

## The above structure commend helps provided more information of data elements and their types. The data types listed are:
## chr, int, num for the 8 variables in the dataframe.


nrow(survey_df)
ncol(survey_df)

## rows and columns give more info about my data counting the number of rows and records.  This info is also provided
## in my str() command when it states 'data.frame':	136 obs. of  8 variables:  '


## iii.	Create a Histogram of the HSDegree variable using the ggplot2 package.
## 1.	Set a bin size for the Histogram.
## 2.	Include a Title and appropriate X/Y axis labels on your Histogram Plot.

library(ggplot2)
theme_set(theme_minimal())


ggplot(survey_df, aes(HSDegree)) + geom_histogram(bins = 10)

## Title: 2014 American Community Survey - High School Degrees
## X label: High School Degrees
## y label:  No. of High School Degrees


o = ggplot(survey_df, aes(HSDegree)) 
p = o + geom_histogram(bins = 10, fill ="white", color ="black")
p + xlab("High School Degrees") +
    ylab("No. of High School Degrees") +
    ggtitle("2014 American Community Survey - High School Degrees")

## iv.	Answer the following questions based on the Histogram produced:
##  1.	Based on what you see in this histogram, is the data distribution unimodal?
## Answer:  This is a unimodel distrbution with one peak.
##
##  2.	Is it approximately symmetrical?
## Answer: Where two or more variables share a direct relationship to make a symmetrical dataset, on which
## the left side mirrors the right side. This chart is symmetrical but skewed to the left.
##
##  3.	Is it approximately bell-shaped?
## Answer:  The current is slightly skewed and hence not perfect bell-shaped as is the normal distribution.
##
##  4.	Is it approximately normal?
## Answer: It does not appear to be normal but very close.  With normal distribution, two or more variables share a direct relationship 
## to make a symmetrical data set, on which the left half mirrors the right half. 
## These graphs are called bell curves due to their clearly defined, bell-like shape. In a normal distribution graph, 
## the mean (average), median, and mode are all equal. I pulled the mean and median and they are not the same.  Mean
## is less than the median but not by much.

mean(survey_df$HSDegree)
median(survey_df$HSDegree)
## difference between mean and median is minimal.

##  5.	If not normal, is the distribution skewed? If so, in which direction?
## Answer.  Yes.  It is skewed. Skewness is a way to describe the symmetry of a distribution.
## A distribution is left skewed if it has a “tail” on the left side of the distribution.  This chart
## is skewed to the left. In a left skewed distribution, the mean is less than the median as shown again below.
##

mean(survey_df$HSDegree)
median(survey_df$HSDegree)
min(survey_df$HSDegree)
max(survey_df$HSDegree)
sd(survey_df$HSDegree)

### I used the describe() after installing the psych package where I gathered more info about the data within the file
### including mean, sd, median, min, max, range, skew, kurtosis, etc.
describe(survey_df)

## As you can see, the data is slightly skewed to the left.  The difference between Mean and Median is very small.


##  6.	Include a normal curve to the Histogram that you plotted.




library(ggplot2)
theme_set(theme_minimal())


i = ggplot(survey_df, aes(HSDegree)) + 
    geom_histogram(aes(y= ..density..), bins = 10, colour = "black", fill = "white") + 
    labs(x = "High School Degrees", y = "Density")
i

##add normal curve to above

i + stat_function(fun = dnorm, args = list(mean = mean(survey_df$HSDegree, na.rm = TRUE), sd=sd(survey_df$HSDegree, na.rm=TRUE)), colour="red", size =1)




##  7.	Explain whether a normal distribution can accurately be used as a model for this data.
## Answer: We can use a normal distribution for this data.  The data is left skewed but not by much.
## Real life distributions are usually skewed. If there are too much skewness in the data,
## then many statistical model don’t work. So in skewed data, the tail region may act as an outlier for the statistical 
## model and we know that outliers adversely affect the model’s performance especially regression-based models.
## There are statistical model that are robust to outlier like a Tree-based models but it will limit the possibility to try
## other models. So there is a necessity to transform the skewed data to close enough to a Gaussian distribution or 
## Normal distribution. 
## A log transformation can help to fit a very skewed distribution into a Gaussian one. After log transformation we can easily
## see pattern in our data.

## v.	Create a Probability Plot of the HSDegree variable.

##qqplotdf <- qplot(sample=survey_df$HSDegree, stat="qq")
##qqplotdf 
##The above worked but gave me a warning.  Trying it with the below command

ggplot(survey_df, aes(sample=HSDegree)) + stat_qq() + stat_qq_line()



## vi.	Answer the following questions based on the Probability Plot:
##1.	Based on what you see in this probability plot, is the distribution approximately normal? Explain how you know.
##Answer:  The QQPlot curve's data points all fall very close to the ideal diagonal line.  The distribution is close to normal but is
## slightly left skewed.


##2.	If not normal, is the distribution skewed? If so, in which direction? Explain how you know.
## Answer:  Visually, the chart looked skewed to the left.  After analyzing the mean and the median that are really close
## to one another (very slightly skewed) and then looking at the ggplot line, looks like the data is close to normal and maybe very slightly left skewed.


##vii.	Now that you have looked at this data visually for normality, you will now quantify normality 
## with numbers using the stat.desc() function. Include a screen capture of the results produced.

stat.desc(survey_df$HSDegree, basic = TRUE, norm = FALSE)

##viii.	In several sentences provide an explanation of the result produced for skew, kurtosis, and z-scores. In addition, 
## explain how a change in the sample size may change your explanation?

##Answer: In statistics, skewness and kurtosis are the measures which tell about the shape of the data distribution or simply, 
## both are numerical methods to analyze the shape of data set unlike, plotting graphs and histograms which are graphical methods. 
##These are normality tests to check the irregularity and asymmetry of the distribution.

# Calculate Kurtosis in R
library(moments)
t <- survey_df$HSDegree
kurtosis(t)

##In statistics, a z-score tells us how many standard deviations away a value is from the mean. As shown below, the SD
## is around the mean (most around zeros, very few outliers)

z_scores <- (survey_df$HSDegree-mean(survey_df$HSDegree))/sd(survey_df$HSDegree)

z_scores

## or done using the scale command below:

scale (survey_df$HSDegree)

### I used the describe() after installing the psych package where I gathered more info about the data within the file
### including mean, sd, median, min, max, range, skew, kurtosis, etc.
describe(survey_df)

### According to describe, Kurtosis was still positive and skew was negative but not much.  The proves my observations
### that the data is skewed to the left slightly but not by much.


## The above command shows a positive Kurtosis of 7.46 which means This simply means that fewer data values are located near the mean and more data values
##are located on the tails.
## A zero Kurtosis shows that the distribution is equal to the normal distribution which has the following bell-shape.  Our figure is
## not zero so as we described above, not close to normal.
## The skewness and kurtosis statistics appear to be very dependent on the sample size.  Smaller sample sizes can give results that are very misleading. 


