##Test Scores
## A professor has recently taught two sections of the same course with only one difference between the sections.
## In one section, he used only examples taken from sports applications, and in the other section, he used examples 
## taken from a variety of application areas. The sports themed section was advertised as such; so students knew which 
## type of section they were enrolling in. The professor has asked you to compare student performance in the two sections
## using course grades and total points earned in the course. You will need to import the Scores.csv dataset that has 
## been provided for you. Use the appropriate R functions to answer the following questions:

options(max.print=10000)

## Set the working directory to the root of your DSC 520 directory
setwd("C:\\Users\\Saima\\Class_Assignments\\dsc520")

## Load the `scores.csv` to
scores_df <- read.csv("data/scores.csv")
scores_df

class(scores_df)


##    1) What are the observational units in this study?
## Answer:  We will run some commands before answering the questions to understand the data first.

categories <- unique(scores_df)
categories

str(scores_df)

## The above structure command helps provided more information of data elements and their types. 

nrow(scores_df)
ncol(scores_df)


## As seen above, there are 3 variables, 38 rows and three columns.  The variables in the dataset are:
## Count (integer), Score (Integer), and Section (character).  the professor is asking us to compare student 
## performance in the two sections using course grades and total points earned in the course.  So we will focus on the 
## section and Score variable as observational units. In statistics, a unit of observation is the unit described by the 
## data that is analyzed.


##    2) Identify the variables mentioned in the narrative paragraph and determine which are categorical and quantitative?

## Answer:  The "Section" variable is categorical and the "Score" is quantitative.  The Categorical variable is made of categories.e.g.
## in our example, our categories are:  "Sports" and "Regular" and can also be referred to as binary variable. A Quantitative Variables, sometimes 
## referred to as “numeric” variables, these are variables that represent a measurable quantity. Examples include: Number of students in a class, etc.
## In our example, "Scores" is a quantitative measure and is numeric.


##    3) Create one variable to hold a subset of your data set that contains only the Regular Section and one variable 
## for the Sports Section.

##Answer:  This can be accomplished using the "subset" command as follows:

sportsData<-subset(scores_df, scores_df$Section=="Sports")
sportsData

regularData<-subset(scores_df, scores_df$Section=="Regular")
regularData

##I also ran some additional commands on scores to understand the data within these datasets individually
##I also changed the defaults of Basic from TRUE to FALSE and norm to TRUE


mean(scores_df$Score)
median(scores_df$Score)
min(scores_df$Score)
max(scores_df$Score)
sd(scores_df$Score)

mean(sportsData$Score)
median(sportsData$Score)
min(sportsData$Score)
max(sportsData$Score)
sd(sportsData$Score)

mean(regularData$Score)
median(regularData$Score)
min(regularData$Score)
max(regularData$Score)
sd(regularData$Score)




shapiro.test(sportsData$Score)
shapiro.test(regularData$Score)
shapiro.test(scores_df$Score)

###In the shapiro-Wilk test, if p-value is less than .005, it indicates a deviation from normality.  In our case, the 
### individual segment section files as well as the combined file hav p-values greater than .005, showing that the distribution
### seems normal.  However, since the sample size in not large, we should interpret normality in conjunction with histograms,
###or Q-Q plots, and the values of skew and kurtosis.




##    4) Use the Plot function to plot each Sections scores and the number of students achieving that score. Use additional 
##   Plot Arguments to label the graph and give each axis an appropriate label. Once you have produced your Plots answer
##   the following questions:


library(ggplot2)
theme_set(theme_minimal())

##below is the histogram for the sportsData section by showing Scores on X Axis and No. of Students on Y Axis

s = ggplot(sportsData, aes(Score)) 
sp = s + geom_histogram(bins = 10, fill ="white", color ="black")
sp + xlab("scores") +
    ylab("No. of Students") +
    ggtitle("SportsData Section Scores by Students Histogram")

## Below is the Scatterplot as well for the same data

ggplot(sportsData, aes(x=Score, y=Count)) + geom_point() + ggtitle("SportsData Scores vs. Student Count") + xlab("Scores") + ylab("Student Count")


##below is the histogram for the RegularsData section by showing Scores on X Axis and No. of Students on Y Axis
library(ggplot2)
theme_set(theme_minimal())
r = ggplot(regularData, aes(Score)) 
rp = s + geom_histogram(bins = 10, fill ="white", color ="black")
rp + xlab("scores") +
    ylab("No. of Students") +
    ggtitle("RegularData Section Scores by Students Histogram")

## Below is the Scatterplot as well for the same data

ggplot(regularData, aes(x=Score, y=Count)) + geom_point() + ggtitle("RegularData Scores vs. Student Count") + xlab("Scores") + ylab("Student Count")


##Lastly, I wanted to see how the data looks like in a histogram in the combined data not separated by section (not required by assignment)
library(ggplot2)
theme_set(theme_minimal())
c = ggplot(scores_df, aes(Score)) 
cp = s + geom_histogram(bins = 10, fill ="white", color ="black")
cp + xlab("scores") +
    ylab("No. of Students") +
    ggtitle("All Sections Scores by Students Histogram")

## Below is the Scatterplot as well for the same data

ggplot(scores_df, aes(x=Score, y=Count)) + geom_point() + ggtitle("All Sections Scores vs. Student Count") + xlab("Scores") + ylab("Student Count")

##Running analysis on the different sections


library(psych)

describe(sportsData$Score)


library(pastecs)

stat.desc(sportsData$Score)

library(psych)

describe(regularData$Score)


library(pastecs)

stat.desc(regularData$Score)

library(psych)

describe(scores_df$Score)


library(pastecs)

stat.desc(scores_df$Score)


### Positive Kurtosis means a pointy and heavy-tailed distribution where negative indicates a flat and light tailed
### distribution.  For a normal distribution, the kurtosis should be zero or closer to zero.  In all these three files
### (individual segments and for the total segments), the kurtosis is negative implying that the not pointy but flat.  
### Negative valuse of skew indicate a build-up of high scores whereas positive values of skew indicate too many low scores
### in the distribution.  The further the value is from zero the more likely it is that the data are not normally distributed.
### In our case, the three files the skew is slightly negative which implies that scores are slightly skewed to the right but
### close to the middle (mean).
### Please note that we have a field "count" but not truly number of students so one variable that we could use is missing from 
### the dataset.  The scores appear to be grouped by multiple users with count grouped together.  The scores listed by
### group count is most likely group score totals.



##by(data = sportsData, INDICES = sportsData$Score, FUN = stat.desc)

##    a)  Comparing and contrasting the point distributions between the two section, looking at both tendency and 
##        consistency: Can you say that one section tended to score more points than the other? Justify and explain 
##         your answer.

#####Answer:  The four measures of central tendency are mean, median, mode and the midrange.  
### see the describe command I ran abovue for the two sections:
library(psych)

describe(sportsData$Score)


library(psych)

describe(regularData$Score)

### The results are:   describe(regularData$Score)
### vars  n   mean    sd    median trimmed   mad    min max range  skew kurtosis   se
### 1       19 327.63 33.27   325  328.24 37.06     265 380   115 -0.07    -1.09 7.63


### describe(sportsData$Score)
### vars  n   mean    sd median trimmed   mad min max range  skew kurtosis    se
###   1 19  307.37 58.03    315  308.53 66.72 200 395   195 -0.42    -1.06 13.31

### As seen above,the mean and median for regular section data is slightly higher than the sports section. This shows that 
### regular section scored slightly more than the sports section.

###In statistics, consistency of procedures, such as computing confidence intervals or conducting hypothesis tests, is a desired property 
###of their behaviour as the number of items in the data set to which they are applied increases indefinitely. In particular, consistency 
### requires that the outcome of the procedure with unlimited data should identify the underlying truth.  Standard Deviation of a normal
### distribution is 1 and mean is 0.  

##In statistics, a z-score tells us how many standard deviations away a value is from the mean. As shown below, the SD
## is around the mean (most around zeros, very few outliers).  As shown below from the results that the z_scores absolute
## values are around zero and 1. Shows somewhat consistency.

z_scores_reg <- (regularData$Score-mean(regularData$Score))/sd(regularData$Score)

z_scores_reg

z_scores_sport <- (sportsData$Score-mean(sportsData$Score))/sd(sportsData$Score)

z_scores_sport


##    b)  Did every student in one section score more points than every student in the other section? If not, explain
##        what a statistical tendency means in this context.

## Answer:  Please note that we do not have student individual data other than count so we could not do this calculation by individual student.


## However, I would like to use an apply function for which I have to change the dataframe to matrix and do some comparison.  Please see below

scores_df
options(scipen = 999) 
Scores_matrix<-data.matrix(scores_df, rownames.force = NA)
head(Scores_matrix)

apply(Scores_matrix,2,sum, na.rm=TRUE)
##Above I used the apply function but that totaled all scores, all counts, and totaled all sections so I rather used the aggregate function below
## as still figuring out the apply.




aggregate(x = scores_df$Score,                # Specify data column
          by = list(scores_df$Section),              # Specify group indicator
          FUN = sum)                            # Specify function (i.e. sum)

## I used teh aggregate that that tells me the total score by Segment.  It provides the following results:

## Group.1    x
## 1 Regular 6225
## 2  Sports 5840

## On an aggregate, the Regular section scored more than the Sports section.

## Below I categorized by Count as well and by section using the aggregate command for comparison of scores by segment and by group of students:



aggregate(x = scores_df$Score,                # Specify data column
          by = list(scores_df$Section, scores_df$Count),              # Specify group indicator
          FUN = sum)                            # Specify function (i.e. sum)

##  Group.1 Group.2    x
## 1 Regular      10 3085
## 2  Sports      10 4320
## 3 Regular      20 2790
## 4  Sports      20  900
## 5 Regular      30  350
## 6  Sports      30  620


## As you see the results above, though overall the Regular scores in aggregate are higher than sports, analyzing by groups
## of students, Sports scores are slightly higher for Group 10 and Group 30 compared to Regular Group 10 and Group 30 student totals.





## We had to first change the dataframe to matrix for us to use apply as very restrictive.  I used the apply
## command to sum all columns (2) in the Housing_matrix.  I removed NA as if there is even one NA, the sum or any
## function will have a result of NA.  Please note that to remove exponential numbers, we use the above option command.  
## Easy to read the figures that way.


##    c)  What could be one additional variable that was not mentioned in the narrative that could be influencing 
##        the point distributions between the two sections?

## Answer:  I think, individual students data is missing in the narrative.  We have a field "count" but not truly number of students so one variable that we could use is missing from 
### the dataset.  The scores appear to be grouped by multiple users with count grouped together.  The scores listed by
### group count is most likely group score totals.

