
# Assignment: ASSIGNMENT Week4. Exercise 4.2
# Name: Rahmanzai, Saima
# Date: 2021-04-09


## We interact with a few datasets in this course, one you are already familiar with, the 2014 American Community Survey 
## and the second is a Housing dataset, that provides real estate transactions recorded from 1964 to 2016.  For this 
## exercise, you need to start practicing some data transformation steps – which will carry into next week, as you learn 
## some additional methods.  For this week, using either dataset (or one of your own – although I will let you know 
## ahead of time that the Housing dataset is used for a later assignment, so not a bad idea for you to get more 
## comfortable with now!), perform the following data transformations:

options(max.print=400000)


library("readxl")
Housing_df <- read_excel("C:\\Users\\Saima\\Class_Assignments\\dsc520\\data\\week-7-housing.xlsx")
Housing_df

##    a) Use the apply function on a variable in your dataset

Housing_matrix<-data.matrix(Housing_df, rownames.force = NA)
head(Housing_matrix)

options(scipen = 999) 
apply(Housing_matrix,2,sum, na.rm=TRUE)
##I had to first change the dataframe to matrix for me to use apply as very restrictive.  I used the apply
## command to sum all columns (2) in the Housing_matrix.  I removed NA as if there is even one NA, the sum or any
## function will have a result of NA.  Please note that to remove exponential numbers, we use the above option command.  
## Easy to read the figures that way.

apply(Housing_matrix,2,min, na.rm=TRUE)
## I had to first change the dataframe to matrix for me to use apply as very restrictive.  I used the apply
## command to find the minimum of all columns (2 means columns) in the Housing_matrix.

##    b) Use the aggregate function on a variable in your dataset

## data(Housing_df, package = 'ggplot2')
## head(Housing_df)

## I want to calculate the average price by city in the Housing_df dataframe using aggregate as follows:

aggregate(x = Housing_df$`Sale Price`,                # Specify data column
          by = list(Housing_df$postalctyn),              # Specify group indicator
          FUN = min)                            # Specify function (i.e. sum, min, max)


aggregate(x = Housing_df$`Sale Price`,                # Specify data column
          by = list(Housing_df$zip5),              # Specify group indicator
          FUN = min)                            # Specify function (i.e. sum, min, max)


## The above first aggregate command aggregates the data by postalctyn and gives me the minimum Sales Price which is 
## 698 for REDMOND

## The second aggregate command groups the minimum Sales Price by Zip. x is the sales price.  The results are shown below:
##   Group.1      x
## 1   98052   2031
## 2   98053    698
## 3   98059 645000
## 4   98074 434000



##    c) Use the plyr function on a variable in your dataset – more specifically, I want to see you split some data, 
##       perform a modification to the data, and then bring it back together 

library(plyr)
head(Housing_df)

library(dplyr)


str(Housing_df)

cdata <- ddply(Housing_df, c("postalctyn"), summarize,
               N=sum(!is.na(Housing_df$`Sale Price`)),
               mean=mean(Housing_df$`Sale Price`, na.rm=TRUE),
               sd = sd(Housing_df$`Sale Price`, na.rm = TRUE),
               se = sd/sqrt(N)
)
cdata


zip_char<- as.character(Housing_df$zip5)

filter(Housing_df, zip_char == "98074")

zip_char<- as.character(Housing_df$zip5)
## I will then use the mutate() command to add this year as a new column in the existing dataframe.
mutate(Housing_df, zip_char)

year<-(substring(Housing_df$`Sale Date`,1,4))
year

## I will then use the mutate() command to add this year as a new column in the existing dataframe.

mutate(Housing_df, year)

## I will copy the first few columns of the result below:

## # A tibble: 73 x 24
## `Sale Date`         `Sale Price` sale_reason sale_instrument sale_warning sitetype addr_full   zip5 ctyname postalctyn
## <dttm>                     <dbl>       <dbl>           <dbl> <chr>        <chr>    <chr>      <dbl> <chr>   <chr>     
##1 2006-04-14 00:00:00      1369900           1               3 NA           R1       24620 NE ~ 98074 SAMMAM~ REDMOND   
##2 2006-05-08 00:00:00      1389900           1               3 NA           R1       24628 NE ~ 98074 SAMMAM~ REDMOND   
##3 2006-06-06 00:00:00      1650000           1               3 NA           R1       2005 250T~ 98074 SAMMAM~ REDMOND  

    


##    d) Check distributions of the data

## Load the ggplot2 package
library(ggplot2)
theme_set(theme_minimal())


ggplot(Housing_df, aes(x=`Sale Price`, y=zip5)) + geom_point()


ggplot(Housing_df, aes(x=`Sale Price`, y=zip5)) + geom_point()+ geom_smooth()


ggplot(Housing_df, aes(x=`Sale Price`, y=zip5)) + geom_point() + ggtitle("Housing Sales Prices by Zip_Code") + xlab("Housing Sales Price (Dollars)") + ylab("Housing by Zip Code")

## Create a histogram using and using 10 bins
library(ggplot2)
theme_set(theme_minimal())



ggplot(Housing_df, aes(Housing_df$`Sale Price`)) + geom_histogram(bins = 10)

ggplot(Housing_df, aes(Housing_df$`Sale Price`))  +  geom_density()


j = ggplot(Housing_df, aes(Housing_df$`Sale Price`)) + 
    geom_histogram(aes(y= ..density..), bins = 10, colour = "black", fill = "white") + 
    labs(x = "Housing Sales Prices", y = "Density")
j

##add normal curve to above

j + stat_function(fun = dnorm, args = list(mean = mean(Housing_df$`Sale Price`, na.rm = TRUE), sd=sd(Housing_df$`Sale Price`, na.rm=TRUE)), colour="red", size =1)





mean(Housing_df$`Sale Price`)
median(Housing_df$`Sale Price`)
min(Housing_df$`Sale Price`)
max(Housing_df$`Sale Price`)
sd(Housing_df$`Sale Price`)



### I used the describe() after installing the psych package where I gathered more info about the data within the file
### including mean, sd, median, min, max, range, skew, kurtosis, etc.
describe(Housing_df)

## See partial results below:
##          vars     n      mean        sd    median   trimmed       mad      min        max      range
##Sale Price 2     12865 660737.75  404381.08 593000.00 605920.40 212011.80   698.00 4400000.00 4399302.00

##                           skew kurtosis      se
##Sale Date                   NA       NA      NA
##Sale Price                4.49    29.22 3565.22

##After analyzing all the data, I draw the following conclusion on Sales Price.
## Visually, the Histogram shows data skewed to the left.  After analyzing the mean and the median.  Shows that they are close but not
## that close. This means distribution is close to normal but skewed to the left (positive skew as right tail is longer).  There are outliers in the data as well as addressed in the question below. 
## The skew in Describe shows 4.49 and Kurtosis of 29.22 while standard error is 3565.22.  Looks like more Sales Price are in the lower Price range category.
## Positive kurtosis means the distribution is Leptokurtic: More values in the distribution tails and more values close to the mean 
## (i.e. sharply peaked with heavy tails).  If kurtosis is zero, the distribution is closer to normal also known as 
## Mesokurtic: Distributions that are moderate in breadth and curves with a medium peaked height. If it was negative, it would have been
## Platykurtic: Fewer values in the tails and fewer values close to the mean (i.e. the curve has a flat peak and has more dispersed scores with lighter tails).
## Based on the infomation, our data is not quite normal and positively skewed.



##    e) Identify if there are any outliers

##Answer:  As seen in the commands above, and looking at the graphs, there are outliers.  The Min and Max under describe shows that the Min 
## Sales Price is $698 and Max is $4,400,000.  However, that does depend on the type of building or could be an error.  If we narrow down the
## dataset by type of building or identify and remove errors, we can clean the file with outliers.  Depends on what is our focus of testing model or Hypothesis.


##    f) Create at least 2 new variables

## Answer:  As seen above as well, I created a character version of zip code called zip_char using then using the Mutate() function, created a new column
## in the Housing_df using the information from the zip5 numeric data.  As seen in the aggregate command above, I used the
## character zip file I created to group the data by as well.



zip_char<- as.character(Housing_df$zip5)
mutate(Housing_df, zip_char)

year<-(substring(Housing_df$`Sale Date`,1,4))
year

## I created another new variable called year by using a substring command and extracting it from the existing Sales Date field
## in the housing_df.  I will then use the mutate() command to add this year as a new column in the existing dataframe.

mutate(Housing_df, year)

## My Values section in the environment window shows the zip_char adn year variables I created.  They are both character types.



