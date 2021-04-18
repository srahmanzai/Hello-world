
# Assignment: ASSIGNMENT Week5
# Name: Rahmanzai, Saima
# Date: 2021-04-13


## Using either the same dataset(s) you used in the previous weeks’ exercise or a brand-new dataset
## of your choosing, perform the following transformations (Remember, anything you learn about the Housing dataset 
## in these two weeks can be used for a later exercise!)
## a.	Using the dplyr package, use the 6 different operations to analyze/transform the data - GroupBy, Summarize, 
##      Mutate, Filter, Select, and Arrange – Remember this isn’t just modifying data, you are learning about your
##      data also – so play around and start to understand your dataset in more detail
## b.	Using the purrr package – perform 2 functions on your dataset.  You could use zip_n, keep, discard, compact, etc.
## c.	Use the cbind and rbind function on your dataset
## d.	Split a string, then concatenate the results back together


options(max.print=400000)


library("readxl")
Housing_df <- read_excel("C:\\Users\\Saima\\Class_Assignments\\dsc520\\data\\week-7-housing.xlsx")
Housing_df


## a.	Using the dplyr package, use the 6 different operations to analyze/transform the data - GroupBy, Summarize, 
## Mutate, Filter, Select, and Arrange – Remember this isn’t just modifying data, you are learning about your data 
## also – so play around and start to understand your dataset in more detail

library(magrittr)
data(Housing_df, package = 'ggplot2')
dim(head(Housing_df, n=4))
## The head function with n=4 gives the first four rows and the dim gives the dimenstions which is then 4 by 24
## The above can be demonstrated using the Pipes function in dplyr that pipes a function into function.  We get the same 
## result as the above code with just one line below:

Housing_df %>% head(4) %>% dim

### Results shown below:
## > Housing_df %>% head(4) %>% dim
## [1]  4 24

class(Housing_df)
## Result shown below:
## [1] "tbl_df"     "tbl"        "data.frame"

##The above commands confirms that our dataset is a tibble (tbl).  It shows only that many columns as shown in view and 
## shows the type underneath column headings.

library(dplyr)
head(Housing_df)

##The select function of dplyr takes the data.frame as its first argument then the desired column as subsequent arguments.
## See demonstration below as I want to select Sale Price, zip, and Cityname fields

select(Housing_df, 'Sale Price', zip5, ctyname)
## See results below:
##select(Housing_df, 'Sale Price', zip5, ctyname)
# A tibble: 12,865 x 3
##`Sale Price`  zip5 ctyname
##<dbl> <dbl> <chr>  
##1       698000 98052 REDMOND
##2       649990 98052 REDMOND
##3       572500 98052 NA     
##4       420000 98052 REDMOND
##5       369900 98052 REDMOND
##6       184667 98053 NA     
##7      1050000 98053 NA     
##8       875000 98053 NA     
##9       660000 98053 NA     
##10       650000 98052 REDMOND


## or using pipes as below to get the same result:

Housing_df %>% select('Sale Price', zip5, ctyname)
### Result below:
## # A tibble: 12,865 x 3
##`Sale Price`  zip5 ctyname
##<dbl> <dbl> <chr>  
##    1       698000 98052 REDMOND
##2       649990 98052 REDMOND
##3       572500 98052 NA     
##4       420000 98052 REDMOND
##5       369900 98052 REDMOND
##6       184667 98053 NA     
##7      1050000 98053 NA     
##8       875000 98053 NA     
##9       660000 98053 NA     
##10       650000 98052 REDMOND

## if I wanted to find column names starting with s, I can use "starts_with" function.  There are others like ends_with, or contains.

Housing_df %>% select(starts_with('s'))
## See results below:
## # A tibble: 12,865 x 8
##`Sale Date`         `Sale Price` sale_reason sale_instrument sale_warning sitetype square_feet_total_living sq_ft_lot
##<dttm>                     <dbl>       <dbl>           <dbl> <chr>        <chr>                       <dbl>     <dbl>
##    1 2006-01-03 00:00:00       698000           1               3 NA           R1                           2810      6635
##    2 2006-01-03 00:00:00       649990           1               3 NA           R1                           2880      5570

## Next I will explore the filter function in dplyr.  

Housing_df %>% filter(zip5 == '98052')

## The result filters the data applicable to the 98052 zip only.  See partial result output below:
## # A tibble: 7,452 x 24
##`Sale Date`         `Sale Price` sale_reason sale_instrument sale_warning sitetype addr_full   zip5 ctyname postalctyn
##<dttm>                     <dbl>       <dbl>           <dbl> <chr>        <chr>    <chr>      <dbl> <chr>   <chr>     
##    1 2006-01-03 00:00:00       698000           1               3 NA           R1       17021 NE ~ 98052 REDMOND REDMOND   
##2 2006-01-03 00:00:00       649990           1               3 NA           R1       11927 178~ 98052 REDMOND REDMOND   
##3 2006-01-03 00:00:00       572500           1               3 NA           R1       13315 174~ 98052 NA      REDMOND   
##4 2006-01-03 00:00:00       420000           1               3 NA           R1       3303 178T~ 98052 REDMOND REDMOND   

## We can use more than one value as well as shown below:

Housing_df %>% filter(zip5 %in% c('98052','98053'))
##See partial results output below:
### A tibble: 12,791 x 24
##`Sale Date`         `Sale Price` sale_reason sale_instrument sale_warning sitetype addr_full   zip5 ctyname postalctyn
##<dttm>                     <dbl>       <dbl>           <dbl> <chr>        <chr>    <chr>      <dbl> <chr>   <chr>     
##    1 2006-01-03 00:00:00       698000           1               3 NA           R1       17021 NE ~ 98052 REDMOND REDMOND   
##2 2006-01-03 00:00:00       649990           1               3 NA           R1       11927 178~ 98052 REDMOND REDMOND   
##3 2006-01-03 00:00:00       572500           1               3 NA           R1       13315 174~ 98052 NA      REDMOND   
##4 2006-01-03 00:00:00       420000           1               3 NA           R1       3303 178T~ 98052 REDMOND REDMOND   
##5 2006-01-03 00:00:00       369900           1               3 15           R1       16126 NE ~ 98052 REDMOND REDMOND   
##6 2006-01-03 00:00:00       184667           1              15 18 51        R1       8101 229T~ 98053 NA      REDMOND   
##7 2006-01-04 00:00:00      1050000           1               3 NA           R1       21634 NE ~ 98053 NA      REDMOND   

## I can use filter to find Sale Price more than 1,000,000 as below:

Housing_df %>% filter(`Sale Price`> 1000000)
## See partial results below:
## # A tibble: 934 x 24
##`Sale Date`         `Sale Price` sale_reason sale_instrument sale_warning sitetype addr_full   zip5 ctyname postalctyn
##<dttm>                     <dbl>       <dbl>           <dbl> <chr>        <chr>    <chr>      <dbl> <chr>   <chr>     
##    1 2006-01-04 00:00:00      1050000           1               3 NA           R1       21634 NE ~ 98053 NA      REDMOND   
##    2 2006-01-12 00:00:00      1392000           1               3 NA           R1       2428 W LA~ 98052 REDMOND REDMOND   
##    3 2006-01-23 00:00:00      1445000           1               3 NA           R1       20425 NE ~ 98053 NA      REDMOND  

##  Another example with date field and another field sales price:
Housing_df %>% filter(`Sale Date`>'2016-01-01' & `Sale Price`>590000)
##See partial results below:
## > Housing_df %>% filter(`Sale Date`>'2016-01-01' & `Sale Price`>590000)
# A tibble: 864 x 24
##`Sale Date`         `Sale Price` sale_reason sale_instrument sale_warning sitetype addr_full   zip5 ctyname postalctyn
##<dttm>                     <dbl>       <dbl>           <dbl> <chr>        <chr>    <chr>      <dbl> <chr>   <chr>     
##    1 2016-01-04 00:00:00       964900           1               3 NA           R1       16910 NE ~ 98052 REDMOND REDMOND   
##    2 2016-01-04 00:00:00       633000           1               3 NA           R1       9827 228T~ 98053 NA      REDMOND   
##    3 2016-01-04 00:00:00       625000           1               3 NA           R1       10135 225~ 98053 NA      REDMOND   

## Next I will explore mutate function.  It creates new columns and modifies existing ones.  I will create a column 
## showing Sales Price by Sq ft.

Housing_df %>% select(`Sale Price`, square_feet_total_living) %>% mutate(Cost_per_Sqft=`Sale Price`/square_feet_total_living)
## See partial result below:
## # A tibble: 12,865 x 3
##`Sale Price` square_feet_total_living Cost_per_Sqft
##<dbl>                    <dbl>         <dbl>
##    1       698000                     2810         248. 
##    2       649990                     2880         226. 
##    3       572500                     2770         207. 


## The summarize function applies functions that return a result of length one such as mean, max, median or other similar functions.


summarize(Housing_df, mean(`Sale Price`))
## The result is:
## # A tibble: 1 x 1
##`mean(\`Sale Price\`)`
##<dbl>
##    1                660738.

## or we can perform multiple computations using summarize

Housing_df %>%
    summarize(AvgPrice=mean(`Sale Price`, na.rm=TRUE),
              MedianPrice=median(`Sale Price`, na.rm=TRUE),
              MaxPrice=max(`Sale Price`, na.rm=TRUE))

##See results below:
## # A tibble: 1 x 3
## AvgPrice MedianPrice MaxPrice
## <dbl>       <dbl>    <dbl>
##     1  660738.      593000  4400000
              
                
## summarize is very useful when used together with group_by function.  We can summarize by zip and then calculate the AvgPrice
## etc. per zip as shown below:

Housing_df %>%
    group_by(zip5) %>%
    summarize(AvgPrice=mean(`Sale Price`, na.rm=TRUE ),
              MedianPrice=median(`Sale Price`, na.rm=TRUE),
              MaxPrice=max(`Sale Price`, na.rm=TRUE))

##See results below:
## # A tibble: 1 x 3
##AvgPrice MedianPrice MaxPrice
##<dbl>       <dbl>    <dbl>
##    1  660738.      593000  4400000


## Sorting is performed with arrange function.  I will arrange the above command by AvgPrice by adding the Arrange function 
## to the code.

Housing_df %>%
    group_by(zip5) %>%
    summarize(AvgPrice=mean(`Sale Price`, na.rm=TRUE),
              MedianPrice=median(`Sale Price`, na.rm=TRUE),
              MaxPrice=max(`Sale Price`, na.rm=TRUE)) %>%
            arrange(AvgPrice)
##See results below:
## # A tibble: 4 x 4
##zip5 AvgPrice MedianPrice MaxPrice
##<dbl>    <dbl>       <dbl>    <dbl>
##    1 98059  645000       645000   645000
##    2 98052  649375.      599950  4400000
##    3 98053  672624.      584000  3850000
##    4 98074  951544.      820000  2160200

## To understand the Housing data, I decided to also look at a boxplot to show Sales price by zip.  I also used
## the scipen option so I can read the sales price not in short form notation.

options(scipen = 999) 
boxplot(Housing_df$`Sale Price`~Housing_df$zip5, main='Housing Data Boxplot', ylab='Sales Price', xlab = 'Zip Code')

##I will submit the resulting chart as a separate attachment 


## b.	Using the purrr package – perform 2 functions on your dataset.  You could use zip_n, keep, discard, compact, 
## etc.

##Answer:  purr package functions work on lists and vectors.  Our Housing data file is a data.frame.  We will first convert
### it to a list for teh purrr functions to work.

library(tidyverse)


housing_data_list <- split(Housing_df, seq(nrow(Housing_df)))         # Convert rows to list
head(housing_data_list)                                        # Print list
## See partial result below:
## $`1`
## A tibble: 1 x 24
##`Sale Date`         `Sale Price` sale_reason sale_instrument sale_warning sitetype addr_full    zip5 ctyname postalctyn
##<dttm>                     <dbl>       <dbl>           <dbl> <chr>        <chr>    <chr>       <dbl> <chr>   <chr>     
##    1 2006-01-03 00:00:00       698000           1               3 NA           R1       17021 NE 1~ 98052 REDMOND REDMOND   
## ... with 14 more variables: lon <dbl>, lat <dbl>, building_grade <dbl>, square_feet_total_living <dbl>,
##   bedrooms <dbl>, bath_full_count <dbl>, bath_half_count <dbl>, bath_3qtr_count <dbl>, year_built <dbl>,
##   year_renovated <dbl>, current_zoning <chr>, sq_ft_lot <dbl>, prop_type <chr>, present_use <dbl>

##$`2`
## A tibble: 1 x 24
##`Sale Date`         `Sale Price` sale_reason sale_instrument sale_warning sitetype addr_full    zip5 ctyname postalctyn
##<dttm>                     <dbl>       <dbl>           <dbl> <chr>        <chr>    <chr>       <dbl> <chr>   <chr>     
##    1 2006-01-03 00:00:00       649990           1               3 NA           R1       11927 178T~ 98052 REDMOND REDMOND   

## We will try Pluck function that selects a particular element from the list.  We will try to pluck the second row
ls1 <- housing_data_list
pluck(ls1, 2)

##See result below:
### A tibble: 1 x 24
##`Sale Date`         `Sale Price` sale_reason sale_instrument sale_warning sitetype addr_full    zip5 ctyname postalctyn
##<dttm>                     <dbl>       <dbl>           <dbl> <chr>        <chr>    <chr>       <dbl> <chr>   <chr>     
##    1 2006-01-03 00:00:00       649990           1               3 NA           R1       11927 178T~ 98052 REDMOND REDMOND   
# ... with 14 more variables: lon <dbl>, lat <dbl>, building_grade <dbl>, square_feet_total_living <dbl>,
#   bedrooms <dbl>, bath_full_count <dbl>, bath_half_count <dbl>, bath_3qtr_count <dbl>, year_built <dbl>,
#   year_renovated <dbl>, current_zoning <chr>, sq_ft_lot <dbl>, prop_type <chr>, present_use <dbl>

## below I used the keep function to keep the first two lists in the list dataset

housing_data_list %>% 
    purrr::keep(names(.) == '1' | names(.) == '2')
##See results below:
##$`1`
# A tibble: 1 x 24
##`Sale Date`         `Sale Price` sale_reason sale_instrument sale_warning sitetype addr_full    zip5 ctyname postalctyn
##<dttm>                     <dbl>       <dbl>           <dbl> <chr>        <chr>    <chr>       <dbl> <chr>   <chr>     
##    1 2006-01-03 00:00:00       698000           1               3 NA           R1       17021 NE 1~ 98052 REDMOND REDMOND   
# ... with 14 more variables: lon <dbl>, lat <dbl>, building_grade <dbl>, square_feet_total_living <dbl>,
#   bedrooms <dbl>, bath_full_count <dbl>, bath_half_count <dbl>, bath_3qtr_count <dbl>, year_built <dbl>,
#   year_renovated <dbl>, current_zoning <chr>, sq_ft_lot <dbl>, prop_type <chr>, present_use <dbl>

##$`2`
# A tibble: 1 x 24
##`Sale Date`         `Sale Price` sale_reason sale_instrument sale_warning sitetype addr_full    zip5 ctyname postalctyn
##<dttm>                     <dbl>       <dbl>           <dbl> <chr>        <chr>    <chr>       <dbl> <chr>   <chr>     
##    1 2006-01-03 00:00:00       649990           1               3 NA           R1       11927 178T~ 98052 REDMOND REDMOND   
# ... with 14 more variables: lon <dbl>, lat <dbl>, building_grade <dbl>, square_feet_total_living <dbl>,
#   bedrooms <dbl>, bath_full_count <dbl>, bath_half_count <dbl>, bath_3qtr_count <dbl>, year_built <dbl>,
#   year_renovated <dbl>, current_zoning <chr>, sq_ft_lot <dbl>, prop_type <chr>, present_use <dbl>




## Now if I wanted to discard the first list in the list dataset, I would use the discard function


head(housing_data_list) %>% 
    purrr::discard(names(.) == '1')
## As you can see in the results below, the results starts with element 2
##> head(housing_data_list) %>% 
##+     purrr::discard(names(.) == '1')
##$`2`
## A tibble: 1 x 24
##`Sale Date`         `Sale Price` sale_reason sale_instrument sale_warning sitetype addr_full    zip5 ctyname postalctyn
##<dttm>                     <dbl>       <dbl>           <dbl> <chr>        <chr>    <chr>       <dbl> <chr>   <chr>     
##    1 2006-01-03 00:00:00       649990           1               3 NA           R1       11927 178T~ 98052 REDMOND REDMOND   
# ... with 14 more variables: lon <dbl>, lat <dbl>, building_grade <dbl>, square_feet_total_living <dbl>,
#   bedrooms <dbl>, bath_full_count <dbl>, bath_half_count <dbl>, bath_3qtr_count <dbl>, year_built <dbl>,
#   year_renovated <dbl>, current_zoning <chr>, sq_ft_lot <dbl>, prop_type <chr>, present_use <dbl>


## c.	Use the cbind and rbind function on your dataset

### I combined the housing_data_list[1] with the original housing_df data.frame using the cbind function.  Prequisite is
### for rbind or cbind that it has to have the same number of columns or rows.  By combining using cbind, the number
### of variables went up from 24 to 48.  This can be confirmed with viewing the str commands ran on housing_df and then 
### on housing_df2 (another dataset created after cbind in executed)

housing_df2 <-cbind(Housing_df,housing_data_list[1])

head(housing_df2)



str(housing_df2)

## The structure command on teh housing_df2 created by cbind shows variables increased from 24 to 48
## str(housing_df2)
##'data.frame':	12865 obs. of  48 variables:
##    $ Sale Date                 : POSIXct, format: "2006-01-03" "2006-01-03" "2006-01-03" "2006-01-03" ...
##$ Sale Price                : num  698000 649990 572500 420000 369900 ...
##$ sale_reason               : num  1 1 1 1 1 1 1 1 1 1 ...
##$ sale_instrument           : num  3 3 3 3 3 15 3 3 3 3 ...

str(Housing_df)

housing_df3<-rbind(Housing_df,Housing_df)

str(housing_df3)

## As the strs for the newly created using rbind shows that three are 24,730 rows vs. 12,865 as I instructed via
## rbind to combine the housing_df twice.

str(Housing_df)

## d.	Split a string, then concatenate the results back together


head(Housing_df)

library(stringr)
### I would like to see which months sales happen mostly so will create a month column by splitting the monthe data from
###sale date

Year_Month_Day_Sale <- str_split(string = Housing_df$`Sale Date`, pattern = "-")
head(Year_Month_Day_Sale )
## See partial result below:
##[[1]]
##[1] "2006" "01"   "03"  

##[[2]]
##[1] "2006" "01"   "03"  

##[[3]]
##[1] "2006" "01"   "03"  

Sale_Month <- sapply(Year_Month_Day_Sale , "[", 2)
head(Sale_Month)
##See result below:
## [1] "01" "01" "01" "01" "01" "01"




### If I wanted to know quick how many january sales are in my data I can use str_detect.

Sale_Month_Occurance = str_detect(Sale_Month, "01")
sum(Sale_Month_Occurance)
##The results shows below:
## 1] 618

###As the results show, there were 618 occurance.  

### I will then use the mutate function to add the Sale_Month to the data file

mutate(Housing_df, Sale_Month)
##Partial result shows that the No. of variables is 25 vs. 24 since we added Sale_Month
## # A tibble: 12,865 x 25
##`Sale Date`         `Sale Price` sale_reason sale_instrument sale_warning sitetype addr_full   zip5 ctyname postalctyn
##<dttm>                     <dbl>       <dbl>           <dbl> <chr>        <chr>    <chr>      <dbl> <chr>   <chr>     
##    1 2006-01-03 00:00:00       698000           1               3 NA           R1       17021 NE ~ 98052 REDMOND REDMOND   

### Next, I will concatenate the results back together

Year_Month_Day_Sale2<-test1<-sapply(Year_Month_Day_Sale, paste, collapse = " ")
head(Year_Month_Day_Sale2)

## See result below
## [1] "2006 01 03" "2006 01 03" "2006 01 03" "2006 01 03" "2006 01 03" "2006 01 03"

### Additional examples


zip_char<- as.character(Housing_df$zip5)

filter(Housing_df, zip_char == "98074")

zip_char<- as.character(Housing_df$zip5)
## I will then use the year

## I will then use the mutate() command to add this year as a new column in the existing dataframe.

mutate(Housing_df, year)





