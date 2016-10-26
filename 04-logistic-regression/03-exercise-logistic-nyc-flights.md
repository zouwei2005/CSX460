---
title: 'NYCFlights: Arrival Delay Logictic Model'
author: '[Your Name]'
date: '[Today''s Date]'
output: html_document
---




## Logsitic and Inverse Logistic Transformation 

- Write an R function for the logistic function. The function should accept a `numeric` vector with values `[-Inf,Inf]` and produce a numeric vector in the the range `[0,1]`.

- Plot the logistic function from  `[-10,10]`

- Write a R function for the inverse logistic function. The function should accept a `numeric` vector with values `[0,1]` and prodcuce a numeric vector in the range `[-Inf,Inf]`

- Plot the Inverse Logistic function from `[0,1]`


**Hint:** For plotting curves see `?graphics::curve` or `?ggplot2::stat_function`




# NYCFlights Model

Using the rectangular data that you created from assignment and following the *Predicting Medical Expenses* example from the text (*MLwR*), create a model for arr_delay >= 15 minutes. Follow *MLwR* structure   for building a model. Describe/Explain each of the steps and show all work in codeblocks below.

KNIT YOUR DOCUMENT AS *HTML* AND SUBMIT IT AND THE `Rmd` file.   

## Step 1: Collect Data 


```r
setwd("C:/Users/homeguest01/CSX460/04-logistic-regression")
library("readr")
YX <- read_csv("YX.csv")
```

```
## Parsed with column specification:
## cols(
##   .default = col_integer(),
##   origin = col_character(),
##   carrier = col_character(),
##   tailnum = col_character(),
##   dest = col_character(),
##   time_hour = col_datetime(format = ""),
##   dt = col_datetime(format = ""),
##   temp = col_double(),
##   dewp = col_double(),
##   humid = col_double(),
##   wind_speed = col_double(),
##   wind_gust = col_double(),
##   precip = col_double(),
##   pressure = col_double(),
##   visib = col_double(),
##   time_hour.weather = col_datetime(format = "")
## )
```

```
## See spec(...) for full column specifications.
```

```r
spec(YX)
```

```
## cols(
##   origin = col_character(),
##   year = col_integer(),
##   month = col_integer(),
##   day = col_integer(),
##   hour = col_integer(),
##   dep_time = col_integer(),
##   sched_dep_time = col_integer(),
##   dep_delay = col_integer(),
##   arr_time = col_integer(),
##   sched_arr_time = col_integer(),
##   arr_delay = col_integer(),
##   carrier = col_character(),
##   flight = col_integer(),
##   tailnum = col_character(),
##   dest = col_character(),
##   air_time = col_integer(),
##   distance = col_integer(),
##   minute = col_integer(),
##   time_hour = col_datetime(format = ""),
##   dt = col_datetime(format = ""),
##   temp = col_double(),
##   dewp = col_double(),
##   humid = col_double(),
##   wind_dir = col_integer(),
##   wind_speed = col_double(),
##   wind_gust = col_double(),
##   precip = col_double(),
##   pressure = col_double(),
##   visib = col_double(),
##   time_hour.weather = col_datetime(format = "")
## )
```

## Step 2: Explore and Prep The Data


One of the things not done in the MLwR text is a pairwise comparison between the response and each of the predictors. Make sure to do this; this is often very illustrative of the relationship between that predictor and the response. This can be done with `pairs` or `psych::panel.pairs`



```r
str(YX$arr_delay)
```

```
##  int [1:336776] 11 12 -9 -9 -9 -8 3 -11 -17 -14 ...
```

```r
summary(YX$arr_delay)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
##  -86.000  -17.000   -5.000    6.895   14.000 1272.000     9430
```

```r
YX$arr_delay_cat <- cut(YX$arr_delay,
                     breaks=c(-Inf, 15, Inf),
                     labels=c("short","long"))
YX$arr_delay_cat2 <- cut(YX$arr_delay,
                     breaks=c(-Inf, 15, Inf),
                     labels=c("0","1"))
plot(arr_delay_cat ~ dep_delay, data=YX)
```

![plot of chunk Step 2: Explore and Prep The Data](figure/Step 2: Explore and Prep The Data-1.png)

## Step 3:  Train The Model


```r
glm.output1 = glm(arr_delay_cat ~ dep_delay, family=binomial(logit), data=YX)
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```r
#glm.output2 = glm(arr_delay_cat ~ ., family=binomial(logit), data=YX)
```

## Step 4: Evaluate Performance

Think about some of the tools that you have for evaluating performance.  Choose one and articulate why you have chosen it.


```r
summary(glm.output1)
```

```
## 
## Call:
## glm(formula = arr_delay_cat ~ dep_delay, family = binomial(logit), 
##     data = YX)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.5010  -0.4034  -0.3277  -0.2389   3.7365  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -2.3602886  0.0072719  -324.6   <2e-16 ***
## dep_delay    0.1074273  0.0004426   242.7   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 358622  on 327345  degrees of freedom
## Residual deviance: 181305  on 327344  degrees of freedom
##   (9430 observations deleted due to missingness)
## AIC: 181309
## 
## Number of Fisher Scoring iterations: 7
```

```r
summary(glm.output1)
```

```
## 
## Call:
## glm(formula = arr_delay_cat ~ dep_delay, family = binomial(logit), 
##     data = YX)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.5010  -0.4034  -0.3277  -0.2389   3.7365  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -2.3602886  0.0072719  -324.6   <2e-16 ***
## dep_delay    0.1074273  0.0004426   242.7   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 358622  on 327345  degrees of freedom
## Residual deviance: 181305  on 327344  degrees of freedom
##   (9430 observations deleted due to missingness)
## AIC: 181309
## 
## Number of Fisher Scoring iterations: 7
```

 
## Step 5: Improve Performance 

Show some steps for improving model performance.




# Question:

Is this a good model?  (Write your answer here.)


# PART B:

Your model should be good at explaining tardiness. Now, assume that your job is to predict arrival delays a month in advance. You can no longer use all the features in your model. Retrain your model using only features that will be *known* only a month in advance of the departure time.  Show all steps as above.

