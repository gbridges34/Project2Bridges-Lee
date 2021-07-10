Project 2
================
George Bridges and Marcus Lee
6/27/2021

# Friday Report

## Introduction section

This data set contains the daily count of rental bikes between the years
of 2011 and 2012 in the Capital bike share system with the corresponding
weather and seasonal information.

The purpose of our analysis is to try to predict the number of users
using predictive models which will include linear regressions, and
different tree models. See the modeling section to know more about which
models we have used.

## Data

``` r
data_days <- read_csv("day.csv")
```

After looking at the `day.csv`, the variables not selected for Marcus
analysis are listed below:

-   `instant`
-   `dteday`
-   `casual` I will be using the `cnt` variable
-   `registered` I will be using the `cnt` variable
-   `yr`
-   `mnth`
-   `holiday`
-   `workingday`
-   `weathersit`

George decided to include weathersit, but exclude season. I also
included ‘holiday’ and ‘workingday’ I will be using the ‘cnt’ variable
in place of ‘casual’ or ‘registered.’

``` r
data_days2 <- data_days %>% select(-c(instant, dteday, casual, registered, yr, mnth, holiday, workingday, weathersit))
```

``` r
data_days22 <- data_days %>% select(-c(instant, dteday, casual, registered, yr, mnth, season))
```

``` r
data_days3 <- data_days2 %>% mutate(weekday = case_when(weekday==1 ~ "Monday",
                                                        weekday==2 ~ "Tuesday",
                                                        weekday==3 ~ "Wednesday",
                                                        weekday==4 ~ "Thursday",
                                                        weekday==5 ~ "Friday",
                                                        weekday==6 ~ "Saturday",
                                                        weekday==0 ~ "Sunday"))
```

``` r
data_days33 <- data_days22 %>% mutate(weekday = case_when(weekday==1 ~ "Monday",
                                                        weekday==2 ~ "Tuesday",
                                                        weekday==3 ~ "Wednesday",
                                                        weekday==4 ~ "Thursday",
                                                        weekday==5 ~ "Friday",
                                                        weekday==6 ~ "Saturday",
                                                        weekday==0 ~ "Sunday"))
```

The code below will filter by the day.

``` r
# The code that is in comment is for the automation later 
data_by_day <- data_days3 %>% filter(weekday == params$day) 
#data_by_day <- data_days3 %>% filter(weekday=="Monday")
```

``` r
#data_by_day22 <- data_days33 %>% filter(weekday=="Monday")
data_by_day22 <- data_days33 %>% filter(weekday == params$day)
```

## Training and Testing set

I will be randomly sample from the data in order to form a training(use
70% of the data) and test set(use 30% of the data). I set a seed to make
the work reproducible.

``` r
set.seed(123)
train <- sample(1:nrow(data_by_day), size=nrow(data_by_day)*0.7)
test <- dplyr::setdiff(1:nrow(data_by_day), train)

daydataTrain <- data_by_day[train, ]
daydataTest <- data_by_day[test, ]
```

``` r
train2 <- sample(1:nrow(data_by_day22), size=nrow(data_by_day22)*0.7)
test2 <- dplyr::setdiff(1:nrow(data_by_day22), train2)

daydataTrain2 <- data_by_day22[train2, ]
daydataTest2 <- data_by_day22[test2, ]
```

## Summary Statistics

I need to describe the purpose of this summary statistics/plot and what
the reader may be able to determine from it.

``` r
grouped_mean.sd_on_some_variables <- daydataTrain %>% group_by(season) %>% summarise(
  n=n(),
  avg_temp = round(mean(temp),2),
  avg_atemp = round(mean(atemp),2),
  avg_hum = round(mean(hum),2),
  avg_windspeed = round(mean(windspeed),2),
  avg_cnt = round(mean(cnt),2),
  sd_temp = round(sd(temp),2),
  sd_atemp = round(sd(atemp),2),
  sd_hum = round(sd(hum),2),
  sd_windspeed = round(sd(windspeed),2),
  sd_cnt = round(sd(cnt),2)
) %>% rename('Average temperature'=avg_temp,
             'Average feeling temperature'=avg_atemp,
             'Average humidity' = avg_hum,
             'Average Windspeed' = avg_windspeed,
             'Average count of total rental bikes'= avg_cnt,
             'Standard deviation of temperature' = sd_temp,
             'Standard deviation of feeling temperature' = sd_atemp,
             'Standard deviation of humidity' = sd_hum,
             'Standard deviation of windspeed' = sd_windspeed,
             'Standard deviation of count of total rental bikes' = sd_cnt)
```

``` r
grouped_meansd <- daydataTrain2 %>% group_by(weathersit) %>% summarise(
  n=n(),
  avg_temp = round(mean(temp),2),
  avg_atemp = round(mean(atemp),2),
  avg_hum = round(mean(hum),2),
  avg_windspeed = round(mean(windspeed),2),
  avg_cnt = round(mean(cnt),2),
  avg_hol = round(mean(holiday),2),
  avg_work = round(mean(workingday),2),
  sd_temp = round(sd(temp),2),
  sd_atemp = round(sd(atemp),2),
  sd_hum = round(sd(hum),2),
  sd_windspeed = round(sd(windspeed),2),
  sd_cnt = round(sd(cnt),2)
  
) %>% rename('Average temperature'=avg_temp,
             'Average feeling temperature'=avg_atemp,
             'Average humidity' = avg_hum,
             'Average Windspeed' = avg_windspeed,
             'Average count of total rental bikes'= avg_cnt,
             'Proportion of days that are holidays' = avg_hol,
             'Proportion of days that are working days' = avg_work,
             'Standard deviation of temperature' = sd_temp,
             'Standard deviation of feeling temperature' = sd_atemp,
             'Standard deviation of humidity' = sd_hum,
             'Standard deviation of windspeed' = sd_windspeed,
             'Standard deviation of count of total rental bikes' = sd_cnt)
```

``` r
kable(grouped_mean.sd_on_some_variables, caption= "Summary Statistics on some of the variables based off of the different seasons")
```

| season |   n | Average temperature | Average feeling temperature | Average humidity | Average Windspeed | Average count of total rental bikes | Standard deviation of temperature | Standard deviation of feeling temperature | Standard deviation of humidity | Standard deviation of windspeed | Standard deviation of count of total rental bikes |
|-------:|----:|--------------------:|----------------------------:|-----------------:|------------------:|------------------------------------:|----------------------------------:|------------------------------------------:|-------------------------------:|--------------------------------:|--------------------------------------------------:|
|      1 |  15 |                0.32 |                        0.32 |             0.60 |              0.22 |                             2915.07 |                              0.10 |                                      0.09 |                           0.13 |                            0.10 |                                           1138.92 |
|      2 |  19 |                0.50 |                        0.48 |             0.57 |              0.22 |                             4831.79 |                              0.14 |                                      0.13 |                           0.15 |                            0.06 |                                           1958.46 |
|      3 |  22 |                0.73 |                        0.65 |             0.63 |              0.16 |                             5796.14 |                              0.07 |                                      0.11 |                           0.13 |                            0.05 |                                           1546.11 |
|      4 |  16 |                0.44 |                        0.43 |             0.67 |              0.16 |                             4859.00 |                              0.11 |                                      0.10 |                           0.13 |                            0.06 |                                           1596.60 |

Summary Statistics on some of the variables based off of the different
seasons

``` r
kable(grouped_meansd, caption= "Summary Statistics on some of the variables based off of the different weather situations")
```

| weathersit |   n | Average temperature | Average feeling temperature | Average humidity | Average Windspeed | Average count of total rental bikes | Proportion of days that are holidays | Proportion of days that are working days | Standard deviation of temperature | Standard deviation of feeling temperature | Standard deviation of humidity | Standard deviation of windspeed | Standard deviation of count of total rental bikes |
|-----------:|----:|--------------------:|----------------------------:|-----------------:|------------------:|------------------------------------:|-------------------------------------:|-----------------------------------------:|----------------------------------:|------------------------------------------:|-------------------------------:|--------------------------------:|--------------------------------------------------:|
|          1 |  48 |                0.53 |                        0.50 |             0.56 |              0.18 |                             5219.46 |                                    0 |                                        1 |                              0.19 |                                      0.17 |                           0.10 |                            0.07 |                                           1736.92 |
|          2 |  24 |                0.42 |                        0.41 |             0.67 |              0.20 |                             3512.79 |                                    0 |                                        1 |                              0.17 |                                      0.16 |                           0.13 |                            0.07 |                                           1789.23 |

Summary Statistics on some of the variables based off of the different
weather situations

Below we see a contingency table that displays holidays and working
days. As one would expect, no day is both a working day and a holiday

``` r
table(daydataTrain2$holiday, daydataTrain2$workingday)
```

    ##    
    ##      1
    ##   0 72

From the summary statistics produced above, we can inspect the averages
and standard deviations for the `temp`, `atemp` `hum`, `windspeed`,
`cnt` variables grouped by the `season` variable. Based on the sample
size for each `season`, we can expect some of the variables to be
skewed. Each of these statistics are also available in the second table
except grouped by weather situation. Similarly, we can expect some
skewness based on the different numbers of days that were each weather
situation. Also note there are the averages for workingdays and holiday,
showing just how often a working day and holiday

Here below are multiple plots based off of some of the variables in the
`daydataTrain`. For each of these plots, I was trying to get a feeling
like if the weather is much colder, then there would be a lesser amount
of total rental bikes including both the `casual` and `registered`
variable.

``` r
# Box plot seasons temperature
ggplot(daydataTrain, aes(group=season, temp, y="", fill=season))+geom_boxplot()+ggtitle("Boxplot of temperature based off of Season")+ylab("Seasons")+xlab("Temperature")
```

![](Friday_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
# Scatterplot cnt vs temp based off of season

ggplot(daydataTrain, aes(x=cnt, y=temp, color=season))+geom_point()+labs(x="Count of total rental bikes", y="Temperature", title="Scatterplot of Temperature vs Count of total rental bikes based off of seasons")
```

![](Friday_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
# Scatterplot of Temperature vs Windspeed
ggplot(daydataTrain, aes(x=temp,y=windspeed, color=season))+geom_point()+geom_smooth(aes(temp,windspeed),method=lm)+labs(x="Temperature", y="Windspeed", title="Scatterplot of Temperature vs Windspeed")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Friday_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->

``` r
#boxplot temperature versus weather situation
ggplot(daydataTrain2, aes(group=weathersit, temp, y="", fill=weathersit))+geom_boxplot()+ggtitle("Boxplot of temperature based off of Weather Situation") +ylab("Weather Situations")+xlab("Temperature")
```

![](Friday_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
#scatterplot cnt vs humidity based off of weather situation
ggplot(daydataTrain2, aes(x=cnt, y=hum, color=weathersit))+geom_point()+labs(x="Count of total rental bikes", y="Humidity", title="Scatterplot of Humidity vs Count of total rental bikes based off of weather situations")
```

![](Friday_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

``` r
# Scatterplot of Humidity vs Windspeed
ggplot(daydataTrain2, aes(x=hum,y=windspeed, color=weathersit))+geom_point()+geom_smooth(aes(hum,windspeed),method=lm)+labs(x="Humidity", y="Windspeed", title="Scatterplot of Humidity vs Windspeed")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Friday_files/figure-gfm/unnamed-chunk-16-3.png)<!-- -->

Looking at the first boxplot, we can inspect whether the temperatures
are skewed based off of season and see potential outliers. For the
second boxplot, we can see weather humidity is skewed based off weather
situation and see potential outliers.

For the 1st scatterplot, we can inspect the relationship between the
count of total rental bikes against temperature based off of different
seasons. If we were to make separate regression fit lines, we would
expect each of the fit lines to have different slopes because we grouped
by season. So if the total rental bikes increases, we can assume that
the temperature increases as well.

For the 2nd scatterplot, we can inspect the relationship between the
temperature and the windspeed based off of the different seasons. Based
off of the plot, as the temperature increases, windspeed may decrease.

For the 3rd scatterplot, we can see the relationship between total
rental bikes against humidity based on weather situation. I would expect
higher humidity to mean less rented bikes.

For the 4th scatterplot, we can see the relationship between windspeed
and humidity in the different weather situations. I would not expect to
see a strong relationship here for any of the weather situations, though
it is possible that dryer air is lighter and therefore might blow a
little faster but that is pure speculation on George’s part.

## Modeling

A linear regression model attempts to model the relationship between
variables by fitting a linear equation to observed data. The most common
method of fitting is that of least squares, in which the line is ‘drawn’
based on the line that will minimize the sum of the squares of the
vertical deviations of each data point from the line, also called the
residuals of each data point. If a single explanatory variable is used,
the model is called simple linear regression, if multiple explanatory
variables are used then it is called multiple linear regression.

``` r
# Did not include the categorical variables
model_1 <- lm(cnt ~ temp+atemp+hum+windspeed, data=daydataTrain)
summary(model_1)
```

    ## 
    ## Call:
    ## lm(formula = cnt ~ temp + atemp + hum + windspeed, data = daydataTrain)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3290.9 -1164.0  -143.6  1243.2  3104.6 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     5073       1304   3.889 0.000234 ***
    ## temp            7305       3083   2.370 0.020700 *  
    ## atemp          -2284       3489  -0.655 0.514947    
    ## hum            -3748       1394  -2.688 0.009067 ** 
    ## windspeed      -3722       2692  -1.383 0.171271    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1535 on 67 degrees of freedom
    ## Multiple R-squared:  0.3695, Adjusted R-squared:  0.3319 
    ## F-statistic: 9.816 on 4 and 67 DF,  p-value: 2.605e-06

``` r
anova(model_1)
```

    ## Analysis of Variance Table
    ## 
    ## Response: cnt
    ##           Df    Sum Sq  Mean Sq F value    Pr(>F)    
    ## temp       1  73866049 73866049 31.3329 4.377e-07 ***
    ## atemp      1    552433   552433  0.2343   0.62991    
    ## hum        1  13637966 13637966  5.7850   0.01893 *  
    ## windspeed  1   4508769  4508769  1.9126   0.17127    
    ## Residuals 67 157949669  2357458                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
VIF(model_1)
```

    ##      temp     atemp       hum windspeed 
    ##  9.735573  9.840068  1.079310  1.128985

``` r
#included weathersit and temp, the two variables I thought would be most predictive
model_2 <- lm(cnt ~ weathersit+temp, data=daydataTrain2)
summary(model_2)
```

    ## 
    ## Call:
    ## lm(formula = cnt ~ weathersit + temp, data = daydataTrain2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3014.6 -1185.1  -364.1  1365.7  3892.0 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   3560.2      803.9   4.429 3.47e-05 ***
    ## weathersit   -1137.7      382.6  -2.974  0.00405 ** 
    ## temp          5293.9      962.9   5.498 6.06e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1473 on 69 degrees of freedom
    ## Multiple R-squared:  0.4283, Adjusted R-squared:  0.4117 
    ## F-statistic: 25.85 on 2 and 69 DF,  p-value: 4.19e-09

``` r
anova(model_2)
```

    ## Analysis of Variance Table
    ## 
    ## Response: cnt
    ##            Df    Sum Sq  Mean Sq F value    Pr(>F)    
    ## weathersit  1  46603378 46603378  21.466 1.648e-05 ***
    ## temp        1  65624166 65624166  30.227 6.061e-07 ***
    ## Residuals  69 149800511  2171022                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
VIF(model_2)
```

    ## weathersit       temp 
    ##   1.078922   1.078922

The idea of fitting a random forest model is that you take bootstrap
samples, and fit a tree to each of those using a random subset of the
predictors. Finally, you average the results. I used the ‘rf’ method,
the training data set, and repeated cross validation to fit the model, I
then test it on the test data.

``` r
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
#create random forest model
raFore <- train(cnt ~ temp+atemp+hum+windspeed+weathersit,
                method = "rf",
                trControl = trctrl,
                data = daydataTrain2,
                preProcess = c("center", "scale"))
#predict for our cnt variable and compare to test data
raFore_pred <- predict(raFore, newdata = daydataTest2)

raFore_pred2 <- table(raFore_pred, daydataTest2$cnt)
misclass2 <- 1 - (sum(diag(raFore_pred2))/sum(raFore_pred2))
misclass2
```

    ## [1] 0.9375

To predict the total amount of rental bikes, I used the ensemble model
using the `gbm` method and I tuned on the training set(Using repeated
cross validation). Also, I evaluated on the test data set for figuring
out the misclassification rate and I would use this to make a better
model.

``` r
trctrl <- trainControl(method = "repeatedcv", number=10, repeats=5)
#create a boosted model tree
boostree <- train(cnt ~ temp+atemp+hum+windspeed, 
               method = 'gbm', 
               trControl=trctrl, 
               data=daydataTrain, 
               preProcess=c("center", "scale"),
               verbose = FALSE)
#predict the values for our cnt variable and compare it to our testing data. 
boostree_pred <- predict(boostree, newdata=daydataTest)
#a frequency of how many of each response there is. 
boostree_pred2 <- table(boostree_pred, daydataTest$cnt)
misclass <- 1-(sum(diag(boostree_pred2))/sum(boostree_pred2))
misclass
```

    ## [1] 0.875

## Comparison
