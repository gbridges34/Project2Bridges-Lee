Project 2
================
George Bridges and Marcus Lee
6/27/2021

# Monday Report

## Introduction section ?

This section should briefly describes the data and the variable I will
have to work with

Target variable will be the `casual` and/or `registered` variables in
some way (perhaps the sum of them, that is up to us)

You should also mention the purpose of the analysis and the methods we
will use to model the response.

## Data

``` r
data_days <- read_csv("day.csv")
```

After looking at the `day.csv`, the variables not selected for this
analysis are listed below:

-   `instant`
-   `dteday`
-   `casual` I will be using the `cnt` variable
-   `registered` I will be using the `cnt` variable

``` r
data_days2 <- data_days %>% select(-c(instant, dteday, casual, registered))
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

The code below will filter by the day

``` r
# The code that is in comment is for the automation later 
# data_by_day <- data_days3 %>% filter(weekday == params$day) 
data_by_day <- data_days3 %>% filter(weekday=="Monday")
```

## Training and Testing set

I will be randomly sample from the data in order to form a training(use
70% of the data) and test set(use 30% of the data). I set a seed to make
the work reproducible.

``` r
set.seed(123)
trainIndex <- createDataPartition(data_by_day$cnt, p=0.7, list = FALSE)

train.data_by_day <- data_by_day[trainIndex,]
test.data_by_day <- data_by_day[-trainIndex,]
```

## Summarization

## Modeling

## Comparison
