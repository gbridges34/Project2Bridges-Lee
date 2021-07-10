ST 558 Project 2
================
George Bridges and Marcus Lee
6/27/2021

For the readme.md:

needs a brief description of the purpose of the repo.

You should also state all the packages required to run the analyses you
do.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(knitr)
```

``` r
#Automation of reports
days = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday")
output_days_file = paste0(days, ".md")

params = lapply(days, FUN=function(x){
  return(list(day=x))
})

report_by_day = tibble(days, output_days_file, params)
```

``` r
# generating the reports
library(rmarkdown)

apply(report_by_day, MARGIN = 1, FUN=function(x){
  render(input = "Project2.Rmd", output_file = x[[1]], params=x[[3]])
})
```

# Reports for each day

## Monday

Here is [Monday’s report](Monday.md).

## Tuesday

Here is [Tuesday’s report](Tuesday.md).

## Wednesday

Here is [Wednesday’s report](Wednesday.md).

## Thursday

Here is [Thursday’s report](Thursday.md).

## Friday

Here is [Friday’s report](Friday.md).

## Saturday

Here is [Saturday’s report](Saturday.md).

## Sunday

Here is [Sunday’s report](Sunday.md).
