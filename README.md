ST 558 Project 2
================
George Bridges and Marcus Lee
6/27/2021

# List of packages we have used

Packages we have used for this project:

-   `ggplot2`
-   `tidyverse`
-   `dplyr`
-   `caret`
-   `tidyr`
-   `readr`
-   `knitr`
-   `regclass`
-   `gbm`
-   `Metrics`

# Automate day reports using Project2.Rmd

So for this project, we are trying to automate R markdown. The code down
below passes a list of the days into `Project2.Rmd`.

Packages required to run the analyses are in the `Project2.Rmd` file.

``` r
days = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday")
output_days_file = paste0(days, ".md")

params = lapply(days, FUN=function(x){
  return(list(day=x))
})

report_by_day = tibble(days, output_days_file, params)
```

# Generating the days’ reports.

``` r
library(rmarkdown)

apply(report_by_day, MARGIN = 1, FUN=function(x){
  render(input = "Project2.Rmd", output_file = x[[2]], params=x[[3]])
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
