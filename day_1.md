Advent of Code - Day 1
================

## Read advent function

Helper function to direclty access the problem data.

``` r
read_advent_input <- function(day = 1, cookie = NULL){
  if(is.null(cookie)){
    cookie <- Sys.getenv("ADVENT_COOKIE")
  }
  if(cookie == ""){
    stop("Unable to find cookie, please specify it in the function or save it as an environment variable using 'Sys.setenv('ADVENT_COOKIE'='<your cookie here>')")
  }
  
  req <- httr::GET(glue::glue('https://adventofcode.com/2020/day/{day}/input'), 
                   httr::add_headers(.headers = c(
                     "user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.198 Safari/537.36",
                     "cookie" = paste0("session=", cookie)
                   )))
  
  
  setdiff(stringr::str_split(rawToChar(req[["content"]]), "\n")[[1]], "")
}
```

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

## Problem data

``` r
input <- read_advent_input(1) %>%
  as.numeric()
```

## 1. Task

``` r
prod(input[input %in% (2020 - input)])
```

    ## [1] 445536

## 2. Task

``` r
input %>%
  purrr::map(~{
    input[input %in% (2020 - .x - input)]
  }) %>%
  purrr::reduce(c) %>%
  unique  %>%
  prod
```

    ## [1] 138688160
