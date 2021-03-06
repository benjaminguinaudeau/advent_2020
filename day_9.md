Advent of Code - Day 9
================

## Packages

``` r
# devtools::install_github("benjaminguinaudeau/adventr")
suppressMessages(library(dplyr))
library(stringr)
library(purrr)
library(adventr)
```

## Problem data

``` r
input <- read_advent_of_code(9) %>%
  head(-1) %>%
  as.numeric %>%
  glimpse
```

    ##  num [1:1000] 17 14 2 35 39 31 5 25 1 29 ...

## Task 1

``` r
1:length(input) %>%
 map_lgl(~{
   if(.x < 26) return(F)
    !any((input[.x] - input[(.x-26):(.x-1)]) %in% setdiff(input[(.x-26):(.x-1)], input[.x]/2))    
  }) %>%
  which
```

    ## [1] 501

``` r
submit_to_advent_of_code(answer = input[501], day = 9, level = 1)
```

## Task 2

``` r
target <- input[501]

trig <- T
index <- 5
while(index < 501 & trig){
  index <- index + 1
  if(target %in% cumsum(input[index:501])){
    trig <- F
    out <- which(cumsum(input[index:501]) == target)
  }
}

vec <- input[index:(index + out -1)]
min(vec) + max(vec)
```

    ## [1] 1261309

## Task 2

``` r
submit_to_advent_of_code(answer = 1261309, day = 9, level = 2)
```
