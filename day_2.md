Advent of Code - Day 2
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
input <- read_advent_of_code(2) %>%
  glimpse
```

    ##  chr [1:1000] "6-7 w: wwhmzwtwwk" "10-12 q: qqqqqqqqqqqdqqq" ...

## Data Wranling

``` r
data <- input %>%
  unglue::unglue_data("{min}-{max} {letter}: {password}") %>%
  mutate_at(vars(min, max), as.numeric) %>%
  glimpse
```

    ## Rows: 1,000
    ## Columns: 4
    ## $ min      <dbl> 6, 10, 16, 2, 3, 2, 13, 1, 4, 15, 11, 1, 5, 8, 1, 13, 6, 10,…
    ## $ max      <dbl> 7, 12, 17, 4, 4, 10, 19, 3, 6, 17, 12, 7, 6, 12, 8, 16, 9, 1…
    ## $ letter   <chr> "w", "q", "d", "q", "q", "m", "s", "l", "h", "l", "n", "p", …
    ## $ password <chr> "wwhmzwtwwk", "qqqqqqqqqqqdqqq", "ddddgdddddkddddsxddd", "sh…

## Task 1

``` r
data %>%
  mutate(real = str_count(password, letter), 
         valid = real >= min & real <= max) %>%
  count(valid)
```

    ##   valid   n
    ## 1 FALSE 523
    ## 2  TRUE 477

## Task 2

``` r
data %>% 
  mutate(letter_1 = str_sub(password, min, min), 
         letter_2 = str_sub(password, max, max),
         valid = (letter_1 == letter) + (letter_2 == letter) == 1) %>%
  count(valid)
```

    ##   valid   n
    ## 1 FALSE 314
    ## 2  TRUE 686

## Benchmark between unglue and stringr

``` r
unglue_way <- function(){
  unglue::unglue_data(input, "{min}-{max} {letter}: {password}") %>%
    as_tibble() %>%
    mutate_at(vars(min, max), as.numeric)
}

stringr_way <- function(){
  input %>%
    str_extract_all("(?=\\b)\\w+") %>% #bashR::simule_map(1)
    map_dfr(~{
      tibble::tibble(min = .x[1], max = .x[2], letter = .x[3], password = .x[4])
    }) %>%
    mutate_at(vars(min, max), as.numeric)
}
```

``` r
identical(unglue_way(), stringr_way())
```

    ## [1] TRUE

``` r
microbenchmark::microbenchmark(unglue_way, stringr_way, times = 1000)
```

    ## Unit: nanoseconds
    ##         expr min lq   mean median uq  max neval
    ##   unglue_way  30 33 38.143     33 34 2000  1000
    ##  stringr_way  31 33 36.666     33 34  673  1000
