Advent of Code - Day 3
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
input <- read_advent_of_code(3) %>%
  glimpse
```

    ##  chr [1:323] ".........#..##.##.............." ...

## Itinary helper

This function computes the itinary of your tobbogan given a slope and an
intercept.

``` r
get_itinary <- function(res, slope = 1, intercept = 1){
  tibble(pattern = res[seq(1, length(res), intercept)],
         slope = slope, 
         intercept = intercept) %>%
    mutate(y = 1:n(), 
           x = 1 + slope*(y-1), 
           modulo = str_length(pattern)) %>%
    split(1:nrow(.)) %>% 
    map_dfr(~{
      
      .x$sign <- str_extract(.x$pattern, paste0("(?<=^.{", (.x$x -1) %% .x$modulo, "})."))
      
      .x$new_pattern <- str_replace(.x$pattern, 
                                    paste0("(?<=^.{", (.x$x -1) %% .x$modulo, "})."),
                                    ifelse(.x$sign == "#", "X", "0"))
      
      return(.x)
    })
  
}
```

## Task 1

``` r
input %>%
  get_itinary(slope = 3, intercept = 1) %>%
  summarise(n_trees = sum(sign == "#")) %>%
  glimpse
```

    ## Rows: 1
    ## Columns: 1
    ## $ n_trees <int> 268

## Task 2

``` r
bind_rows(
  input %>% get_itinary(slope = 1, intercept = 1),
  input %>% get_itinary(slope = 3, intercept = 1),
  input %>% get_itinary(slope = 5, intercept = 1),
  input %>% get_itinary(slope = 7, intercept = 1),
  input %>% get_itinary(slope = 1, intercept = 2),
) %>%
  group_by(slope, intercept) %>% 
  summarise(n_trees = sum(sign == "#")) %>%
  pull(n_trees) %>%
  prod %>%
  glimpse
```

    ## `summarise()` regrouping output by 'slope' (override with `.groups` argument)

    ##  num 3.09e+09
