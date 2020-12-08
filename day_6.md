Advent of Code - Day 6
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
input <- read_advent_of_code(6) %>%
  glimpse
```

    ##  chr [1:2227] "elmcifawqgkp" "dynwslejfcrkpv" "" "pagrovuzscemqiy" ...

## Data Wrangling

``` r
answers <- tibble(x = input) %>%
  mutate(group = cumsum(x == "")) %>%
  filter(x != "") %>%
  add_count(group, name = "group_total") %>%
  tidyr::separate_rows(x, sep = "") %>%
  filter(x != "")

tmp <- tibble(answers =  input %>%
                paste(collapse = "\n") %>%
                str_split("\n\n") %>%
                .[[1]]) %>%
  mutate(group = 1:n()) %>%
  tidyr::separate_rows(answers, "sep" = "\n") %>%
  filter(answers != "") %>%
  add_count(group, name = "n_group") %>%
  tidyr::separate_rows(answers, "sep" = "") %>%
  filter(answers != "")
```

## Task 1

``` r
tmp %>%
  distinct(group, answers) %>%
  nrow()
```

    ## [1] 6775

``` r
submit_to_advent_of_code(answer = 6775, day = 6, level = 1)
```

## Task 2

``` r
tmp %>%
  count(group, answers, n_group) %>%
  filter(n == n_group) %>%
  nrow()
```

    ## [1] 3356

``` r
submit_to_advent_of_code(answer = 3356, day = 6, level = 2)
```
