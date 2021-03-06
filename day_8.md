Advent of Code - Day 8
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
input <- read_advent_of_code(8) %>%
  head(-1) %>%
  glimpse
```

    ##  chr [1:636] "acc -13" "jmp +37" "acc -19" "jmp +1" "jmp +1" "jmp +413" ...

## Data wrangling

``` r
dt <- tibble::tibble(input) %>%
  tidyr::separate(input, c("action", "param"), sep = "\\s", convert = T)

run_program <- function(dt){
  acc <- 0 ; index <- 1 ; already <- c()
  
  while(index <= nrow(dt)){
    if(index %in% already) return(list(acc_rec = acc))
    already <- c(index, already)
    
    if(dt[index,]$action == "acc"){ acc <- acc + dt[index,]$param ; index <- index + 1 }
    if(dt[index,]$action == "nop") index <- index + 1
    if(dt[index,]$action == "jmp") index <- index + dt[index,]$param
  }
  return(list(acc_final = acc))
}
```

## Task 1

``` r
run_program(dt)
```

    ## $acc_rec
    ## [1] 1939

``` r
submit_to_advent_of_code(answer = 1939, day = 8, level = 1)
```

## Task 2

``` r
to_test <- which(dt$action != "acc")

to_test %>% #bashR::simule_map(1)
  walk(~{
    dt$action[.x] <- ifelse(dt$action[.x] == "nop", "jmp", "nop")
    out <- run_program(dt)
    if(!is.null(out[["acc_final"]])){
      print(.x)
      print(out)
    }
  })
```

    ## [1] 414
    ## $acc_final
    ## [1] 2212

## Task 2

``` r
submit_to_advent_of_code(answer = 2212, day = 8, level = 2)
```
