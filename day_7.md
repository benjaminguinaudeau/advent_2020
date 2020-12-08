Advent of Code - Day 7
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
input <- read_advent_of_code(7) %>%
  head(-1) %>%
  glimpse
```

    ##  chr [1:594] "drab tan bags contain 4 clear gold bags." ...

``` r
dt <- tibble(containers = str_extract(input, "^.*?(?=bag)"), 
             contained = str_extract_all(input, "\\d\\s.*?(?=bag)")) %>%
  mutate(group_id = 1:n()) %>%
  tidyr::unnest(contained) %>%
  mutate(n_contained = as.numeric(str_extract(contained, "\\d")), 
         contained = str_remove(contained, "\\d\\s"), )


index <- 1
tree <- dt
while(!all(is.na(tree$contained))){
  tree <- tree %>%
    rename(!!paste0("containers_", index) := containers, 
           !!paste0("n_contained_", index + 1) := n_contained, 
           !!paste0("group_id_", index + 1) := group_id, 
           containers = contained) %>%
    left_join(dt, by = c("containers"))
  index <- index + 1
}
```

## Task 1

``` r
tree %>%
  select(contains("containers")) %>%
  filter_at(vars(contains("containers")), any_vars(str_detect(., "shiny gold"))) %>%
  distinct(containers_1) %>%
  filter(str_detect(containers_1, "shiny gold", negate = T))
```

    ## # A tibble: 370 x 1
    ##    containers_1     
    ##    <chr>            
    ##  1 "drab tan "      
    ##  2 "vibrant lime "  
    ##  3 "pale lime "     
    ##  4 "dull gray "     
    ##  5 "light fuchsia " 
    ##  6 "drab gold "     
    ##  7 "striped orange "
    ##  8 "shiny violet "  
    ##  9 "dotted lime "   
    ## 10 "drab magenta "  
    ## # … with 360 more rows

``` r
submit_to_advent_of_code(answer = 370, day = 7, level = 1)
```

## Task 2

``` r
get_content <- function(container){
  
  current <- dt %>%
    filter(str_detect(containers, !!str_trim(container)))
  
  if(nrow(current) == 0) return(0)
  
  sum(current$n_contained * (1 +current$contained %>% map_dbl(get_content)))
}

get_content("shiny gold")
```

    ## [1] 29547

``` r
submit_to_advent_of_code(answer = 29547, day = 7, level = 2)
```
