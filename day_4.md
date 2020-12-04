Advent of Code - Day 4
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
input <- read_advent_of_code(4) %>%
  glimpse
```

    ##  chr [1:1001] "hcl:#6b5442 ecl:brn iyr:2019" "pid:637485594 hgt:171cm" ...

## Data Wrangling

``` r
dt <- input %>%
  paste(collapse = "\n") %>%
  str_split("\n\n") %>%
  .[[1]] %>% 
  map_dfr(~{
    .x %>%
      str_split("\\s") %>%
      .[[1]] %>%
      map_dfc(~{
        tibble::tibble(!!str_extract(.x, "^.+(?=\\:)") := str_extract(.x, "(?<=\\:).+$"))
      })
    
  })
```

## Problem 1

``` r
valid_1 <- dt %>%
  filter_at(vars(hcl, ecl, iyr, pid, hgt, eyr, byr), ~!is.na(.x)) %>%
  glimpse
```

    ## Rows: 196
    ## Columns: 9
    ## $ hcl  <chr> "#6b5442", "#bba027", "6962f7", "#18171d", "#623a2f", "#18171d",…
    ## $ ecl  <chr> "brn", "brn", "oth", "grn", "brn", "hzl", "blu", "brn", "#f615b1…
    ## $ iyr  <chr> "2019", "2028", "1974", "2019", "1957", "2016", "2016", "2019", …
    ## $ pid  <chr> "637485594", "153cm", "2616015", "268398556", "183179186", "7511…
    ## $ hgt  <chr> "171cm", "173cm", "191cm", "67cm", "153cm", "184cm", "191cm", "1…
    ## $ eyr  <chr> "2021", "2027", "2025", "2027", "2029", "2024", "2021", "2021", …
    ## $ byr  <chr> "1986", "2004", "2015", "1951", "2013", "1992", "1938", "1991", …
    ## $ cid  <chr> NA, "54", NA, NA, NA, NA, NA, NA, NA, NA, "159", "59", "96", "19…
    ## $ `NA` <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …

``` r
submit_to_advent_of_code(answer = 196, day = 4, level = 1)
```

## Problem 2

``` r
btwn <- function(x, low, high){
  return(x >= low & x <= high)
}

valid_1 %>%
  # Years
  filter(str_detect(byr, "^\\d{4}$") & btwn(as.numeric(byr), 1920, 2002)) %>%
  filter(str_detect(iyr, "^\\d{4}$") & btwn(as.numeric(iyr), 2010, 2020)) %>%
  filter(str_detect(eyr, "^\\d{4}$") & btwn(as.numeric(eyr), 2020, 2030)) %>%
  # Height
  filter(str_detect(hgt, "\\d+(cm|in)"))%>%
  filter(ifelse(str_detect(hgt, "cm"), 
                btwn(as.numeric(str_extract(hgt, "\\d+")), 150, 193),
                btwn(as.numeric(str_extract(hgt, "\\d+")), 59, 76))) %>%
  # Colors
  filter(str_detect(hcl, "#[[0-9a-f]]{6}")) %>%
  filter(ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")) %>%
  # Passport ID
  filter(str_detect(str_pad(pid, "0", 9, side = "right"), "^\\d{9}$"))
```

    ## # A tibble: 114 x 9
    ##    hcl     ecl   iyr   pid       hgt   eyr   byr   cid   `NA` 
    ##    <chr>   <chr> <chr> <chr>     <chr> <chr> <chr> <chr> <chr>
    ##  1 #6b5442 brn   2019  637485594 171cm 2021  1986  <NA>  <NA> 
    ##  2 #18171d hzl   2016  751161201 184cm 2024  1992  <NA>  <NA> 
    ##  3 #b6652a blu   2016  313406514 191cm 2021  1938  <NA>  <NA> 
    ##  4 #623a2f brn   2019  145249653 167cm 2021  1991  <NA>  <NA> 
    ##  5 #fffffd hzl   2012  912552538 160cm 2023  1946  159   <NA> 
    ##  6 #18171d amb   2015  782818257 72in  2026  1952  59    <NA> 
    ##  7 #602927 amb   2018  783160698 173cm 2026  1986  96    <NA> 
    ##  8 #a97842 hzl   2011  912273414 171cm 2030  1960  199   <NA> 
    ##  9 #ceb3a1 amb   2013  567057004 156cm 2029  1942  116   <NA> 
    ## 10 #888785 blu   2012  028571975 160cm 2029  1933  <NA>  <NA> 
    ## # … with 104 more rows

``` r
submit_to_advent_of_code(answer = 114, day = 4, level = 2)
```
