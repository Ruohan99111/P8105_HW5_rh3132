HW5 document
================

``` r
library (tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readr)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(ggplot2)
```

\#problem1

``` r
data_url = "https://raw.githubusercontent.com/washingtonpost/data-homicides/master/homicide-data.csv"
homicide_data = read.csv(data_url)

formatted_data =
  homicide_data |>
  mutate(combined_city_state = paste(city, state, sep = ", ")) |> 
  group_by(combined_city_state) |> 
  summarise(
    total_cases = n(), 
    unresolved_cases = sum(disposition %in% c("Closed without arrest", "Open/No arrest"))  
  )

formatted_data = formatted_data[formatted_data$combined_city_state != "Tulsa, AL", ] 
formatted_data
```

    ## # A tibble: 50 × 3
    ##    combined_city_state total_cases unresolved_cases
    ##    <chr>                     <int>            <int>
    ##  1 Albuquerque, NM             378              146
    ##  2 Atlanta, GA                 973              373
    ##  3 Baltimore, MD              2827             1825
    ##  4 Baton Rouge, LA             424              196
    ##  5 Birmingham, AL              800              347
    ##  6 Boston, MA                  614              310
    ##  7 Buffalo, NY                 521              319
    ##  8 Charlotte, NC               687              206
    ##  9 Chicago, IL                5535             4073
    ## 10 Cincinnati, OH              694              309
    ## # ℹ 40 more rows

Baltimore analysis

``` r
baltimore_data = homicide_data |> 
  mutate(combined_city_state = paste(city, state, sep = ", ")) |> 
  mutate(case_status = ifelse(disposition == "Closed by arrest", "resolved_cases", "unresolved_cases")) |> 
  filter(combined_city_state == "Baltimore, MD")

baltimore_analysis = prop.test(x = sum(baltimore_data$case_status %in% c("unresolved_cases")), n = nrow(baltimore_data), correct = FALSE)

baltimore_analysis_refined = broom::tidy(baltimore_analysis) |> 
  mutate(conf_interval = paste(conf.low, conf.high, sep = ", ")) |> 
  rename(proportion_estimate = estimate) |> 
  select(proportion_estimate, conf_interval)

baltimore_analysis_refined
```

    ## # A tibble: 1 × 2
    ##   proportion_estimate conf_interval                       
    ##                 <dbl> <chr>                               
    ## 1               0.646 0.627741062080148, 0.662985215590964

City wide analysis

``` r
analysis_df = formatted_data |> 
  mutate(
    test_result_per_city = map2(unresolved_cases, total_cases, ~prop.test(x = .x, n = .y)),
    tidy_result_per_city = map(test_result_per_city, broom::tidy)
  ) |> 
  select(combined_city_state, tidy_result_per_city) |> 
  unnest(tidy_result_per_city) |> 
  select(combined_city_state, estimate, conf.low, conf.high) 

tidy_city_df = analysis_df |> 
  mutate(ci_range = paste(conf.low, conf.high, sep = ", ")) |> 
  rename(proportion_unresolved = estimate) |> 
  select(combined_city_state, proportion_unresolved, ci_range)

tidy_city_df
```

    ## # A tibble: 50 × 3
    ##    combined_city_state proportion_unresolved ci_range                           
    ##    <chr>                               <dbl> <chr>                              
    ##  1 Albuquerque, NM                     0.386 0.337260384254284, 0.4375766065555…
    ##  2 Atlanta, GA                         0.383 0.352811897036302, 0.4148218839536…
    ##  3 Baltimore, MD                       0.646 0.627562457662644, 0.6631598604016…
    ##  4 Baton Rouge, LA                     0.462 0.414198741860307, 0.5110239600187…
    ##  5 Birmingham, AL                      0.434 0.399188948632167, 0.4689557481890…
    ##  6 Boston, MA                          0.505 0.464621930200304, 0.5450880517726…
    ##  7 Buffalo, NY                         0.612 0.568798964634228, 0.6540879392535…
    ##  8 Charlotte, NC                       0.300 0.26608198188312, 0.335899860867845
    ##  9 Chicago, IL                         0.736 0.723995888425454, 0.7473997873066…
    ## 10 Cincinnati, OH                      0.445 0.407960574220688, 0.4831438806189…
    ## # ℹ 40 more rows

Visualization

``` r
ggplot(analysis_df, aes(x = reorder(combined_city_state, estimate), y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "blue") +
  coord_flip() +  
  labs(x = "City",
       y = "Proportion of Unsolved Homicides",
       title = "City-wise Unsolved Homicide Proportions and Confidence Intervals") +
  theme_minimal()
```

![](HW5_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

\#Problem 2

``` r
data_files = list.files(path = "./data/", full.names = TRUE)
```
