TidyTuesday 2018 - Week 24 - Cats vs Dogs (USA)
================
Roberto Preste
2018-09-11

This is my work for week 24 (2018) of the
[\#TidyTuesday](https://thomasmock.netlify.com/post/tidytuesday-a-weekly-social-data-project-in-r/)
project.

We are dealing with puppies this week\! While the original article from
the [Washington
Post](https://www.washingtonpost.com/news/wonk/wp/2014/07/28/where-cats-are-more-popular-than-dogs-in-the-u-s-and-all-over-the-world/?utm_term=.670d783ef6cc)
shows the distribution of cats and dogs in the entire globe, for this
week we’ll focus only on data coming from the USA, using a dataset
offered by
[data.world](https://data.world/datanerd/cat-vs-dog-popularity-in-u-s).

All code and data can be found in my dedicated GitHub repository
[MyTidyTuesday](https://github.com/robertopreste/MyTidyTuesday).

-----

``` r
library(tidyverse)
library(magrittr)
library(readxl)
library(skimr)
```

-----

Let’s first read in the data and rename the columns for simplicity.

``` r
df <- read_excel("data/catsvdogs.xlsx", skip = 1, 
                 col_names = c("location", "households_1000", "perc_households_pets", 
                               "num_pet_households_1000", "perc_dog_owners", 
                               "dog_own_households_1000", "mean_num_dogs_per_household", 
                               "dog_population_1000", "perc_cat_owners", "cat_own_households_1000", 
                               "mean_num_cats_per_household", "cat_population_1000"))
```

``` r
head(df)
```

    ## # A tibble: 6 x 12
    ##   location households_1000 perc_households… num_pet_househo… perc_dog_owners dog_own_househo… mean_num_dogs_p…
    ##   <chr>              <dbl>            <dbl>            <dbl>           <dbl>            <dbl>            <dbl>
    ## 1 Alabama             1828             59.5             1088            44.1              807              1.7
    ## 2 Arizona             2515             59.5             1497            40.1             1008              1.8
    ## 3 Arkansas            1148             62.4              716            47.9              550              2  
    ## 4 Califor…           12974             52.9             6865            32.8             4260              1.6
    ## 5 Colorado            1986             61.3             1217            42.5              845              1.6
    ## 6 Connect…            1337             54.4              728            28.3              379              1.3
    ## # … with 5 more variables: dog_population_1000 <dbl>, perc_cat_owners <dbl>, cat_own_households_1000 <dbl>,
    ## #   mean_num_cats_per_household <dbl>, cat_population_1000 <dbl>

-----

## Data Exploration

Now we can have a look at the data structure.

``` r
skim(df)
```

    ## Skim summary statistics
    ##  n obs: 49 
    ##  n variables: 12 
    ## 
    ## ── Variable type:character ──────────────────────────────────────────────────────────────────────────────────────
    ##  variable missing complete  n min max empty n_unique
    ##  location       0       49 49   4  20     0       49
    ## 
    ## ── Variable type:numeric ────────────────────────────────────────────────────────────────────────────────────────
    ##                     variable missing complete  n    mean      sd    p0   p25    p50    p75    p100     hist
    ##      cat_own_households_1000       0       49 49  728.06  717.29  33   247    501    876    3687   ▇▅▂▁▁▁▁▁
    ##          cat_population_1000       0       49 49 1492.8  1459.86  63   514   1185   1844    7118   ▇▅▂▁▁▁▁▁
    ##      dog_own_households_1000       0       49 49  876.37  891.83  38   273    638   1069    4260   ▇▆▂▁▁▁▁▁
    ##          dog_population_1000       0       49 49 1414.16 1464.66  42   410   1097   1798    7163   ▇▅▂▁▁▁▁▁
    ##              households_1000       0       49 49 2403.9  2514.05 221   765   1759   2632   12974   ▇▃▁▁▁▁▁▁
    ##  mean_num_cats_per_household       0       49 49    2.04    0.19   1.7   1.9    2      2.2     2.6 ▅▇▇▆▅▂▁▂
    ##  mean_num_dogs_per_household       0       49 49    1.59    0.2    1.1   1.4    1.6    1.7     2.1 ▁▁▃▇▂▂▁▁
    ##      num_pet_households_1000       0       49 49 1342.59 1358.25  63   475    957   1611    6865   ▇▆▂▁▁▁▁▁
    ##              perc_cat_owners       0       49 49   31.64    5.68  11.6  29     31.3   33.8    49.5 ▁▁▁▇▇▂▁▁
    ##              perc_dog_owners       0       49 49   36.97    6.67  13.1  32.9   36.6   42.5    47.9 ▁▁▁▃▇▇▇▆
    ##         perc_households_pets       0       49 49   56.86    6.93  21.9  53.6   56.8   61.3    70.8 ▁▁▁▁▂▇▆▁

Luckily there are no missing values, so we can proceed with our
analysis.

-----

### Pet-friendly households per US State

Let’s first visualize the percentage of household with pets in each
State.

``` r
df %>% 
    ggplot(aes(x = reorder(location, -perc_households_pets), 
               y = perc_households_pets, fill = location)) + 
    geom_col() + 
    coord_flip() + 
    labs(x = "US State", y = "%", title = "Percentage of households with pets", subtitle = "District of Columbia seems to be not so pet-friendly.") + 
    guides(fill = FALSE)
```

![](Week_24_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

It is clear that in every US State at least half of the households have
pets; particularly, we can see that Vermont is definitely a pet-friendly
State, with more than 70% household having at least one dog or cat.  
District of Columbia, instead, doesn’t seem to like pets this much,
scoring a little more than 20% in this chart.

### Dog- and cat-owning households

Let’s see if there is any preference for dogs over cats (or viceversa)
in these States.

``` r
df %>% 
    ggplot(aes(x = reorder(location, -perc_households_pets), fill = location)) + 
    geom_col(aes(y = dog_own_households_1000 - cat_own_households_1000)) + 
    coord_flip() + 
    labs(x = "US State", y = "Difference (in 1000s households)", 
         title = "Dog- vs cat-owning households", 
         subtitle = "Households with dogs definitely outnumber those hosting cats.") + 
    guides(fill = FALSE) + 
    scale_y_continuous(breaks = c(-250, 0, 250, 500, 750, 1000, 1250))
```

![](Week_24_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

For this plot I computed the difference between dog-owning households
and cat-owning ones, in thousands: negative values represent a
preference for cats, while positive values denote a higher number of
households hosting dogs.  
A couple of peculiar data points are Texas and Massachusetts, where
people seem to definitely love dogs, in the former case, and cats, in
the latter.

### Mean number of dogs/cats per household

We might be interested in knowing whether, as the number of households
with pets increases, so does the mean number of dogs/cats hosted in each
household. Let’s find out.

``` r
gath_df <- df %>% 
    mutate(dogs = mean_num_dogs_per_household, 
           cats = mean_num_cats_per_household) %>% 
    select(location, num_pet_households_1000, dogs, cats) %>% 
    gather(key = "pet", value = "value", dogs, cats)
```

``` r
gath_df
```

    ## # A tibble: 98 x 4
    ##    location             num_pet_households_1000 pet   value
    ##    <chr>                                  <dbl> <chr> <dbl>
    ##  1 Alabama                                 1088 dogs    1.7
    ##  2 Arizona                                 1497 dogs    1.8
    ##  3 Arkansas                                 716 dogs    2  
    ##  4 California                              6865 dogs    1.6
    ##  5 Colorado                                1217 dogs    1.6
    ##  6 Connecticut                              728 dogs    1.3
    ##  7 Delaware                                 189 dogs    1.4
    ##  8 District of Columbia                      63 dogs    1.1
    ##  9 Florida                                 4138 dogs    1.5
    ## 10 Georgia                                 2093 dogs    1.6
    ## # … with 88 more rows

``` r
gath_df %>% 
    ggplot(aes(x = num_pet_households_1000, y = value, color = pet)) + 
    geom_smooth() + 
    geom_point(alpha = 0.5) + 
    labs(x = "Households (in 1000s)", y = "Number of pets", 
         title = "Mean number of dogs/cats per household", 
         subtitle = "The number of pets per household seems to reach a plateau after 1M households with pets.")
```

![](Week_24_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Although the data are a bit messy, an interesting trend is visible here:
initially, as the number of households with pets grows, the mean number
of pets per household steeply grows as well, and this is true for both
dogs and cats. However, after about the first million of households with
pets, the mean number of pets seems to reach a plateau, with cats
outnumbering dogs on average.

-----

## Discussion

So we have found two interesting things here:

  - most households have a kind of preference for dogs over cats, but  
  - on average, there are more cats than dogs in each household.

With these information, we can try to normalize the number of households
with pets by the mean number of dogs/cats hosted.

``` r
norm_df <- df %>% 
    mutate(norm_dogs = dog_own_households_1000 * mean_num_dogs_per_household, 
           norm_cats = cat_own_households_1000 * mean_num_cats_per_household) %>% 
    select(location, norm_dogs, norm_cats, perc_households_pets)
```

``` r
norm_df
```

    ## # A tibble: 49 x 4
    ##    location             norm_dogs norm_cats perc_households_pets
    ##    <chr>                    <dbl>     <dbl>                <dbl>
    ##  1 Alabama                 1372.     1252.                  59.5
    ##  2 Arizona                 1814.     1412.                  59.5
    ##  3 Arkansas                1100       807.                  62.4
    ##  4 California              6816      7005.                  52.9
    ##  5 Colorado                1352      1220.                  61.3
    ##  6 Connecticut              493.      811.                  54.4
    ##  7 Delaware                 158.      192.                  56.6
    ##  8 District of Columbia      41.8      62.7                 21.9
    ##  9 Florida                 4077      4366.                  54.4
    ## 10 Georgia                 2435.     2178.                  55.1
    ## # … with 39 more rows

``` r
norm_df %>% 
    ggplot(aes(x = reorder(location, -perc_households_pets), fill = location)) + 
    geom_col(aes(y = norm_dogs - norm_cats)) + 
    coord_flip() + 
    labs(x = "US State", y = "Difference (in 1000s households)", 
         title = "Dog- vs cat-owning households (normalized)", 
         subtitle = "With normalized data, we see that cats win the fight.") + 
    guides(fill = FALSE) + 
    scale_y_continuous(breaks = c(-1000, -500, 0, 500, 1000, 1500))
```

![](Week_24_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

With this computation, we can clearly see that most US States host more
cats than dogs in their houses.  
We could have reached the same conclusion by simply plotting the total
population of dogs and cats, with a few differences.

``` r
df %>% 
    ggplot(aes(x = reorder(location, -perc_households_pets), fill = location)) + 
    geom_col(aes(y = dog_population_1000 - cat_population_1000)) + 
    coord_flip() + 
    labs(x = "US State", y = "Difference (in 1000s pets)", 
         title = "Difference of dog/cat population", 
         subtitle = "Most US States host cats, rather than dogs.") + 
    guides(fill = FALSE) + 
    scale_y_continuous(breaks = c(-1000, -500, 0, 500, 1000, 1500))
```

![](Week_24_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

-----

``` r
sessionInfo()
```

    ## R version 3.6.0 (2019-04-26)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: macOS Mojave 10.14.5
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] skimr_1.0.6     readxl_1.3.1    magrittr_1.5    forcats_0.4.0   stringr_1.4.0   dplyr_0.8.1     purrr_0.3.2    
    ##  [8] readr_1.3.1     tidyr_0.8.3     tibble_2.1.1    ggplot2_3.1.1   tidyverse_1.2.1
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_0.2.5 xfun_0.7         haven_2.1.0      lattice_0.20-38  colorspace_1.4-1 generics_0.0.2  
    ##  [7] vctrs_0.1.0      htmltools_0.3.6  yaml_2.2.0       utf8_1.1.4       rlang_0.3.4      pillar_1.4.1    
    ## [13] glue_1.3.1       withr_2.1.2      modelr_0.1.4     plyr_1.8.4       munsell_0.5.0    gtable_0.3.0    
    ## [19] cellranger_1.1.0 rvest_0.3.4      evaluate_0.14    labeling_0.3     knitr_1.23       fansi_0.4.0     
    ## [25] broom_0.5.2      Rcpp_1.0.1       scales_1.0.0     backports_1.1.4  jsonlite_1.6     hms_0.4.2       
    ## [31] digest_0.6.19    stringi_1.4.3    grid_3.6.0       cli_1.1.0        tools_3.6.0      lazyeval_0.2.2  
    ## [37] crayon_1.3.4     pkgconfig_2.0.2  zeallot_0.1.0    xml2_1.2.0       lubridate_1.7.4  assertthat_0.2.1
    ## [43] rmarkdown_1.13   httr_1.4.0       rstudioapi_0.10  R6_2.4.0         nlme_3.1-139     compiler_3.6.0
