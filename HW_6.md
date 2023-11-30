HW_6
================
Amy Liu
2023-11-29

## Problem 1

- Create a city_state variable (e.g. “Baltimore, MD”), and a binary
  variable indicating whether the homicide is solved. Omit cities
  Dallas, TX; Phoenix, AZ; and Kansas City, MO – these don’t report
  victim race. Also omit Tulsa, AL – this is a data entry mistake. For
  this problem, limit your analysis those for whom victim_race is white
  or black. Be sure that victim_age is numeric.

``` r
homicide_df = 
  read_csv("./homicide-data.csv", na = c("", "NA", "Unknown")) %>% 
  mutate(city_state = str_c(city, state, sep = ", ")) %>% 
  mutate(solved = ifelse(disposition == "Closed by arrest",1,0)) %>% 
  mutate(victim_age = as.numeric(victim_age)) %>% 
  filter(!city_state %in% c("Dallas, TX", "Phoenix, AZ", "Kansas City, MO","Tulsa, AL"), 
         (victim_race %in% c("White", "Black")))
```

    ## Rows: 52179 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (8): uid, victim_last, victim_first, victim_race, victim_sex, city, stat...
    ## dbl (4): reported_date, victim_age, lat, lon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

- For the city of Baltimore, MD, use the glm function to fit a logistic
  regression with resolved vs unresolved as the outcome and victim age,
  sex and race as predictors. Save the output of glm as an R object;
  apply the broom::tidy to this object; and obtain the estimate and
  confidence interval of the adjusted odds ratio for solving homicides
  comparing male victims to female victims keeping all other variables
  fixed.

``` r
baltimore = 
  homicide_df %>% 
  filter(city == "Baltimore") %>% 
  mutate(victim_sex = as.factor(victim_sex),
         victim_race = as.factor(victim_race),
         solved = factor(solved, levels = c(0, 1)))

baltimore_logit_model = 
  glm(solved ~ victim_age + victim_race + victim_sex,
      data = baltimore, 
      family = binomial())

baltimore_model=
  baltimore_logit_model %>% 
  broom::tidy() %>% 
  knitr::kable(digits = 3)

baltimore_model
```

| term             | estimate | std.error | statistic | p.value |
|:-----------------|---------:|----------:|----------:|--------:|
| (Intercept)      |    0.310 |     0.171 |     1.810 |   0.070 |
| victim_age       |   -0.007 |     0.003 |    -2.024 |   0.043 |
| victim_raceWhite |    0.842 |     0.175 |     4.818 |   0.000 |
| victim_sexMale   |   -0.854 |     0.138 |    -6.184 |   0.000 |

``` r
save(baltimore_model, file = "./baltimore_logit_model")

odd_ratio = baltimore_logit_model %>% 
  broom::tidy() %>% 
  filter(term == 'victim_sexMale') %>% 
  summarise(estimate = estimate,
            odd_ratio = exp(estimate),
            odd_ratio_lower = exp(estimate - 1.96 * std.error),
            odd_ratio_upper = exp(estimate + 1.96 * std.error)
         ) %>% 
  knitr::kable(digits = 3)

odd_ratio
```

| estimate | odd_ratio | odd_ratio_lower | odd_ratio_upper |
|---------:|----------:|----------------:|----------------:|
|   -0.854 |     0.426 |           0.325 |           0.558 |

- Run glm for each of the cities in your dataset, and extract the
  adjusted odds ratio (and CI) for solving homicides comparing male
  victims to female victims

``` r
logit_model= 
  homicide_df %>% 
  nest(data = -city_state) %>% 
  mutate(
    logit_model = map(.x = data, ~glm(solved ~ victim_age + victim_race + victim_sex, data = .x, family = binomial())),
    logit_results = map(logit_model, broom::tidy)
    ) %>%
  select(city_state, logit_results) %>% 
  unnest(logit_results) %>% 
  mutate(
    OR = exp(estimate),
    CI_lower = exp(estimate - 1.96 * std.error),
    CI_upper = exp(estimate + 1.96 * std.error))

logit_model
```

    ## # A tibble: 188 × 9
    ##    city_state term  estimate std.error statistic p.value    OR CI_lower CI_upper
    ##    <chr>      <chr>    <dbl>     <dbl>     <dbl>   <dbl> <dbl>    <dbl>    <dbl>
    ##  1 Albuquerq… (Int…  6.12e-1   0.632    0.969    3.33e-1 1.84     0.534    6.37 
    ##  2 Albuquerq… vict… -1.97e-2   0.00909 -2.16     3.04e-2 0.981    0.963    0.998
    ##  3 Albuquerq… vict…  4.12e-1   0.416    0.990    3.22e-1 1.51     0.668    3.41 
    ##  4 Albuquerq… vict…  5.70e-1   0.385    1.48     1.39e-1 1.77     0.831    3.76 
    ##  5 Atlanta, … (Int…  8.69e-1   0.242    3.60     3.21e-4 2.39     1.49     3.83 
    ##  6 Atlanta, … vict… -1.16e-2   0.00466 -2.50     1.24e-2 0.988    0.979    0.997
    ##  7 Atlanta, … vict…  2.69e-1   0.284    0.945    3.45e-1 1.31     0.749    2.28 
    ##  8 Atlanta, … vict…  7.71e-5   0.194    0.000397 1.00e+0 1.00     0.684    1.46 
    ##  9 Baltimore… (Int…  3.10e-1   0.171    1.81     7.04e-2 1.36     0.975    1.91 
    ## 10 Baltimore… vict… -6.73e-3   0.00332 -2.02     4.30e-2 0.993    0.987    1.00 
    ## # ℹ 178 more rows

- Create a plot that shows the estimated ORs and CIs for each city.
  Organize cities according to estimated OR, and comment on the plot.

``` r
logit_model %>% 
  filter(term == "victim_sexMale") %>% 
  mutate(city_state = fct_reorder(city_state, OR)) %>% 
  ggplot(aes(x = city_state, y = OR)) +
  geom_point() + 
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
```

<img src="HW_6_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" />

## Problem 2

- Use 5000 bootstrap samples and, for each bootstrap sample, produce
  estimates of these two quantities. Plot the distribution of your
  estimates, and describe these in words. Using the 5000 bootstrap
  estimates, identify the 2.5% and 97.5% quantiles to provide a 95%
  confidence interval for r2 and log(β̂ 0∗β̂2)

load data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2022-01-01",
    date_max = "2022-12-31") |>
  mutate(
    name = recode(id, USW00094728 = "CentralPark_NY"),
    tmin = tmin / 10,
    tmax = tmax / 10) |>
  select(name, id, everything())
```

    ## using cached file: /Users/zhiqiliu/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2023-11-29 21:02:10.152205 (8.544)

    ## file min/max dates: 1869-01-01 / 2023-11-30

``` r
bootstrap_estimates=
  weather_df %>% 
  modelr::bootstrap(n = 5000) %>% 
  mutate(
    weather_linear = map(strap, ~lm(tmax ~ tmin +prcp, data = .x)),
    weather_tidy= map(weather_linear, broom::tidy),
    weather_glance = map(weather_linear, broom::glance))%>%
  unnest(weather_tidy,weather_glance) %>% 
  select(term, estimate, r.squared) %>% 
  pivot_wider(names_from = term, values_from = estimate)%>%
  rename(
    beta_0 = `(Intercept)`,
    beta_1 = tmin,
    beta_2 = prcp)
```

    ## Warning: `unnest()` has a new interface. See `?unnest` for details.
    ## ℹ Try `df %>% unnest(c(weather_tidy, weather_glance))`, with `mutate()` if
    ##   needed.

``` r
result_bootstrap = 
  bootstrap_estimates %>% 
  mutate(r.squared = r.squared,
         log_beta1_2 = log(beta_1 * beta_2)) %>%
  na.omit()
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `log_beta1_2 = log(beta_1 * beta_2)`.
    ## Caused by warning in `log()`:
    ## ! NaNs produced

``` r
head(result_bootstrap)
```

    ## # A tibble: 6 × 5
    ##   r.squared beta_0 beta_1   beta_2 log_beta1_2
    ##       <dbl>  <dbl>  <dbl>    <dbl>       <dbl>
    ## 1     0.910   8.05  1.02  0.000518       -7.54
    ## 2     0.905   8.11  0.992 0.00171        -6.38
    ## 3     0.905   8.07  1.02  0.000485       -7.62
    ## 4     0.901   8.32  0.997 0.00711        -4.95
    ## 5     0.900   7.98  1.00  0.00200        -6.21
    ## 6     0.927   7.56  1.04  0.000127       -8.93

#### Plot the distribution of estimates

Plot the distribution of R_squared

``` r
result_bootstrap %>% 
  ggplot(aes(x = r.squared)) + 
  geom_density() +
  labs(
    title = "Distribution of Estimates R_square",
    x = "R_square Estimates ",
    y = "Density")
```

<img src="HW_6_files/figure-gfm/unnamed-chunk-7-1.png" width="90%" />

The distribution of r.squared is approximately normal with r.squared
value about 0.92. Since r.squrred valus is close to 1, there is a strong
linear relationship between maximum temperature and minimum temperature
and precipitation as explanatory variables. Thereofore, using minimum
temperature and precipitation can predict the maximum temperature well.

Plot the distribution of log(beta_1 \* beta_2)

``` r
result_bootstrap %>% 
  ggplot(aes(x = log_beta1_2)) + 
  geom_density() +
  labs(
    title = "Distribution of Estimates log(beta_1 * beta_2",
    x = "log(beta_1 * beta_2) Estimates ",
    y = "Density")
```

<img src="HW_6_files/figure-gfm/unnamed-chunk-8-1.png" width="90%" />

The distribution of log(beta_1 \* beta_2)d is left-skewed with
log(beta_1 \* beta_2) value about -5.5.

- Using the 5000 bootstrap estimates, identify the 2.5% and 97.5%
  quantiles to provide a 95% confidence interval for r2 and log(β̂ 0∗β̂

``` r
r_squared_CI = 
  quantile(pull(result_bootstrap, r.squared),
           probs = c(0.025,0.975)) %>%
  knitr::kable(digits = 3, col.names = "R_squared")


logbeta_1_2_CI = 
  quantile(pull(result_bootstrap, log_beta1_2),
           probs = c(0.025,0.975)) %>%
  knitr::kable(digits = 3, col.names = "log(beta_1 * beta_2)")
```
