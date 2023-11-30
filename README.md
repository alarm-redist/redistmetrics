
<!-- README.md is generated from README.Rmd. Please edit that file -->

# redistmetrics <a href="https://alarm-redist.org/redistmetrics/"><img src="man/figures/logo.png" align="right" height="132" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/alarm-redist/redistmetrics/workflows/R-CMD-check/badge.svg)](https://github.com/alarm-redist/redistmetrics/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/redistmetrics)](https://CRAN.R-project.org/package=redistmetrics)
![CRAN
downloads](http://cranlogs.r-pkg.org/badges/grand-total/redistmetrics)
<!-- badges: end -->

`redistmetrics` is one of the R packages developed and maintained by the
[ALARM Project](https://alarm-redist.org/). `redistmetrics`
provides the back-end for the computation of summary statistics for a
redistricting plan. It provides a more direct access point to use
methods in `redist` without requiring `redist` objects.

## Installation

You can install the stable version of `redistmetrics` from CRAN with:

``` r
install.packages('redistmetrics')
```

You can install the development version of `redistmetrics` from
[GitHub](https://github.com/alarm-redist/redistmetrics) with:

``` r
if (!requireNamespace('remotes')) install.packages('remotes')
remotes::install_github('alarm-redist/redistmetrics')
```

## Example

``` r
library(redistmetrics)
```

`redistmetrics` offers support for 4 common input types and has examples
of each, all based on New Hampshire:

``` r
data(nh)
```

This example is based on `comp_polsby()` for the Polsby Popper
compactness, but `comp_polsby()` can be substituted for any implemented
measure!

#### Single Plan:

For a single plan, we can pass the single plan to the input. We also
pass an argument to `shp` which takes in an `sf` dataframe. `r_2020`
here is the Republican proposal for New Hampshire’s congressional
districts.

``` r
comp_polsby(plans = nh$r_2020, shp = nh)
#> [1] 0.2324375 0.1582763
```

The output here is a numeric vector, where each entry is the output for
a district. The first district here has a compactness of about 0.23 and
the second district has a compactness of about 0.16.

Now, if you’re redistricting in R, we recommend using the R package
`redist`. In which case, you would have a `redist_map` object.

We can load an example here with:

``` r
data(nh_map)
```

For redist maps, the workflow is identical!

``` r
comp_polsby(plans = nh_map$r_2020, shp = nh)
#> [1] 0.2324375 0.1582763
```

#### Multiple Plans:

For multiple plans, we can pass either a matrix of plans or a
`redist_plans` object to plans. We will still need `nh` or `nh_map` to
provide the shapes.

If we have a matrix, we can compare with `nh_m` a matrix of plans, where
each column indicates a plan.

``` r
data(nh_m)
```

From there, the process is nearly identical. Here we compute the Polsby
Popper compactness for the first two columns:

``` r
comp_polsby(plans = nh_m[, 1:2], shp = nh)
#> [1] 0.1844955 0.1796426 0.2324375 0.1582763
```

Now we got 4 outputs: 1 for each district x 2 for each plan x 2 plans.

If we are using `redist`, we likely have a `redist_plans` object which
hides the matrix as an attribute to give a more familiar tidy workflow.
With that, we can do a very similar process:

First, we load the plans object (included as an example):

``` r
data(nh_plans)
```

The benefit of using a `redist_plans` object is that we can cleanly
`mutate` into it using the `.` shortcut:

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
nh_plans <- nh_plans %>% mutate(polsby = comp_polsby(plans = ., shp = nh))
#> Linking to GEOS 3.9.3, GDAL 3.5.2, PROJ 8.2.1; sf_use_s2() is TRUE
```

Now our values are cleanly held in the `redist_plans` object:

``` r
head(nh_plans)
#> # A tibble: 6 × 4
#>   draw   district total_pop polsby
#>   <fct>     <int>     <dbl>  <dbl>
#> 1 d_2020        1    688739  0.184
#> 2 d_2020        2    688790  0.180
#> 3 r_2020        1    688676  0.232
#> 4 r_2020        2    688853  0.158
#> 5 1             1    688961  0.235
#> 6 1             2    688568  0.349
```

Detailed information on each measure are contained in the vignettes and
references are contained in the function documentation.
