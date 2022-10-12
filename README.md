Jed Hammoud

<!-- README.md is generated from README.Rmd. Please edit that file -->

GITHUB ID jh5764a

# HW04Jh5764a

<!-- badges: start -->
<!-- badges: end -->

The goal of HW04Jh5764a is to gain information about data frames.
Function 1 allows you to get the names of each variable in a data set,
allows you to get the class of each variable, allows you to find the
number of unique data points in each variable and allows you to find the
number of NA’s in each variable. Function 2 Allows you to visualize the
distribution and density of the data in each variable in a given data
frame. It works by taking a specific class as an input and then returns
a histogram or bar plot of the all of the variables of the specific
class.

## Installation

You can install the development version of HW04Jh5764a like so:

This package is only available by permission of the author on my github
repo using the install_github() function from the (devtools) package.

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub.
