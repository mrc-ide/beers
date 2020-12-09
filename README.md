
<!-- badges: start -->

[![R build
status](https://github.com/mrc-ide/beers/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/beers/actions)
[![Coverage
Status](https://img.shields.io/codecov/c/github/mrc-ide/beers/master.svg)](https://codecov.io/github/mrc-ide/beers?branch=master)
<!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

# beers

## Introduction

The beers package provides the Beers ordinary and modified methods for
interpolating between 5-yearly points, and subdividing 5-yearly
agebands. The most likely usage is with demographic data that has been
presented at 5-yearly intervals, or in 5-yearly age-bands. The Beers
algorithm can be used to interpolate, or subdivide those data
respectively, and is notably used by UNWPP for that purpose.

## Usage

Four functions provide interpolation or subdivision, with the ordinary
or modified Beers method. For interpolation, the algorithm requires at
least 6 points, and for subdivision at least 5 points.

``` r
beers_int_ordinary(c(1, 2, 4, 8, 16, 32))
#>  [1]  1.0000  1.1061  1.2696  1.4780  1.7229  2.0000  2.3092  2.6545  3.0437
#> [10]  3.4879  4.0000  4.5931  5.2791  6.0681  6.9701  8.0000  9.1793 10.5355
#> [19] 12.1007 13.9100 16.0000 18.4083 21.1732 24.3336 27.9291 32.0000
beers_int_modified(c(1, 2, 4, 8, 16, 32))
#>  [1]  1.0000  1.1672  1.3435  1.5362  1.7526  2.0000  2.2856  2.6165  3.0003
#> [10]  3.4445  3.9570  4.5464  5.2227  5.9982  6.8887  7.9140  9.0979 10.4676
#> [19] 12.0529 13.8859 16.0000 18.4296 21.2095 24.3746 27.9598 32.0000
beers_sub_ordinary(c(10, 20, 40, 80, 160))
#>  [1]  1.061  1.635  2.084  2.449  2.771  3.092  3.453  3.892  4.442  5.121
#> [11]  5.931  6.860  7.890  9.020 10.299 11.793 13.562 15.652 18.093 20.900
#> [21] 24.083 27.649 31.604 35.955 40.709
beers_sub_modified(c(10, 20, 40, 80, 160))
#>  [1]  1.672  1.763  1.927  2.164  2.474  2.856  3.309  3.838  4.442  5.125
#> [11]  5.894  6.763  7.755  8.905 10.253 11.839 13.697 15.853 18.330 21.141
#> [21] 24.296 27.799 31.651 35.852 40.402
```

## Installation

You can install beers from github with:

``` r
# install.packages("devtools")
devtools::install_github("mrc-ide/beers")
```

## License

MIT + file LICENSE © [Wes Hinsley](https://github.com/weshinsley).
