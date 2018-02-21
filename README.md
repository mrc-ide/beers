---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r, echo = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-"
)
```

# beers

The beers package provides the Beers ordinary and modified methods for interpolating
between 5-yearly points, and subdividing 5-yearly agebands. The most likely usage is 
with demographic data that has been presented at 5-yearly intervals, or in 5-yearly
age-bands. The Beers algorithm can be used to interpolate, or subdivide those data
respectively, and is notably used by UNWPP for that purpose.

## Usage

Four functions provide interpolation or subdivision, with the ordinary or modified
Beers method. For interpolation, the algorithm requires at least 6 points, and for
subdivision at least 5 points.

```{r example}
beers_int_ordinary(c(1, 2, 4, 8, 16, 32))
beers_int_modified(c(1, 2, 4, 8, 16, 32))
beers_sub_ordinary(c(10, 20, 40, 80, 160))
beers_sub_modified(c(10, 20, 40, 80, 160))
```

## Installation

You can install beers from github with:

```{r gh-installation, eval = FALSE}
# install.packages("devtools")
devtools::install_github("mrc-ide/beers")
```


## License

MIT + file LICENSE © [Wes Hinsley](https://github.com/weshinsley).
=======
# beers
Beers Inteprolation - As a test-case for learning to write an R package
