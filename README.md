# DiscDots

## NOTE: This package is still in development and not ready for use

## Overview

Discrete dot plots built on ggplot2

## Install 

Install the development version from github with the
**devtools** package,

```r
library(devtools)
install_github("joshua-james-f/DiscDots")
```

## Example 

```r
library(DiscDots)
values <- c(10, 8, 8, 6, 6, 6, 5, 4, 3, 2)
col_names <- c("R", "Python", "Stata", "D3.js", "Tableau", "Spark", "SQL", "Hadoop", "C++", "SAS")

DiscDots(values, var_names=col_names)
```

<a href="url"><img src="https://raw.githubusercontent.com/joshua-james-f/DiscDots/master/figure/Skills.png" width="450" ></a>

*Note: Package still in development*
