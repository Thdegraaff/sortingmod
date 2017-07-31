# Overview

sortingmod is a package for estimating the sorting model - a discrete choice model which explains the location decision of heterogeneous individuals over a set of alternative locations. The model is developed by Bayer et al. (2004) following the work of Berry et al. (1995). It relies on the assumptions that individuals choose a location that maximizes their utility, and that heterogeneous individuals with different characteristics have different preferences, and different valuation for location characteristics. The results of the model provide choice probabilities for each alternative, as well as insight on the valuation patterns of heterogeneous agents, and marginal willingness-to-pay values for location characteristics.

The package is constructed as an accompanying tool for the Summer School in “Hedonic price analysis and the residential location choice”, hosted by the Kraks Fond - Institute for Urban Economic Research (Copenhagen, Denmark).

# Installation

`sortingmod` is not currently available from CRAN, but you can install the development version from github with (note that you need to install the package `devtools` to be able to install packages from GitHub:

```r
# install.packages("devtools")
# library("devtools")
devtools::install_github("thdegraaff/sortingmod")
```

Once installation is completed, the package can be loaded.

```{r}
library(sortingmod)
```

# Components

- `first_stage(code_name, Z_names, X_names, data, print_detail = 3)` - Estimates the first stage of a sorting model
- `second_stage(s1.results, data, endog = NULL, instr = NULL)` - Estimates the second of a sorting model
- `sorting_inst(s1.results, endog, data, n.iterations = 3, stepsize = 0.05)` - Calculates an instrument for an endogenous variable in a sorting model setting


The data for the estimation should be in data frame format, and it must contain: 

- One or more columns indicating characteristics (`Z_names`) of a unique agent making a discrete choice between available alternatives (individuals, households, etc.).
- A vector indicating the chosen alternative (`code_name`). 
- One or more columns indicating characteristics (`X_names`) of the chosen alternative.
- Each row of the data frame should represent a unique individual. The number of alternatives should therefore be smaller or equal to the number of individuals.
- Individual and alternative characteristics should be numeric variables. Factor variables are not yet supported, so it's best to transform them into numeric dummy variables if you intend to include them in the model.


# Usage

```{r, eval = FALSE}
# Define dataset, alternative identifier variable and explanatory variable sets X and Z
    data <- municipality

# Estimate the first stage of the sorting model
    X.vars <- c("lnprice","nature","monuments","schools_3km")
    Z.vars <- c("income","double_earner_hh","hh_kids","age")
    Alt.var = "mun_code"
    s1.results <- first_stage(code_name = Alt.var,
                               X_names = X.vars
                               Z_names = Z.vars,
                               data = data,
                               print_detail = 1)

# Estimate the second stage of the model, assuming exogeneity of explanatory variables
   s2.results <- second_stage(s1.results, data)

# Define endogenous variables and calculate an instrument based on equilibrium conditions. 
# Estimate the second stage of the sorting model using the calculated instrument.
    endog <- ("lnprice")
    phat <- sorting_inst(s1.results, "lnprice", data, stepsize = 0.02)
    plot(phat$sorting_inst, phat$endogenous, xlab="Instrument", ylab="Endogeneous variable")
    
# Estimate the second stage of the model, using instruments for the endogenous variables.
  instruments <- cbind(sorting_inst = phat$sorting_inst, historical.monuments = monuments)
  s2.results <- second_stage(s1.results, data, c("lnprice","cafes_1km"), instruments)

```

# Getting help

If you need assistance using `sortingmod`, you can get in touch by emailing [t.de.graaff@vu.nl](t.de.graaff@vu.nl) or [o.d.levkovich@vu.nl](o.d.levkovich@vu.nl).


# References

Bayer, P., McMillan, R., & Rueben, K. (2004). An equilibrium model of sorting in an urban housing market (No. w10865). National Bureau of Economic Research.

Berry, S., Levinsohn, J., & Pakes, A. (1995). Automobile prices in market equilibrium. Econometrica: Journal of the Econometric Society, 841-890.
