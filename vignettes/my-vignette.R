## ------------------------------------------------------------------------
library(sortingmod)

## ------------------------------------------------------------------------
load("~/sortingmod/data/municipality.rda")

## ------------------------------------------------------------------------
head(municipality)

## ---- results = "hide"---------------------------------------------------
code_name <- c("mun_code")
Z_names <- c("age","income")
X_names <- c("lnprice","nature","monuments")
s1.results <- first_stage(code_name, Z_names, X_names, dat = municipality)

## ------------------------------------------------------------------------
summary(s1.results)

## ---- results = "hide"---------------------------------------------------
# endog <- c("lnprice")
# inst.var <- sorting_inst(s1.results, endog, dat = municipality)

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

