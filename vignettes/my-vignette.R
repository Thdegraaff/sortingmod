## ------------------------------------------------------------------------
library(sortingmod)

## ---- echo = FALSE-------------------------------------------------------
load("~/sortingmod/data/municipality_new.rda")
code_name <- c("mun_code")
Z_names <- c("age","income","hh_kids")
X_names <- c("lnprice","nature","monuments")
head(municipality[,c("id",Z_names,code_name,X_names)])
municipality <- subset(municipality, randstad ==1)

## ---- results = "hide"---------------------------------------------------
code_name <- c("mun_code")
Z_names <- c("age","income")
X_names <- c("lnprice","nature","monuments")
# s1.results <- first_stage(code_name, Z_names, X_names, data = municipality)

## ---- echo = FALSE-------------------------------------------------------
# summary(s1.results)

## ---- eval = FALSE-------------------------------------------------------
#  second_stage(s1.results, data)
#  
#  endog <- ("lnprice")
#  second_stage(s1.results, data, endog = endog, instr = instrument)

## ---- eval = FALSE-------------------------------------------------------
#  endog <- ("lnprice")
#  phat <- sorting_inst(s1.results, endog, data, n.iterations = 3, stepsize = 0.05)

## ---- echo = FALSE-------------------------------------------------------
# summary(phat$IV_results)

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

