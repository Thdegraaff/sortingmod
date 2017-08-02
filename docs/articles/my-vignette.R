## ---- eval=FALSE---------------------------------------------------------
#  # install.packages("devtools")
#  # library("devtools")
#  devtools::install_github("thdegraaff/sortingmod")

## ------------------------------------------------------------------------
library(sortingmod)

## ---- echo = FALSE-------------------------------------------------------
  data <- municipality
  code_name <- c("mun_code")
  Z_names <- c("age","income","hh_kids")
  X_names <- c("lnprice","nature","monuments")
  head(municipality[,c("id",Z_names,code_name,X_names)])

## ---- results = "hide"---------------------------------------------------
  code_name = c("mun_code")
  X_names = c("lnprice","kindergardens_1km","p_mig_west","nature","monuments","cafes_1km")
  Z_names = c("income","double_earner_hh","hh_kids","age", "migskill")
  s1.results <- first_stage(code_name, Z_names, X_names, data = municipality, print_detail = 1)

## ---- eval = FALSE-------------------------------------------------------
#    s2.results <- second_stage(s1.results, data)
#  
#    endog <- ("lnprice")
#    s2.results <- second_stage(s1.results, data = municipality, endog = endog, instr = instrument)
#    summary(s2.results)

## ---- results = "hide"---------------------------------------------------
  endog <- ("lnprice")
  phat <- sorting_inst(s1.results, endog, data = municipality, stepsize = 0.02)

## ------------------------------------------------------------------------
summary(phat$IV_results)

