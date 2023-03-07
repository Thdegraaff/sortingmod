#' Estimates the second of a sorting model
#'
#' @details This function estimates an OLS or an instrument variable.
#' if an instrument variable procedure is needed both the instrument and the endogenous variables should be given
#'
#'
#' @param s1.results Indicates the (maxLik) object estimation results of the first stage of the sorting model
#' @param data Dataset to be used
#' @param endog Indicates the endogenous variable(s) to be instrumented (from the dataset in parentheses)
#' @param instr indicates the intrument(s) for the endogenous variable
#'
#' @return An estimation object
#'
#' @importFrom miscTools stdEr
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by_ inner_join group_by row_number
#' @importFrom AER ivreg
#'
#' @export
#'
#' @examples
#' data <- municipality
#' s1.results <- first_stage(code_name = "mun_code",
#'                          X_names = c("lnprice","kindergardens_1km","p_mig_west","nature","monuments","cafes_1km"),
#'                          Z_names = c("income","double_earner_hh","hh_kids","age", "migskill"),
#'                          data = data,
#'                          print_detail = 1)
#' endog <- ("lnprice")
#' phat <- sorting_inst(s1.results, "lnprice", data, stepsize = 0.02)
#'
#' s2.results <- second_stage(s1.results, data)
#' s2.results <- second_stage(s1.results, data, "lnprice", phat$sorting_inst)
#'
second_stage <- function(s1.results, data, endog = NULL, instr = NULL){

  if ((is.null(endog) & !is.null(instr)) | (!is.null(endog) & is.null(instr))){
    print("If endogenous both instrument and indicator of the endogeneous variable should be given otherwise none should be given")
    break
  }

  if (length(endog)>length(instr)){
    print("Warning : more endogenous regressors than instruments")
    break
  }

  # Load data from first_stage estimation output
  x <-      s1.results$X_names # coefficients of alternatives
  code <-   s1.results$code_name # indicators of the alternative chosen,
  base_alt <- s1.results$base_alt # base alternative that is left out
  if (!is.null(instr)){
    instr <- data.frame(instr) # Store instrument in data.frame, if it isn't one already (useful to get the column names for the formula)
  }


  # Combine alternative chosen and coefficients of alternatives
  X <- data.frame(data[code],data[x])
  X[,code] <- as.character(X[,code])

  # retrieve alternative constants from the estimates
  asc.index <- names(s1.results$estimate)  %in%  as.character(X[,code])
  alts <- s1.results$estimate[asc.index]
  asc.df <- data.frame(names(s1.results$estimate)[asc.index],
                       alts, row.names = NULL)
  names(asc.df) <- c(code, "asc")

  # Add the reference alternative to the dataframe
  asc.df[,code] <- as.character(asc.df[,code])
  asc.df <- rbind(c(as.numeric(base_alt),0),asc.df)

  # Combine alternative dataframe with instrument dataframe if needed
  if (is.null(instr)) {
    asc.df <- data.frame(asc.df)
  } else {
    asc.df <- data.frame(asc.df, instr)
  }

  # Add the weights and include weight of the alternative (calculated by the median)
  asc.se.weights <- data.frame(names(coef(s1.results)[asc.index]), stdEr(s1.results)[asc.index])
  names(asc.se.weights) <- c(code, "se.weights")
  asc.se.weights[,code] <- as.character(asc.se.weights[,code])
  asc.se.weights <- rbind(c(as.numeric(base_alt),median(asc.se.weights$se.weights)),asc.se.weights)

  # Create alternative database
  data_alt <- asc.df %>%
    inner_join(X, by=code) %>%
    inner_join(asc.se.weights, by=code) %>%
    group_by_(code) %>%
    filter(row_number()==1)

  # Build the model formulae
    # formula_ols <- formula(paste("asc~", paste(x, collapse = " + ")))
    # formula_iv  <- formula(paste("asc~", paste(x, collapse = " + "),"|",
                                #  paste(x, collapse = " + "),"-", paste(endog), "+ instr"))

  # Do estimation; if no instrument is given just OLS; otherwise ivreg from the AER package
  if (is.null(instr)) {
    formula_ols <- formula(paste("asc~", paste(x, collapse = " + ")))
    estimates <- lm(formula_ols, data = data_alt, weights = 1/se.weights)
  } else {
    formula_iv  <- formula(paste("asc~", paste(x, collapse = " + "),"|",
                                 paste(c(paste(names(instr)), x[x!=endog]), collapse = " + ")))
    estimates <- ivreg(formula_iv, data = data_alt, weights = 1/se.weights)
  }

  return(estimates)
}
