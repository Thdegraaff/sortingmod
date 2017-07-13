

second_stage <- function(s1.results, dat, instrument = NULL){

  # Load data from first_stage estimation output
  x <-      s1.results$X_names # coefficients of alternatives
  code <-   s1.results$code_name # indicators of the alternative chosen,
  base_alt <- s1.results$base_alt # base alternative that is left out

  # Combine alternative chosen and coefficients of alternatives
  X <- data.frame(dat[code],dat[x])
  X[,code] <- as.character(X[,code])

  # Create alternative database
  asc.index <- names(s1.results$estimate)  %in%  as.character(X[,code])
  alts <- s1.results$estimate[asc.index]
  asc.df <- data.frame(names(s1.results$estimate)[asc.index],
                       alts, row.names = NULL)
  names(asc.df) <- c(code, "asc")
  asc.df[,code] <- as.character(asc.df[,code])
  asc.df <- rbind(c(as.numeric(base_alt),0),asc.df)

  asc.se.weights <- data.frame(names(coef(s1.results)[asc.index]), stdEr(s1.results)[asc.index])
  names(asc.se.weights) <- c(code, "se.weights")
  asc.se.weights[,code] <- as.character(asc.se.weights[,code])
  asc.se.weights <- rbind(c(as.numeric(base_alt),median(asc.se.weights$se.weights)),asc.se.weights)

  datacity <- asc.df %>%
    inner_join(X, by=code) %>%
    inner_join(asc.se.weights, by=code) %>%
    group_by_(code) %>%
    filter(row_number()==1)

  datacity
}
