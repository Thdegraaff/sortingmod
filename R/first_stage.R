#' Estimates the first stage of a sorting model
#'
#' @details
#' Optimization occurs via the maxLik package for an obtimization of a conditional logit model
#'
#' @param code_name Indicates (with name or column number) the vector with alternative chosen
#' @param Z_names Indicates (with names or column numbers) the vectors with individual data
#' @param X_names Indicates (with names or column numbers) the vectors with alternative data
#' @param data Dataset to be used
#' @param print_detail Controls the output detail of maxLik (default is set at 3)
#'
#' @return A (maxLik) object with the estimates (maxLik) together with the indicators of the alternative chosen,
#'  the vectors with individual data, the vectors with city/regional data and the name of the reference alternative
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by_ summarise_all funs
#' @importFrom maxLik maxLik
#'
#' @export
#'
#' @examples
#' code_name <- c("mun_code")
#' Z_names <- c("age","income")
#' X_names <- c("lnprice","nature","monuments")
#' data <- municipality
#' s1.results <- first_stage(code_name, Z_names, X_names, data, print_detail = 1)
first_stage <- function(code_name, Z_names, X_names, data, print_detail = 3){

  code  <- data[code_name]
  Z     <- data[Z_names]
  X     <- data[X_names]

  Z <- data.frame(code,Z)
  X <- data.frame(code,X)
  group <- names(X)[1]

  # First, create the city specific data matrices
  data_alt <- X %>%
    group_by_(group) %>%
    summarise_all(funs(mean))
  data_alt <- data.matrix(data_alt)
  rownames(data_alt) <- data_alt[,1]

  # Second, create the individual specific data matrices (select here your variables)
  datamat <- data.matrix(Z)
  id <- as.numeric(as.factor(datamat[,1]))

  # demean the invididual data, not where dummies
  dummies.ind <- grep(TRUE, apply(datamat,2, function(x)  all(x %in% c(0,1)))) # identify dummy variables
  datamat[,-c(1,dummies.ind)]<- scale(datamat[,-c(1,dummies.ind)], scale = FALSE, center = TRUE)

  # data[,2:ncol(data)]<-scale(data[,2:ncol(data)])
  # data_alt <- scale(data_alt)

  # make sure each variable has a name
  varNames <- c(outer(colnames(datamat[,2:ncol(datamat)]), colnames(data_alt[,2:ncol(data_alt)]), FUN= paste, sep=":"))
  # There should be one base catetory! Here the first
  varNames <- c(rownames(data_alt)[2:nrow(data_alt)],varNames)

  startValues = rep(0,length(varNames))
  gradi <- matrix(rep(0, nrow(datamat)*length(varNames)), nrow(datamat), length(varNames))
  names(startValues) <- varNames

  LogLikFun <- function(param) {
    Prob <- rep(0, nrow(datamat))
    mat <- diag(nrow(data_alt))
    mat <- mat[,-1]
    for (i in 1:nrow(datamat)){
      xij <- cbind(mat,kronecker(t(datamat[i,2:ncol(datamat)]),(data_alt[,2:ncol(data_alt)]), make.dimnames=TRUE))
          col.order <- c(seq(1:(nrow(data_alt)-1)), match(varNames, colnames(xij))[!is.na(match(varNames, colnames(xij)))])
          row.order <- c(1,match(paste0(":",varNames), rownames(xij))[!is.na(match(paste0(":",varNames), rownames(xij)))])
          xij <- xij[row.order, col.order]
      xbij <- xij[,nrow(data_alt):length(varNames)] %*% param[nrow(data_alt):length(varNames)] + c(0,param[1:nrow(data_alt)-1])
      pij <- exp(xbij)/sum(exp(xbij))
      Prob[i] <- log(pij[id[i]])
      Yvec <- rep(0,nrow(data_alt))
      Yvec[id[i]] <- 1
      Yvec <- t(Yvec - pij)%*%xij
      gradi[i,] <<- Yvec[,]
    }
    Prob
  }

  gradlik <- function(param) gradi

  estimates <- maxLik(LogLikFun, gradlik, start=startValues, print.level=print_detail, method = "BHHH")

  estimates$code_name <- code_name
  estimates$Z_names <- Z_names
  estimates$X_names <- X_names
  estimates$base_alt <- rownames(data_alt)[1]

  return(estimates)
}
