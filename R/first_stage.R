#' Calculates the first stage of a sorting model
#'
#' @details
#' Optimization occurs via the maxLik package for an obtimization of a conditional logit model
#'
#' @param code_name Indicates (with name or column number) the vector with alternative chosen
#' @param Z_names Indicates (with names or column numbers) the vectors with individual data
#' @param X_names Indicates (with names or column numbers) the vectors with alternative data
#' @param dat Dataset to be used
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
#' data <- municipality
#' model_output <- first_stage("mun_code", c("age","income"),
#'                 c("lnprice","monuments"), data)
first_stage <- function(code_name, Z_names, X_names, dat){

  code  <- dat[code_name]
  Z     <- dat[Z_names]
  X     <- dat[X_names]

  Z <- data.frame(code,Z)
  X <- data.frame(code,X)
  group <- names(X)[1]

  # First, create the city specific data matrices
  datacity <- X %>%
    group_by_(group) %>%
    summarise_all(funs(mean))
  datacity <- data.matrix(datacity)
  rownames(datacity) <- datacity[,1]

  # Second, create the individual specific data matrices (select here your variables)
  data <- data.matrix(Z)
  id <- as.numeric(as.factor(data[,1]))

  # demean the invididual data, not where dummies
  dummies.ind <- grep(TRUE, apply(data,2, function(x)  all(x %in% c(0,1)))) # identify dummy variables
  data[,-c(1,dummies.ind)]<- scale(data[,-c(1,dummies.ind)], scale = FALSE, center = TRUE)

  # data[,2:ncol(data)]<-scale(data[,2:ncol(data)])
  # datacity <- scale(datacity)

  # make sure each variable has a name
  varNames <- c(outer(colnames(data[,2:ncol(data)]), colnames(datacity[,2:ncol(datacity)]), FUN= paste, sep=":"))
  # There should be one base catetory! Here the first
  varNames <- c(rownames(datacity)[2:nrow(datacity)],varNames)

  startValues = rep(0,length(varNames))
  gradi <- matrix(rep(0, nrow(data)*length(varNames)), nrow(data), length(varNames))
  names(startValues) <- varNames

  LogLikFun <- function(param) {
    Prob <- rep(0, nrow(data))
    mat <- diag(nrow(datacity))
    mat <- mat[,-1]
    for (i in 1:nrow(data)){
      xij <- cbind(mat,kronecker(t(data[i,2:ncol(data)]),(datacity[,2:ncol(datacity)]), make.dimnames=TRUE))
          col.order <- c(seq(1:(nrow(datacity)-1)), match(varNames, colnames(xij))[!is.na(match(varNames, colnames(xij)))])
          row.order <- c(1,match(paste0(":",varNames), rownames(xij))[!is.na(match(paste0(":",varNames), rownames(xij)))])
          xij <- xij[row.order, col.order]
      xbij <- xij[,nrow(datacity):length(varNames)] %*% param[nrow(datacity):length(varNames)] + c(0,param[1:nrow(datacity)-1])
      pij <- exp(xbij)/sum(exp(xbij))
      Prob[i] <- log(pij[id[i]])
      Yvec <- rep(0,nrow(datacity))
      Yvec[id[i]] <- 1
      Yvec <- t(Yvec - pij)%*%xij
      gradi[i,] <<- Yvec[,]
    }
    Prob
  }

  gradlik <- function(param) gradi

  estimates <- maxLik(LogLikFun, gradlik, start=startValues, print.level=3, method = "BHHH")

  estimates$code_name <- code_name
  estimates$Z_names <- Z_names
  estimates$X_names <- X_names
  estimates$base_alt <- rownames(datacity)[1]

  return(estimates)
}
