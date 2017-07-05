library("dplyr")
library("maxLik")

first_stage <- function(code, Z, X, dat){

  X <- gsub(" ","",unlist(strsplit(X,"\\+")))
  Z <- gsub(" ","",unlist(strsplit(Z,"\\+")))
  code <- gsub(" ","",unlist(strsplit(code,"\\+")))

  code  <- dat[code]
  Z     <- dat[Z]
  X     <- dat[X]

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

  # scaling the data
  data[,2:ncol(data)]<-scale(data[,2:ncol(data)])
  datacity <- scale(datacity)

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

  Estimates <- maxLik(LogLikFun, gradlik, start=startValues, print.level=3, method = "BHHH")

  return(Estimates)
}
