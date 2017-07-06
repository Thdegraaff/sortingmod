library("dplyr")
library("sandwich")

sorting_inst <- function(m1, endog, dat){

  # Load data from first_stage estimation output

      # +++ NEEDS UPDATE ACCORDING TO M1 OUTPUT !!! +++
          x <- c("lnprice","nature")                ## m1$z
          z <- c("age", "income","mig")
          code = c("mun_code")                      ## m1$code
          base_alt <- c("34")                     ## m1$base_alt
          dat <- municipality                      ## m1$data
          datamat <- data.matrix(dat)

          endog.var = "lnprice" # endog
          if (!(endog.var %in% x)){
            print("The endogenous variable is not included in the first stage estimate")
            break
          }

      # Set the alternative characteristics and asc's for the initial OLS estimation
          X <- data.frame(dat[code],dat[x])
              X[,code] <- as.character(X[,code])

          asc.index <- names(m1$estimate)  %in%  as.character(Z[,code])
          asc.df <- data.frame(names(m1$estimate)[asc.index],
                               m1$estimate[asc.index], row.names = NULL)
          names(asc.df) <- c(code, "asc")
          asc.df[,code] <- as.character(asc.df[,code])
          asc.df <- rbind(c(base_alt,0),asc.df)

          datacity <- asc.df %>% inner_join(X, by=code) %>% group_by_(code) %>% filter(row_number()==1)


  # Run OLS to obtain preliminary results
      formula <- formula(paste("asc ~", paste(x, collapse = " + ")))
      ols.res <- lm(formula ,data = datacity)
      # sqrt(diag(vcovHC(ols.res, type = "HC1"))) # Robust se

  # Generate instrument with contraction mapping

      xj<- cbind(cons.=1,data.matrix(datacity[,x]))
      yj <- data.matrix(datacity[,"asc"])
      b <- solve(t(xj)%*%xj)%*%(t(xj)%*%yj)
      xbij <- matrix(0,nrow=nrow(dat), ncol = nrow(xj))

      shrs <- dat %>% group_by_(code) %>% summarise(shrs = n())

      difm<-100
      while (difm>0.0005){
          for (i in 1:nrow(dat)){
              xij <- kronecker(t(datamat[i,z]),as.matrix(xj[,x]),  make.dimnames=TRUE)
              xbij[i,] <- xij %*% m1$estimate[!asc.index] + xj%*%b
          }
          pij <- exp(xbij)/rowSums(exp(xbij))
          ldif<- log(colSums(pij)/shrs[,-1])
          difm <- t(as.matrix(ldif))%*%as.matrix(ldif)
          xj[,endog.var]  <-as.matrix(xj[,endog.var] + 0.01*ldif*xj[,endog.var])
          print(difm)
      }
      sorting_inst <- xj[,endog.var]
      corr.inst <- cor(xj[,endog.var],datacity[,endog.var])

      print(corr.inst)

  return(list(sorting_inst = sorting_inst,corr.inst= corr.inst))
}
