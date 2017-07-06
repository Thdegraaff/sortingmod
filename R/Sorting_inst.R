library("dplyr")

sorting_inst <- function(m1, endog, dat){

  # Load data from first_stage estimation output
      z <- m1$Z_names
      x <- m1$X_names
      code <- m1$code_name
      base_alt <- m1$base_alt

      dat <- dat
      datamat <- data.matrix(dat)

      endog.var = endog
      if (!(endog.var %in% x)){
        print("The endogenous variable is not included in the first stage estimate")
        break
      }

  # Set the alternative characteristics and asc's for the initial OLS estimation
      X <- data.frame(dat[code],dat[x])
          X[,code] <- as.character(X[,code])

      asc.index <- names(m1$estimate)  %in%  as.character(X[,code])
      asc.df <- data.frame(names(m1$estimate)[asc.index],
                           m1$estimate[asc.index], row.names = NULL)
      names(asc.df) <- c(code, "asc")
      asc.df[,code] <- as.character(asc.df[,code])
      asc.df <- rbind(c(base_alt,0),asc.df)

      datacity <- asc.df %>% inner_join(X, by=code) %>% group_by_(code) %>% filter(row_number()==1)

  # Begin iteration
      xj.ite <- datacity
      yj <- data.matrix(datacity[,"asc"])
      shrs <- dat %>% group_by_(code) %>% summarise(shrs = n())
      nobs <- sum(shrs[-1])

      for (i in seq(1:10)){

            # Run OLS to obtain preliminary results
                formula <- formula(paste("asc ~", paste(x, collapse = " + ")))
                ols.res <- lm(formula ,data = xj.ite)

            # Generate instrument with contraction mapping
                xj<- cbind(cons.=1,data.matrix(xj.ite[,x]))
                dimnames(xj)[1] <- xj.ite[,code]
                b <- solve(t(xj)%*%xj)%*%(t(xj)%*%yj)
                xbij <- matrix(0,nrow=nrow(dat), ncol = nrow(xj))

                difm<-100
                while (difm>0.0005){
                    for (i in 1:nrow(dat)){
                        xij <- kronecker(t(datamat[i,z]),as.matrix(xj[,x]),  make.dimnames=TRUE)
                            col.order <- names(m1$estimate[!asc.index])
                            row.order <- paste0(":",c(base_alt,names(m1$estimate[asc.index])))
                            xij<- xij[row.order,col.order]
                        xbij[i,] <- xij %*% m1$estimate[!asc.index] + xj%*%b
                    }
                    pij <- exp(xbij)/rowSums(exp(xbij))
                    dif<- (colSums(pij) - shrs[,-1])/nobs
                    ldif<- log(colSums(pij)/nobs) - log(shrs[,-1]/nobs)
                    difm <- sqrt(max(t(as.matrix(dif))%*%as.matrix(dif)))
                    xj[,endog.var]  <-as.matrix(xj[,endog.var] + 0.1*ldif*xj[,endog.var])
                    print(difm)
                }
                xj.ite[,endog.var] <- xj[,endog.var]
      }
      sorting_inst <- xj[,endog.var]
      corr.inst <- cor(xj[,endog.var],datacity[,endog.var])

  return(list(sorting_inst = sorting_inst,corr.inst= corr.inst))
}
