
#' Calculates an instrument for an endogenous variable in a sorting model setting
#'
#' @details
#' Instrument is calculated using a logit model estimation, assuming market clearing conditions
#' given no unobserved heterogeneity between alternative choices (following Bayer et al. (2004))
#'
#' @param s1.results Indicates the (maxLik) object estimation results of the first stage of the sorting model
#' @param endog Indicates the endogenous variable to be instrumented
#' @param dat Dataset to be used
#' @param n.iterations Indicates the number of iterations
#' @param stepsize Indicates the contraction-mapping scaling coefficient
#'
#' @return A list containing (1) a vector of the computed instrument, and (2) the correlation between
#' the computed instrument and the original variable.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by_ summarise inner_join filter
#' @importFrom maxLik maxLik
#'
#' @export
#'
#' @examples
#' data <- municipality
#' model_output <- first_stage("mun_code", c("age","income"),
#'                 c("lnprice","monuments"), data)
#' endog <- ("lnprice")
#' phat <- sorting_inst(model_output, "lnprice", data)

sorting_inst <- function(s1.results, endog, dat, n.iterations = 3, stepsize = 0.05){

  # Prepare inputs

          # Load data from first_stage estimation output
              z <- s1.results$Z_names
              x <- s1.results$X_names
              code <- s1.results$code_name
              base_alt <- s1.results$base_alt

              dat <- dat
              datamat <- data.matrix(dat[,c(code,z)])

              endog.var = endog
              if (!(endog.var %in% x)){
                print("The endogenous variable is not included in the first stage estimate")
                break
              }

          # Set the alternative characteristics and asc's for the initial OLS estimation
              X <- data.frame(dat[code],dat[x])
                  X[,code] <- as.character(X[,code])

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

              datacity <- asc.df %>% inner_join(X, by=code) %>% inner_join(asc.se.weights, by=code) %>% group_by_(code) %>% filter(row_number()==1)

          # De-mean the invididual data, not where dummies
              dummies.ind <- grep(TRUE, apply(datamat,2, function(x)  all(x %in% c(0,1)))) # identify dummy variables
              datamat[,-c(1,dummies.ind)]<- scale(datamat[,-c(1,dummies.ind)], scale = FALSE, center = TRUE)


  # Begin iteration

          # Set number of iterations
              if (n.iterations!=3 & is.numeric(n.iterations)){
                n.iterations = seq(1:n.iterations)
              }else{
                n.iterations = seq(1:3)
              }
          # Step contraction-mapping coefficient
              if (stepsize!=0.05 & is.numeric(stepsize)){
                stepsize = stepsize
              }
          # Set convergence check
              convergence = TRUE

          # Calculate observed count in each alternative.
              observed.count <- dat %>% group_by_(code) %>% summarise(observed.count = n())

          # Set prelimnary input variables for the iteration
              xj.ite <- datacity
              yj <- data.matrix(datacity[,"asc"])
              formula <- formula(paste(colnames(yj),"~", paste(x, collapse = " + ")))

          # Iteration
          for (ite in n.iterations){
            print(paste("iteration",ite,sep = " "))

            # Run OLS to obtain preliminary results
                ols.res <- lm(formula ,data = xj.ite, weights = se.weights)

            # Generate instrument with contraction mapping
                xj<- cbind(cons.=1,data.matrix(xj.ite[,x]))
                dimnames(xj)[1] <- xj.ite[,code]
                b <- solve(t(xj)%*%xj)%*%(t(xj)%*%yj)
                bij <- s1.results$estimate[!asc.index]
                Uij <- matrix(0,nrow=nrow(dat), ncol = nrow(xj))

                difm<-100           # Reset convergence value of ite i
                difm.ite <- difm    # Reset convergence value of ite i-1
                while (difm>0.0005){
                    for (i in 1:nrow(dat)){
                        xij <- kronecker(t(datamat[i,z]),as.matrix(xj[,x]),  make.dimnames=TRUE)
                             col.order <- colnames(xij)
                             bij <- bij[col.order]
                          #  row.order <-  paste0(":",c(base_alt,names(alts)))
                          #  xij<- xij[row.order,col.order]
                        Uij[i,] <- xij %*% bij + xj %*% b
                    }

                    pij <- exp(Uij)/rowSums(exp(Uij))
                    dif<- (colSums(pij) - observed.count[,-1])/sum(pij)
                    ldif<- log(colSums(pij)) - log(observed.count[,-1])
                    difm <- sqrt(max(as.matrix(dif)*as.matrix(dif)))
                    xj[,endog.var]  <- as.matrix(xj[,endog.var] + stepsize*ldif*xj[,endog.var])
                    print(difm)

                    # expect_message(difm.ite>difm,difm.ite<difm,"No convergence")
                    if (difm.ite<difm){
                      convergence = FALSE
                      break
                    }
                    difm.ite<-difm
                }
                if (convergence == FALSE){
                  break
                }
                xj.ite[,endog.var] <- xj[,endog.var]
          }

  # Check output for convergence before returning results

        if (convergence == TRUE){
          inst.var <- xj[,endog.var]
          print(inst.var)
          inst.corr <- cor(xj[,endog.var],datacity[,endog.var])
          print(paste("Correlation with endogenous variable == ",format(inst.corr,digits = 5),sep=" "))
        }else{
          inst.var <- NULL
          inst.corr <- NULL
          print("No convergence")
        }

  return(list(sorting_inst = inst.var,inst.corr= inst.corr))
}
