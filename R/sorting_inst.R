#' Calculates an instrument for an endogenous variable in a sorting model setting
#'
#' @details
#' Instrument is calculated using a logit model estimation, assuming market clearing conditions
#' given no unobserved heterogeneity between alternative choices (following Bayer et al. (2004))
#'
#' @param s1.results Indicates the (maxLik) object estimation results of the first stage of the sorting model
#' @param endog Indicates the endogenous variable to be instrumented
#' @param data Dataset to be used
#' @param n.iterations Indicates the number of iterations
#' @param stepsize Indicates the contraction-mapping scaling coefficient
#'
#' @return A list containing (1) Results of the IV estimation, with the computed vector as instrument for the endogenous variable. (2) a vector of the computed instrument, (3) the correlation between
#' the computed instrument and the original variable, and (4) the vector of the endogenous variable
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr pull group_by_ summarise inner_join filter
#' @importFrom maxLik maxLik
#' @importFrom AER ivreg
#' @importFrom miscTools stdEr
#'
#' @export
#'
#' @examples
#' data <- municipality
#' s1.results <- first_stage(code_name = "mun_code",
#'                            X_names = c("lnprice","kindergardens_1km","p_mig_west",
#'                                        "nature","monuments","cafes_1km"),
#'                            Z_names = c("income","double_earner_hh","hh_kids","age", "migskill"),
#'                            data = data,
#'                            print_detail = 1)
#' endog <- ("lnprice")
#' phat <- sorting_inst(s1.results, "lnprice", data, stepsize = 0.02)
#' plot(phat$sorting_inst, phat$endogenous, xlab="Instrument", ylab="Endogeneous variable")
#'
sorting_inst <- function(s1.results, endog, data, n.iterations = 3, stepsize = 0.05){

  # Prepare inputs

          # Load data from first_stage estimation output
              z <- s1.results$Z_names
              x <- s1.results$X_names
              code <- s1.results$code_name
              base_alt <- s1.results$base_alt

              datamat <- data.matrix(data[,c(code,z)])

              endog.var = endog
              if (!(endog.var %in% x)){
                print("The endogenous variable is not included in the first stage estimate")
                break
              }

          # Set the alternative characteristics and asc's for the initial OLS estimation
              X <- data.frame(data[code],data[x])
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

              data_alt <- asc.df %>% inner_join(X, by=code) %>% inner_join(asc.se.weights, by=code) %>% group_by_(code) %>% filter(row_number()==1)

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
                stepsize.ite = stepsize
              }else{
                stepsize.ite = 0.05
              }
          # Set convergence check
              convergence = TRUE

          # Calculate observed count in each alternative.
              observed.count <- data %>% group_by_(code) %>% summarise(observed.count = n())

          # Set prelimnary input variables for the iteration
              xj <- data_alt
              yj <- data.matrix(data_alt[,"asc"])

          # Run OLS to obtain preliminary results
              formula_ols <- formula(paste(colnames(yj),"~", paste(x, collapse = " + ")))
              formula_iv  <- formula(paste(c(formula_ols),"|", paste(c("instr", x[x!=endog]), collapse = " + ")))

              ols_estimates <- lm(formula_ols ,data = data_alt, weights = 1/se.weights)
              b <- coef(ols_estimates)

              xj <- cbind(cons.=1,data.matrix(xj[,x]))
              dimnames(xj)[1] <- data_alt[,code]
              bij <- s1.results$estimate[!asc.index]

          # Iteration
          for (ite in n.iterations){
            print(paste("iteration",ite,sep = " "))
            print("sqrt dif max value: ")

            # Generate instrument with contraction mapping

                Uij <- matrix(0,nrow=nrow(data), ncol = nrow(xj))

                difm<-100           # Reset convergence value of ite i
                difm.ite <- difm    # Reset convergence value of ite i-1
                while (difm>0.0005){
                    for (i in 1:nrow(data)){
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
                    xj[,endog.var]  <- as.matrix(xj[,endog.var] + stepsize.ite*ldif*xj[,endog.var])
                    print(format(difm,digits = 5))

                    # expect_message(difm.ite>difm,difm.ite<difm,"No convergence")
                    if (difm.ite<difm | is.na(difm)){
                      convergence = FALSE
                      break
                    }
                    difm.ite<-difm
                }
                if (convergence == FALSE){
                  break
                }

                instr <- xj[,endog.var]
                iv_estimates <- ivreg(formula_iv, data = data_alt, weights = 1/se.weights)
                b <- coef(iv_estimates)
                stepsize.ite = stepsize * abs(ols_estimates$coefficients[endog]/iv_estimates$coefficients[endog]) # Rescale the stepsize
          }

  # Check output for convergence before returning results

        if (convergence == TRUE){
          inst.var <- xj[,endog.var]
          print(inst.var)
          inst.corr <- cor(xj[,endog.var],data_alt[,endog.var])
          endog.var <- pull(data_alt[,endog.var], endog.var)
          print(summary(iv_estimates))
          print(paste("Correlation with endogenous variable == ",format(inst.corr,digits = 4),sep=" "))
        }else{
          inst.var <- NULL
          inst.corr <- NULL
          iv_estimates <- NULL
          endog.var <- NULL
          print("No convergence")
        }

  return(list(IV_results = iv_estimates, sorting_inst = inst.var,inst.corr= inst.corr, endogenous = endog.var))
}
