library("dplyr")

first_stage <- function(id, X, Z){
  X <- data.frame(id,X)
  Z <- data.frame(id,Z)
  datacity <- Z %>%
    group_by(id) %>%
    summarise_all(funs(mean))
  print(datacity)
}
