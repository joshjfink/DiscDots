##########
#' Create Gauge Matrix
#'
#' @param values Number of  filled in dots for each variable
#' @return matrix of 2's and 1's to be fed into plotting functions.
gen_mat <- function(values, ...) {
  n <- length(values)
  m <- matrix(nrow=n, ncol=max(values))
  for(i in 1:n){
    g_len <- max(values)
    t_ <- rep(1, values[i])
    f_ <- rep(0, g_len-values[i])
    ifelse(values[i]==g_len,
           m[i,] <- rep(2, values[i]),
           m[i,] <- factor(t(c(t_, f_)))
    )
    return(m)
  }
}
