#' @S3method summary spdur
summary.spdur <- function(object, ...) {
  # Find index to separate 2 equations
  start_split <- which(names(object$coefficients)=='(Intercept)')[2]
  end_duration <- start_split - 1
  
  table <- cbind(Estimate = coef(object),
                 StdErr = object$se,
                 t = object$zstat,
                 p = object$pval)
  
  duration_table <- table[1:end_duration, ]
  split_table <- table[start_split:(nrow(table)-1), ]
  
  res <- list(call = object$call,
              duration = duration_table,
              split = split_table,
              alpha = table[nrow(table), , drop=FALSE])
  class(res) <- 'summary.spdur'
  return(res)
}