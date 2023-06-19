#' Accessor methods for spdur Objects
#' 
#' Several standard accessor methods for a \code{spdur} class object.
#' 
#' @param object an object inheriting from class \code{spdur}.
#' @param x  \code{spdur} class object for terms
#' @param model return full model, or only duration or risk equations, or 
#'   distribution parameters.
#' @param \dots not used
#' 
#' @seealso \code{\link{AIC.spdur}}, \code{\link{BIC.spdur}}
#' 
#' @name accessors
#' 
#' @examples
#' data(model.coups)
#' 
NULL
#> NULL

#' @rdname accessors
#' 
#' @method logLik spdur
#' 
#' @examples 
#' logLik(model.coups)
#' 
#' @export
logLik.spdur <- function(object, ...) 
{
  p <- nobs(object) - length(object$coefficients)
  val <- object$logL
  attr(val, "nobs") <- nobs(object)
  attr(val, "df")  <- p
  class(val) <- 'logLik'  
  
  return(val)
}

#' @rdname accessors
#' @method nobs spdur
#' @examples 
#' nobs(model.coups)
#' 
#' @export
nobs.spdur <- function(object, ...)
{
  val <- object$obs
  return(val)
}

#' @rdname accessors
#' @method coef spdur
#' @examples 
#' coef(model.coups)
#' 
#' @export
coef.spdur <- function (object, model = c("full", "duration", "risk", "distr"), ...) 
{
  model <- match.arg(model)
  res  <- object$coefficients
  res  <- switch(
    model, 
    full = structure(c(res$duration, res$risk, res$distr), 
      .Names = c(paste("duration", names(res$duration), sep = "_"), 
                 paste("risk", names(res$risk), sep = "_"),
                 names(res$distr))), 
    duration = res$duration, 
    risk = res$risk,
    distr = res$distr)
  return(res)
}

#' @rdname accessors
#' @method vcov spdur
#' @examples 
#' vcov(model.coups)
#' 
#' @export
#' @importFrom stats vcov
vcov.spdur <- function (object, model = c("full", "duration", "risk", "distr"), ...) 
{
  model <- match.arg(model)
  res   <- object$vcov
  if (model == "full") 
    return(res)
  
  # otherwise:
  cf <- object$coefficients[[model]]
  wi <- seq(along = object$coefficients$duration)
  di <- nrow(res)
  res <- if (model == "duration") {
    res[wi, wi]
  } else if (model == "risk") {
    res <- res[-c(wi, di), -c(wi, di)]
  } else {
    res <- res[di, di, drop = FALSE]
  }
  colnames(res) <- rownames(res) <- names(cf)
  return(res)
}

#' @rdname accessors
#' @method model.matrix spdur
#' @examples 
#' head(model.matrix(model.coups))
#' 
#' @export
model.matrix.spdur <- function(object, model = c("duration", "risk"), ...) 
{
  model <- match.arg(model)
  args <- list(...)
  # get the correct data
  if (!"data" %in% names(list(...))) {
    data <- switch(
      model, 
      duration = object$mf.dur, 
      risk = object$mf.risk)
    args[["data"]] <- data
  }
  args[["object"]] <- terms(object, model = model)
  do.call(model.matrix, args)
}

#' @rdname accessors
#' @method terms spdur
#' @examples 
#' terms(model.coups)
#' 
#' @export
terms.spdur <- function(x, model = c("duration", "risk"), ...) 
{
  model <- match.arg(model)
  res  <- switch(
    model, 
    duration = terms(x$mf.dur), 
    risk = terms(x$mf.risk)
  )
  return(res)
}
