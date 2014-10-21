#' Expand call to full names.
#' 
#' Return a call in which all of the arguments which were supplied
#' or have presets are specified by their full names and supplied
#' or default values.
#'  
#' @param definition a function. See \code{\link[base]{match.call}}.
#' @param call an unevaluated call to the function specified by definition.
#'  See \code{\link[base]{match.call}}.
#' @param expand.dots logical. Should arguments matching ... in the call be 
#'  included or left as a ... argument? See \code{\link[base]{match.call}}.
#' @param eval logical, defaults to TRUE. Should function arguments be 
#'  evaluated in the returned call or not?
#'
#' @return An object of class call. 
#' @author fabians
#' @seealso \code{\link[base]{match.call}}
#' @keywords internal
expand.call <- function(definition=NULL,
                        call=sys.call(sys.parent(1)),
                        expand.dots = TRUE,
                        eval=FALSE)
{
  
  safeDeparse <- function(expr){
    #rm line breaks, whitespace             
    ret <- paste(deparse(expr), collapse="")
    return(gsub("[[:space:]][[:space:]]+", " ", ret))
  }
  
  print(call)
  call <- match.call(definition, call, expand.dots)
  
  #supplied args:
  ans <- as.list(call)
  if(eval) ans[-1] <- lapply(ans[-1], eval)
  
  #possible args:
  frmls <- formals(safeDeparse(ans[[1]]))
  #remove formal args with no presets:
  frmls <- frmls[!sapply(frmls, is.symbol)]
  
  add <- which(!(names(frmls) %in% names(ans)))
  return(as.call(c(ans, frmls[add])))
}