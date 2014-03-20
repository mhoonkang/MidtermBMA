#' An object of '\code{summary.bma}' class. 
#'
#' An Object of class '\code{summary.bma}' is created when the \code{summary} function is executed on an object of class '\code{bma}'.
#'
#' 
#' An object of the class '\code{summary.bma}' has the following slots:
#' \itemize{
#' \item{expected.coefficient}{The expected value of each coefficient}
#' \item{Posterior.prob}{The posterior probability that the coefficient is non-zero}
#' }
#' 
#' @author Myunghoon Kang: \email{myunghoon@@wustl.edu}
#' @aliases summary.bma-class initialize, summary.bma-method summary, summary.bma-method show, summary.bma-method print, summary.bma-method
#' @rdname summary.bma
#' @export