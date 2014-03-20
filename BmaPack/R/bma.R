#' An object of '\code{bma}' (Bayesian Model Averaging) class
#' 
#' An object of class '\code{bma}' are created by the \code{fitbma} functions
#'
#' 
#' An object of the class \code{bma} has the following slots:
#' \itemize{
#' \item{coefficient}{A list consisting of a coefficients matrix for each model}
#' \item{R.squared}{A vector of the value of R-sqaured for each model}
#' \item{posterior.odds}{A vector of the posterior model odds for each model}
#' \item{expected.coeff}{A vector of the posterior expected value of each coefficient}
#' \item{posterior.prob}{A vector of the posterior probabilities that each coefficient is non-zero}
#' }
#'
#' @author Myunghoon Kang: \email{myunghoon@@wustl.edu}
#' @aliases bma-class initialize, bma-method summary, bma-method plot, bma-method show, bma-method print, bma-method
#' @rdname bma
#' @export
setClass(Class="bma",  
         representation=representation(
           coefficient="matrix",
           R.squared="numeric",
           posterior.odds = "numeric",
           expected.coeff = "numeric",
           posterior.prob = "numeric"
         ),
         prototype=prototype(
           coefficient = matrix(),
           R.squared = numeric(),
           posterior.odds =  numeric(),
           expected.coeff = numeric(),
           posterior.prob = numeric()
         )
)

#' @export
setMethod("initialize", "bma", function(.Object, ...){ 
  value=callNextMethod()
  validObject(value)
  return(value)
}
)