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
setClass(Class="summary.bma",  
         representation=representation(
           expected.coefficient = "numeric",
           posterior.prob = "numeric"
         ),
         prototype=prototype(
           expected.coefficient = numeric(),
           posterior.prob = numeric()
         )
)

#' @export
setMethod("initialize", "summary.bma", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
)


#' @export
setMethod("print","summary.bma", 
          function(x, ...){
            cat("The expected value of each coefficient: \n")
            print(round(x@expected.coefficient,7))
            cat("\n")
            cat("The posterior probability that the coefficient is non-zero: \n")
            print(round(x@posterior.prob,7))
          }
)

#' @export
setMethod("show","summary.bma", 
          function(object){       
            print(object)
          })