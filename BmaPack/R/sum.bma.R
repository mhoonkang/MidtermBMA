#' An object of '\code{sum.bma}' class. 
#'
#' An Object of class '\code{sum.bma}' is created when the \code{summary} function is executed on an object of class '\code{bma}'.
#'
#' 
#' An object of the class '\code{sum.bma}' has the following slots:
#' \itemize{
#' \item{expected.coefficient}{The expected value of each coefficient}
#' \item{Posterior.prob}{The posterior probability that the coefficient is non-zero}
#' }
#' 
#' @author Myunghoon Kang: \email{myunghoon@@wustl.edu}
#' @aliases sum.bma-class initialize, sum.bma-method sum, sum.bma-method show, sum.bma-method print, sum.bma-method
#' @rdname sum.bma
#' @export
setClass(Class="sum.bma",  
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
setMethod("initialize", "sum.bma", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
)


#' @export
setMethod("print","sum.bma", 
          function(x, ...){
            cat("The expected value of each coefficient: \n")
            print(round(x@expected.coefficient,7))
            cat("\n")
            cat("The posterior probability that the coefficient is non-zero: \n")
            print(round(x@posterior.prob,7))
          }
)

#' @export
setMethod("show","sum.bma", 
          function(object){       
            print(object)
          })