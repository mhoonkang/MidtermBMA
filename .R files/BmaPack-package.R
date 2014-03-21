#' BmaPack
#'
#' Package for Bayesian model averaging for linear models  
#' @name BmaPack
#' @docType package
#' @author  Myunghoon Kang \email{myunghoon@@wustl.edu}
#' @seealso \code{\link{fitBMA}}
#' @seealso \code{\link{plotBMA}}
#' @seealso \code{\link{summaryBMA}}
#' @examples 
#'
#' \dontrun{
#' # Create a random 10 by 5 covariate matrix
#' x <- matrix(rnorm(50,0,1),10,5)
#' # Create a vector of the values for the dependent variable
#' y <- 2+1.2*x[,1]+2.2*x[,2]+0.2*x[,3]+3.2*x[,4]+1.8*x[,5]+rnorm(10,0,3)
#' # run fitBMA function
#' fitBMA(x=x,y=y,g=3)
#' }
#' @rdname BmaPack-package
#' 
NULL
