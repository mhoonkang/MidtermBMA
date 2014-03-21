#' Plot the Object of '\code{bma}' Class
#' 
#' This function plots 1) the expected value of each coefficient and 2) the posterior probability that the coefficient is non-zero
#' 
#' @param x An object of '\code{bma}' class
#' @param ... Arguments to be passed to methods
#' 
#' @return Useful two plots from an object of '\code{bma}' class
#' \item{The expected value of each coefficient}
#' \item{the posterior probability that the coefficient is non-zero}
#' @author Myunghoon Kang \email{myunghoon@@wustl.edu}
#' @seealso \code{\link{fitBMA}}
#' @seealso \code{\link{summary.BMA}}
#' @examples
#' 
#' # Create a random 10 by 5 covariate matrix
#' x <- matrix(rnorm(50,0,1),10,5)
#' # Create a vector of the values for the dependent variable
#' y <- 2+1.2*x[,1]+2.2*x[,2]+0.2*x[,3]+3.2*x[,4]+1.8*x[,5]+rnorm(10,0,3)
#' # run fitBMA function
#' a <- fitBMA(x=x,y=y,g=3)
#' plot(a)
#' @rdname plot.BMA
#' @aliases plot.BMA, ANY-method
#' @export
setMethod("plot","bma", 
          function(x, ...){
            X <- length(x@posterior.prob)
            coefficient <- x@expected.coeff
            prob <- x@posterior.prob
            barplot(coefficient, ylim=c(ifelse(min(coefficient)>=0,0,floor(min(coefficient)/0.1)*0.1), ifelse(max(coefficient)<=0,0,ceiling(max(coefficient)/0.1)*0.1)), 
                    xlab="Covariates", ylab="Expected Coefficient value", main="The expected value of \n each coefficient")
            par(ask=TRUE)
            barplot(prob, ylim=c(0,1), xlab="Covariates", ylab="Probability", main="The posterior probability that \n the coefficient is non-zero")
          }
)