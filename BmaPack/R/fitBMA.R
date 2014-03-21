#' Bayesian Model Averaging
#' 
#' This function fits regression for all possible combinations of the covariates and returns 1) all of the coefficients and R-squared values for each model, 2) the posterior model odds for each model, 3) the posterior expected value of each coefficient and 4) the posterior probability that the coefficient is non-zero.
#' 
#' @param x A \code{n} by \code{k} covariate matrix. \code{k} should be greater than or equal to 2.
#' @param y A numeric vector of the values of the dependent variable. The length shoud be \code{n}.
#' @param g A number for Priors
#' @param ... Arguments to be passed to methods
#' 
#' @return An object of '\code{bma}' class with the elements
#' \item{coefficient}{A list consisting of a coefficients matrix for each model}
#' \item{R.squared}{A vector of the value of R-sqaured for each model}
#' \item{posterior.odds}{A vector of the posterior model odds for each model}
#' \item{expected.coeff}{A vector of the posterior expected value of each coefficient}
#' \item{posterior.prob}{A vector of the posterior probabilities that each coefficient is non-zero}
#' @author Myunghoon Kang \email{myunghoon@@wustl.edu}
#' @note This produces an object of a new class '\code{bma}'.
#' @seealso \code{\link{plot.BMA}}
#' @seealso \code{\link{summary.BMA}}
#' @examples
#' 
#' # Create a random 10 by 5 covariate matrix
#' x <- matrix(rnorm(50,0,1),10,5)
#' # Create a vector of the values for the dependent variable
#' y <- 2+1.2*x[,1]+2.2*x[,2]+0.2*x[,3]+3.2*x[,4]+1.8*x[,5]+rnorm(10,0,3)
#' # run fitBMA function
#' fitBMA(x=x,y=y,g=3)
#' @rdname fitBMA
#' @aliases fitBMA,ANY-method
#' @export
setGeneric(name="fitBMA", 
           function(x,y,g,...){
             standardGeneric("fitBMA")
            }
           )

#' @export
setMethod(f="fitBMA", signature(x="matrix", y="numeric", g="numeric"), 
          definition=function(x,y,g){
            n <- nrow(x)
            k <- ncol(x)
            if(k==1) stop("the number of covariates should be greater than or equal to 2")
            one <- rep(1,n) 
            M <- one%*%solve(t(one)%*%one)%*%t(one) 
            if(is.null(colnames(x))) colnames(x) <- paste("x",1:k,sep="") 
            covariate.names <- colnames(x)
            sx <- apply(x, 2, function(x) (x-mean(x))/sd(x))
            sy <- (y-mean(y))/sd(y) 
            combi <- lapply(1:k, function(i) combn(k,i))
            beta <- sapply(1:k, function(i){ 
              sapply(1:ncol(combi[[i]]), function(j){
                solve(t(sx[,combi[[i]][,j]])%*%sx[,combi[[i]][,j]])%*%t(sx[,combi[[i]][,j]])%*%sy
              })
            })
            R.squared <- sapply(1:k, function(i){ 
              sapply(1:ncol(combi[[i]]), function(j){
                (t(sy)%*%sx[,combi[[i]][,j]]%*%solve(t(sx[,combi[[i]][,j]])%*%sx[,combi[[i]][,j]])%*%t(sx[,combi[[i]][,j]])%*%sy-t(sy)%*%M%*%sy)/(t(sy)%*%sy-t(sy)%*%M%*%sy)
              })
            })
            R.squared <- unlist(R.squared) 
            
            
            combi <- sapply(1:k, function(i) matrix(as.vector(t(combi[[i]])), k, ncol(combi[[i]]), byrow=TRUE)) 
            combi <- unlist(combi) 
            combi <- matrix(combi, k)
            beta[[1]] <- matrix(beta[[1]], 1, k, byrow=TRUE)
            beta <- sapply(1:k, function(i) matrix(as.vector(t(beta[[i]])), k, ncol(beta[[i]]), byrow=TRUE)) 
            beta <- unlist(beta) 
            beta <- matrix(beta, k)
            coeff.covariate <- NULL
            for(i in 1:k){ 
              for(j in 1:ncol(beta)){
                temporary <- unique(beta[,j]*(combi[,j]==i))
                coeff.covariate.new <- ifelse(length(temporary)>1, temporary[temporary!=0], temporary)
                coeff.covariate <- c(coeff.covariate, coeff.covariate.new)
              }
            }
            coeff.covariate <- matrix(coeff.covariate, k, ncol(beta), byrow=TRUE)
            beta <- coeff.covariate
            rownames(beta) <- covariate.names
            colnames(beta) <- paste("Model",1:ncol(beta))
            
            B <- sapply(1:ncol(beta), function(i) (1+g)^((n-sum(beta[,i]!=0)-1)/2)*(1+g*(1-R.squared[i]))^(-(n-1)/2)) 
            B.model0 <- 1
            odds <- sapply(1:ncol(beta), function(i) B[i]/(sum(B)+B.model0))
            names(odds) <- paste("Model",1:ncol(beta))

            E <- (g/(g+1))*beta
            expected.coeff <- as.numeric(odds %*% t(E))
            names(expected.coeff) <- covariate.names
            
            beta.dummy <- ifelse(beta==0,0,1)
            total.weight <- as.numeric(odds %*% t(beta.dummy))
            names(total.weight) <- covariate.names
            
            names(R.squared) <- paste("Model",1:ncol(beta))
            
            beta[beta==0]<-NA 
            
            return(new("bma", coefficient=beta, R.squared=R.squared, posterior.odds=odds, expected.coeff=expected.coeff, posterior.prob=total.weight))
          })