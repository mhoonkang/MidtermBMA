\docType{package}
\name{BmaPack}
\alias{BmaPack}
\alias{BmaPack-package}
\title{BmaPack}
\description{
Package for Bayesian model averaging for linear models
}
\examples{
\dontrun{
# Create a random 10 by 5 covariate matrix
x <- matrix(rnorm(50,0,1),10,5)
# Create a vector of the values for the dependent variable
y <- 2+1.2*x[,1]+2.2*x[,2]+0.2*x[,3]+3.2*x[,4]+1.8*x[,5]+rnorm(10,0,3)
# Create a a vector of the regression coefficients priors
g <- 3
# run fitBMA function
fitBMA(x=x,y=y,g=g)
}
}
\author{
Myunghoon Kang \email{myunghoon@wustl.edu}
}

