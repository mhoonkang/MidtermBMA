\name{fitBMA}
\alias{fitBMA}
\alias{fitBMA,ANY-method}
\title{Bayesian Model Averaging}
\usage{
fitBMA(x, y, g, ...)
}
\arguments{
  \item{x}{A \code{n} by \code{k} covariate matrix.
  \code{k} should be greater than or equal to 2.}

  \item{y}{A numeric vector of the values of the dependent
  variable. The length shoud be \code{n}.}

  \item{g}{A number for Priors}

  \item{...}{Arguments to be passed to methods}
}
\value{
An object of '\code{bma}' class with the elements
\item{coefficient}{A list consisting of a coefficients
matrix for each model} \item{R.squared}{A vector of the
value of R-sqaured for each model} \item{posterior.odds}{A
vector of the posterior model odds for each model}
\item{expected.coeff}{A vector of the posterior expected
value of each coefficient} \item{posterior.prob}{A vector
of the posterior probabilities that each coefficient is
non-zero}
}
\description{
This function fits regression for all possible combinations
of the covariates and returns 1) all of the coefficients
and R-squared values for each model, 2) the posterior model
odds for each model, 3) the posterior expected value of
each coefficient and 4) the posterior probability that the
coefficient is non-zero.
}
\note{
This produces an object of a new class '\code{bma}'.
}
\examples{
# Create a random 10 by 5 covariate matrix
x <- matrix(rnorm(50,0,1),10,5)
# Create a vector of the values for the dependent variable
y <- 2+1.2*x[,1]+2.2*x[,2]+0.2*x[,3]+3.2*x[,4]+1.8*x[,5]+rnorm(10,0,3)
# run fitBMA function
fitBMA(x=x,y=y,g=3)
}
\author{
Myunghoon Kang \email{myunghoon@wustl.edu}
}
\seealso{
\code{\link{plotBMA}}

\code{\link{summaryBMA}}
}

