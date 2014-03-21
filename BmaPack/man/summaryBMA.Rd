\name{summaryBMA}
\alias{ANY-method}
\alias{summaryBMA}
\alias{summaryBMA,}
\title{Summarize the Object of '\code{bma}' Class}
\usage{
summaryBMA(x, ...)
}
\arguments{
  \item{x}{An object of '\code{bma}' class}
  
  \item{...}{Arguments to be passed to methods
}
\value{
An object of '\code{sum.bma}' class with the elements
\item{expected.coefficient}{The expected value of each
coefficient} \item{posterior.prob}{the posterior
probability that the coefficient is non-zero}
}
\description{
This function summarizes an object of '\code{bma}' class
}
\note{
This produces an object of a new class '\code{sum.bma}'.
}
\examples{
# Create a random 10 by 5 covariate matrix
x <- matrix(rnorm(50,0,1),10,5)
# Create a vector of the values for the dependent variable
y <- 2+1.2*x[,1]+2.2*x[,2]+0.2*x[,3]+3.2*x[,4]+1.8*x[,5]+rnorm(10,0,3)
# run fitBMA function
a <- fitBMA(x=x,y=y,g=3)
summary(a)
}
\author{
Myunghoon Kang \email{myunghoon@wustl.edu}
}
\seealso{
\code{\link{fitBMA}}

\code{\link{plotBMA}}
}

