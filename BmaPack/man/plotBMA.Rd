\name{plotBMA}
\alias{ANY-method}
\alias{plotBMA}
\alias{plotBMA,}
\title{Plot the Object of '\code{bma}' Class}
\usage{
plotBMA(x, ...)
}
\arguments{
  \item{x}{An object of '\code{bma}' class}

  \item{...}{Arguments to be passed to methods}
}
\value{
Useful two plots from an object of '\code{bma}' class
\item{The expected value of each coefficient} \item{the
posterior probability that the coefficient is non-zero}
}
\description{
This function plots 1) the expected value of each
coefficient and 2) the posterior probability that the
coefficient is non-zero
}
\examples{
# Create a random 10 by 5 covariate matrix
x <- matrix(rnorm(50,0,1),10,5)
# Create a vector of the values for the dependent variable
y <- 2+1.2*x[,1]+2.2*x[,2]+0.2*x[,3]+3.2*x[,4]+1.8*x[,5]+rnorm(10,0,3)
# run fitBMA function
a <- fitBMA(x=x,y=y,g=3)
plot(a)
}
\author{
Myunghoon Kang \email{myunghoon@wustl.edu}
}
\seealso{
\code{\link{fitBMA}}

\code{\link{summary.BMA}}
}

