\docType{class}
\name{bma-class}
\alias{bma-class}
\alias{bma-method}
\alias{initialize,}
\alias{plot,}
\alias{print,}
\alias{show,}
\alias{summary,}
\title{An object of '\code{bma}' (Bayesian Model Averaging) class}
\description{
An object of class '\code{bma}' are created by the
\code{fitbma} functions
}
\details{
An object of the class \code{bma} has the following slots:
\itemize{ \item{coefficient}{A list consisting of a
coefficients matrix for each model} \item{R.squared}{A
vector of the value of R-sqaured for each model}
\item{posterior.odds}{A vector of the posterior model odds
for each model} \item{expected.coeff}{A vector of the
posterior expected value of each coefficient}
\item{posterior.prob}{A vector of the posterior
probabilities that each coefficient is non-zero} }
}
\author{
Myunghoon Kang: \email{myunghoon@wustl.edu}
}

