\name{datapolr}
\alias{datapolr}
\docType{data}
\title{
Example Data for a Profile Likelihood in Proportional Odds Models
}
\description{
This data is used to illustrate how to obtain values for a profile likelihood of a parameter of interest in a proportional odds model.
}
\usage{data(datapolr)}
\format{
  A data frame with 66 observations on the following 5 variables.
  \describe{
    \item{\code{id}}{a numeric vector; unique identification number}
    \item{\code{y}}{a numeric vector; ordinal outcome variable; should be defined as a factor}
    \item{\code{x1}}{a numeric vector; covariate}
    \item{\code{x2}}{a numeric vector; covariate}
    \item{\code{group}}{a numeric vector; covariate and a parameter of interest}
  }
}
\details{
This data is used to illustrate how to obtain values for a profile likelihood of a parameter of interest in a proportional odds model. A parameter of interest is \code{group} indicator variable, \code{y} is an ordinal outcome, and \code{x1} and \code{x2} are covariates in a proportional odds model.
}
\examples{
data(datapolr)
datapolr$y <- as.factor(datapolr$y)
xx <- profilelike.polr(y ~ x1 + x2, data=datapolr, profile.theta="group", 
				method="logistic", lo.theta=-2, hi.theta=2.5, length=500)
profilelike.plot(theta=xx$theta, profile.lik.norm=xx$profile.lik.norm, round=2)
}
\keyword{datasets}
