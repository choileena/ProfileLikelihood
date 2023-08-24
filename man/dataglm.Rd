\name{dataglm}
\alias{dataglm}
\docType{data}
\title{
Example Data for a Profile Likelihood in Generalized Linear Models
}
\description{
This data is used to illustrate how to obtain values for a profile likelihood of a parameter of interest in a generalized linear model.
}
\usage{data(dataglm)}
\format{
  A data frame with 100 observations on the following 5 variables.
  \describe{
    \item{\code{id}}{a numeric vector; unique identification number}
    \item{\code{y}}{a numeric vector; binary outcome variable}
    \item{\code{x1}}{a numeric vector; covariate}
    \item{\code{x2}}{a numeric vector; covariate}
    \item{\code{group}}{a numeric vector; covariate and a parameter of interest}
  }
}
\details{
This data is used to illustrate how to obtain values for a profile likelihood of a parameter of interest in a logistic regression model. A parameter of interest is \code{group} indicator variable, \code{y} is a binary outcome, and \code{x1} and \code{x2} are covariates in a logistic regression model.
}
\examples{
data(dataglm)
xx <- profilelike.glm(y ~ x1 + x2, data=dataglm, profile.theta="group", 
				family=binomial(link="logit"), length=500, round=2)
profilelike.plot(theta=xx$theta, profile.lik.norm=xx$profile.lik.norm, round=2)
}
\keyword{datasets}
