\name{profilelike.polr}
\alias{profilelike.polr}
\title{
Profile Likelihood for Proportional Odds Models
}
\description{
This function provides values for a profile likelihood and a normalized profile likelihood for a parameter of interest in a proportional odds model.
}
\usage{
profilelike.polr(formula, data, profile.theta, method = "logistic", 
	lo.theta = NULL, hi.theta = NULL, length = 300, round = 2, 
	subset = NULL, weights = NULL, offset = NULL, na.action = stats::na.exclude, ...)
}
\arguments{
  \item{formula}{
see corresponding documentation in \code{\link[MASS:polr]{polr}}.
}
  \item{data}{
a data frame. See corresponding documentation in \code{\link[MASS:polr]{polr}}.
}
  \item{profile.theta}{
a parameter of interest, theta; must be a numeric variable.
}
  \item{method}{
see corresponding documentation in \code{\link[MASS:polr]{polr}}.
}
  \item{lo.theta}{
lower bound for a parameter of interest to obtain values for a profile likelihood.
}
  \item{hi.theta}{
upper bound for a parameter of interest to obtain values for a profile likelihood.
}
  \item{length}{
length of numerical grid values for a parameter of interest to obtain values for a profile likelihood.
}
  \item{round}{
the number of decimal places for \code{\link{round}} function to automatically define lower and upper bounds of numerical grid for a parameter of interest. If an automatically defined parameter range is not appropriate, increase the number or specify lo.theta and hi.theta.
}
  \item{subset}{
should not be provided.
}
  \item{weights}{
should not be provided.
}
  \item{offset}{
should not be provided.
}
  \item{na.action}{defaults to \link[stats]{na.exclude}}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{
This function provides values for a profile likelihood and a normalized profile likelihood for a parameter of interest in a proportional odds model. Users must define a parameter of interest in a proportional odds model. This function can be used for proportional odds models comparable with the \code{\link[MASS:polr]{polr}} function. However, arguments weights, subset, and offset should not be provided. A normalized profile likelihood is obtained by a profile likelihood being divided by the maximum value of the profile likelihood so that a normalized profile likelihood ranges from 0 to 1.
}
\value{
A \sQuote{profilelike} object, with elements:
\item{theta }{numerical grid values for a parameter of interest in a specified range (between lower and upper bounds).}
\item{profile.lik }{numerical values for a profile likelihood corresponding to theta in a specified range (between lower and upper bounds).}
\item{profile.lik.norm }{numerical values for a normalized profile likelihood ranging from 0 to 1.}
}
\author{
Leena Choi <naturechoi@gmail.com>
}

\section{Warning }{
	Arguments weights, subset, and offset in the \code{\link[MASS:polr]{polr}} function are not comparable.
	
	Missing values should be removed.
	}

\seealso{
\code{\link{profilelike.plot}}, \code{ \link{profilelike.summary}}, \code{ \link{profilelike.lm}}, \code{ \link{profilelike.glm}}, \code{ \link{profilelike.gls}}, \code{ \link{profilelike.lme}}, \code{ \link[MASS:polr]{polr}}
}

\examples{
data(datapolr)
datapolr$y <- as.factor(datapolr$y)
xx <- profilelike.polr(y ~ x1 + x2, data=datapolr, profile.theta="group", 
				method="logistic", lo.theta=-2, hi.theta=2.5, length=500)
plot(xx)
}
