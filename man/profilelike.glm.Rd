\name{profilelike.glm}
\alias{profilelike.glm}
\title{
Profile Likelihood for Generalized Linear Models
}
\description{
This function provides values for a profile likelihood and a normalized profile likelihood for a parameter of interest in a generalized linear model.
}
\usage{
profilelike.glm(formula, data, profile.theta, family = stats::gaussian, 
	offset.glm = NULL, lo.theta = NULL, hi.theta = NULL, length = 300, 
	round = 2, subset = NULL, weights = NULL, offset = NULL, na.action = stats::na.exclude, ...)
}
\arguments{
  \item{formula}{
see corresponding documentation in \code{\link{glm}}.
}
  \item{data}{
a data frame. See corresponding documentation in \code{\link{glm}}.
}
  \item{profile.theta}{
a parameter of interest, theta; must be a numeric variable.
}
  \item{family}{
see corresponding documentation in \code{\link{glm}}.
}
  \item{offset.glm}{
same usage as offset in \code{\link{glm}}. See corresponding documentation for offset in \code{\link{glm}}.
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
should not be provided. Instead use offset.glm.
}
  \item{na.action}{defaults to \link[stats]{na.exclude}}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{
This function provides values for a profile likelihood and a normalized profile likelihood for a parameter of interest in a generalized linear model. Users must define a parameter of interest in a generalized linear model. This function can be used for generalized linear models comparable with the \code{\link{glm}} function. However, arguments weights, subset, and offset should not be provided. An argument offset in \code{\link{glm}} function can be provided using offset.glm. A normalized profile likelihood is obtained by a profile likelihood being divided by the maximum value of the profile likelihood so that a normalized profile likelihood ranges from 0 to 1.
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
	Arguments weights, subset, and offset in the \code{\link{glm}} function are not comparable.
	
	Missing values should be removed.
	}

\seealso{
\code{\link{profilelike.plot}}, \code{ \link{profilelike.summary}}, \code{ \link{profilelike.lm}}, \code{ \link{profilelike.polr}}, \code{ \link{profilelike.gls}}, \code{ \link{profilelike.lme}}, \code{ \link{glm}}
}

\examples{
data(dataglm)
xx <- profilelike.glm(y ~ x1 + x2, data=dataglm, profile.theta="group", 
				family=binomial(link="logit"), length=500, round=2)
plot(xx)
}
