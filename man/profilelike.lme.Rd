\name{profilelike.lme}
\alias{profilelike.lme}
\title{
Profile Likelihood for Linear Mixed-Effects Models
}
\description{
This function provides values for a profile likelihood and a normalized profile likelihood for a parameter of interest in a linear mixed-effects model.
}
\usage{
profilelike.lme(formula, data, subject, random, correlation = NULL, 
	profile.theta, method = "ML", lo.theta = NULL, hi.theta = NULL, length = 300,
	round = 2, subset = NULL, weights = NULL, na.action = stats::na.exclude, ...)
}
\arguments{
  \item{formula}{
see corresponding documentation in \code{\link[nlme:lme]{lme}}.
}
  \item{data}{
a data frame. See corresponding documentation in \code{\link[nlme:lme]{lme}}.
}
  \item{subject}{
see corresponding documentation in \code{\link[nlme:lme]{lme}}.
}
  \item{random}{
see corresponding documentation in \code{\link[nlme:lme]{lme}}.
}
  \item{correlation}{
see corresponding documentation in \code{\link[nlme:lme]{lme}}.
}
  \item{profile.theta}{
a parameter of interest, theta; must be a numeric variable.
}
  \item{method}{
see corresponding documentation in \code{\link[nlme:lme]{lme}}.
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
  \item{na.action}{defaults to \link[stats]{na.exclude}}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{
This function provides values for a profile likelihood and a normalized profile likelihood for a parameter of interest in a linear mixed-effects model. Users must define a parameter of interest in a linear mixed-effects model. This function can be used for models comparable with the \code{\link[nlme:lme]{lme}} function. However, arguments weights and subset should not be provided. A normalized profile likelihood is obtained by a profile likelihood being divided by the maximum value of the profile likelihood so that a normalized profile likelihood ranges from 0 to 1.
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
	Arguments weights and subset in the \code{\link[nlme:lme]{lme}} function are not comparable.
	
	Missing values should be removed.
	}

\seealso{
\code{\link{profilelike.plot}}, \code{ \link{profilelike.summary}}, \code{ \link{profilelike.lm}}, \code{ \link{profilelike.glm}}, \code{ \link{profilelike.polr}}, \code{ \link{profilelike.gls}}, \code{ \link[nlme:lme]{lme}}
}

\examples{
\dontrun{
xx <- profilelike.lme(formula = yield ~ endpoint, random = ~ 1 | id, 
		correlation=corAR1(form = ~ 1 | id), data=Gasoline, subject="Sample", 
		profile.theta="vapor", method="ML", lo.theta=1, hi.theta=5, length=500, round=2)
plot(xx, round=4)
}
}
