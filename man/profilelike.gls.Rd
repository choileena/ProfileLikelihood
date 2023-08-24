\name{profilelike.gls}
\alias{profilelike.gls}
\title{
Profile Likelihood for Linear Models for Longitudinal Responses Fitted by Generalized Least Squares
}
\description{
This function provides values for a profile likelihood and a normalized profile likelihood for a parameter of interest in a linear model for longitudinal responses fitted by generalized least squares.
}
\usage{
profilelike.gls(formula, data, correlation = NULL, subject, profile.theta, 
		method = "ML", lo.theta, hi.theta, length = 300, round = 2, 
		subset = NULL, weights = NULL, ...)
}
\arguments{
  \item{formula}{
see corresponding documentation in \code{\link[nlme:gls]{gls}}.
}
  \item{data}{
a data frame. See corresponding documentation in \code{\link[nlme:gls]{gls}}.
}
  \item{correlation}{
see corresponding documentation in \code{\link[nlme:gls]{gls}}.
}
  \item{subject}{
see corresponding documentation in \code{\link[nlme:gls]{gls}}.
}
  \item{profile.theta}{
a parameter of interest, theta; must be a numeric variable.
}
  \item{method}{
see corresponding documentation in \code{\link[nlme:gls]{gls}}.
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
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{
This function provides values for a profile likelihood and a normalized profile likelihood for a parameter of interest in a linear model for longitudinal responses fitted by generalized least squares. Users must define a parameter of interest in the model. This function can be used for models for longitudinal responses comparable with the \code{\link[nlme:gls]{gls}} function. However, arguments weights and subset should not be provided. A normalized profile likelihood is obtained by a profile likelihood being divided by the maximum value of the profile likelihood so that a normalized profile likelihood ranges from 0 to 1.
}
\value{
\item{theta }{numerical grid values for a parameter of interest in a specified range (between lower and upper bounds).}
\item{profile.lik }{numerical values for a profile likelihood corresponding to theta in a specified range (between lower and upper bounds).}
\item{profile.lik.norm }{numerical values for a normalized profile likelihood ranging from 0 to 1.}
}
\author{
Leena Choi <naturechoi@gmail.com>
}

\section{Warning }{
	Arguments weights and subset in the \code{\link[nlme:gls]{gls}} function are not comparable.
	
	Missing values should be removed.
	}

\seealso{
\code{\link{profilelike.plot}}, \code{ \link{profilelike.summary}}, \code{ \link{profilelike.lm}}, \code{ \link{profilelike.glm}}, \code{ \link{profilelike.polr}}, \code{ \link{profilelike.lme}}, \code{ \link[nlme:gls]{gls}}
}
\examples{
data(Gasoline, package = "nlme")
xx <- profilelike.gls(formula=yield ~ endpoint, correlation=nlme::corAR1(form = ~ 1 | id),
	data=Gasoline, subject="Sample", profile.theta="vapor", method="ML", 
	lo.theta=1, hi.theta=5, length=500, round=2)
profilelike.plot(theta=xx$theta, profile.lik.norm=xx$profile.lik.norm, round=4)
}
