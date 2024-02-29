\name{profilelike.lm}
\alias{profilelike.lm}
\title{
Profile Likelihood for Linear Models
}
\description{
This function provides values for a profile likelihood and a normalized profile likelihood for a parameter of interest in a linear model.
}
\usage{
profilelike.lm(formula, data, profile.theta, lo.theta = NULL, hi.theta = NULL, 
	length = 300, round = 2, subset = NULL, weights = NULL, offset = NULL, na.action = stats::na.exclude, ...)
}
\arguments{
  \item{formula}{
see corresponding documentation in \code{\link{lm}}.
}
  \item{data}{
a data frame. See corresponding documentation in \code{\link{lm}}.
}
  \item{profile.theta}{
a parameter of interest, theta; must be a numeric variable.
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
This function provides values for a profile likelihood and a normalized profile likelihood for a parameter of interest in a linear model. Users must define a parameter of interest in a linear model. This function can be used for linear models comparable with the \code{\link{lm}} function. However, arguments weights, subset, and offset should not be provided. A normalized profile likelihood is obtained by a profile likelihood being divided by the maximum value of the profile likelihood so that a normalized profile likelihood ranges from 0 to 1.
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
	Arguments weights, subset, and offset in the \code{\link{lm}} function are not comparable.
	
	Missing values should be removed.
	}

\seealso{
\code{\link{profilelike.plot}}, \code{ \link{profilelike.summary}},  \code{ \link{profilelike.glm}}, \code{ \link{profilelike.polr}}, \code{ \link{profilelike.gls}}, \code{ \link{profilelike.lme}}, \code{ \link{lm}}
}

\examples{
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- c(rep(0,10), rep(1,10))
weight <- c(ctl, trt)
dd <- data.frame(group=group, weight=weight)
xx <- profilelike.lm(formula = weight ~ 1, data=dd, profile.theta="group",
			lo.theta=-2, hi.theta=1, length=500)
plot(xx)
}
