\name{ProfileLikelihood-package}
\alias{ProfileLikelihood-package}
\alias{ProfileLikelihood}
\docType{package}
\title{
Profile Likelihood for a Parameter in Commonly Used Statistical Models
}
\description{
This package provides profile likelihoods for a parameter of interest in commonly used statistical models. The models include linear models, generalized linear models, proportional odds models, linear mixed-effects models, and linear models for longitudinal responses fitted by generalized least squares. The package also provides plots for normalized profile likelihoods as well as the maximum profile likelihood estimates and the \code{k}th likelihood support intervals (Royall, 1997).
}
\details{
Use \code{ \link{profilelike.lm}}, \code{ \link{profilelike.glm}}, \code{ \link{profilelike.polr}}, \code{ \link{profilelike.gls}} and \code{ \link{profilelike.lme}} to obtain profile likelihoods and normalized profile likelihoods, and plot the normalized profile likelihoods using \code{\link{plot.profilelike}}.
Use \code{\link{summary.profilelike}} to obtain the maximum profile likelihood estimate and the \code{k}th likelihood support intervals.
}

\author{
Leena Choi <naturechoi@gmail.com>

Maintainer: Leena Choi <naturechoi@gmail.com>
}

\references{
	
Royall, Richard M. (1997). Statistical Evidence: A Likelihood Paradiam. Chapman & Hall/CRC.

Pawitan, Yudi (2001). In All Likelihood: Statistical Modelling and Inference Using Likelihood. Oxford University Press.
}

\keyword{ profile likelihood }
\keyword{ profile likelihood plot }
\keyword{ likelihood support interval }

\seealso{
\code{\link{profilelike.lm}}, \code{ \link{profilelike.glm}}, \code{ \link{profilelike.polr}}, \code{ \link{profilelike.gls}}, \code{ \link{profilelike.lme}}, \code{ \link{profilelike.plot}}, \code{ \link{profilelike.summary}}
}

\examples{
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- c(rep(0,10), rep(1,10))
weight <- c(ctl, trt)
dd <- data.frame(group=group, weight=weight)
xx <- profilelike.lm(formula = weight ~ 1, data=dd, profile.theta="group",
				lo.theta=-2, hi.theta=1, length=500)
plot(xx, round=2)
}
