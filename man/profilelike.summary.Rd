\name{profilelike.summary}
\alias{profilelike.summary}
\title{
Summary for the Maximum Profile Likelihood Estimate and Likelihood Support Intervals
}
\description{
The function provides the maximum profile likelihood estimate and likelihood support intervals  (Royall, 1997).
}
\usage{
profilelike.summary(k, theta = theta, profile.lik.norm = profile.lik.norm, 
		round = 2)
}
\arguments{
  \item{k}{
strength of evidence for the \code{k}th likelihood support interval.
}
  \item{theta}{
numerical grid values for a parameter of interest in a specified range.
}
  \item{profile.lik.norm}{
numerical values for a normalized profile likelihood ranging from 0 to 1.
}
  \item{round}{
the number of decimal places for \code{\link{round}} function for presentation of the maximum profile likelihood estimate and the kth likelihood support intervals.
}
}
\details{
The function provides the maximum profile likelihood estimate and likelihood support intervals for a profile likelihood obtained from \code{\link{profilelike.lm}}, \code{\link{profilelike.glm}}, \code{\link{profilelike.polr}}, \code{\link{profilelike.gls}} and \code{\link{profilelike.lme}}. The \code{k}th likelihood support interval and the likelihood support interval (\code{k}=6.8) corresponding to a 95\% confidence interval based on a normal approximation are provided.
}
\value{
\item{k }{strength of evidence for the \code{k}th likelihood support interval.}
\item{mle }{the maximum profile likelihood estimate.}
\item{LI.k }{the \code{k}th likelihood support interval.}
\item{LI.norm }{likelihood support interval (\code{k}=6.8) corresponding to a 95\% confidence interval based on a normal approximation.}
}
\references{
	
Royall, Richard M. (1997). Statistical Evidence: A Likelihood Paradiam. Chapman & Hall/CRC.

Pawitan, Yudi (2001). In All Likelihood: Statistical Modelling and Inference Using Likelihood. Oxford University Press.
}
\author{
Leena Choi <naturechoi@gmail.com>
}
\seealso{
\code{\link{profilelike.plot}}, \code{ \link{profilelike.lm}}, \code{ \link{profilelike.glm}}, \code{ \link{profilelike.polr}}, \code{ \link{profilelike.gls}}, \code{ \link{profilelike.lme}}
}

\examples{
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- c(rep(0,10), rep(1,10))
weight <- c(ctl, trt)
dd <- data.frame(group=group, weight=weight)
xx <- profilelike.lm(formula = weight ~ 1, data=dd, profile.theta="group",
				lo.theta=-2, hi.theta=1, length=500)
profilelike.plot(theta=xx$theta, profile.lik.norm=xx$profile.lik.norm, round=2)
profilelike.summary(k=8, theta=xx$theta, profile.lik.norm=xx$profile.lik.norm, round=3)
}
