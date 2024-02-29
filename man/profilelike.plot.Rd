\name{profilelike.plot}
\alias{profilelike.plot}
\alias{plot.profilelike}
\title{
Profile Likelihood Plot
}
\description{
The function provides a plot for a normalized profile likelihood as well as the maximum profile likelihood estimate and the \code{k}th likelihood support intervals (Royall, 1997).}
\usage{
\method{plot}{profilelike}(x, ...)

profilelike.plot(theta, profile.lik.norm, ...)
}
\arguments{
  \item{x}{
\sQuote{profilelike} object
}
  \item{\dots}{
additional arguments, either arguments to \code{plot.default} or:
    \itemize{
        \item{k}{strength of evidence for the \code{k}th likelihood support interval. Defaults to 1/8 and 1/32. Set to FALSE to ignore.}
        \item{round}{the number of decimal places for \code{\link{round}} function for presentation of the maximum profile likelihood estimate and the \code{k}th likelihood support intervals.}
        \item{showText}{if FALSE, text is not displayed on plot}
        \item{textx}{\sQuote{x} position for text; this is scaled to 0:1}
        \item{texty}{starting \sQuote{y} position for text; defaults to 0.9}
    }
}
  \item{theta}{
numerical grid values for a parameter of interest in a specified range.
}
  \item{profile.lik.norm}{
numerical values for a normalized profile likelihood ranging from 0 to 1.
}
}
\details{
The function provides a plot for a normalized profile likelihood obtained from \code{\link{profilelike.lm}}, \code{\link{profilelike.glm}}, \code{\link{profilelike.polr}}, \code{\link{profilelike.gls}} and \code{\link{profilelike.lme}}. The maximum profile likelihood estimate, the \code{k}th likelihood support interval (\code{k}=8, \code{k}=20, and \code{k}=32), and the likelihood support interval (\code{k}=6.8) corresponding to a 95\% confidence interval based on a normal approximation are also presented. 
}
\value{
A normalized profile likelihood plot with the maximum profile likelihood estimate and the \code{k}th likelihood support intervals.
}
\references{
	
Royall, Richard M. (1997). Statistical Evidence: A Likelihood Paradiam. Chapman & Hall/CRC.

Pawitan, Yudi (2001). In All Likelihood: Statistical Modelling and Inference Using Likelihood. Oxford University Press.
}
\author{
Leena Choi <naturechoi@gmail.com>
}

\seealso{
\code{\link{profilelike.summary}}, \code{ \link{profilelike.lm}}, \code{ \link{profilelike.glm}}, \code{ \link{profilelike.polr}}, \code{ \link{profilelike.gls}}, \code{ \link{profilelike.lme}}
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
summary(xx, k=8, round=3)
}
