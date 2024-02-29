\name{LI}
\alias{LI}
\alias{LI.default}
\alias{LI.profilelike}
\title{
Likelihood Interval
}
\description{
ADD
}
\usage{
LI(x, k)
}
\arguments{
  \item{x}{
\sQuote{profilelike} object
}
  \item{k}{
ADD
}
}
\details{
ADD
}
\value{
ADD
}

\seealso{
\code{ \link{profilelike.lm}}, \code{ \link{profilelike.glm}}, \code{ \link{profilelike.polr}}, \code{ \link{profilelike.gls}}, \code{ \link{profilelike.lme}}
}

\examples{
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- c(rep(0,10), rep(1,10))
weight <- c(ctl, trt)
dd <- data.frame(group=group, weight=weight)
xx <- profilelike.lm(formula = weight ~ 1, data=dd, profile.theta="group",
			lo.theta=-2, hi.theta=1, length=500)
LI(xx)
LI(xx, c(8, 32))
}
