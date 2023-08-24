\name{LR.pvalue}
\alias{LR.pvalue}
\title{
P-values based on LR statistics for 2 x 2 Tables
}
\description{
This function provides p-values based on likelihood ratio (LR) statistics for 2 x 2 tables.
}
\usage{
LR.pvalue(y1, y2, n1, n2, interval=0.01)
}
\arguments{
  \item{y1}{
the number of success for treatment 1.
}
  \item{y2}{
the number of success for treatment 2.
}
  \item{n1}{
the sample size for treatment 1.
}
  \item{n2}{
the sample size for treatment 2.
}
  \item{interval}{
grid for evaluating a parameter of interest to obtain values for likelihoods. The default is 0.01.
}
}
\details{
This function provides p-values based on the profile and conditional likelihood ratio (LR) statistics for 2 x 2 tables.
The function also provides the profile and conditional likelihood support intervals (\code{k}=6.8) corresponding to a 95\% confidence interval based on a normal approximation. For comparison purpose, p-values from Pearson's Chi-squared test, Fisher's exact test and Pearson's Chi-squared test with continuity correction are also provided.}
\value{
\item{mle.lor.uncond }{the maximum likelihood estimate for log odds ratio.}
\item{mle.lor.cond }{the maximum conditional likelihood estimate for log odds ratio.}
\item{LI.norm.profile }{profile likelihood support interval (\code{k}=6.8) corresponding to a 95\% confidence interval based on a normal approximation.}
\item{LI.norm.cond }{conditional likelihood support interval (\code{k}=6.8) corresponding to a 95\% confidence interval based on a normal approximation.}
\item{LR.profile }{profile likelihood ratio.}
\item{LR.cond }{conditional likelihood ratio.}
\item{Pvalue.LR.profile }{p-value based on the profile LR statistic.}
\item{Pvalue.LR.cond }{p-value based on the conditional LR statistic.}
\item{Pvalue.chisq.test }{p-value from Pearson's Chi-squared test.}
\item{Pvalue.fisher.test }{p-value from Fisher's exact test.}
\item{Pvalue.chisq.cont.correction }{p-value from Pearson's Chi-squared test with continuity correction.}
}
\author{
Leena Choi <naturechoi@gmail.com>
}

\section{Warning }{
	Likelihood intervals, LRs and the corresonding p-values are not reliable with empty cells (y1=0 or y2=0) in 2 x 2 tables.
	
	P-values from Pearson's Chi-squared test, Fisher's exact test and Pearson's Chi-squared test with continuity correction are provided only for comparison purpose. For more options, use \code{\link{chisq.test}} and \code{\link{fisher.test}} for these tests.
	}

\seealso{
\code{\link{profilelike.plot}}, \code{ \link{profilelike.summary}}, \code{ \link{profilelike.glm}}
}
\examples{
	(fit <- LR.pvalue(y1=20, y2=30, n1=50, n2=50, interval=0.01))
}
\keyword{LR p-value}
\keyword{2-by-2 table}
