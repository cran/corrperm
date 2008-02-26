\name{spearnoties}
\alias{spearnoties}
\title{Spearman Correlation and Asymptotic P-value}
\description{
Computes Spearman correlation and asymptotic p-value with no
correction for ties.  Only intended for use by cp.test.
}
\usage{
spearnoties(x, y, alt = 1)
}

\arguments{
  \item{x}{a vector of x data}
  \item{y}{a vector of y data}
  \item{alt}{alt specifies the sign of the correlation for the
 alternative hypothesis: 1 if >0 and -1 if <0}
}
\value{
 a vector consisting of the p-value, the Spearman correlation and
the number of pairs used in the calculation.
}

\author{Douglas M. Potter}

\examples{

x<-rnorm(10)
y<-x+rnorm(10)

spearnoties(x,y)


## The function is currently defined as
function(x,y,alt=1){
#asymptotic spearman test, assumes no ties 
sel<-is.na(x-y)==F #elim NAs
d<-rank(x[sel])-rank(y[sel])
n<-length(d)
spear<-1-(6*sum(d^2))/(n^3-n)
p<-1-pnorm(spear*sqrt(n-1))
if(alt==-1)p<-1-p
#return one-sided p-value, statistic, number of comparisons
return(c(p,spear,n))
  }
}

\keyword{htest}
