\name{corrperm-package}
\alias{corrperm-package}
\alias{corrperm}
\docType{package}
\title{
Permutation Tests of Correlation with Repeated Measurements
}
\description{
Three permutation tests of correlation useful 
when there are repeated measurements 
}
\details{
\tabular{ll}{
Package: \tab corrperm\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2008-02-26\cr
License: \tab GPL-3\cr
}
The function cp.test tests the null hypothesis of no correlation
of two variables (x and y) that are measured in repeated 
independent experiments. Each experiment measures either x or y
on the same fixed number of populations.  Inference is based on 
permutation tests.  Three different statistics are used in the tests,
 which are one-sided, and are computed at the same time.

In the absence of ties, the Spearman correlation, \eqn{r}, 
equals \eqn{1 - 6 \sum d^{2} / (n^{3} - n)}{1 - 6 sum d^2/(n^3 - n)}
where \eqn{d}{d} is the difference in ranks for 
a x,y pair,  \eqn{n}{n} is the number of populations (x,y pairs) and 
the summation is over all \eqn{d}{d}. Under the null hypothesis
the variance of \eqn{r} is \eqn{1 / (n - 1)}{1/(n-1)}. 
The tests are based on the combining the Spearman correlations or p-values 
for all possible combinations of x and y experiments.

The inverse-variance weighted average Spearman correlation is:  
\eqn{S = \sum_{j,k} (n_{jk} - 1) r_{jk} / (m \sum_{j,k}  (n_{jk} - 1))}{%
S = sum (njk - 1) rjk/(m sum (njk - 1))},
 where \eqn{n_{jk}}{njk} is the number of
x,y pairs and \eqn{r_{jk}}{rjk} is the correlation for the \eqn{j^{th}}{jth} x 
and the \eqn{k^{th}}{kth} y experiment, and
the summation is over all possible pairs. 

The unweighted Fisher's combining function is:
\eqn{F = -2 \sum_{j,k} \ln(p_{jk})}{F = -2 sum ln(pjk)}
, where \eqn{p_{jk}}{pjk} is the p-value of the Spearman test 
for the \eqn{j^{th}}{jth} x and the \eqn{k^{th}}{kth} y experiment. 

The weighted Fisher's combining function is:
\eqn{Fw = -2 \sum_{j,k} (n_{jk} - 1) \ln(p_{jk}) / \sum_{j,k} (n_{jk} - 1)}{%
Fw = -2 sum (njk - 1)log(pjk)/sum (njk - 1)}.

Each experiment must sample the same populations,
but the number of experiments measuring x and y can differ.  Missing
values are allowed.
}
\author{Douglas M. Potter

Maintainer:  Douglas M Potter <dmp71@pitt.edu>
}
\references{
Potter, D.M. and Acilan C., Permutation Tests of Correlation for Data with Repeated Measurements,
available from D. Potter via email.
}

\keyword{htest}
\examples{

##9 different populations on which 7 experiments 
##measure x and 5 measure y

npop<-9
nxexpt<-7
nyexpt<-5

##population effects

popeff<-rnorm(npop,0,.5)

x<-rnorm(nxexpt*npop)
y<-rnorm(nyexpt*npop)
x<-matrix(x,nrow=npop)
y<-matrix(y,nrow=npop)

for(i in 1:npop){
	for(j in 1:nxexpt){
		x[i,j]<-x[i,j]+popeff[i]
	}
	for(j in 1:nyexpt){
		y[i,j]<-y[i,j]+popeff[i]
	}
}

cp.test(x,y,100)


}
