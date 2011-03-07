\name{cp.test}
\alias{cp.test}
\title{Permutation Tests of Correlation with Repeated Measurements}
\description{
Three permutation tests of correlation useful 
when there are repeated measurements 
}
\usage{
cp.test(xx, yy, nrep = 1000, alt = 1)
}

\arguments{
  \item{xx}{data for variable xx in a matrix in which each row
corresponds to a population 
and each column corresponds to an experiment}
  \item{yy}{data for variable yy arranged as for xx}
  \item{nrep}{number of Monte Carlo replicates for the permuatation
 test}
  \item{alt}{alt specifies the sign of the correlation for the
 alternative hypothesis: 1 if >0 and -1 if <0}
}
\details{
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
\eqn{S = \sum_{j,k} (n_{jk} - 1) r_{jk} / \sum_{j,k} (n_{jk} - 1)}{%
S = sum (njk - 1) rjk/(m sum (njk - 1))},
 where  \eqn{n_{jk}}{njk} is the number of
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

cp.test calls spearnoties.
}
\value{
  \item{Alternative}{direction of alternative hypothesis: positive or negative
correlation}
  \item{F.p.value}{one-sided p-value based on 
Fisher's combining function}
  \item{Fw.p.value}{one-sided p-value based on 
a weighted Fisher's combining function}
  \item{S.p.value}{one-sided p-value based on 
a weighted average Spearman correlation}
  \item{Correlation}{weighted average Spearman correlation}
}
\references{
Potter, D.M. and Acilan C., Permutation Tests of Correlation for Data with Repeated Measurements,
available from D. Potter via email.
}
\author{Douglas M. Potter}
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

## The function is currently defined as
function(xx,yy,nrep=1000,alt=1){
#assumes data is matrix, rows=populations, columns=replicate experiments
if(alt==1)alte<-"One-sided p-values for positive correlation"
else if(alt==-1)alte<-"One-sided p-values for negative correlation"
else stop("alt must be 1 or -1")

p0<-1
p0w<-0
ntot0<-0
stat0<-0
npop<-length(xx[,1])
nxexpt<-length(xx[1,])
nyexpt<-length(yy[1,])
for(i in 1:nxexpt){
for(j in 1:nyexpt){
out<-spearnoties(xx[,i],yy[,j],alt)
if(out[3]>2){
ntot0<-ntot0+(out[3]-1)
p0<-p0*out[1]
p0w<-p0w+log(out[1])*(out[3]-1)
stat0<-stat0+out[2]*(out[3]-1)
  }
  }
  }
stat0<-stat0/ntot0
p0w<-p0w/ntot0
#print(ntot0)
ptest<-rep(1,nrep)
ptestw<-rep(0,nrep)
stat<-rep(0,nrep)
ntot<-rep(0,nrep)
for(irep in 1:nrep){
sam<-sample(1:npop)
for(i in 1:nxexpt){
for(j in 1:nyexpt){
out<-spearnoties(x[,i],y[sam,j],alt)
if(out[3]>2){
ntot[irep]<-ntot[irep]+out[3]-1
ptest[irep]<-ptest[irep]*out[1]
ptestw[irep]<-ptestw[irep]+log(out[1])*(out[3]-1)
stat[irep]<-stat[irep]+out[2]*(out[3]-1)

  }
  }
  }
  }
stat<-stat/ntot#weighted by number of pairs that contribute
ptestw<-ptestw/ntot
#print(c(p0,ptest,ptestw))
#print(c(stat0,stat,length(stat[stat<0])))
#print(ntot)

ret.val<-list(Alternative=alte,F.p.value=length(ptest[ptest<=p0])/nrep,
Fw.p.value=length(ptestw[ptestw<=p0w])/nrep,
S.p.value=length((alt*stat)[alt*stat>=alt*stat0])/nrep)
names(ret.val$Alternative)<-" "
names(ret.val$F.p.value)<-"Fisher, unweighted"
names(ret.val$Fw.p.value)<-"Fisher, weighted"
names(ret.val$S.p.value)<-"Spearman"

return(ret.val)
  }
}

\keyword{htest}

