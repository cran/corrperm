spearnoties <-
function(x,y,alt=1){
#asymptotic spearman test, assumes no ties 
sel<-is.na(x-y)==FALSE #elim NAs
d<-rank(x[sel])-rank(y[sel])
n<-length(d)
spear<-1-(6*sum(d^2))/(n^3-n)
p<-1-pnorm(spear*sqrt(n-1))
if(alt==-1)p<-1-p
#return one-sided p-value, statistic, number of comparisons
return(c(p,spear,n))
}

