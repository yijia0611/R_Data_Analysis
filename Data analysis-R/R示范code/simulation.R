#creating empty vector
Empty <- NULL

#giving parameters to exponential distribution
n <- 40
lambda <- 0.2

#in that for cycle i created 1000 samples from
#exponential distribution, in each samples there are
#40 elements
for (i in 1:1000) {
  Empty <- c(Empty, mean(rexp(n, lambda)))
}

Empty
# theoratical expected value
expected_value <- 1/lambda

#Histogram for sample means
hist(Empty, xlab='Sample mean', main='Distribution of sample mean for n=40',col='beige')
abline(v=mean(Empty),lwd=3, col='darkslategray4')
abline(v=expected_value,lwd=3, col='firebrick')
legend(c("Sample","Population"), x='topright', lwd=c(3,3), col=c('darkslategray4','firebrick'))
