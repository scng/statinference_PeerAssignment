set.seed(111)
#par(mfrow=c(1,2))
#hist(rexp(1000))

exp_sample_mean = NULL
for(i in 1:1000) exp_sample_mean = c(exp_sample_mean, mean(rexp(40, rate=0.2)))
#hist(me)

set.seed(111)
sample_size <- 40
lambda <- 0.2
exp_sample_means = NULL
for(i in 1:1000) exp_sample_means = c(exp_sample_means, mean(rexp(sample_size, rate=lambda)))
mean(exp_sample_means)

require(ggplot2)
sample_df <- as.data.frame(exp_sample_means)
g <- ggplot(sample_df, aes(x=exp_sample_means))
g <- g + geom_histogram(aes(y = ..density..), binwidth=0.35, fill='blue', color='white')
g <- g + stat_function(fun = dnorm, args=list(mean=5, sd=5/sqrt(sample_size)), bin_width=0.5, color='red')
g