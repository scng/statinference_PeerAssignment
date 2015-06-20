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
g <- g + stat_function(fun = dnorm, args=list(mean=1/lambda, sd=(1/lambda)/sqrt(sample_size)), bin_width=0.5, color='red')
g <- g + ggtitle("Comparison between Distribution of Simulation and Normal Distribution")
g <- g + xlab("mean") + ylab("density")
#g <- g + geom_vline(xintercept = mean(exp_sample_means), color='green')
#g <- g + geom_vline(xintercept = 1/lambda, color='purple')
g

require(datasets)
?ToothGrowth
#str(ToothGrowth)
summary(ToothGrowth)

table(ToothGrowth$supp, ToothGrowth$dose)

g2 <- ggplot(data=ToothGrowth, aes(x=as.factor(dose), y=len, fill=supp)) 
g2 <- g2 + geom_boxplot()
g2 <- g2 + facet_grid(. ~ supp) 
g2 <- g2 + xlab("Dose in miligrams") + ylab("Tooth length") 
g2 <- g2 + guides(fill = guide_legend(title ="Supplement type"))
g2

t1 <- t.test(len~supp, paired = FALSE, var.equal = TRUE, data=ToothGrowth)
t1
t1$statistics


