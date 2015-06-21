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

#t1 <- t.test(len~supp, paired = FALSE, var.equal = TRUE, data=ToothGrowth)
t1 <- t.test(len~supp, paired = FALSE, data=ToothGrowth)
t1


ToothGrowth.05 <- ToothGrowth[ToothGrowth$dose == 0.5, "len"]
ToothGrowth.10 <- ToothGrowth[ToothGrowth$dose == 1.0, "len"]
ToothGrowth.20 <- ToothGrowth[ToothGrowth$dose == 2.0, "len"]

t.05.10 <- t.test(ToothGrowth.05, ToothGrowth.10, paired = FALSE, var.equal = TRUE)

t.10.20 <- t.test(ToothGrowth.10, ToothGrowth.20, paired = FALSE, var.equal = TRUE)

t.05.20 <- t.test(ToothGrowth.05, ToothGrowth.20, paired = FALSE, var.equal = TRUE)

t.05.10
t.10.20
t.05.20

ToothGrowth.05.VC <- ToothGrowth[ToothGrowth$dose == 0.5 & ToothGrowth$supp == "VC", "len"]
ToothGrowth.05.OJ <- ToothGrowth[ToothGrowth$dose == 0.5 & ToothGrowth$supp == "OJ", "len"]
ToothGrowth.10.VC <- ToothGrowth[ToothGrowth$dose == 1.0 & ToothGrowth$supp == "VC", "len"]
ToothGrowth.10.OJ <- ToothGrowth[ToothGrowth$dose == 1.0 & ToothGrowth$supp == "OJ", "len"]
ToothGrowth.20.VC <- ToothGrowth[ToothGrowth$dose == 2.0 & ToothGrowth$supp == "VC", "len"]
ToothGrowth.20.OJ <- ToothGrowth[ToothGrowth$dose == 2.0 & ToothGrowth$supp == "OJ", "len"]

t.05 <- t.test(ToothGrowth.05.VC, ToothGrowth.05.OJ, paired = FALSE)
t.10 <- t.test(ToothGrowth.10.VC, ToothGrowth.10.OJ, paired = FALSE)
t.20 <- t.test(ToothGrowth.20.VC, ToothGrowth.20.OJ, paired = FALSE)