set.seed(111)
par(mfrow=c(1,2))
hist(rexp(1000))

me = NULL
for(i in 1:1000) me = c(me, mean(rexp(40)))
hist(me)
