set.seed(123)
populations <- rnorm(1000,10,3)
samp <- sample(populations,100)
samp
mean(samp)
sd(samp)
# ÖÃÐÅÇø¼ä
sde <- sd(samp)/sqrt(100)
upper <- mean(samp)+ qnorm(0.995)*sde
lower <- mean(samp)- qnorm(0.995)*sde