### 
# HW1 Question 3
###


###Demonstrate binomial distribution
install.packages("TeachingDemos"); #A nice package of learning statistics
library(TeachingDemos); 
vis.binom();


### Probability distribution: The true distribution model
#Consider binomial(n,p)
n=100; p=0.3;

#Probability mass function:
fx = dbinom(25:35, size=n, prob=p);
plot(25:35, fx, type="p");

#Distribution function:
Fx = pbinom(25:35, size=n, prob=p);
plot(25:35, Fx, type="s");

### Empirical distribution: The data distribution, often used to simulate the true distribution model
sampleSize = 100;
dat = rbinom(n=sampleSize, size=n, prob=p);

#Empirical probability mass:
table(dat)/sampleSize; 
fx.e = unlist(lapply(25:35, function(x){sum(dat==x)/sampleSize}));
fx.e;
plot(25:35, fx.e, type="p");
hist(dat, breaks=50);

#Empirical distribution
Fx.e = cumsum(fx.e);
Fx.e;
plot(25:35, Fx.e, type="s");

#Empirical mean
mean(dat);

#Empirical variance
var(dat);

### Approximate binomial distribution by normal distribution (by Central Limit Theorem)
#Consider X ~ Binomial(n, p);
#Distribution approximation
par(mfrow=c(2,2));
ns = c(5, 10, 50, 100); #a seq of n
p = 0.3;
sampleSize = 100;
for (i in 1:length(ns)) {
  sampleX = rbinom(sampleSize, ns[i], p); #generate binomial random variables
  meanX = ns[i]*p;
  sdX = sqrt(ns[i]*p*(1-p));
  normalizedX = (sampleX - meanX)/sdX; #Normalize the random variables
  hist(normalizedX, prob=TRUE, breaks=20, xlab=paste("n = ", ns[i], sep=""));
  x = seq(-3, 3, 0.1); #a grid for drawing normal density curve
  lines(x, dnorm(x)); #add the the probability density function of the normal distn. 
}

#Probability approximation for P(25 <= X <= 35);
n=100;
#accurate value
1 - pbinom(25, n, p); #or
1 - pbinom(35, n, p); #
#Normal approximation based on true mean and standard deviation
meanX = n*p;
sdX = sqrt(n*p*(1-p));	
test1 = 1 - pnorm((25-meanX)/sdX);
test2 = 1 - pnorm((35-meanX)/sdX);
test1 - test2;


