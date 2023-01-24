

alpha = 0.05;
p0 = 0.5;
p1 = 0.7;

#Function for calculating the power of two-sided Z test for proportion
power.Z = function(alpha, p0, p1, n){
  zscore = qnorm(alpha/2, lower.tail=F); 
  a = sqrt(p1*(1-p1))/sqrt(p0*(1-p0));
  b = (p1-p0)/sqrt(p1*(1-p1)/n);
  power = 1 - pnorm(zscore/a - b) + pnorm(-zscore/a - b); 
  return(power); 
}

#calculate power at given sample size n; 
power.Z(alpha=alpha, p0=p0, p1=p1, n=30); 

#find power values for various n values. Search for the smallest n s.t. power is at least 0.95
N = 30:1000;
powers = unlist(lapply(N, power.Z, alpha=alpha, p0=p0, p1=p1));
indices = which(powers >= 0.99); #indices of n values that make power >= 0.95. 
N[indices[1]]; #the smallest such n value. 