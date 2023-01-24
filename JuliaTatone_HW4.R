#2a

#regression model

?mtcars;

plot(mtcars$hp,mtcars$cyl);

summary (lm(hp ~ cyl, data = mtcars));

abline(lm(hp ~ cyl, data = mtcars));

#a1
# at alpha = 0.05, hp and cyl are statistically significant (associated) with a p-value of 3.48e-09

#a2
#hp = -51.054 + 31.958(cyl)
#at cyl = 4... hp = 76.778
#at cyl = 6... hp = 140.694
#at cyl = 8... hp = 204.61


#2b

model <- glm(vs ~ hp, data = mtcars, family = binomial);
summary(model);

# at alpha = 0.05, vs and hp are statistically significant (associated) with a p-value of 0.01234
#vs = 8.37802 - 0.06856(hp)
#hp = 150

geno_o = c(803, 185, 12); #observed genotye counts: AA, Aa, aa
p = (803+ 185/2)/sum(geno_o); #freq of allele 
p;
geno_e = c(p^2, 2*p*(1-p), (1-p)^2) * sum(geno_o); #expected genotype counts under the HWE.
geno_e;

stat = sum( (geno_o - geno_e)^2/geno_e); #the statistic of the chi-squared test. 
stat; 
pvalue = 1-pchisq(stat, df=1);
pvalue; 

