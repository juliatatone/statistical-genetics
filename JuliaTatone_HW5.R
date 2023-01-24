

####==========================================
# Estimate allele freq based on hyplotype freq;
# Test for kinkage disequilibrium (LD)
# (FM V4 Example 1.6 on page 17)
####==========================================

#Assume allele + is indexed by 1, allele - is indexed by 2. Col for A, row for B 
# The observed haplotypes counts are: A1B1 4; A1B2 4; A2B1 13; A2B1 26
ObsNum = matrix(c(10, 12, 25, 53), ncol=2); #observed numbers

###Calculate D and D'
#Haplotype freqs
pA1B1 = 10/sum(ObsNum); 
pA1B2 = 12/sum(ObsNum); 
pA2B1 = 25/sum(ObsNum); 
pA2B2 = 53/sum(ObsNum); 
c(pA1B1, pA1B2, pA2B1, pA2B2);

#Allele freqs
pA1 = sum(ObsNum[, 1])/sum(ObsNum);  
pA2 = sum(ObsNum[, 2])/sum(ObsNum);  
pB1 = sum(ObsNum[1, ])/sum(ObsNum);  
pB2 = sum(ObsNum[2, ])/sum(ObsNum);  
c(pA1, pA2, pB1, pB2);

#LD measures 
D = pA1B1 - pA1*pB1;
D; 

pA1B1*pA2B2 - pA1B2*pA2B1

#since D is positive, we use the positive bound for D_extreme
D_extreme = min(pA1*pB2, pA2*pB1);
D_extreme;

D_prime = D/D_extreme;
D_prime; 

##### Testing for LE/LD
###Method 1: Chi-square goodness-of-fit test
ExpFreq = matrix(c(pA1*pB1, pA1*pB2, pA2*pB1, pA2*pB2), ncol=2); #expected freq
ExpFreq;
ExpNum = ExpFreq*sum(ObsNum); #expected numbers
ExpNum;

Stat = sum((ObsNum-ExpNum)^2/ExpNum);
Stat;
pvalue = 1-pchisq(Stat, df=1);
pvalue; #large p-value means not rejecting H_0, means LE is not rejected

#Use one function for the test. 
#  Note the warning message: sample is too small for chi-square approximation. 
chisq.test(ObsNum, correct=F);


