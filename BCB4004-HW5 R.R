#BCB4004 HW5

##Hannah Smith

#Q2


#Q4

#Part A 
# The observed haplotypes counts are: AB 10; Ab 12; aB 25; ab 53
ObsNum = matrix(c(10, 12, 25, 53), ncol=2); #observed numbers

###Calculate D and D'
#Haplotype freqs
pAB = 10/sum(ObsNum); 
pAb = 12/sum(ObsNum); 
paB = 25/sum(ObsNum); 
pab = 53/sum(ObsNum); 
c(pAB, pAb, paB, pab);

#Allele freqs
pA = sum(ObsNum[, 1])/sum(ObsNum);  
pa = sum(ObsNum[, 2])/sum(ObsNum);  
pB = sum(ObsNum[1, ])/sum(ObsNum);  
pb = sum(ObsNum[2, ])/sum(ObsNum);  
c(pA, pa, pB, pb);

#LD measures 
D = pAB - pA*pB;
D; 

#since D is positive, we use the positive bound for D_extreme
D_extreme = min(pA*pb, pa*pB);
D_extreme;

D_prime = D/D_extreme;
D_prime; 

#Part B
#Testing for LE/LD
ExpFreq = matrix(c(pA*pB, pA*pb, pa*pB, pa*pb), ncol=2); #expected freq
ExpFreq;
ExpNum = ExpFreq*sum(ObsNum); #expected numbers
ExpNum;

Stat = sum((ObsNum-ExpNum)^2/ExpNum);
Stat;
pvalue = 1-pchisq(Stat, df=1);
pvalue;

#Use one function for the test. 
chisq.test(ObsNum, correct=F);

#Yates' continuity correction, which is better if cell counts are small (e.g., <= 5)
chisq.test(ObsNum, correct=T);
