################################################
### Principles of Statistical Data Analysis   ##
### PC lab on permutation and rank tests      ##
################################################


load("shrimps2.RData")
head(shrimps2)

str(shrimps2)

shrimps2$group <- as.factor(shrimps2$group)
str(shrimps2)

shrimps <- shrimps2[c(1:10,27:36),]
row.names(shrimps) <- 1:20
str(shrimps)
dim(shrimps) # dimensions
table(shrimps$group) # sample sizes

## 2.1
wilcox.test(formula = PCB.conc~group, data = shrimps, 
  alternative = "less", exact = TRUE)

choose(20, 10)

choose(36, 18)

A <- combn(20, 10)

A[,1:5] 

test <- wilcox.test(formula = PCB.conc~group, data = shrimps,
	alternative = "less", exact = TRUE)
W.obs <- test$statistic
W.obs

W.star <- combn(20, 10, function(x) {
		group1 <- shrimps$PCB.conc[x] 
		# obs in group 2 = all obs not in group 1
		group2 <- shrimps$PCB.conc[! (1:20) %in% x]
		test <- wilcox.test(group1, group2,
		    alternative = "less")
		return(test$statistic)
	})
# A histogram of the exact permutation null distribution 
hist(W.star, breaks = 30, 
  main = "Exact permutation null distribution")

criticalValue <- quantile(W.star, probs = 0.05)
criticalValue

hist(W.star, breaks = 30, main = 
  "Exact permutation null distribution Mann-Whitney U statistic")
abline(v = c(criticalValue,W.obs), col = c("blue","red"),
  lty = c(1,2), lwd = 2)
legend("topleft", legend = c("Critical", "Observed"), 
  col = c("blue","red"), lty = c(1,2), lwd = 2)

mean(W.star <= W.obs)

t.test(formula = PCB.conc~group, data = shrimps)

test <- t.test(formula = PCB.conc~group, data = shrimps)
t.obs <- test$statistic
t.obs

t.star <- combn(20, 10, function(x) {
		group1 <- shrimps$PCB.conc[x]
		group2 <- shrimps$PCB.conc[!(1:20) %in% x]
		test <- t.test(group1, group2)
		return(test$statistic)
	})

# Note that we are dealing with a 2-sided test 
criticalValues <- quantile(t.star, probs = c(0.025,0.975))
criticalValues

mean(abs(t.star) >= abs(t.obs))

2*(1 - mean(t.star < abs(t.obs)))

mean(t.star^2>= t.obs^2)

hist(t.star, breaks = 30, main = 
  "Exact permutation null distribution t-test statistic")
abline(v = c(criticalValues, t.obs),
  col = c("blue","blue","red"), lty = c(1,1,2), lwd = 2)
legend("topleft", legend = c("Critical", "Observed"), 
  col = c("blue","red"), lty = c(1,2), lwd = 2)

## 2.2
test.obs <- t.test(formula = PCB.conc ~ group,
	data = shrimps)
t.obs <- test.obs$statistic # observed statistic
t.obs
N <- 1000 # 1000  permutations out of choose(20,10) = 184756.
perm.data <- shrimps   # copy data for permutations
t.star.approx <- replicate(N, {
		# shuffle the group labels
		perm.data$group <- sample(perm.data$group)
		test.perm <- t.test(PCB.conc ~ group,
		    data = perm.data)
		return(test.perm$statistic)
	})

N <- 1000 
t.star.approx <- rep(NA,N)
for(i in 1:N)
{
	perm.data$group <- sample(perm.data$group)
	test <- t.test(PCB.conc ~ group,
	  data = perm.data)
	t.star.approx[i]<-test$statistic
}

criticalValues <- quantile(t.star.approx, probs = c(0.025,0.975))
criticalValues
# Because of the random sampling there will be different values
# for each time that you execute this script!

mean(abs(t.star.approx) >=  abs(t.obs))
# Again, because of the random sampling the p-value will be
# different for each execution.

## 2.3
wilcox.test(formula = PCB.conc~group, data = shrimps,
  exact = FALSE, correct  = FALSE)

# rank transform the outcomes
shrimps$RankPCB <- rank(shrimps$PCB)
# take the sum of the ranks of group 1
W1 <- sum(shrimps$RankPCB[shrimps$group == 1])

n1 <- table(shrimps$group)[1]
n2 <-  table(shrimps$group)[2]
n <- n1 + n2
Exp.W1 <- n1*(n+1)/2
Var.W1 <- n1*n2*(n+1)/12
W1.stand <- (W1 - Exp.W1)/sqrt(Var.W1)
W1.stand

criticalValues <- c(qnorm(0.025), qnorm(0.975))
criticalValues

2*(1 - pnorm(abs(W1.stand)))

1-pchisq(W1.stand^2, df = 1)

## 2.4
# install.packages("coin")
library("coin")
# An exact permutation null distribution is used
wilcox_test(formula = PCB.conc~group, data = shrimps,
  distribution = "exact")

# An approximation of the exact null distribution is used
wilcox_test(formula = PCB.conc~group, data = shrimps,
  distribution = approximate(B = 1000))


# An asymptotic  approximation is used
wilcox_test(formula = PCB.conc~group, data = shrimps,
  distribution = "asymptotic")

