
# 0) prepare data

NB <- c(15.4, 13.5, 13.3, 12.4, 12.8, 13.5, 14.5, 13.9,
        11.0, 15.0, 17.0, 13.8, 17.4, 16.5, 14.4)

EFB<- c(16.5, 13.2, 13.6, 13.6, 14.0, 14.0, 16.0, 14.1,
        11.5, 14.4, 16.0, 13.2, 16.6, 18.5, 14.5)

CBF_data <- data.frame("Subject" = c(1:15),
                       "NB" = NB,
                       "EFB" = EFB)

obs.cor <- cor(NB, EFB, method = "spearman")

# 1) graphing and exploratory data analysis
#### GRAPHS

# standard scatterplot with LS line
plot(CBF_data$NB, CBF_data$EFB)
abline(lm(CBF_data$EFB ~ CBF_data$NB))

# scatterplot of differences with smoother (ideally about zero)
diff <- CBF_data$EFB - CBF_data$NB
plot(diff)
lines(x = 1:15, y = rep(0, times=15))
lines(lowess(diff), col = "red")

# boxplot of differences with horizontal about mean
boxplot(diff)
abline(h = mean(diff), col = "red", lwd = 3)

#### EXPLORATORY DATA ANALYSIS
# central Q: explore data wrt association
mean(diff)
sd(diff)

# other: in reference to graphs

####--------------------------------####

# 2) Estimate Spearman Rank Correlation
cor(CBF_data$EFB, CBF_data$NB, method = "spearman")


# 3) permutation test
N <- 10000
spearman.r.star <- replicate(N, {
  NB_new <- sample(NB)
  spearman.r <- cor(NB_new, EFB, method = "spearman")
  return(spearman.r)
})
CritValues <- quantile(spearman.r.star, probs = c(0.975))
hist(spearman.r.star, breaks = 40)
abline(v = c(CritValues,obs.cor), col = c("red","blue")
                              , lwd = 2)
print(c(max(spearman.r.star), obs.cor))
sum(spearman.r.star >= obs.cor)
mean(spearman.r.star >= obs.cor)


# 4) Asymptotic approximation
## standardized coefficients (with ties!)
n <- 15
NB_ranked <- rank(NB) # 1 tie: 5.5 (2)
EFB_ranked <- rank(EFB) # 4 ties:
#                              2.5 (2)
#                              4.5 (2)
#                              6.5 (2)
#                              11.5 (2)
Diff_ranked <- NB_ranked - EFB_ranked
g <- 1
t_i <- 2
h <- 4
u_i <- 2
r.star <- (n*(n^2 - 1) - 6*sum(Diff_ranked^2) - 0.5*(g*(t_i*(t_i^2 - 1)) + h*(u_i*(u_i^2 - 1)))) / (((n*(n^2 - 1) - g*(t_i*(t_i^2 - 1))) * (n*(n^2 - 1) - h*(u_i*(u_i^2 - 1))))^0.5)

r.standard <- (n-1)^(0.5) * r.star
p.value <- 1 - pnorm(r.standard)

# 5) Compare two distributions
probabilities <- seq(0.0001, 0.9999, by = 0.01)
probabilities[1:5]

perm.Quantiles <- quantile(spearman.r.star, probabilities)
n.Quantiles <- qnorm(probabilities)

plot(n.Quantiles, perm.Quantiles)
qqline(perm.Quantiles)

# or
qqnorm(perm.Quantiles)
qqline(perm.Quantiles)
