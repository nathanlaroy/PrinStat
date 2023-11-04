data <- read.table("VitB1.txt", header = T)
data$vitB1 <- as.factor(data$vitB1)

### 3.1
# 3.1.1
test <- t.test(formula = growth ~ vitB1, data = data, distribution = "exact")
t.obs <- test$statistic
t.obs

t.star <- combn(22, 11, function(x){
  group1 <- data$growth[x]
  group2 <- data$growth[!(1:22) %in% x]
  test <- t.test(group1, group2)
  return(test$statistic)
})

CritV <- quantile(t.star, prob = c(0.025, 0.975))
CritV

mean(abs(t.star) >= abs(t.obs))

# 3.1.2
group1 <- rnorm(10, mean = 16.9, sd = 5.3)
group2 <- rnorm(10, mean = 26.1, sd = 4.2)

data2 <- data.frame(group = c(rep(0,10), rep(1,10)),
                    growth = c(group1,group2))

t.star <- combn(22, 11, function(x){
  group1 <- data2$growth[x]
  group2 <- data2$growth[!(1:22) %in% x]
  test <- t.test(group1, group2)
  return(test$statistic)
})