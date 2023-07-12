# code fragment for permutation test

set.seed(2023)

N <- 10000 # number of simulation 
nObs <- 14 # number of normal subjects
corPerm <- numeric(length=N)

auc_ind1 <- rnorm(nObs) # metaplasticity index 
glx_ind1 <- rnorm(nObs) # glx or glx deviation index

for (i in 1:N) {
  shuf.auc <- auc_ind1[sample(length(auc_ind1))]
  corPerm[[i]] <- cor(abs(glx_ind1), shuf.auc)
}

ci <- NA
quantile(corPerm, 0.025) # lower end of 95% confidence interval
quantile(corPerm, 0.975) # upper end of 95% confidence interval

p_val <- (sum(corPerm>= cor(glx_ind1, auc_ind1))+1)/length(corPerm)
p_val # p-value might differ due to the nature of generating random numbers
