library(dplyr)

data <- read.csv("terror.csv")

hypothesized_mean <- 0.85
confidence_level <- 0.95
data_filtered <- data %>% filter(!is.na(Success))
mean_success <- mean(data_filtered$Success)

t_test <- t.test(data_filtered$Success, mu = hypothesized_mean, alternative = "less")
print(t_test)
cat("\n-- T-test Results --\n")
cat("mean: ", mean_success, "\n")
cat("t: ", t_test$statistic, "\n")
cat("p_value: ", t_test$p.value, "\n")
cat("lower_t: ", t_test$conf.int[1], "\n")
cat("upper_t: ", t_test$conf.int[2], "\n")


se_mean <- sd(data_filtered$Success) / sqrt(length(data_filtered$Success))
wald_statistic <- (mean_success - hypothesized_mean) / se_mean
p_value <- 2 * pnorm(-abs(wald_statistic))

df <- length(data_filtered$Success) - 1
t <- qt((1 + confidence_level) / 2, df)
lower_bound <- "-Inf"
upper_bound <- mean_success + t * se_mean

cat("\n-- Wald Test Results --\n")
cat("mean:", mean_success, "\n")
cat("Wald_test:", wald_statistic, "\n")
cat("p-value:", p_value, "\n")
cat("lower_t: ", lower_bound, "\n")
cat("upper_t: ", upper_bound, "\n")
