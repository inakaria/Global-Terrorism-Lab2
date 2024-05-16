library(dplyr)

data <- read.csv("terror.csv")

perform_t_test <- function(df1, df2) {
    test_result <- t.test(df1$Success, df2$Success, alternative="two.sided", var.equal=FALSE)

    return(list(success_were_killed=mean(df1$Success),
                success_no_killed=mean(df2$Success),
                p_value=test_result$p.value,
                t=test_result$statistic,
                lower_t=test_result$conf.int[1],
                upper_t=test_result$conf.int[2]))

}

df_group_1 <- data %>% filter(Killed>0, !is.na(Success))
df_group_2 <- data %>% filter(Killed==0, !is.na(Success))
test_3_result <- perform_t_test(df_group_1, df_group_2)

cat("-- T-test Results --\n")
cat("success_were_killed: ", test_3_result$success_were_killed, "\n")
cat("success_no_killed: ", test_3_result$success_no_killed, "\n")
cat("p_value: ", test_3_result$p_value, "\n")
cat("t: ", test_3_result$t, "\n")
cat("conf_lower_t: ", test_3_result$lower_t, "\n")
cat("conf_upper_t: ", test_3_result$upper_t, "\n")


perform_wald_test <- function(data_1, data_2) {
    se <- sqrt(data_1$var_were_killed + data_2$var_no_killed)
    t_wald <- (data_1$success_were_killed - data_2$success_no_killed) / se
    p_value_wald <- 2*pnorm(-abs(t_wald))

    lower_wald <- data_1$success_were_killed - data_2$success_no_killed - qnorm(0.975) * se
    upper_wald <- data_1$success_were_killed - data_2$success_no_killed + qnorm(0.975) * se

    return(list(success_were_killed=data_1$success_were_killed,
                success_no_killed=data_2$success_no_killed,
                p_value=p_value_wald,
                wald_test=t_wald,
                conf_lower_wald=lower_wald,
                conf_upper_wald=upper_wald))
}

wald_df_group_1 <- df_group_1 %>% 
                   summarise(success_were_killed = mean(Success), 
                             var_were_killed = var(Success) / n())

wald_df_group_2 <- df_group_2 %>% 
                   summarise(success_no_killed = mean(Success), 
                             var_no_killed = var(Success) / n())

wald_3_result <- perform_wald_test(wald_df_group_1, wald_df_group_2)

cat("\n-- Wald Test Results --\n")
cat("success_were_killed: ", wald_3_result$success_were_killed, "\n")
cat("success_no_killed: ", wald_3_result$success_no_killed, "\n")
cat("p_value: ", wald_3_result$p_value, "\n")
cat("Wald_test: ", wald_3_result$wald_test, "\n")
cat("conf_lower_wald: ", wald_3_result$conf_lower_wald, "\n")
cat("conf_upper_wald: ", wald_3_result$conf_upper_wald, "\n\n")
