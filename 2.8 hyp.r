library(dplyr)

data <- read.csv("terror.csv")

perform_t_test <- function(df1, df2) {
    test_result <- t.test(df1$Killed, df2$Killed, alternative="two.sided", var.equal=FALSE)

    return(list(mean_killed_first_group=mean(df1$Killed),
                mean_killed_second_group=mean(df2$Killed),
                p_value=test_result$p.value,
                t=test_result$statistic,
                lower_t=test_result$conf.int[1],
                upper_t=test_result$conf.int[2]))

}

df_group_1 <- data %>% filter(Success==1, !is.na(Killed))
df_group_2 <- data %>% filter(Success==0, !is.na(Killed))
test_3_result <- perform_t_test(df_group_1, df_group_2)

cat("-- T-test Results --\n")
cat("mean_killed_successful: ", test_3_result$mean_killed_first_group, "\n")
cat("mean_killed_unsuccessful: ", test_3_result$mean_killed_second_group, "\n")
cat("p_value: ", test_3_result$p_value, "\n")
cat("t: ", test_3_result$t, "\n")
cat("conf_lower_t: ", test_3_result$lower_t, "\n")
cat("conf_upper_t: ", test_3_result$upper_t, "\n")


perform_wald_test <- function(data_1, data_2) {
    se <- sqrt(data_1$var_killed_1 + data_2$var_killed_2)
    t_wald <- (data_1$mean_killed_1 - data_2$mean_killed_2) / se
    p_value_wald <- 2*pnorm(-abs(t_wald))

    lower_wald <- data_1$mean_killed_1 - data_2$mean_killed_2 - qnorm(0.975) * se
    upper_wald <- data_1$mean_killed_1 - data_2$mean_killed_2 + qnorm(0.975) * se

    return(list(mean_killed_first_group=data_1$mean_killed_1,
                mean_killed_second_group=data_2$mean_killed_2,
                p_value=p_value_wald,
                wald_test=t_wald,
                conf_lower_wald=lower_wald,
                conf_upper_wald=upper_wald))
}

wald_df_group_1 <- df_group_1 %>% 
                   summarise(mean_killed_1 = mean(Killed), 
                             var_killed_1 = var(Killed) / n())

wald_df_group_2 <- df_group_2 %>% 
                   summarise(mean_killed_2 = mean(Killed), 
                             var_killed_2 = var(Killed) / n())

wald_3_result <- perform_wald_test(wald_df_group_1, wald_df_group_2)

cat("\n-- Wald Test Results --\n")
cat("mean_killed_successful: ", wald_3_result$mean_killed_first_group, "\n")
cat("mean_killed_unsuccessful: ", wald_3_result$mean_killed_second_group, "\n")
cat("p_value: ", wald_3_result$p_value, "\n")
cat("Wald_test: ", wald_3_result$wald_test, "\n")
cat("conf_lower_wald: ", wald_3_result$conf_lower_wald, "\n")
cat("conf_upper_wald: ", wald_3_result$conf_upper_wald, "\n\n")
