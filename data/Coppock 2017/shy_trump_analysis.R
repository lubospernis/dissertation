# Replication Script for:
# Coppock, Alexander. 2017. 
# “Did Shy Trump Supporters Bias the 2016 Polls? Evidence from a Nationally-representative List Experiment” 
# Statistics, Politics, and Policy (forthcoming)

rm(list = ls())

# Set up ------------------------------------------------------------------

# Uncomment to set working directory
# setwd("")

# Uncomment to install packages
# install.packages(c("tidyverse", "xtable", "list", "survey", "coefplot", "sandwich", "lmtest", "broom"))

library(tidyverse)
library(coefplot)
library(xtable)
library(list)
library(survey)
library(sandwich)
library(lmtest)
library(broom)

# some helper functions 
add_parens <- function(x, digits=3){
  x <- as.numeric(x)
  return(paste0("(", sprintf(paste0("%.", digits, "f"), x), ")"))
}

format_num <- function(x, digits=3){
  x <- as.numeric(x)
  return(paste0(sprintf(paste0("%.", digits, "f"), x)))
}


# Load Data
trump <- read_csv("shy_trump_cleaned.csv")


# Direct Estimate (all choices) -------------------------------------------

design <- svydesign(ids = ~ 1,
                    weights = trump$WEIGHTS,
                    data = trump)

prop.table(svytable(formula = ~ vote_choice, design))


# Table 1: Distribution of List Responses ---------------------------------

tab <- prop.table(svytable(formula = ~ list_Y + Z, design), margin = 2)

colnames(tab) <- c("Control List", "Treatment List")
rownames(tab) <- c("0 items", "1 item", "2 items", "3 items", "4 items")

tab %>%
  xtable(digits = 2) %>%
  print.xtable(only.contents = TRUE, 
               include.colnames = FALSE, 
               hline.after = c())


# List Experiment Estimate ------------------------------------------------

fit <- lm(list_Y ~ Z, weights = WEIGHTS, data = trump)
fit_r <- coeftest(fit, vcov. = vcovHC(fit, type = "HC2"))
fit_r

0.32547778 - fit_r[2,2]

fit_2 <- lm(list_Y ~ Z, weights = WEIGHTS, data = filter(trump, direct_Y == 0))
coef(fit_2)[1]
sum(coef(fit_2))

fit_2_r <- coeftest(fit_2, vcov. = vcovHC(fit, type = "HC2"))
fit_2_r


# Regressions

# This function makes all four estimates (plus differences) and is bootstrappable.
four_ests <- function(df){
  
  df$boot_index <- 1:nrow(df)
  
  direct_fit_adj <- glm(direct_Y ~ pid_7 + republican + democrat + LV_INDEX_DUMMY + 
                   black + hispanic + white + female + 
                   educ_1 + educ_2 + educ_3 + USHHI2,
                 family = binomial(), weights = WEIGHTS, data = df)
  
  list_fit_adj <-
    try(
    ictreg(list_Y ~ pid_7 + republican + democrat + LV_INDEX_DUMMY + 
             black + hispanic + white + female + 
             educ_1 + educ_2 + educ_3 + USHHI2,
           weights = "WEIGHTS",
           treat = "Z",
           J = 3,
           method = "nls",
           data = data.frame(df))
    )
  
  # extra covs
  covs_df <- 
    df %>% select(boot_index, pid_7, female, 
                  race_4, LV_INDEX_DUMMY, 
                  EDUATT, income_quintile)
  grouped_df <- 
  df %>%
    mutate(entire_sample = 1) %>%
    gather(col, value, pid_7, female, race_4, LV_INDEX_DUMMY, EDUATT, income_quintile, entire_sample) %>% 
    mutate(group = paste0(col, "_", value)) %>%
    left_join(covs_df) %>%
    group_by(group) 
  
  estimates <-
    grouped_df %>%
    do(data_frame(
      direct_est = with(data.frame(.), weighted.mean(direct_Y, WEIGHTS)),
      list_est = coef(lm(list_Y ~ Z, weights = WEIGHTS, data = data.frame(.)))[2],
      difference = direct_est - list_est,
      direct_est_adj = mean(arm::invlogit(predict(direct_fit_adj, newdata = data.frame(.)))),
      list_est_adj = tryCatch(
        mean(predict(list_fit_adj, newdata = data.frame(.))$fit),
        error = function(err)
          NA
      ),
      difference_adj = tryCatch(
        direct_est_adj - list_est_adj,
        error = function(err)
          NA
      )
    ))
  
  return(estimates)
}

set.seed(343)
bootstrap_replicates <-
  trump %>% 
  # This step takes a while
  bootstrap(m = 2000) %>%
  do(four_ests(.))

boot_summary <- 
bootstrap_replicates %>%
  gather(estimator, estimate, direct_est, list_est, difference, direct_est_adj, list_est_adj, difference_adj) %>%
  group_by(group, estimator) %>%
  summarise(se = sd(estimate, na.rm = TRUE),
            ui = quantile(estimate, 0.975, na.rm = TRUE),
            li = quantile(estimate, 0.025, na.rm = TRUE))


ests <- trump %>% 
  four_ests() %>%
  gather(key = estimator, value = est, -group)

ns <- 
trump %>%
  mutate(entire_sample = 1) %>%
  gather(col, value, pid_7, female, race_4, LV_INDEX_DUMMY, EDUATT, income_quintile, entire_sample) %>% 
  mutate(group = paste0(col, "_", value)) %>%
  group_by(group) %>%
  summarize(n = n())


results_df <- 
  ests %>% left_join(boot_summary) %>% left_join(ns) %>%
  ungroup()  %>%
  mutate(difference = factor(grepl(x = estimator, "difference"), 
                             levels = c(F, T), 
                             labels = c("Estimates", "Differences")),
         group = factor(group, 
                        levels = c("entire_sample_1", "pid_7_1", "pid_7_2", "pid_7_3", "pid_7_4", "pid_7_5", "pid_7_6", "pid_7_7",
                      "LV_INDEX_DUMMY_0", "LV_INDEX_DUMMY_1", "EDUATT_1", "EDUATT_2", "EDUATT_3", "EDUATT_4",
                      "female_0", "female_1",
                      "income_quintile_20th - 40th Income Percentile",
                      "income_quintile_40th - 60th Income Percentile", "income_quintile_60th - 80th Income Percentile",
                      "income_quintile_Above 80th Income Percentile",  "income_quintile_Below 20th Income Percentile",
                      "race_4_White", "race_4_Black", "race_4_Hispanic", "race_4_Other Race"),
           labels = c(
             "Entire Sample",
             "Strong Democrat",
             "Not very strong Democrat",
             "Lean Democrat",
             "Independent",
             "Lean Republican",
             "Not very strong Republican",
             "Strong Republican",
             "Unlikely Voter",
             "Likely Voter",
             "Less than High School",
             "High School or Some College",
             "College",
             "Graduate School",
             "Men", "Women",
             "Below 20th Income Percentile",
             "20th - 40th Income Percentile",
             "40th - 60th Income Percentile",
             "60th - 80th Income Percentile",
             "Above 80th Income Percentile",
             "White",
             "Black",
             "Hispanic",
             "Other Race")),
         group = factor(group, levels = rev(levels(group))),
         estimator = factor(
           estimator,
           levels = c("direct_est", "list_est", "difference", 
                      "direct_est_adj", "list_est_adj", "difference_adj"),
           labels = c(
             "Direct Question Estimate",
             "List Experiment Estimate",
             "Difference",
             "Adjusted Direct Question Estimate",
             "Adjusted List Experiment Estimate",
             "Adjusted Difference")
         ))


results_df <- within(results_df,{
  subgroup <- rep(NA, nrow(results_df))
  subgroup[group %in% c("Entire Sample")] <- "Entire Sample"
  subgroup[group %in% c("Less than High School",
             "High School or Some College",
                        "College",
                        "Graduate School")] <- "Education"
  subgroup[group %in% c("Men", "Women")] <- "Gender"
  subgroup[group %in% c("Below 20th Income Percentile",
                        "20th - 40th Income Percentile",
                        "40th - 60th Income Percentile",
                        "60th - 80th Income Percentile",
                        "Above 80th Income Percentile")] <- "Income"
  subgroup[group %in% c("White",
                        "Black",
                        "Hispanic",
                        "Other Race")] <- "Race"
  subgroup[group %in% c("Strong Democrat",
                        "Not very strong Democrat",
                        "Lean Democrat",
                        "Independent",
                        "Lean Republican",
                        "Not very strong Republican",
                        "Strong Republican")] <- "Party ID"
  subgroup[group %in% c("Unlikely Voter",
                        "Likely Voter")] <- "Vote Propensity"
  subgroup[group %in% c("Men", "Women")] <- "Gender"
  subgroup <- factor(subgroup, levels = c("Entire Sample","Party ID", "Education", "Income", "Gender", "Race", "Vote Propensity"))
})


line_df <- data.frame(difference = c("Estimates", "Differences"),
                      xintercept = c(NA, 0))


g <- 
ggplot(results_df, aes(x = est, y = group, group = estimator, color = estimator, shape = estimator)) +
  geom_point(position = position_dodgev(height = .5)) +
  geom_errorbarh(aes(xmin = li, xmax = ui), position = position_dodgev(height = .5), height = 0) +
  facet_grid(subgroup~difference, scales = "free_y", space = "free_y") +
  theme_bw() +
  geom_vline(data = line_df,
             aes(xintercept = xintercept),
             linetype = "dashed") +
  guides(color = guide_legend(ncol = 2, reverse = FALSE),
         shape = guide_legend(ncol = 2, reverse = FALSE)) +
  theme(
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text.y = element_blank(),
    legend.key.width = unit(4, "lines")
  )
g


# Appendix Table ---------------------------------------------------------------

results_df <- 
  results_df %>%
  mutate(entry = paste0(format_num(est*100, 1), " ", add_parens(se*100, 1)))

results_tab <- 
results_df %>%
  select(group, estimator, entry, subgroup, n) %>%
  spread(key = estimator, value = entry) %>%
  arrange(subgroup, rev(group)) %>%
  select(-subgroup) %>%
  mutate(n = format(n, big.mark = ",")) %>%
  select(group, n, 
         `Direct Question Estimate`, `List Experiment Estimate`, Difference,
         `Adjusted Direct Question Estimate`, `Adjusted List Experiment Estimate`, `Adjusted Difference`)

results_tab %>%
xtable %>%
  print.xtable(include.colnames = FALSE, 
               include.rownames = FALSE, 
               hline.after = c(1, 8, 12, 17, 19, 23), 
               only.contents = TRUE)

# Combined Estimators --------------------------------------------------

combined_estimator <- function(df) {
  list_est_nos <-
    lm(list_Y ~ Z,
       weights = WEIGHTS,
       data = filter(df, direct_Y == 0))
  direct_est <-
    as.numeric(coef(lm(
      direct_Y ~ 1, weights = WEIGHTS, data = df
    ))[1])
  combined_est <-
    as.numeric(direct_est + (1 - direct_est) * coef(list_est_nos)[2])
  diff <-  combined_est - direct_est
  return(data.frame(
    direct_est = direct_est,
    combined_est = combined_est,
    diff = diff
  ))
}

# Combined Estimate
trump %>% combined_estimator()

# Bootstrap for uncertainty
set.seed(343)

combined_boot <-
  # This step takes a while
  bootstrap(trump, m = 2000) %>%
  do(combined_estimator(.))

sd(combined_boot$diff)
sd(combined_boot$combined_est)
sd(combined_boot$diff)

# Placebo test
placebo_fit <- lm(list_Y ~ Z, weights = WEIGHTS, data = filter(trump, direct_Y == 1))
placebo_r <- coeftest(placebo_fit, vcov. = vcovHC(placebo_fit, type = "HC2"))
est <- placebo_r[2,1]
se <- placebo_r[2,2]
p_value <- 2 * pnorm(abs((est - 1)/se),lower.tail = FALSE)
p_value
