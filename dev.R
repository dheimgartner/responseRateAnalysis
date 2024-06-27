devtools::load_all()

library(tidyverse)

rr <- responseRateAnalysis::response_rates

rr %>%
  select(sample_size, n_invited) %>%
  mutate(same = sample_size == n_invited)

dat <- function(dat = response_rates) {
  dat %>%
    pivot_longer(matches("flag")) %>%
    filter(value == 1) %>%
    rename(key = name) %>%
    select(-value)
}

dat() %>%
  ggplot(aes(x = response_burden_score, y = response_rate, col = key)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  scale_color_brewer(type = "qual", palette = 2)


dat() %>%
  # filter(survey_id != 39) %>%
  ggplot(aes(x = response_burden_score, y = response_rate, col = key)) +
  geom_point() +
  geom_point(aes(size = ifelse(survey_id == 40, 3, 1)), show.legend = FALSE) +
  geom_smooth(formula = y ~ x, method = "lm") +
  Heimisc::my_theme() +
  Heimisc::add_grid() +
  scale_color_brewer(type = "qual", palette = 2) +
  labs(x = "Response burden score", y = "Response rate",
       col = " ")



data <- default_data()
data$weight <- 1

## estimate -> estimate_flag, clustered_standard_errors


test <- seq(0, 10, 0.01)
plot(transform(test))
plot(backtransform(transform(test)))

fit <- estimate()
model.frame(fit) %>% view()


## pred -> pred_cluster_se
debugonce(pred_cluster_se)

fit <- estimate(flag = "no_yes")
vc <- vcov(fit)
pred_cluster_se(fit, vc)


