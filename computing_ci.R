devtools::load_all()

rm(list = ls())

data <-
  default_data() %>%
  filter(no_yes == 1)

data

fit <- lm(y ~ x, data = data)
summary(fit)

ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)


new_x <- data.frame(x = seq(0, 5, by = 0.01))
p <- predict(fit, newdata = new_x, interval = "confidence")

df <- as.data.frame(p)
df$x <- new_x$x

ggplot(df, aes(x = x)) +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2)


predict.lm

debugonce(predict.lm)
predict(fit, newdata = new_x, interval = "confidence")











# Install and load necessary packages
library(clubSandwich)

# Sample data
set.seed(123)
your_data <- data.frame(
  x = rnorm(100),
  y = rnorm(100),
  cluster = rep(1:10, each = 10)
)

# Fit the linear model
fit <- lm(y ~ x, data = your_data)

# Define new data for prediction
new_data <- data.frame(x = seq(min(your_data$x), max(your_data$x), length.out = 100))

# Predict the fitted values
predictions <- predict(fit, newdata = new_data)

# Get the design matrix for the new data
X_new <- model.matrix(~ x, new_data)

# Compute clustered standard errors
cluster_var <- your_data$cluster
vcov_cluster <- vcovCR(fit, cluster = cluster_var, type = "CR2")

# Calculate the standard errors for the predictions
se_fit <- sqrt(diag(X_new %*% vcov_cluster %*% t(X_new)))

# Critical value for the 95% confidence interval
critical_value <- qt(0.975, df = fit$df.residual)

# Compute confidence intervals
new_data$fit <- predictions
new_data$lwr <- predictions - critical_value * se_fit
new_data$upr <- predictions + critical_value * se_fit

# Plot the data and the linear model with confidence intervals
ggplot(your_data, aes(x = x)) +
  geom_point(aes(y = y)) +
  geom_line(data = new_data, aes(x = x, y = fit), color = "blue") +
  geom_ribbon(data = new_data, aes(x = x, ymin = lwr, ymax = upr), alpha = 0.2) +
  labs(title = "Linear Regression with Clustered Standard Errors",
       x = "X", y = "Y")










## apply to my data
data <-
  default_data() %>%
  filter(no_yes == 1)

data
fit <- lm(y ~ x, data = data)
new_data <- data
predictions <- predict(fit, newdata = new_data)
X_new <- model.matrix(~ x, new_data)

cluster_var <- data$survey_id
vcov_cluster <- clubSandwich::vcovCR(fit, cluster = cluster_var, type = "CR2")


se_fit <- sqrt(diag(X_new %*% vcov_cluster %*% t(X_new)))


critical_value <- qt(0.975, df = fit$df.residual)

new_data$fit <- predictions
new_data$lwr <- predictions - critical_value * se_fit
new_data$upr <- predictions + critical_value * se_fit

ggplot(data, aes(x = x)) +
  geom_point(aes(y = y)) +
  geom_line(data = new_data, aes(x = x, y = fit), color = "blue") +
  geom_ribbon(data = new_data, aes(x = x, ymin = lwr, ymax = upr), alpha = 0.2) +
  labs(title = "Linear Regression with Clustered Standard Errors",
       x = "X", y = "Y")


## implement in function modeling2.R
devtools::load_all()
library(tidyverse)

data <-
  default_data() %>%
  filter(no_yes == 1)

fit <- estimate(y ~ x, data, weights = NULL)
cluster <- data$survey_id
vcov_ <- vcov(fit, cluster, "CR2")
coef_clustered <- coef_clustered(fit, vcov_)



fit <- add_clustered(fit, cluster, "CR2")
summary(fit)






new_data <- model.matrix(fit)
cluster <- data$survey_id
predict.blobb(fit, new_data, cluster = cluster, type = "CR2")






## clean pipeline
data <- default_data()
fit <- lm(y ~ x + yes_yes + yes_no + no_no, data = data, weights = weight)
fit_clustered <- add_clustered(fit, cluster = data$survey_id, type = "CR2")
summary(fit_clustered)

p <- predict(fit_clustered, newdata = data, cluster = data$survey_id, type = "CR2")



























