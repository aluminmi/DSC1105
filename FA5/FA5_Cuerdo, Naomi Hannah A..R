library(ggplot2)
library(tidyverse)

# Load and Explore the Data
storeSales <- read.csv("C:/Users/naomi/Downloads/store_sales_data.csv")

# Check structure and summary
glimpse(storeSales)
summary(storeSales)
head(storeSales)

# Distribution of sales_count
ggplot(storeSales, aes(x = sales_count)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black") +
  labs(title = "Distribution of Sales Count", x = "Sales Count", y = "Frequency")

# Frequency of each store_size
ggplot(storeSales, aes(x = store_size)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Frequency of Store Size", x = "Store Size", y = "Count")

# Proportion of days with promo
storeSales %>%
  count(promo) %>%
  mutate(proportion = n / sum(n))

# Proportion of days with holiday
storeSales %>%
  count(holiday) %>%
  mutate(proportion = n / sum(n))

# Fit a Poisson Regression Model
poisson_model <- glm(
  sales_count ~ day_of_week + promo + holiday + store_size,
  family = poisson(link = "log"),
  data = storeSales
)

summary(poisson_model)

# Interpret the effect of promotion on sales
exp(coef(poisson_model)["promo"])

# Interpret how store size affects expected sales
exp(coef(poisson_model)[grep("store_size", names(coef(poisson_model)))])

# Assess Model Fit: Checking for overdispersion
deviance(poisson_model) / df.residual(poisson_model)

# Make Predictions
new_data <- tibble(
  day_of_week = c(1, 7),
  promo = c(1, 0),
  holiday = c(0, 1),
  store_size = c("medium", "large")
)

predicted_sales <- predict(poisson_model, newdata = new_data, type = "response")
predicted_sales

