---
title: "Effectiveness of Exercise Analysis"
author: "Your Name"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Load Necessary Libraries
```{r}
library(dplyr)
library(ggplot2)
library(caret)
library(corrplot)
library(boot)
library(infer)
```

## Load the Dataset
```{r}
data <- read.csv(file = "C:/Users/ADMIN/Downloads/bodyPerformance.csv")

# Inspect the data
str(data)
summary(data)
```

## Data Cleaning
### Convert `class` to a Factor
```{r}
data$class <- as.factor(data$class)
```

### Check and Handle Missing Values
```{r}
missing_values <- colSums(is.na(data))
print(missing_values)

data <- na.omit(data)
```

### Detect and Handle Outliers
```{r}
numeric_vars <- data %>% select_if(is.numeric)
for (col in names(numeric_vars)) {
  q1 <- quantile(data[[col]], 0.25)
  q3 <- quantile(data[[col]], 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  data <- data %>% filter(data[[col]] >= lower_bound & data[[col]] <= upper_bound)
}
```

### Standardize Numeric Variables
```{r}
numeric_vars <- data %>% select_if(is.numeric)
data[names(numeric_vars)] <- scale(numeric_vars)
```

### Remove Duplicate Rows
```{r}
data <- data[!duplicated(data), ]
```

### Validate Cleaned Data
```{r}
str(data)
summary(data)
```

## Correlation Analysis
```{r}
cor_matrix <- cor(data %>% select_if(is.numeric))
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8, tl.col = "black")
```

## Distribution of the `class` Variable
```{r}
ggplot(data, aes(x = class, fill = class)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Distribution of Classes", x = "Class", y = "Count")
```

## Bootstrap Analysis
### Define Bootstrap Function
```{r}
boot_mean <- function(data, indices) {
  d <- data[indices, ]
  return(mean(d$gripForce))
}
```

### Perform Bootstrap
```{r}
set.seed(123)
boot_results <- boot(data = data, statistic = boot_mean, R = 1000)
print(boot_results)
plot(boot_results)
```

## A/B Testing
### Create Groups Based on Median Age
```{r}
data$group <- ifelse(data$age <= median(data$age), "A", "B")

conversion_rates <- data %>%
  group_by(group) %>%
  summarize(conversion_rate = mean(class == "A"))
print(conversion_rates)
```

### Hypothesis Testing
```{r}
ab_test <- data %>%
  specify(response = class, explanatory = group) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in props", order = c("A", "B"))

observed_diff <- data %>%
  specify(response = class, explanatory = group) %>%
  calculate(stat = "diff in props", order = c("A", "B"))

ab_test %>%
  visualize() +
  shade_p_value(obs_stat = observed_diff$stat, direction = "two-sided")
```

## Predictive Modeling
### Train-Test Split
```{r}
set.seed(123)
trainIndex <- createDataPartition(data$class, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]
```

### Logistic Regression Model
```{r}
log_model <- glm(class ~ age + height_cm + weight_kg + body.fat_. + diastolic + systolic + gripForce + sit.and.bend.forward_cm + sit.ups.counts + broad.jump_cm,
                 data = trainData, family = "binomial")
summary(log_model)
```

### Predictions and Evaluation
```{r}
predictions <- predict(log_model, newdata = testData, type = "response")
testData$predicted_class <- ifelse(predictions > 0.5, "A", "B")

confusionMatrix(as.factor(testData$predicted_class), testData$class)
```
