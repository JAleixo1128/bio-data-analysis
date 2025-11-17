# Natural Selection vs Genetic Drift in Snail Populations

library(ggplot2)
library(gsheet)
library(dplyr)

# Load data
url <- "https://docs.google.com/spreadsheets/d/1QhfezUHodqtNOnJGOnqA62mSfhJUtIItoJQUUYpuCYE/edit?gid=0#gid=0"
snail_data <- gsheet2tbl(url)

# Fix types
snail_data <- snail_data %>%
  mutate(
    exp = as.integer(exp),
    generation = as.integer(generation),
    snailcolor = as.factor(snailcolor),
    snails = as.numeric(snails)
  )

# Summary for plotting
snail_means <- snail_data %>%
  group_by(exp, generation, snailcolor) %>%
  summarise(mean = mean(snails), sd = sd(snails), .groups = "drop")

# Experiment 1 (selection)
exp1 <- filter(snail_means, exp == 1)
ggplot(exp1, aes(generation, mean, color = snailcolor)) +
  geom_point() + geom_line() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .1) +
  labs(title = "Experiment 1: Selection", x = "Generation", y = "Mean Snails")

# Experiment 2 (drift)
exp2 <- filter(snail_means, exp == 2)
ggplot(exp2, aes(generation, mean, color = snailcolor)) +
  geom_point() + geom_line() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .1) +
  labs(title = "Experiment 2: Drift", x = "Generation", y = "Mean Snails")

# t-tests for generation 3
red1 <- filter(snail_data, exp == 1, generation == 3, snailcolor == "red")
white1 <- filter(snail_data, exp == 1, generation == 3, snailcolor == "white")

red2 <- filter(snail_data, exp == 2, generation == 3, snailcolor == "red")
white2 <- filter(snail_data, exp == 2, generation == 3, snailcolor == "white")

t.test(red1$snails, white1$snails)
t.test(red2$snails, white2$snails)

