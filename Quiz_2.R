# Quiz_2
# Mpumalanga Mnyekemfu
# 22 April 2021

# Question 1

ls("package:datasets")

library(tidyverse)
library(plotly)


# Orange dataset 
Orange <- datasets::Orange
# H0: age is not different from circumference
# H1: age is different from circumference

# before t.test, confirm assumptions

# prepare the data
# Calculate mean, sd
mean()  

r_dat <- data.frame(Orange = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("age", 1000), rep("circumference", 1000)))

# plot
ggplot(data = Orange, aes(x = age, fill = sample)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = circumference), colour = NA, alpha = 0.4) +
  labs(x = "Tree")

shapiro.test(Orange)



# Toothgrowth data
ToothGrowth <- datasets::ToothGrowth

# prepare the data
ToothGrowth_ <- data.frame(ToothGrowth = c(rnorm(n = 1000, mean = 10, sd = 3),
                               rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("age", 1000), rep("circumference", 1000)))
# plot


# Question 2

# Separate date column
SACTN_daily_v4.2 %>% separate(date, c("Year","Month","Day"), sep = "-")

#
