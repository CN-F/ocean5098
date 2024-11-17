#############################
### function
#############################
library(dplyr)
library(tidyr)
library(ggplot2)
rm(list=ls())

#############################
# create function
#getAnywhere("cor.test.default")
cor_func<- function(dataset, day_start, day_end, treatment)
{
  #############################
  # set vector of test data
  treatment_start<- dataset[dataset$treatment == treatment, day_start]
  treatment_end<- dataset[dataset$treatment == treatment, day_end]
  
  #############################
  # correlation (r value)
  r<- cor(treatment_start, treatment_end)
  #############################
  # t value
  n <- length(treatment_start)  # 假設樣本數
  t_value <- r * sqrt(n - 2) / sqrt(1 - r^2)
  #############################
  # p value
  degree_of_freedom<- n - 2
  p_value <- 2 * (1 - pt(abs(t_value), df = degree_of_freedom))
  print(p_value)
  
  cat("data: gress length of ", day_start," and ",day_end, "\n",
      "treatment: ", treatment, "\n",
      "t = ", t_value, "\n",
      "p value = ", p_value, "\n",
      "correlation: ", r)
  
  return(data.frame(day_start = day_start, day_end = day_end, 
                    treatment = treatment, correlation = r, 
                    t = t_value, p_value = p_value))
}

#############################
# dataset
rairuoho <- read.table("C:\\ocean5098\\02.3-Rairuoho\\rairuoho_data.txt", header = TRUE)
head(rairuoho,5)

#############################
# 
rairuoho_long <- pivot_longer(rairuoho, cols = c(day3, day4), names_to = "day", values_to = "length")
ggplot(rairuoho_long, aes(x = day, y = length, fill = treatment)) +
  geom_boxplot() +
  labs(title = "Comparison of grass length on day 3 and day 8 between different treatments",
       x = "day",
       y = "length")

rairuoho_long <- pivot_longer(rairuoho, cols = c(day3, day8), names_to = "day", values_to = "length")
ggplot(rairuoho_long, aes(x = day, y = length, fill = treatment)) +
  geom_boxplot() +
  labs(title = "Comparison of grass length on day 3 and day 8 between different treatments",
       x = "day",
       y = "length")


#############################
### test
treatments <- c("water", "nutrient")
day_pairs <- list(c("day3", "day4"), c("day3", "day8"))
results <- data.frame()
for(treatment in treatments)
{
  for(day_pair in day_pairs)
  {
    result<- cor_func(rairuoho, day_pair[1], day_pair[2], treatment)
    results <- rbind(results, result)
  }
}
results
