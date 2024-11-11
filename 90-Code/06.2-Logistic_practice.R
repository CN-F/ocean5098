########################
### practice 6.2
########################
library(ggplot2)
library(gganimate)
rm(list=ls())

########################
# Logistic growth model function
logistic_growth <- function(initial_population, growth_rate, generations) {
  population <- numeric(generations)
  population[1] <- initial_population
  
  for (t in 2:generations) {
    population[t] <- population[t - 1] + (growth_rate * population[t - 1] * (100 - population[t - 1]) / 100)
  }
  
  return(population)
}

# set parameter
initial_population <- 10
generations <- 50

# create result dataframe
growth_rates <- seq(0.5, 3.0, by = 0.5)
results <- data.frame(Generation = rep(1:generations, times = length(growth_rates)),
                      Population = numeric(generations * length(growth_rates)),
                      Growth_Rate = rep(growth_rates, each = generations))

# Calculate population changes at different growth rates
for (rate in growth_rates) {
  populations <- logistic_growth(initial_population, rate, generations)
  results$Population[results$Growth_Rate == rate] <- populations
}

# plot and save result as .gif file
plot <- ggplot(results, aes(x = Generation, y = Population, color = as.factor(Growth_Rate), group = Growth_Rate)) +
  geom_line() +
  geom_hline(yintercept = 100, linetype = "dashed", color = "red") +
  labs(title = "Logistic growth model",
       x = "Generations",
       y = "Population size",
       color = "growth rate") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_discrete(name = "growth rate") +
  transition_reveal(Generation)

# Save animation as .gif
anim <- animate(plot, nframes = 100, fps = 10, width = 600, height = 400)
anim_save("logistic_growth_animation.gif", animation = anim)
