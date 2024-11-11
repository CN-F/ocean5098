############################################
###
############################################
library(ggplot2)
library(gganimate)
library(reshape2)
rm(list=ls())

################
### create dir
gif_dir<- "C://ocean5098//06-gif_practice"
if (dir.exists(gif_dir) == FALSE)
{
  dir.create(gif_dir)
}
################
### Define the growth function
grow <- function(start_1, start_2) {
  num_gen <- 30  # Number of generations
  N1 <- numeric(num_gen)  # Initialize N1
  N2 <- numeric(num_gen)  # Initialize N2
  generation <- 1:num_gen  # Generation labels
  growth.rate <- 1.2  # Growth rate
  K1 <- 100  # Carrying capacity for species 1
  K2 <- 120  # Carrying capacity for species 2
  a12 <- 0.8  # Competition coefficient for sqecies 1
  a21 <- 0.8  # Competition coefficient for sqecies 2
  
  N1[1] <- start_1  # Initialize population of species 1
  N2[1] <- start_2  # Initialize population of species 2
  
  for (i in 2:num_gen) {
    N1[i] <- N1[i-1] + (growth.rate * N1[i-1] * (K1 - N1[i-1] - (a12 * N2[i-1])) / K1)
    N2[i] <- N2[i-1] + (growth.rate * N2[i-1] * (K2 - N2[i-1] - (a21 * N1[i-1])) / K2)
  }
  # plot
  plot(generation, N1, type = "b", ylim = c(0, max(K1, K2)), ylab = "Population Size", xlab = "Generations", main = "Population Dynamics")
  lines(generation, N2, col = 2)  # Add species 2's curve
  legend("topright", legend = c("Species 1", "Species 2"), col = c("black", "red"), lty = 1)
}
dev.new(width = 10, height = 7)
par(mar=c(5,4,1,1),mfrow=c(5,1),las=1)
grow(1,0)
text(4,110,"Species 1 alone")
grow(0,1)
text(4,110,"Species 2 alone")
grow(1,2)
text(6,110,"Both Species competing")

############################################
### make anime
############################################
### Define the growth function
grow <- function(start_1, start_2) {
  num_gen <- 30  # Number of generations
  N1 <- numeric(num_gen)  # Initialize N1
  N2 <- numeric(num_gen)  # Initialize N2
  generation <- 1:num_gen  # Generation labels
  growth.rate <- 1.2  # Growth rate
  K1 <- 100  # Carrying capacity for species 1
  K2 <- 120  # Carrying capacity for species 2
  a12 <- 0.8  # Competition coefficient for sqecies 1
  a21 <- 0.8  # Competition coefficient for sqecies 2
  
  N1[1] <- start_1  # Initialize population of species 1
  N2[1] <- start_2  # Initialize population of species 2
  
  for (i in 2:num_gen) {
    N1[i] <- N1[i-1] + (growth.rate * N1[i-1] * (K1 - N1[i-1] - (a12 * N2[i-1])) / K1)
    N2[i] <- N2[i-1] + (growth.rate * N2[i-1] * (K2 - N2[i-1] - (a21 * N1[i-1])) / K2)
  }
  
  # return
  return(data.frame(Generation = 1:num_gen, N1 = N1, N2 = N2))
}

# Collect data for different scenarios
data_alone1 <- grow(10, 0)
data_alone2 <- grow(0, 10)
data_competing <- grow(10, 10)

# Combine data into one data frame
data_alone1$Scenario <- "S1" # Species 1 Alone
data_alone2$Scenario <- "S2" # Species 2 Alone
data_competing$Scenario <- "Both" # Both Species Competing

# Combine all scenarios into one data frame
combined_data <- rbind(data_alone1, data_alone2, data_competing)

# Reshape for ggplot
data_long <- melt(combined_data, id.vars = c("Generation", "Scenario"), 
                  variable.name = "Species", 
                  value.name = "Population")

# Create the animation
p <- ggplot(data_long, aes(x = Generation, y = Population, color = Species)) +
  geom_line(size = 1.2) +
  labs(title = 'Population Dynamics: {frame_time}', 
       x = 'Generations', 
       y = 'Population Size') +
  transition_time(Generation) +
  ease_aes('linear') +
  facet_wrap(~ Scenario) +  # Create separate plots for each scenario
  theme_minimal() + 
  scale_color_manual(values = c("blue", "red"), labels = c("Species 1", "Species 2"))

# Save the animation as a GIF
anim <- animate(p, nframes = 30, fps = 10, width = 800, height = 600)
anim_save(file.path(gif_dir,"population_dynamics_animation.gif"), animation = anim)
