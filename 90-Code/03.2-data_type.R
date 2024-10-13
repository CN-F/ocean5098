# Create the initial data frame
start_experiment <-  c(4, 5, 3, 6, 7)
end_experiment <-  c(9, 6, 5, 4, 7)
plant_data <- data.frame(start_experiment, end_experiment)
row.names(plant_data) <-  c("plant 1", "plant 2", "plant 3", "plant 4", "plant 5")
plant_data

# Reshape the data frame
library(tidyr)
long_plant_data <- gather(plant_data, key = "time", value = "height", start_experiment:end_experiment)
long_plant_data$time <- factor(long_plant_data$time, levels = c("start_experiment", "end_experiment"))
long_plant_data

# Create the list my.experiment with the reshaped data frame as the first element
my.experiment <- list()

# Add the data frame to my.experiment
my.experiment[[1]] <- long_plant_data

# Create the growth list
### Character vector of row names
row_names <- rownames(plant_data)

### Numeric vector of percentage growth
initial_heights <- plant_data$start_experiment
final_heights <- plant_data$end_experiment
percentage_growth <- ((final_heights - initial_heights) / initial_heights) * 100

### Data frame with plants and their growth
growth_table <- data.frame(
  plants = row_names,
  growth = percentage_growth
)

### Add the growth list as the second element of my.experiment
growth_list <- list(row_names, percentage_growth, growth_table)
my.experiment[[2]] <- growth_list

# Add a message as the third element
my.experiment[[3]] <- "The growth of the plants varies; some increased, while others decreased."

### Print the my.experiment list to verify
print(my.experiment)
