###################
### library
library(readxl)
library(dplyr)
library(ggplot2)
library(leaflet)
library(factoextra)
library(lme4)
library(MASS)
library(car)
library(lmerTest)
library(survival)
rm(list=ls())

###################
### read dataset
xlsx_file<- "Ecological adaptation drives wood frog population divergence in life history traits.xlsx"
frog_path<- paste0("C:\\Users\\user\\Desktop\\frogs\\", xlsx_file)
frogs<- as.data.frame(read_xlsx(frog_path))
colnames(frogs)<- gsub(" ", ".", colnames(frogs))
colnames(frogs)<- gsub("\\(|\\)", "", colnames(frogs))
head(frogs)
# Extract unique population names
population <- unique(frogs$Population.Name)
population
# Define color list for populations
col_list<- c("red","#00FF00", "blue", "cyan", "magenta", "yellow2", "orange", 
             "purple", "brown", "pink", "gray", "#008080", "black")
# Assign colors to each population
population_no<- 1
for (population_no in 1:length(population))
{
  frogs[frogs$Population.Name == population[population_no],"col"]<- col_list[population_no]
}

###################
### mapping
leaflet(frogs) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(
    lng = ~longitude,
    lat = ~latitude,
    radius = 500,
    color = frogs$col,       
    fillOpacity = 0.5,
    stroke = TRUE,
    weight = 10,
    popup = ~paste(Frog.ID)
  ) %>%
  addScaleBar(position = "bottomright") %>%
  addMeasure() %>%
  addControl("<strong>Species Distribution Map</strong>", position = "topright", 
             className = "leaflet-control-custom")%>%
  addLegend(
    position = "bottomleft",
    colors = col_list,
    labels = population,
    title = "Population Groups",
    opacity = 0.7)

###################
# Analyze factors influencing hatching date
# Convert Hatching.Date to Date type
frogs$Hatching.Date <- as.Date(frogs$Hatching.Date, format = "%m/%d/%y")

# Extract month as a proxy for hatching time
frogs$hatch_month <- as.numeric(format(frogs$Hatching.Date, "%m"))

# set custom_theme
custom_theme <- theme(
  panel.background = element_blank(),  # Remove panel background
  axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
  legend.position = "right",           # Position the legend on the right
  panel.border = element_rect(color = "black", fill = NA, size = 0.5)  # Keep the border
)

# Plot hatching date vs latitude
ggplot(frogs, aes(x = latitude, y = hatch_month,color = Population.Name)) +
  geom_point(shape = 16, size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = col_list) +
  labs(title = "Hatching Month vs Latitude", x = "Latitude", y = "Hatching Month")+
  custom_theme

# Perform linear regression analysis
model_hatch_lat <- lm(hatch_month ~ latitude, data = frogs)
summary(model_hatch_lat)

# Plot hatching date vs longitude
ggplot(frogs, aes(x = longitude, y = hatch_month,color = Population.Name)) +
  geom_point(shape = 16, size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = col_list) +
  labs(title = "Hatching Month vs Longitude", x = "Longitude", y = "Hatching Month")+
  custom_theme

# Perform linear regression analysis
model_hatch_lon <- lm(hatch_month ~ longitude, data = frogs)
summary(model_hatch_lon)

# Plot hatching date vs ENM score
ggplot(frogs, aes(x = ENM.score, y = hatch_month,color = Population.Name)) +
  geom_point(shape = 16, size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = col_list) +
  labs(title = "Hatching Month vs ENM Score", x = "ENM Score", y = "Hatching Month")+
  custom_theme

# Perform linear regression analysis
model_hatch_enm <- lm(hatch_month ~ ENM.score, data = frogs)
summary(model_hatch_enm)

###################
# Compare morphological characteristics across different populations
# Use MANOVA to compare morphological features across populations
manova_result <- manova(cbind(SVL.mm, Femur.length.mm, Weight.g) ~ Population.Name, data = frogs)
summary(manova_result, test = "Pillai")

# If MANOVA shows significant differences, perform ANOVA for individual variables
anova_SVL.mm <- aov(SVL.mm ~ Population.Name, data = frogs)
summary(anova_SVL.mm)

anova_femur <- aov(Femur.length.mm ~ Population.Name, data = frogs)
summary(anova_femur)

anova_Weight.g <- aov(Weight.g ~ Population.Name, data = frogs)
summary(anova_Weight.g)

# Visualize morphological differences across populations
ggplot(frogs, aes(x = Population.Name, y = SVL.mm, color = Population.Name)) +
  geom_boxplot() +
  labs(title = "snout-vent length(SVL) by Population", x = "Population", y = "SVL (mm)")+
  scale_color_manual(values = col_list) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  custom_theme

ggplot(frogs, aes(x = Population.Name, y = Femur.length.mm, color = Population.Name)) +
  geom_boxplot() +
  labs(title = "Femur Length by Population", x = "Population", y = "Femur Length (mm)")+
  scale_color_manual(values = col_list) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  custom_theme

ggplot(frogs, aes(x = Population.Name, y = Weight.g, color = Population.Name)) +
  geom_boxplot() +
  labs(title = "Weight by Population", x = "Population", y = "Weight (g)")+
  scale_color_manual(values = col_list) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  custom_theme

###################
# Do different populations exhibit different survival strategies?
# Calculate development rate (SVL growth per day)
if (!"development_rate" %in% colnames(frogs)) {
  frogs$development_rate <- with(frogs, SVL.mm / Larval.period.days)
}

# Select columns for PCA
pca_data <- frogs %>%
  dplyr::select(Population.Name, development_rate, Weight.g, SVL.mm, Femur.length.mm, Larval.period.days)

# Remove rows with missing values
pca_data_clean <- na.omit(pca_data)

# Ensure "Population.Name" is a factor
pca_data_clean$Population.Name <- as.factor(pca_data_clean$Population.Name)

# Perform PCA, excluding non-numeric columns (e.g., Population.Name)
pca_result <- prcomp(pca_data_clean %>% dplyr::select(-Population.Name), scale. = TRUE)

# Extract scores and loadings
pca_scores <- as.data.frame(pca_result$x)
pca_scores$Population.Name <- pca_data_clean$Population.Name
pca_loadings <- as.data.frame(pca_result$rotation)

# Calculate the mean score for each population to reduce clutter
population_means <- pca_scores %>%
  group_by(Population.Name) %>%
  summarise(across(starts_with("PC"), mean))

# Plot PCA biplot with simplified points and selected variables
fviz_pca_biplot(pca_result, 
                col.ind = "gray",  # Individuals in gray (less prominent)
                col.var = "black", # Variables in black
                palette = "jco",   # Color palette for populations
                addEllipses = FALSE, # No ellipses to reduce clutter
                label = "var",     # Label variables
                repel = TRUE,      # Avoid overlapping labels
                geom = "point",    # Only show points for individuals
                pointsize = 2,     # Smaller points for individuals
                alpha = 0.5,       # Transparency of individual points
                title = "PCA Biplot of Survival Strategies by Population",
                xlab = paste0("PC1 (", round(summary(pca_result)$importance[2, 1] * 100, 1), "%)"),
                ylab = paste0("PC2 (", round(summary(pca_result)$importance[2, 2] * 100, 1), "%)")
) +
  # Add population means as colored points
  geom_point(data = population_means, aes(x = PC1, y = PC2, color = Population.Name), size = 4, alpha = 1) +
  theme_minimal() +
  scale_color_manual(values = col_list) +
  theme(legend.position = "right")+
  custom_theme

###################
# Does temperature and larval density jointly affect tadpole mortality?
# Create a binary variable indicating whether the individual died
frogs$dead <- ifelse(frogs$Mortality == 1, 1, 0)

# Use GLM to analyze the effect of temperature and density on mortality
glm_death <- glm(dead ~ larval.period.temperature.C + larval.density.treatment, data = frogs, family = binomial)

# Check model summary
summary(glm_death)

# Plot temperature vs mortality
ggplot(frogs, aes(x = larval.period.temperature.C, y = dead)) +
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = T) +
  labs(title = "Temperature vs Mortality", x = "Temperature (°C)", y = "Mortality (1 = Dead)")+
  custom_theme

# Plot density treatment vs mortality
ggplot(frogs, aes(x = larval.density.treatment, y = dead)) +
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = T) +
  labs(title = "Density Treatment vs Mortality", x = "Density Treatment", y = "Mortality (1 = Dead)")+
  custom_theme

# Plot interaction between temperature and density on mortality
ggplot(frogs, aes(x = larval.period.temperature.C, y = dead, color = larval.density.treatment)) +
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = T) +
  labs(title = "Temperature and Density Interaction on Mortality", x = "Temperature (°C)", y = "Mortality (1 = Dead)") +
  facet_wrap(~ larval.density.treatment)+
  custom_theme
