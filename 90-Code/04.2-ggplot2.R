#########################
### ggplot practice
#########################
library(ggplot2)
library(dplyr)
library(Biobase)
rm(list=ls())

#########################
### read rairuoho data
rairuoho_path<- "C:\\ocean5098\\02.3-Rairuoho\\rairuoho.csv"
rairuoho_data<- read.csv(rairuoho_path, header = T)
head(rairuoho_data,5)
colnames(rairuoho_data)

#########################
### pdf  Increased size for better readability
plot_dir<- "C:\\ocean5098\\04-plot_practice"
pdf_path<- paste0(plot_dir, "\\ggplot.pdf")
pdf_path
pdf(pdf_path, width = 5, height = 4) #Increased size

#########################
### Grass Growth Over Time with Error Bars
summarized_data <- rairuoho_data %>%
  group_by(day, treatment) %>%
  summarize(mean_length = mean(length), sd_length = sd(length))

ggplot(summarized_data, aes(x = day, y = mean_length, color = treatment, group = treatment)) +
  scale_color_manual(values = c("#45e0ab", "#ffbc00")) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_length - sd_length, ymax = mean_length + sd_length), width = 0.2) +
  labs(title = "Grass Growth Over Time with Error Bars", x = "Day", y = "Mean Length")

#########################
### Average Grass Growth Under Different Treatments
summarized_data <- rairuoho_data %>%
  group_by(day, treatment) %>%
  summarize(AverageGrowth = mean(length))

ggplot(summarized_data, aes(x = day, y = AverageGrowth, fill = treatment)) +
  scale_fill_manual(values = c("#45e0ab", "#ffbc00")) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Grass Growth Under Different Treatments",
    x = "Day",
    y = "Average Grass Growth",
    fill = "Treatment"
  ) +
  theme_minimal() + 
  theme(
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5, size = 12), 
    axis.title = element_text(size = 10), 
    axis.text = element_text(size = 8), 
    legend.text = element_text(size = 8), 
    legend.position = c(0.05, 0.95), #Near top-left corner
    legend.justification = c("left", "top"), # Align to top-left
    legend.key.size = unit(0.5, "cm"), #Reduced key size
    legend.box = "vertical" #Vertical legend orientation.  Consider "horizontal" if space is limited.
  )


#########################
###　Grass Growth Under Different Treatments
ggplot(rairuoho_data, aes(x = day, y = length, color = treatment)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("#45e0ab", "#ffbc00")) + 
  labs(
    title = "Grass Growth Under Different Treatments",
    x = "Day",
    y = "Grass Growth",
    color = "Treatment"
  ) +
  theme_minimal() + 
  theme(
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5, size = 12), 
    axis.title = element_text(size = 10), 
    axis.text = element_text(size = 8), 
    legend.text = element_text(size = 8), 
    legend.position = c(0.05, 0.95), #Near top-left corner
    legend.justification = c("left", "top"), # Align to top-left
    legend.key.size = unit(0.5, "cm"), #Reduced key size
    legend.box = "vertical" #Vertical legend orientation. Consider "horizontal" if space is limited.
  )


#########################
###　Total Grass Growth Under Different Treatments
summarized_data <- rairuoho_data %>%
  group_by(treatment) %>%
  summarize(TotalGrowth = sum(length))

ggplot(summarized_data, aes(x = treatment, y = TotalGrowth, fill = treatment)) +
  geom_bar(stat = "identity", width = 0.2) +
  scale_fill_manual(values = c("#45e0ab", "#ffbc00")) + 
  labs(
    title = "Total Grass Growth Under Different Treatments",
    x = "Treatment",
    y = "Total Grass Growth",
    fill = "Treatment") +
  
  theme_minimal() + 
  theme(
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5, size = 12), 
    axis.title = element_text(size = 10), 
    axis.text = element_text(size = 8), 
    legend.text = element_text(size = 8), 
    legend.position = c(0.98, 0.95), #Near top-left corner
    legend.justification = c("right", "top"), # Align to top-left
    legend.key.size = unit(0.5, "cm"), #Reduced key size
    legend.box = "vertical" #Vertical legend orientation. Consider "horizontal" if space is limited.
  ) +
  ylim(0, 10000)

dev.off()
openPDF(pdf_path)



