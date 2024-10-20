###########################
### plot practice
###########################
library(Biobase)
rm(list=ls())
###########################
###
ocean_dir<- "C:\\ocean5098"
plot_dir<- paste0(ocean_dir,"\\04-plot_practice")
if (dir.exists(plot_dir) == FALSE)
{
  dir.create(plot_dir)
}

###########################
###
x<- iris$Petal.Length
y<- iris$Petal.Width

###########################
### pdf
pdf_path<- paste0(plot_dir, "\\basic_plot.pdf")
pdf(pdf_path)

plot(x,y,
     col = c("black", "red", "green")[iris$Species], 
     pch = c(1, 2, 3)[iris$Species],
     xlab = "Petal length (cm)", 
     ylab = "Petal width (cm)",
     main = "Petal width vs. length")

abline(lm(iris$Petal.Width ~ iris$Petal.Length), col = "black")
text(5, 0.5, "R= 0.96")
legend("topleft", legend = levels(iris$Species),
       col = c("black", "red", "green"), 
       pch = c(1, 2, 3))

dev.off()
openPDF(pdf_path)
