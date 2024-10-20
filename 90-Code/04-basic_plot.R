###########################
### plot practice
###########################
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
iris
pairs (iris[1:4])
###########################
###
pdf_path<- paste0(plot_dir, "iris_scatterplot_matrix.pdf")
pdf(pdf_path)
pairs(iris[1:4], main = "Iris Scatterplot Matrix")
dev.off()

###########################
###