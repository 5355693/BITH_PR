library(ggplot2)
cells <- read.csv(file = "Data/Cells_surveyed.csv", header = T)
ggplot(data = cells, aes(x = mean, fill = factor(BITH))) + geom_dotplot(method="histodot",binwidth = 0.085,stackgroups = TRUE)
ggplot(data = cells, aes(x = mean, fill = BITH_cat)) + geom_histogram() + xlab("Predicted probability of Bicknell's\nThrush presence") + 
  ylab("Number of areas surveyed") + guides(fill=guide_legend(title="Bicknell's Thrush\ndetected?"))
