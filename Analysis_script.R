library(ggplot2)
cells <- read.csv(file = "Data/Cells_surveyed.csv", header = T)
ggplot(data = cells, aes(x = mean, fill = factor(BITH))) + geom_dotplot(method="histodot",binwidth = 0.085,stackgroups = TRUE)
png(file = "/Users/johnlloyd/Dropbox/VCE/BITH Puerto Rico/Figure3.png")
ggplot(data = cells, aes(x = mean, fill = BITH_cat)) + geom_histogram() + xlab("Predicted probability of Bicknell's\nThrush presence in survey block") + 
  ylab("Number of blocks") + guides(fill=guide_legend(title="Bicknell's Thrush\ndetected?"))
dev.off()
summary(cells)
