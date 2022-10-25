source("code/dendrogram new.R")
source("code/distances boxplot.R")

cairo_pdf("plots/Figure2_revision.pdf", height = 7.85, width = 6.7)
ggarrange(p_distances + theme(legend.position = "none"),
          p_cluster,
          common.legend = FALSE,
          nrow = 2, heights  = c(2,5), labels = "AUTO", font.label = label_par)


dev.off()

