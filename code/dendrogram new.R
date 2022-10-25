library(dplyr)
library(ggplot2)
library(ggraph)

metadata <- readRDS("data/2021-08-04 metadata.RDS")
# seqtab.clr <- readRDS("results/seqtab.clr_genus.RDS")
# seqtab.clr <- readRDS("results/seqtab.clr.RDS")

# seqtab.final <- readRDS("data/ASV_raw_counts.RDS")

# seqtab.clr <- seqtab.clr[, colMeans(seqtab.clr > 0) > 0.1]

seqtab.clr <- readRDS("results/seqtab.clr.RDS")
# seqtab.clr <- seqtab.clr[grepl("V", rownames(seqtab.clr)), ]
seqtab.clr <- seqtab.clr[, colMeans(seqtab.clr > 0) >  0.05]


# cols <- gg_color_hue(10)  

# cols <- ggsci::pal_jco()(10)


dist.clr <- dist(seqtab.clr)

min(dist.clr)

hc <- hclust(dist.clr, method="ward.D2")


library(dendextend)
# 
gr <- as.dendrogram(hc)
# 
# 
matching_labels <- substr(get_leaves_attr(gr, "label"), 1,5)
matching_labels[grepl("V", matching_labels)] <- substr(matching_labels[grepl("V", matching_labels)], 1,4)
# 
# 
# Sex <- metadata$Geschlecht[match(matching_labels, metadata$LNR)]
# 
# gr <- assign_values_to_leaves_nodePar(gr, Sex, "Sex")
gr <- assign_values_to_leaves_nodePar(gr, matching_labels, "subject")




# ragg::agg_png("plots/presentation/samples_cluster.png",
#               res=300, width=3400, height=3000)


p_cluster <- ggraph(gr, circular = TRUE, layout = "auto") +
  
  
  # geom_node_point(aes(col = case_when(grepl("FIT", label) ~ "FIT",
  # grepl("Nor", label) ~ "Norgen",
  # TRUE ~ ""))) +
  # geom_node_point(aes(col = Sex)) +
  # geom_node_point(aes(col = subject)) +
  
  
  
  geom_edge_elbow(edge_width = 0.25) + 
  # geom_path(data = function(x) x[x$leaf, ], aes(x, y, group = subject),
  #           col = "black", lwd = 3, alpha = 0.75, lineend = "round") +    
  geom_path(data = function(x) x[x$leaf, ], aes(x, y, group = subject, col = grepl("V", label)),
            lwd = 2, alpha = 1, lineend = "round") +    
  # geom_node_point(aes(filter = !leaf), size = 1) +
  # geom_node_point(aes(filter = leaf), size = 1.5, shape = 21, col = "black", fill = "white") +
  geom_node_point(aes(filter = leaf,
                      fill = case_when(grepl("FIT", label) ~ "FIT",
                                       grepl("Nor", label) ~ "Norgen")),
                  size = 1, shape = 21, col = "black") +
  # geom_node_text(aes(label = subject)) +
  theme_void(base_size = 8) + 
  # xlab("samples") +
  # ylab("Aitchison distance") +
  coord_fixed() +
  # scale_color_manual(values = cols[c(10, 5)]) +
  # scale_fill_manual(values = cols[c(2, 4)]) +
  # 
  scale_color_manual("Subject", values = cols[c(3, 4)], labels = c("Patient", "Volunteer")) +
  scale_fill_manual("Sample type", values = cols[c(1, 2)]) +
  
  scale_x_continuous(expand = c(0.01,0.01)) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(panel.grid.major.y =  element_blank()) +
  theme(panel.grid.minor.y =  element_blank()) +
  theme(legend.position = c(0, 0.9)) +
  theme(legend.title = element_text(face = "bold")) +
  theme(legend.key.size = unit(1, "mm")) +
  # geom_hline(yintercept =  64) +
  # geom_hline(yintercept = min(dist.clr)) +
  # geom_hline(yintercept = max(dist.clr[dist.clr < 60])) +
  # geom_hline(yintercept = min(dist.clr[dist.clr > 60])) +
  NULL

p_cluster

  
