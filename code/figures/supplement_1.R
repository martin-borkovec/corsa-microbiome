source("code/load_all.R")

p1_0 <-
  rbind(make_scatterplot("Species", plot = FALSE),
        make_scatterplot("Genus", plot = FALSE),
        make_scatterplot("Family", plot = FALSE),
        make_scatterplot("Order", plot = FALSE),
        make_scatterplot("Class", plot = FALSE),
        make_scatterplot("Phylum", plot = FALSE)) %>% 
  mutate(rank = factor(rank, levels = c("ASV", "Species", "Genus", "Family", "Order", "Class", "Phylum"))) %>% 
  ggplot() +
  geom_point(aes(FIT, Norgen, fill = Phylum.col, size = (FIT + Norgen) /2),
             alpha = 1, shape = 21, stroke = 0.2) +
  geom_abline(slope = 1, intercept = 0) +
  scale_size_continuous(range = c(0.5, 1.5), guide = "none") +
  scale_fill_manual("Phylum", values = cols_phyla) +
  labs(x = "FIT mean CLR abundance",
       y = "Norgen mean CLR abundance") +
  theme_bw(base_size = 7) +
  theme(legend.position = "none") +
  facet_grid(rank ~ .) +
  scale_fill_manual(values = cols_phyla)



p2_dat  <-  rbind(get_ICC_inter("Species", plot = FALSE),
                  get_ICC_inter("Genus", plot = FALSE),
                  get_ICC_inter("Family", plot = FALSE),
                  get_ICC_inter("Class", plot = FALSE),
                  get_ICC_inter("Order", plot = FALSE),
                  get_ICC_inter("Phylum", plot = FALSE))

p2_0 <- p2_dat %>% 
  mutate(rank = factor(rank, levels = c("Species", "Genus", "Family", "Order", "Class", "Phylum"))) %>% 
  group_by(rank) %>% 
  mutate(q1 = quantile(ICC, 0.25),
         q2 = quantile(ICC, 0.5),
         q3 = quantile(ICC, 0.75)) %>% 
  ggplot(aes(col = Phylum.col, fill = Phylum.col)) +
  geom_hline(yintercept = 0.9, linetype = "dashed") +
  # geom_segment(inherit.aes = FALSE,
  #              data = function(x) select(x, q1, q3) %>%  distinct,
  #              aes(y = q1, 
  #                  yend = q3, 
  #                  x = -150, xend = - 150), colour = "black",
  #              arrow = arrow(angle = 90, ends = "both")) +
  
  geom_errorbar(aes(x = sum_log_abundance, ymin = `lower bound`, ymax = `upper bound`), alpha = 0.5, size = 0.2, show.legend = FALSE) +
  xlab("Summed log abundances") +
  ylab("ICC") +
  geom_point(aes(x = sum_log_abundance, y = ICC, size = sum_log_abundance), shape = 21, color = "black", stroke = 0.2) +
  geom_boxplot(inherit.aes = FALSE, aes(y = ICC, x = 1850, fill = NULL), outlier.size =  0.1, width = 75, colour = "grey20", size = 0.25) +
  scale_x_continuous(breaks = c(0,500,1000, 1500),
                     minor_breaks = c(250, 750, 1250)) +
  geom_vline(aes(xintercept = 1750)) +
  scale_size_continuous(range = c(0.5, 1.5), guide = "none") +
  theme_bw(base_size = 7) +
  theme(legend.position = "none") +
  facet_grid(rank ~ .) +
  scale_fill_manual(values = cols_phyla) +
  scale_color_manual(values = cols_phyla) 




p3_0 <- rbind(plot_reads("Species", plot = FALSE),
              plot_reads("Genus", plot = FALSE),
              plot_reads("Family", plot = FALSE),
              plot_reads("Order", plot = FALSE),
              plot_reads("Class", plot = FALSE),
              plot_reads("Phylum", plot = FALSE),
              plot_sequences("Species", plot = FALSE),
              plot_sequences("Genus", plot = FALSE),
              plot_sequences("Family", plot = FALSE),
              plot_sequences("Order", plot = FALSE),
              plot_sequences("Class", plot = FALSE),
              plot_sequences("Phylum", plot = FALSE)) %>% 
  mutate(rank = factor(rank, levels = c("Species", "Genus", "Family", "Order", "Class", "Phylum"))) %>% 
  ggplot() +
  geom_split_violin(aes(x = type, y = classified, fill = method), size = 0.1, bw = 0.02) +
  # geom_boxplot(aes(x = type, y = classified, fill = method)) +
  ylim(0,1) +
  labs(y = "Proportion classified",
       x = " ") +
  theme_bw(base_size = 7) +
  theme(legend.position = "top", legend.key.size = unit(3, "mm")) +
  scale_fill_manual("Sample type",values = cols[1:2]) +
  scale_x_discrete(labels = c("Reads", "Sequences")) +
  guides(fill = guide_legend(title.position = "top")) + 
  
  facet_grid(rank ~ .) 

p3_0
# p3_ <- rbind(plot_reads("Species", plot = FALSE),
#              plot_reads("Genus", plot = FALSE),
#              plot_reads("Family", plot = FALSE),
#              plot_reads("Order", plot = FALSE),
#              plot_reads("Class", plot = FALSE),
#              plot_reads("Phylum", plot = FALSE)) %>% 
#   mutate(rank = factor(rank, levels = c("Species", "Genus", "Family", "Order", "Class", "Phylum"))) %>% 
#   ggplot() +
#   geom_boxplot(aes(x = method, y = reads_classified, fill = method)) +
#   ylim(0,1) +
#   theme_bw(base_size = 7) +
#   theme(legend.position = "top") +
#   scale_fill_manual(values = cols[1:2]) +
#   facet_grid(rank ~ .) 
# 
# rbind(p3_, p4_)
# 
# 
# p4_ <- rbind(plot_sequences("Species", plot = FALSE),
#              plot_sequences("Genus", plot = FALSE),
#              plot_sequences("Family", plot = FALSE),
#              plot_sequences("Order", plot = FALSE),
#              plot_sequences("Class", plot = FALSE),
#              plot_sequences("Phylum", plot = FALSE)) %>% 
#   mutate(rank = factor(rank, levels = c("Species", "Genus", "Family", "Order", "Class", "Phylum"))) %>% 
#   ggplot() +
#   geom_boxplot(aes(x = method, y = sequences_classified, fill = method)) +
#   ylim(0,1) +
#   theme_bw(base_size = 7) +
#   theme(legend.position = "top") +
#   scale_fill_manual(values = cols[1:2]) +
#   facet_grid(rank ~ .) 

p0_0 <- rbind(make_scatterplot("Species", plot = FALSE),
              make_scatterplot("Genus", plot = FALSE),
              make_scatterplot("Family", plot = FALSE),
              make_scatterplot("Order", plot = FALSE),
              make_scatterplot("Class", plot = FALSE),
              make_scatterplot("Phylum", plot = FALSE)) %>% 
  mutate(rank = factor(rank, levels = c("ASV", "Species", "Genus", "Family", "Order", "Class", "Phylum"))) %>% 
  ggplot() +
  geom_point(aes(FIT, Norgen, fill = Phylum.col, size = (FIT + Norgen) /2),
             alpha = 1, shape = 21) +
  geom_abline(slope = 1, intercept = 0) +
  scale_size_continuous(range = c(1, 3), guide = "none") +
  scale_fill_manual("Phylum", values = cols_phyla) +
  facet_grid(rank ~ ., switch = "y") +
  theme_bw(base_size = 7) +
  labs(x = "",
       y = "",
       fill = "Phylum") +
  theme(legend.position = "top", legend.key.size = unit(3, "mm")) +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  scale_x_continuous(breaks = 0, labels = c("")) +
  # scale_y_continuous(position = "right") +
  guides(fill = guide_legend(title.position = "top"))
NULL

p0_0

legend_grob1 <- cowplot::get_legend(p0_0)  
legend_grob2 <- cowplot::get_legend(p3_0)  

p0 <- ggplot2::ggplotGrob(p0_0 + theme(legend.position = "none"))


p0 <- p0 %>% 
  cowplot::gtable_remove_grobs(names[grepl("panel", names) | grepl("guide", names)
  ]) %>% 
  cowplot::gtable_squash_cols(c(1:4,6:12)) %>% 
  ggplotify::as.ggplot()

# strips <-  c("strip-r-1",
#              "strip-r-2",
#              "strip-r-3",
#              "strip-r-4",
#              "strip-r-5",
#              "strip-r-6")



p1_ <- ggplot2::ggplotGrob(p1_0)
names <-  p1_$layout$name
p1 <- cowplot::gtable_remove_grobs(p1_, names[grepl("strip", names)]) %>% # | grepl("guide", names)]) %>% 
  cowplot::gtable_squash_cols(c(6)) %>% 
  ggplotify::as.ggplot()

p2_ <- ggplot2::ggplotGrob(p2_0)
names <-  p2_$layout$name
p2 <- cowplot::gtable_remove_grobs(p2_, names[grepl("strip", names)]) %>% # | grepl("guide", names)]) %>% 
  cowplot::gtable_squash_cols(c(6)) %>% 
  ggplotify::as.ggplot()

p3_ <- ggplot2::ggplotGrob(p3_0 + theme(legend.position = "none"))
names <-  p3_$layout$name
p3 <- cowplot::gtable_remove_grobs(p3_, names[grepl("strip", names)]) %>% # | grepl("guide", names)]) %>% 
  cowplot::gtable_squash_cols(c(6)) %>% 
  ggplotify::as.ggplot()

# p4_ <- ggplot2::ggplotGrob(p4_)
# names <-  p4_$layout$name
# p4 <- cowplot::gtable_remove_grobs(p4_, names[grepl("strip", names) | grepl("guide", names)]) %>% 
#   cowplot::gtable_squash_cols(c(6)) %>% 
#   ggplotify::as.ggplot()
# 


sup_p1 <- ggarrange(
  ggarrange(grid::grob(), legend_grob1, legend_grob2,
            nrow = 1, widths = c(0.125,2, 0.75))
  ,
  ggarrange(p0,
            # ggarrange(
            p1,
            p2,
            # p3,
            # common.legend = T,
            # legend.grob = legend_grob,
            # nrow = 1),
            p3,
            nrow = 1, widths = c(0.15,1, 1, 0.75),
            labels = c("", "A", "B", "C")),
  ncol = 1, heights = c(0.05,1))


saveRDS(sup_p1, file = "plots/Supplementary_figure1.RDS")

# library(ggpubr)
# cairo_pdf("plots/Supplementary_figure2.pdf", height = 7.85, width = 6.7)
# 
# ggarrange(
# ggarrange(grid::grob(), legend_grob1, legend_grob2,
#           nrow = 1, widths = c(0.1,2, 0.75))
# ,
# ggarrange(p0,
#           p1,
#           p2,
#           p3,
#           nrow = 1, widths = c(0.1,1, 1, 0.75),
#           labels = c("", "A", "B", "C")),
# ncol = 1, heights = c(0.05,1))
# 
# dev.off()


