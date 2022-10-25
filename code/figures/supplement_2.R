source("code/load_all.R")

dat <- rbind(get_ICC_inter_vol("Species", plot = FALSE),
             get_ICC_intra("Species", "FIT", plot = FALSE),
             get_ICC_intra("Species", "Norgen", plot = FALSE),
             get_ICC_inter_vol("Genus", plot = FALSE),
             get_ICC_intra("Genus", "FIT", plot = FALSE),
             get_ICC_intra("Genus", "Norgen", plot = FALSE),
             get_ICC_inter_vol("Family", plot = FALSE),
             get_ICC_intra("Family", "FIT", plot = FALSE),
             get_ICC_intra("Family", "Norgen", plot = FALSE),
             get_ICC_inter_vol("Class", plot = FALSE),
             get_ICC_intra("Class", "FIT", plot = FALSE),
             get_ICC_intra("Class", "Norgen", plot = FALSE),
             get_ICC_inter_vol("Order", plot = FALSE),
             get_ICC_intra("Order", "FIT", plot = FALSE),
             get_ICC_intra("Order", "Norgen", plot = FALSE),
             get_ICC_inter_vol("Phylum", plot = FALSE),
             get_ICC_intra("Phylum", "FIT", plot = FALSE),
             get_ICC_intra("Phylum", "Norgen", plot = FALSE)
)

dat <- dat %>% 
  mutate(x_intercept = ifelse(type == "inter", 350, 175),
         box_x = ifelse(type == "inter", 385, 190))

sup_p2 <- 
  dat %>% 
  mutate(type = factor(type, levels = c("inter", "FIT", "Norgen"), labels = c("FIT and Norgen", "FIT and FIT", "Norgen and Norgen"))) %>% 
  mutate(rank = factor(rank, levels = c("ASV", "Species", "Genus", "Family", "Order", "Class", "Phylum"))) %>% 
  mutate(`lower bound` = ifelse(`lower bound` < 0,  0, `lower bound`)) %>% 
  mutate(`upper bound` = ifelse(`upper bound` > 1,1, `upper bound`)) %>% 
  ggplot() +
  geom_hline(yintercept = 0.9, linetype = "dashed") +
  geom_errorbar(aes(x = sum_log_abundance, ymin = `lower bound`, ymax = `upper bound`, col = type), alpha = 0.5, size = 0.2) +
  geom_point(aes(x = sum_log_abundance, y = ICC, size = sum_log_abundance, fill = type), shape = 21, color = "black", stroke = 0.2) +
  geom_vline(aes(xintercept = x_intercept)) +
  geom_boxplot(inherit.aes = FALSE, aes(y = ICC, x = box_x, fill = NULL, colour = type), data = function(x) x[x$type == "FIT and Norgen", ],  width = 20, outlier.size =  0.1, size = 0.25, show.legend = FALSE) +
  geom_boxplot(inherit.aes = FALSE, aes(y = ICC, x = box_x, fill = NULL, colour = type), data = function(x) x[x$type != "FIT and Norgen", ],  width = 10, outlier.size =  0.1, size = 0.25, show.legend = FALSE) +
  scale_x_continuous(breaks = function(x) {if(x[2] < 300) return(c(0, 50,100, 150)); return(c(0, 100, 200, 300))},
                     minor_breaks = function(x) {if(x[2] < 300) return(c(0, 25,75, 125)); return(c(0, 50, 150, 250))}) +
  # scale_x_continuous(breaks = function(x) ifelse(TRUE,
  #                                                c(0, 50,100, 150),
  #                                                c(0, 100, 200, 300)) %>%  return 
  #                    # minor_breaks = c(250, 750, 1250)
                     # ) +
  facet_grid(rank ~ type, scales = "free_x") +
  theme_bw(base_size = 7) + 
  theme(legend.position = "none") +
  scale_fill_manual("Comparison", values = cols[c(4, 1, 2)]) +
  scale_color_manual("Comparison", values = cols[c(4, 1, 2)]) +
  scale_size_continuous(range = c(0.5, 1.5), guide = "none") +
  xlab("Summed log abundance") +
  coord_cartesian(ylim = c(0,1))


sup_p2
# p1
# 
# dat %>%
#   dplyr::filter(rank == "Class" & type == "FIT") %>%
#   arrange(sum_log_abundance)

saveRDS(sup_p2, file = "plots/Supplementary_figure2.RDS")

# # png("plots/Figure_ICC_vol.png", height = 297, width = 210, units = "mm",res = 200)
# cairo_pdf(file = "plots/Supplementary_figure1.pdf", height = 7.85, width = 6.7)
# p1
# 
# dev.off()


