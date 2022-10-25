source("code/load_all.R")

p1 <- make_scatterplot("ASV", size_range = c(0.5, 1.5), stroke_size = 0.2)
p2_ <- get_ICC_inter("ASV", size_range = c(0.5, 1.5), stroke_size = 0.2)

  # geom_vline(xintercept = 1200) +
  # geom_boxplot(inherit.aes = FALSE, aes(y = ICC, x = 1300, fill = NULL), outlier.shape = NA, width = 100) +
  # NULL
p2 <- p2_ +
  geom_vline(xintercept = 1200) +
  geom_boxplot(inherit.aes = FALSE, aes(y = ICC, x = 1300, fill = NULL), outlier.size =  0.1, width = 100, colour = "grey20", size = 0.25) +
  scale_x_continuous(breaks = c(0,500,1000),
                     minor_breaks = c(250, 750)) +
  theme_bw(base_size = 7) +
  ylab("ICC (3,1)") +
  NULL

p3 <- df_asv %>% 
  # filter(!volunteer) %>% 
  filter(abundance > 0) %>% 
  group_by(sampleID, method) %>% 
  count() %>%
  ggplot() +
  # geom_boxplot(aes(x = method, y = n, fill = method)) +
  geom_split_violin(aes(x = 1, y = n, fill = method), size = 0.1) +
  scale_fill_manual(values = cols[1:2]) + 
  labs(y = "Richness of samples (ASVs)",
       x = "",
       fill = "Sample type") +
  theme_bw(base_size = 7) +
  theme(legend.key.size = unit(3, "mm"))+ 
  theme(axis.text.x = element_blank(),
        axis.ticks.x  = element_blank())
  # guides(fill = guide_legend(


source("code/functions/geom_split_violin.R")
p4 <- df_asv %>% 
  # filter(!volunteer) %>% 
  # filter(abundance > 0) %>% 
  group_by(sampleID, method) %>% 
  summarise(reads = sum(abundance)) %>%
  # pull(reads) %>%  sort
  # ungroup() %>% 
  ggplot() +
  # geom_boxplot(aes(x = method, y = reads, fill = method)) +
  geom_split_violin(aes(x = 1, y = reads, fill = method), size = 0.1) +
  scale_fill_manual(values = cols[1:2]) + 
  ylim(0, 300000) +
  # scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","), 
  #                    limits = c(0,300000)) +
  labs(y = "Reads per sample",
       x = "",
       fill = "Sample type") +
  theme_bw(base_size = 7)+ 
  theme(axis.text.x = element_blank(),
        axis.ticks.x  = element_blank())



 p6 <- df_asv %>%
  filter(abundance > 0) %>% 
  group_by(ASV) %>% 
  mutate(total_prevalence = n() / 192) %>%
  filter(total_prevalence > 0.05) %>% 
  group_by(ASV, method) %>% 
  summarise(prevalence = n() / 96) %>% 
  # group_by(ASV) %>% 
  #  summarize(x = mean(prevalence)) %>% 
  #  pull(x) %>%  min
  #  
  ggplot() +
  # geom_boxplot(aes(x = method, y = prevalence, fill = method)) +
  # geom_violin(aes(x = method, y = prevalence, fill = method)) +
  geom_split_violin(aes(x = 1, y = prevalence, fill = method), size = 0.1) +
   scale_fill_manual(values = cols[1:2]) + 
   labs(y = "Prevalence of ASVs",
        x = "",
        fill = "Sample type") +
   theme_bw(base_size = 7) + 
   theme(axis.text.x = element_blank(),
         axis.ticks.x  = element_blank())
 
  
p6



source("code/ICCs.R")



library(ggpubr)
cairo_pdf(file = "plots/Figure1.pdf", height = 7.85, width = 6.7)
# pdf(file = "plots/Figure1.pdf", width = 3.35, height = 8.85)
ggarrange(
ggarrange(p1 + scale_fill_manual("Phylum", values = cols_phyla) + theme_bw(base_size = 7),
          p2 + scale_fill_manual("Phylum", values = cols_phyla) + scale_color_manual("Phylum", values = cols_phyla) , nrow = 1, common.legend = TRUE, labels = "AUTO", font.label = label_par),
ggarrange(p5 , ggarrange(p3, p4, p6, nrow = 1, common.legend = TRUE, legend = "bottom", labels = LETTERS[4:6], font.label = label_par), nrow = 1, labels = "C", font.label = label_par, widths = c(2,3)),
nrow = 2)
dev.off()


df_asv %>% 
  select(ASV, ASV_prevalence, method) %>% 
  distinct %>% 
ggplot() +
  geom_density(aes(ASV_prevalence, fill = method))

df_asv %>% 
  select(pat_reads) %>% 
  distinct %>% 
ggplot() +
  geom_density(aes(pat_reads))



