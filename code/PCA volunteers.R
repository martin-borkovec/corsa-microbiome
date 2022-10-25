seqtab.clr <- readRDS("results/seqtab.clr.RDS")
# seqtab.clr <- readRDS("results/seqtab.clr_genus.RDS")
seqtab.clr <- seqtab.clr[grepl("V", rownames(seqtab.clr)), ]
seqtab.clr <- seqtab.clr[, colMeans(seqtab.clr > 0) >  0.05]


pcx <- prcomp(seqtab.clr)

# pc1 <- round(pcx$sdev[1]^2/sum(pcx$sdev^2),2)
# pc2 <- round(pcx$sdev[2]^2/sum(pcx$sdev^2),2)
# xlab <- paste("PC1: ", pc1, sep="")
# ylab <- paste("PC2: ", pc2, sep="")
# # biplot(pcx, cex=c(0.6,0.4), var.axes=F,
# #        scale=1, xlab=xlab, ylab=ylab)

# 
# p1 <- data.frame("explained" = round(pcx$sdev^2/sum(pcx$sdev^2),2)[1:10],
#            "PC" = factor(1:10)) %>% 
#   ggplot() +
#   geom_col(aes(x= PC, y = explained))+
#   theme_bw()
# 
# 
# pcx$rotation %>%
#   as.data.frame() %>%
#   tibble::rownames_to_column("ASV") %>%
#   # left_join(y = distinct(select(df_asv, "Family", "Genus")), by = "Genus") %>%
#   # left_join(y = select(asv_d, "change", "ASV"), by = "ASV") %>%
#   ggplot() +
#   geom_point(aes(x = PC1, y = PC2), show.legend = T)
# 
# 

p2_pca <- pcx$x %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("sampleID") %>% 
  # left_join(y = distinct(select(df_asv, "Family", "Genus")), by = "Genus") %>% 
  left_join(y = distinct(dplyr::select(df_asv, "sampleID", "method", "volunteer", "patID")), by = "sampleID") %>% 
  # filter(!volunteer) %>% 
  # ggplot(aes(x = PC1, y = PC2, col = method)) +
  ggplot(aes(x = PC1, y = PC2, col = method)) +
  labs(x = "PC1 (32%)",
       y = "PC2 (25%)") +
  scale_color_manual("Sample type", values = cols[1:2]) +
  # coord_fixed() +
  # geom_point( show.legend = T) + 
  geom_text(aes(label = substr(patID, 4,4)), fontface = "bold", size = 2) +
  theme_bw(base_size = 7) +
  theme(legend.title = element_text(face = "bold")) +
  NULL



p3_pca <- pcx$x %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("sampleID") %>% 
  # left_join(y = distinct(select(df_asv, "Family", "Genus")), by = "Genus") %>% 
  left_join(y = distinct(dplyr::select(df_asv, "sampleID", "method", "volunteer", "patID")), by = "sampleID") %>% 
  # filter(!volunteer) %>% 
  # ggplot(aes(x = PC1, y = PC2, col = method)) +
  
  ggplot(aes(x = PC3, y = PC4, col = method)) +
  # coord_fixed() +
  labs(x = "PC3 (17%)",
       y = "PC4 (15%)") +
  scale_color_manual("Sample type", values = cols[1:2]) +
  # geom_point( show.legend = T) + 
  geom_text(aes(label = substr(patID, 4,4)), fontface = "bold", size = 2) +
  theme_bw(base_size = 7) +
  theme(legend.title = element_text(face = "bold")) +
  NULL
# 
# 
# ragg::agg_png("plots/PCA_vol.png",
#               # width = 2000,
#               # height = 1000,
#               # pointsize = 12,
#               # res = 500)
#               res=300, width=3300, height=1600, pointsize=12)
# 
# ggpubr::ggarrange(p2,p3, common.legend = T)
# 
# dev.off()


