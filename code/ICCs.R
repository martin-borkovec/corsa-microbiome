seqtab <- readRDS("data/ASV_raw_counts.RDS")

seqtab <- seqtab[, colMeans(seqtab > 0) >  0.05]

ICC_invSimpson <- vegan::diversity(seqtab / rowSums(seqtab), "invsimpson") %>% 
  data.frame() %>% 
  setNames("div") %>% 
  tibble::rownames_to_column("sample") %>% 
  mutate(subject = substr(sample, 1, 5),
         method = substr(sample, 7, 10),
         type = "Simpson",
         rank = "Species") %>% 
  select(subject, div, method) %>% 
  pivot_wider(subject, values_from = div, names_from =  method) %>% 
  select(-subject) %>% 
  psych::ICC()

ICC_Simpson <- vegan::diversity(seqtab / rowSums(seqtab), "simpson") %>% 
  data.frame() %>% 
  setNames("div") %>% 
  tibble::rownames_to_column("sample") %>% 
  mutate(subject = substr(sample, 1, 5),
         method = substr(sample, 7, 10),
         type = "Simpson",
         rank = "Species") %>% 
  select(subject, div, method) %>% 
  pivot_wider(subject, values_from = div, names_from =  method) %>% 
  select(-subject) %>% 
  psych::ICC()


ICC_Shannon <- vegan::diversity(seqtab / rowSums(seqtab), "shannon") %>% 
  data.frame() %>% 
  setNames("div") %>% 
  tibble::rownames_to_column("sample") %>% 
  mutate(subject = substr(sample, 1, 5),
         method = substr(sample, 7, 10),
         type = "Simpson",
         rank = "Species") %>% 
  select(subject, div, method) %>% 
  pivot_wider(subject, values_from = div, names_from =  method) %>% 
  select(-subject) %>% 
  psych::ICC()

 


# 
# seqtab_species <- readRDS("results/seqtab.species.RDS")
# seqtab_genus <- readRDS("results/seqtab.genus.RDS")
# seqtab_family <- readRDS("results/seqtab.family.RDS")
# seqtab_order <- readRDS("results/seqtab.order.RDS")
# seqtab_class <- readRDS("results/seqtab.class.RDS")
# seqtab_phylum <- readRDS("results/seqtab.phylum.RDS")



seqtab.D <- vegan::vegdist(seqtab / rowSums(seqtab), "jaccard")
res <- ape::pcoa(seqtab.D)
ICC_jaccard <- cbind(
res$vectors[,1][grepl("FIT", names(res$vectors[,1])) & !grepl("v", names(res$vectors[,1]))],
res$vectors[,1][grepl("Nor", names(res$vectors[,1])) & !grepl("v", names(res$vectors[,1]))]) %>% 
  psych::ICC()
  

seqtab.D <- vegan::vegdist(seqtab / rowSums(seqtab), "bray")
res <- ape::pcoa(seqtab.D)
ICC_bray <- cbind(
  res$vectors[,1][grepl("FIT", names(res$vectors[,1])) & !grepl("v", names(res$vectors[,1]))],
  res$vectors[,1][grepl("Nor", names(res$vectors[,1])) & !grepl("v", names(res$vectors[,1]))]) %>% 
  psych::ICC()


# 
# 
# sequences_map <- readRDS("data/sequences_map.RDS")
# library(DECIPHER)
# 
# seqs <- sequences_map$ASV
# names(seqs) <- seqs 
# alignment <- AlignSeqs(DNAStringSet(seqs), anchor=NA)
# 
# library(phangorn)
# phang.align <- phyDat(as(alignment, "matrix"), type="DNA")
# dm <- dist.ml(phang.align)
# treeNJ <- NJ(dm) # Note, tip order != sequence order
# fit = pml(treeNJ, data=phang.align)
# 
# ## negative edges length changed to 0!
# 
# fitGTR <- update(fit, k=4, inv=0.2)
# fitGTR <- optim.pml(fitGTR, model="GTR", optInv=TRUE, optGamma=TRUE,
#                     rearrangement = "NNI", control = pml.control(trace = 1))
# 
# saveRDS(fitGTR, "results/fitGTR.RDS")
# 
# 
# 
# detach("package:phangorn", unload=TRUE)



library(phyloseq)

fitGTR <- readRDS("results/fitGTR.RDS")
taxonomy_map <- readRDS("data/taxonomy_map.RDS")
sequences_map <- readRDS("data/sequences_map.RDS")
taxa <- taxonomy_map[,-1]
# rownames(taxa) <- sequences_map$ASV
taxa <- tax_table(taxa)
taxa_names(taxa) <- sequences_map$ASV

otus <- seqtab/rowSums(seqtab)
colnames(otus) <- sequences_map$ASV[match(colnames(seqtab), sequences_map$ASV_ID)]
otus <- otu_table(otus, taxa_are_rows = FALSE)

samples <- sample_data(data.frame(method = substr(row.names(seqtab), 7,9)))
rownames(samples) <- row.names(seqtab)

ps <- phyloseq(taxa, samples, otus, phy_tree(fitGTR$tree))

set.seed(1337)
unifrac_dist <- phyloseq::UniFrac(ps)
unifrac_dist_w <- phyloseq::UniFrac(ps, weighted = TRUE)


res_w <- ape::pcoa(unifrac_dist_w)

ICC_unifrac_w <- cbind(
  res_w$vectors[,1][grepl("FIT", names(res_w$vectors[,1]))],
  res_w$vectors[,1][grepl("Nor", names(res_w$vectors[,1]))]) %>% 
  psych::ICC()

res <- ape::pcoa(unifrac_dist)

ICC_unifrac <- cbind(
  res$vectors[,1][grepl("FIT", names(res$vectors[,1]))],
  res$vectors[,1][grepl("Nor", names(res$vectors[,1]))]) %>% 
  psych::ICC()


dat_beta <- cbind(
rbind(
ICC_Shannon$results[1, c(2,7,8)],
ICC_invSimpson$results[1, c(2,7,8)],
ICC_Simpson$results[1, c(2,7,8)],
ICC_bray$results[1, c(2,7,8)],
ICC_jaccard$results[1, c(2,7,8)],
ICC_unifrac$results[1, c(2,7,8)],
ICC_unifrac_w$results[1, c(2,7,8)]),
"measure" = factor(
c("Shannon",
"invSimpson",
"Simpson",
"bray",
"jaccard",
"unifrac",
"unifrac_w"), levels = 
  c("Shannon",
    "Simpson",
    "invSimpson",
    "bray",
    "jaccard",
    "unifrac",
    "unifrac_w"),
labels = 
  c("Shannon",
    "Simpson",
    "Inv. Simpson",
    "Bray",
    "Jaccard",
    "Unifrac",
    "W. Unifrac "))) 
  
  # pivot_longer(-measure, names_to = "name", values_to = "value") %>% 
p5 <- dat_beta %>% 
  ggplot() +
  geom_linerange(aes(x = measure, ymax = `upper bound`, ymin = `lower bound`), size = 0.5,) +
  geom_point(aes(x = measure, y = ICC), col = "firebrick") +
  ylim(0,1) + 
  ylab("ICC (2,1)") +
  theme_bw(base_size = 7) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())


# png("plots/Figure_ICC_metrics.png", height = 100, width = 210, units = "mm",res = 200)
# p_5
# dev.off()


