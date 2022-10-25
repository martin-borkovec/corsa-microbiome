library(dplyr)
library(tidyr)

df_asv <- readRDS("results/df_asv.RDS")
meta_data <- readRDS("data/2021-08-04 metadata.RDS")

seqtab.genus <- 
  df_asv %>% 
  filter(!is.na(Genus)) %>% 
  group_by(sampleID, Genus) %>% 
  summarise(abundance = sum(abundance)) %>% 
  pivot_wider(names_from = Genus, values_from = abundance) %>% 
  tibble::column_to_rownames("sampleID")

saveRDS(seqtab.genus, "results/seqtab.genus.RDS")

seqtab.pseudocounts_genus  <- zCompositions::cmultRepl(seqtab.genus, method="CZM", output = "p-counts")



seqtab.clr_genus <- log(seqtab.pseudocounts_genus / apply(seqtab.pseudocounts_genus, 1, psych::geometric.mean))
saveRDS(seqtab.clr_genus, "results/seqtab.clr_genus.RDS")

clr_long_genus <- seqtab.clr_genus %>%
  as.data.frame() %>% 
  tibble::rownames_to_column("sampleID") %>% 
  pivot_longer(-sampleID, names_to = "Genus", values_to = "clr")


df_genus <- seqtab.genus %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("sampleID") %>% 
  pivot_longer(-sampleID, names_to = "Genus", values_to = "abundance") %>% 
  left_join(clr_long_genus, by = c("sampleID", "Genus")) %>% 
  mutate(LNR = substring(sampleID, 1, 5),
         patID = case_when(substring(LNR, 1, 4) == "V001" ~ "v0010",
                           substring(LNR, 1, 4) == "V002" ~ "v0020",
                           substring(LNR, 1, 4) == "V003" ~ "v0030",
                           substring(LNR, 1, 4) == "V004" ~ "v0040",
                           substring(LNR, 1, 4) == "V005" ~ "v0050",
                           TRUE ~ LNR),
         # prevalence = colSums(seqtab.final),
         volunteer = grepl("V", LNR), 
         method = case_when(grepl("FIT", sampleID) ~ "FIT",
                            grepl("Nor", sampleID) ~ "Norgen")) %>% 
  left_join(select(meta_data, patID = LNR, BMI, Alter, Geschlecht), by = "patID") %>% 
  group_by(sampleID) %>% 
  mutate(sample_reads = sum(abundance)) %>% 
  mutate(sample_diversity = sum(abundance > 0)) %>% 
  group_by(patID) %>% 
  mutate(pat_reads = sum(abundance)) %>% 
  group_by(Genus) %>% 
  mutate(Genus_reads = sum(abundance)) %>% 
  mutate(Genus_reads_median = median(abundance)) %>% 
  mutate(Genus_prevalence = mean(abundance > 0)) %>% 
  group_by(patID, Genus) %>% 
  mutate(matching = sum(abundance > 0) %in% c(0, 2, 6)) %>% 
  mutate(pat_presence = any(abundance > 0)) %>% 
  ungroup %>% 
  mutate(ratio = abundance/sample_reads,
         logratio = log(abundance/sample_reads))
  

saveRDS(df_genus, "results/df_genus.RDS")

  
# seqtab.final <- seqtab.final[, colSums(seqtab.final > 0) > 1]
# colnames(seqtab.final) <- paste0("ASV_",1:ncol(seqtab.final))
# b <- zCompositions::cmultRepl(seqtab_genus, output = "p-counts")
# 
# conds <- case_when(grepl("FIT", rownames(b) ) ~ "FIT",
#                    grepl("Nor", rownames(b) ) ~ "Norgen")
# 
# b2 <- floor(b)
# b3 <- t(b2)
# clr_genus <- ALDEx2::aldex.clr(b3, conds = conds, useMC = TRUE)
# 
# clr.test_genus <- ALDEx2::aldex.ttest(clr = clr_genus, paired.test = TRUE, verbose = TRUE)
# clr.effect_genus <- ALDEx2::aldex.effect(clr = clr_genus, verbose = TRUE)
# 
# clr.test
# 
# 
# sig_genera <- rownames(clr.test_genus)[clr.test_genus$we.ep < 0.05]
# 
# saveRDS(sig_genera, "results/sig_genera.RDS")
# 
# boxplot(clr.effect$effect)
# 
# data.frame(genus = rownames(clr.effect), effect = clr.effect$effect, p = clr.test$we.eBH) %>% 
#   arrange(effect) %>% 
#   mutate(genus = factor(genus, levels = unique(genus))) %>% 
#   ggplot() +
#   geom_col(aes(x = genus, y = effect, fill = p > 0.05), col = "black")
# 
# data.frame(genus = rownames(clr.effect), effect = clr.effect$effect, p = clr.test$we.eBH) %>% 
#   arrange(effect) %>% 
#   filter(p < 0.05) %>% 
#   mutate(genus = factor(genus, levels = unique(genus))) %>% 
#   ggplot() +
#   geom_col(aes(x = genus, y = effect, fill = p > 0.05), col = "black")
