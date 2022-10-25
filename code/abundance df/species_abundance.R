library(dplyr)
library(tidyr)

df_asv <- readRDS("results/df_asv.RDS")
meta_data <- readRDS("data/2021-08-04 metadata.RDS")

seqtab.species <- 
  df_asv %>% 
  filter(!is.na(Species)) %>%
  group_by(sampleID, Species) %>% 
  summarise(abundance = sum(abundance)) %>% 
  pivot_wider(names_from = Species, values_from = abundance) %>% 
  tibble::column_to_rownames("sampleID")

saveRDS(seqtab.species, "results/seqtab.species.RDS")

seqtab.pseudocounts_species  <- zCompositions::cmultRepl(seqtab.species, method="CZM", output = "p-counts")



seqtab.clr_species <- log(seqtab.pseudocounts_species / apply(seqtab.pseudocounts_species, 1, psych::geometric.mean))
saveRDS(seqtab.clr_species, "results/seqtab.clr_species.RDS")

clr_long_species <- seqtab.clr_species %>%
  as.data.frame() %>% 
  tibble::rownames_to_column("sampleID") %>% 
  pivot_longer(-sampleID, names_to = "Species", values_to = "clr")


df_species <- seqtab.species %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("sampleID") %>% 
  pivot_longer(-sampleID, names_to = "Species", values_to = "abundance") %>% 
  left_join(clr_long_species, by = c("sampleID", "Species")) %>% 
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
  group_by(Species) %>% 
  mutate(Species_reads = sum(abundance)) %>% 
  mutate(Species_reads_median = median(abundance)) %>% 
  mutate(Species_prevalence = mean(abundance > 0)) %>% 
  group_by(patID, Species) %>% 
  mutate(matching = sum(abundance > 0) %in% c(0, 2, 6)) %>% 
  mutate(pat_presence = any(abundance > 0)) %>% 
  ungroup %>% 
  mutate(ratio = abundance/sample_reads,
         logratio = log(abundance/sample_reads))
  

saveRDS(df_species, "results/df_species.RDS")

  
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
