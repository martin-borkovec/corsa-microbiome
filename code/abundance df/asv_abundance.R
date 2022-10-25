library(dplyr)
library(tidyr)

meta_data <- readRDS("data/2021-08-04 metadata.RDS")
seqtab.final <- readRDS("data/ASV_raw_counts.RDS")
# seqtab.final <- readRDS("archive/results/seqtab.final.RDS")
# taxa <- readRDS("archive/results/taxa.RDS")
taxa <- readRDS("data/taxonomy_map.RDS")
# seqtab.final <- seqtab.final[, colSums(seqtab.final > 0) > 1]
# asv2id2genus <- data.frame(ASV = paste0("ASV_", 1:ncol(seqtab.final)), sequence = colnames(seqtab.final), genus = taxa[match(colnames(seqtab.final), rownames(taxa)),6])

seqtab.pseudocounts <- zCompositions::cmultRepl(seqtab.final, method="CZM", output = "p-counts")

saveRDS(seqtab.pseudocounts, "results/seqtab.pseudocounts.RDS")
# seqtab.pseudocounts <- readRDS("results/seqtab.pseudocounts.RDS")


seqtab.clr <- log(seqtab.pseudocounts / apply(seqtab.pseudocounts, 1, psych::geometric.mean))
# seqtab.clr <- seqtab.clr %>%  as.matrix

saveRDS(seqtab.clr, "results/seqtab.clr.RDS")

clr_long <- seqtab.clr %>%
  as.data.frame() %>% 
  tibble::rownames_to_column("sampleID") %>% 
  pivot_longer(-sampleID, names_to = "ASV", values_to = "clr")

df_asv <- 
  seqtab.final %>% #[, colMeans(seqtab.nochim > 0) > 0.1] %>%
  as.data.frame() %>% 
  tibble::rownames_to_column("sampleID") %>% 
  pivot_longer(-sampleID, names_to = "ASV", values_to = "abundance") %>% 
  left_join(clr_long, by = c("sampleID", "ASV")) %>% 
  left_join(taxa, by = c("ASV")) %>% 
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
                            grepl("Nor", sampleID) ~ "Norgen"),
         Species = ifelse(is.na(Species), NA, paste(Genus, Species))) %>% 
  left_join(select(meta_data, patID = LNR, BMI, Alter, Geschlecht), by = "patID") %>% 
  group_by(sampleID) %>% 
  mutate(sample_reads = sum(abundance)) %>% 
  mutate(sample_diversity = sum(abundance > 0)) %>% 
  group_by(patID) %>% 
  mutate(pat_reads = sum(abundance)) %>% 
  group_by(ASV) %>% 
  mutate(ASV_reads = sum(abundance)) %>% 
  mutate(ASV_reads_median = median(abundance)) %>% 
  mutate(ASV_prevalence = mean(abundance > 0)) %>% 
  group_by(patID, ASV) %>% 
  mutate(matching = sum(abundance > 0) %in% c(0, 2, 6)) %>% 
  mutate(pat_presence = any(abundance > 0)) %>% 
  ungroup %>% 
  mutate(ratio = abundance/sample_reads,
         logratio = log(abundance/sample_reads))

saveRDS(df_asv, "results/df_asv.RDS")
