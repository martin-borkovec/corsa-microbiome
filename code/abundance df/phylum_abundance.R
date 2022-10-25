library(dplyr)
library(tidyr)

df_asv <- readRDS("results/df_asv.RDS")
meta_data <- readRDS("data/2021-08-04 metadata.RDS")


seqtab.phylum <- 
  df_asv %>% 
  filter(!is.na(Phylum)) %>% 
  group_by(sampleID, Phylum) %>% 
  summarise(abundance = sum(abundance)) %>% 
  pivot_wider(names_from = Phylum, values_from = abundance) %>% 
  tibble::column_to_rownames("sampleID")

saveRDS(seqtab.phylum, "results/seqtab.phylum.RDS")

seqtab.pseudocounts_phylum  <- zCompositions::cmultRepl(seqtab.phylum, method="CZM", output = "p-counts")

seqtab.clr_phylum <- log(seqtab.pseudocounts_phylum / apply(seqtab.pseudocounts_phylum, 1, psych::geometric.mean))

saveRDS(seqtab.clr_phylum, "results/seqtab.clr_phylum.RDS")

clr_long_phylum <- seqtab.clr_phylum %>%
  as.data.frame() %>% 
  tibble::rownames_to_column("sampleID") %>% 
  pivot_longer(-sampleID, names_to = "Phylum", values_to = "clr")

df_phylum <- seqtab.phylum %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("sampleID") %>% 
  pivot_longer(-sampleID, names_to = "Phylum", values_to = "abundance") %>% 
  left_join(clr_long_phylum, by = c("sampleID", "Phylum")) %>% 
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
  group_by(Phylum) %>% 
  mutate(Phylum_reads = sum(abundance)) %>% 
  mutate(Phylum_reads_median = median(abundance)) %>% 
  mutate(Phylum_prevalence = mean(abundance > 0)) %>% 
  group_by(patID, Phylum) %>% 
  mutate(matching = sum(abundance > 0) %in% c(0, 2, 6)) %>% 
  mutate(pat_presence = any(abundance > 0)) %>% 
  ungroup %>% 
  mutate(ratio = abundance/sample_reads,
         logratio = log(abundance/sample_reads))
  

saveRDS(df_phylum, "results/df_phylum.RDS")

