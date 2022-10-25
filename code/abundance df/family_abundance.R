library(dplyr)
library(tidyr)

df_asv <- readRDS("results/df_asv.RDS")
meta_data <- readRDS("data/2021-08-04 metadata.RDS")

seqtab.family <- 
  df_asv %>% 
  filter(!is.na(Family)) %>% 
  group_by(sampleID, Family) %>% 
  summarise(abundance = sum(abundance)) %>% 
  pivot_wider(names_from = Family, values_from = abundance) %>% 
  tibble::column_to_rownames("sampleID")

saveRDS(seqtab.family, "results/seqtab.family.RDS")

seqtab.pseudocounts_family  <- zCompositions::cmultRepl(seqtab.family, method="CZM", output = "p-counts")

seqtab.clr_family <- log(seqtab.pseudocounts_family / apply(seqtab.pseudocounts_family, 1, psych::geometric.mean))

saveRDS(seqtab.clr_family, "results/seqtab.clr_family.RDS")

clr_long_family <- seqtab.clr_family %>%
  as.data.frame() %>% 
  tibble::rownames_to_column("sampleID") %>% 
  pivot_longer(-sampleID, names_to = "Family", values_to = "clr")

df_family <- seqtab.family %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("sampleID") %>% 
  pivot_longer(-sampleID, names_to = "Family", values_to = "abundance") %>% 
  left_join(clr_long_family, by = c("sampleID", "Family")) %>% 
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
  # filter(!volunteer) %>% 
  left_join(select(meta_data, patID = LNR, BMI, Alter, Geschlecht), by = "patID") %>% 
  group_by(sampleID) %>% 
  mutate(sample_reads = sum(abundance)) %>% 
  mutate(sample_diversity = sum(abundance > 0)) %>% 
  group_by(patID) %>% 
  mutate(pat_reads = sum(abundance)) %>% 
  group_by(Family) %>% 
  mutate(Family_reads = sum(abundance)) %>% 
  mutate(Family_reads_median = median(abundance)) %>% 
  mutate(Family_prevalence = mean(abundance > 0)) %>% 
  group_by(patID, Family) %>% 
  mutate(matching = sum(abundance > 0) %in% c(0, 2, 6)) %>% 
  mutate(pat_presence = any(abundance > 0)) %>% 
  ungroup %>% 
  mutate(ratio = abundance/sample_reads,
         logratio = log(abundance/sample_reads))


saveRDS(df_family, "results/df_family.RDS")

