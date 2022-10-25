seqtab.domain <- 
  df_asv %>% 
  filter(!is.na(Domain)) %>% 
  group_by(sampleID, Domain) %>% 
  summarise(abundance = sum(abundance)) %>% 
  pivot_wider(names_from = Domain, values_from = abundance) %>% 
  tibble::column_to_rownames("sampleID")

saveRDS(seqtab.domain, "results/seqtab.domain.RDS")

seqtab.pseudocounts_domain  <- zCompositions::cmultRepl(seqtab.domain, method="CZM", output = "p-counts")

seqtab.clr_domain <- log(seqtab.pseudocounts_domain / apply(seqtab.pseudocounts_domain, 1, psych::geometric.mean))

clr_long_domain <- seqtab.clr_domain %>%
  as.data.frame() %>% 
  tibble::rownames_to_column("sampleID") %>% 
  pivot_longer(-sampleID, names_to = "Domain", values_to = "clr")

df_domain <- seqtab.domain %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("sampleID") %>% 
  pivot_longer(-sampleID, names_to = "Domain", values_to = "abundance") %>% 
  left_join(clr_long_domain, by = c("sampleID", "Domain")) %>% 
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
  group_by(Domain) %>% 
  mutate(Domain_reads = sum(abundance)) %>% 
  mutate(Domain_reads_median = median(abundance)) %>% 
  mutate(Domain_prevalence = mean(abundance > 0)) %>% 
  group_by(patID, Domain) %>% 
  mutate(matching = sum(abundance > 0) %in% c(0, 2, 6)) %>% 
  mutate(pat_presence = any(abundance > 0)) %>% 
  ungroup %>% 
  mutate(ratio = abundance/sample_reads,
         logratio = log(abundance/sample_reads))


saveRDS(df_domain, "results/df_domain.RDS")

