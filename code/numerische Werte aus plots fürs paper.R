source("code/load_all.R")



scatter_dat <- make_scatterplot("ASV", plot = FALSE)

scatter_dat %>% 
  mutate(Diff = FIT - Norgen) %>% 
  arrange(desc(abs(Diff))) %>% 
  # arrange(Diff) %>% 
  left_join(df_asv %>% 
              # filter(ASV == "ASV_293") %>% 
              select(Taxon = ASV, Genus, Phylum) %>% 
              distinct()) %>% 
  select(ASV = Taxon, Genus, Phylum, FIT, Norgen, Diff)






p2$data$ICC %>%  quantile


df_asv %>% 
  group_by(method, sampleID) %>% 
  summarise(richness = sum(abundance)) %>% 
  # pull(richness) %>%  sort
  summarise(richness = median(richness))

# CLR ---------------------------------------------------------------------



sheet_ASV <- make_scatterplot("ASV", plot = FALSE) %>% 
  mutate(Diff = FIT - Norgen) %>% 
  arrange(desc(abs(Diff))) %>% 
  rename(ASV = Taxon) %>% 
  left_join(df_asv %>% 
              select(ASV, Species:Domain) %>% 
              distinct()) %>% 
  select(-Phylum.col, -rank)


sheet_Species <- make_scatterplot("Species", plot = FALSE) %>% 
  mutate(Diff = FIT - Norgen) %>% 
  arrange(desc(abs(Diff))) %>% 
  rename(Species = Taxon) %>% 
  left_join(df_asv %>% 
              select(Species:Domain) %>% 
              distinct()) %>% 
  select(-Phylum.col, -rank)



sheet_Genus <- make_scatterplot("Genus", plot = FALSE) %>% 
   mutate(Diff = FIT - Norgen) %>% 
   arrange(desc(abs(Diff))) %>% 
   rename(Genus = Taxon) %>% 
   left_join(df_asv %>% 
               select(Genus:Domain) %>% 
               distinct()) %>% 
   select(-Phylum.col, -rank)
 
sheet_Family <- make_scatterplot("Family", plot = FALSE) %>% 
   mutate(Diff = FIT - Norgen) %>% 
   arrange(desc(abs(Diff))) %>% 
   rename(Family = Taxon) %>% 
   left_join(df_asv %>% 
               select(Family:Domain) %>% 
               distinct()) %>% 
   select(-Phylum.col, -rank)
 
sheet_Order <- make_scatterplot("Order", plot = FALSE) %>% 
   mutate(Diff = FIT - Norgen) %>% 
   arrange(desc(abs(Diff))) %>% 
   rename(Order = Taxon) %>% 
   left_join(df_asv %>% 
               select(Order:Domain) %>% 
               distinct()) %>% 
   select(-Phylum.col, -rank)
 
 
sheet_Class <- make_scatterplot("Class", plot = FALSE) %>% 
   mutate(Diff = FIT - Norgen) %>% 
   arrange(desc(abs(Diff))) %>% 
   rename(Class = Taxon) %>% 
   left_join(df_asv %>% 
               select(Class:Domain) %>% 
               distinct()) %>% 
   select(-Phylum.col, -rank)

sheet_Phylum <- make_scatterplot("Phylum", plot = FALSE) %>% 
   mutate(Diff = FIT - Norgen) %>% 
   arrange(desc(abs(Diff))) %>% 
   rename(Phylum = Taxon) %>% 
   left_join(df_asv %>% 
               select(Phylum:Domain) %>% 
               distinct()) %>% 
   select(-Phylum.col, -rank)


wb <- createWorkbook() 

addWorksheet(wb, "ASV")
writeData(wb = wb, sheet = "ASV", sheet_ASV) 
addWorksheet(wb, "Species")
writeData(wb = wb, sheet = "Species", sheet_Species) 
addWorksheet(wb, "Genus")
writeData(wb = wb, sheet = "Genus", sheet_Genus)
addWorksheet(wb, "Family")
writeData(wb = wb, sheet = "Family", sheet_Family)
addWorksheet(wb, "Order")
writeData(wb = wb, sheet = "Order", sheet_Order)
addWorksheet(wb, "Class")
writeData(wb = wb, sheet = "Class", sheet_Class)
addWorksheet(wb, "Phylum")
writeData(wb = wb, sheet = "Phylum", sheet_Phylum)


saveWorkbook(wb, "tables/clr_means.xlsx", overwrite = TRUE)



# ICC ---------------------------------------------------------------------

sheet_ASV <- get_ICC_inter("ASV", plot = FALSE) %>%  
  arrange(sum_log_abundance) %>% 
  select(ASV = taxon,sum_log_abundance: `upper bound` ) %>% 
  left_join(df_asv %>% 
              select(ASV, Species:Domain) %>% 
              distinct())

sheet_Species <- get_ICC_inter("Species", plot = FALSE) %>%  
  arrange(sum_log_abundance) %>% 
  select(Species = taxon,sum_log_abundance: `upper bound` ) %>% 
  left_join(df_asv %>% 
              select(Species:Domain) %>% 
              distinct())

sheet_Genus <- get_ICC_inter("Genus", plot = FALSE) %>% 
  arrange(sum_log_abundance) %>% 
  select(Genus = taxon,sum_log_abundance: `upper bound` ) %>% 
  left_join(df_asv %>% 
              select(Genus:Domain) %>% 
              distinct())

sheet_Family <- get_ICC_inter("Family", plot = FALSE) %>% 
  arrange(sum_log_abundance) %>% 
  select(Family = taxon,sum_log_abundance: `upper bound` ) %>% 
  left_join(df_asv %>% 
              select(Family:Domain) %>% 
              distinct())

sheet_Order <- get_ICC_inter("Order", plot = FALSE) %>% 
  arrange(sum_log_abundance) %>% 
  select(Order = taxon,sum_log_abundance: `upper bound` ) %>% 
  left_join(df_asv %>% 
              select(Order:Domain) %>% 
              distinct())

sheet_Class <- get_ICC_inter("Class", plot = FALSE) %>% 
  arrange(sum_log_abundance) %>% 
  select(Class = taxon,sum_log_abundance: `upper bound` ) %>% 
  left_join(df_asv %>% 
              select(Class:Domain) %>% 
              distinct())

sheet_Phylum <- get_ICC_inter("Phylum", plot = FALSE) %>% 
  arrange(sum_log_abundance) %>% 
  select(Phylum = taxon, sum_log_abundance: `upper bound` ) %>% 
  left_join(df_asv %>% 
              select(Phylum:Domain) %>% 
              distinct())


wb <- createWorkbook() 

addWorksheet(wb, "ASV")
writeData(wb = wb, sheet = "ASV", sheet_ASV) 
addWorksheet(wb, "Species")
writeData(wb = wb, sheet = "Species", sheet_Species) 
addWorksheet(wb, "Genus")
writeData(wb = wb, sheet = "Genus", sheet_Genus)
addWorksheet(wb, "Family")
writeData(wb = wb, sheet = "Family", sheet_Family)
addWorksheet(wb, "Order")
writeData(wb = wb, sheet = "Order", sheet_Order)
addWorksheet(wb, "Class")
writeData(wb = wb, sheet = "Class", sheet_Class)
addWorksheet(wb, "Phylum")
writeData(wb = wb, sheet = "Phylum", sheet_Phylum)


saveWorkbook(wb, "tables/ICCs.xlsx", overwrite = TRUE)


# ALDEX -------------------------------------------------------------------

readRDS("results/aldex_ASVs.RDS")


sheet_ASV <- readRDS("results/aldex_ASVs.RDS") %>% 
  left_join(df_asv %>% 
              select(ASV, Species:Domain) %>% 
              distinct())

sheet_Species <- readRDS("results/aldex_Species.RDS") %>% 
  left_join(df_asv %>% 
              select(Species:Domain) %>% 
              distinct())

sheet_Genus <- readRDS("results/aldex_Genus.RDS") %>% 
  left_join(df_asv %>% 
              select(Genus:Domain) %>% 
              distinct())

sheet_Family <- readRDS("results/aldex_Family.RDS") %>% 
  left_join(df_asv %>% 
              select(Family:Domain) %>% 
              distinct())

sheet_Order <- readRDS("results/aldex_Order.RDS") %>% 
  left_join(df_asv %>% 
              select(Order:Domain) %>% 
              distinct())

sheet_Class <- readRDS("results/aldex_Class.RDS") %>% 
  left_join(df_asv %>% 
              select(Class:Domain) %>% 
              distinct())

sheet_Phylum <- readRDS("results/aldex_Phylum.RDS") %>% 
  left_join(df_asv %>% 
              select(Phylum:Domain) %>% 
              distinct())


wb <- createWorkbook() 

addWorksheet(wb, "ASV")
writeData(wb = wb, sheet = "ASV", sheet_ASV) 
addWorksheet(wb, "Species")
writeData(wb = wb, sheet = "Species", sheet_Species) 
addWorksheet(wb, "Genus")
writeData(wb = wb, sheet = "Genus", sheet_Genus)
addWorksheet(wb, "Family")
writeData(wb = wb, sheet = "Family", sheet_Family)
addWorksheet(wb, "Order")
writeData(wb = wb, sheet = "Order", sheet_Order)
addWorksheet(wb, "Class")
writeData(wb = wb, sheet = "Class", sheet_Class)
addWorksheet(wb, "Phylum")
writeData(wb = wb, sheet = "Phylum", sheet_Phylum)


saveWorkbook(wb, "tables/ALDEX.xlsx", overwrite = TRUE)
