library(tidyr)
library(dplyr)


# setup -------------------------------------------------------------------


effect_function <- function(x) as.character(x)

method <- rep(1:2, 81)
seqtab <- readRDS("results/seqtab.species.RDS")
reads <- t(seqtab[1:162, ])
mm <- model.matrix(~ substr(colnames(reads), 1, 5) +  method)



# ASV ---------------------------------------------------------------------

library(ALDEx2)
seqtab <- readRDS("data/ASV_raw_counts.RDS")
reads <- t(seqtab[1:162, ])
reads <- reads[rowSums(reads > 0) > 10, ]  
saveRDS(rownames(reads), "results/aldex_ASVs_list.RDS")
set.seed(1337)
aldex_glm <- aldex.clr(reads, mm, mc.samples = 512, denom="all", useMC = TRUE)
aldex_effect <- aldex.glm.effect(aldex_glm, useMC = TRUE)

aldex_ttest <- ALDEx2::aldex.clr(reads,
                                 conds = substr(colnames(reads), 7, 9),
                                 mc.samples = 512, useMC = TRUE)
aldex_results <- ALDEx2::aldex.ttest(aldex_ttest, paired.test = T, verbose = T, hist.plot = T) %>% 
  tibble::rownames_to_column("ASV") %>% 
  dplyr::select(ASV, p = wi.eBH) %>% 
  mutate(effect = aldex_effect$method$effect)

saveRDS(aldex_results, "results/aldex_ASVs.RDS")
aldex_results <- readRDS("results/aldex_ASVs.RDS")
sig_ASV <- aldex_results$ASV[aldex_results$p < 0.05]

# df_species <- readRDS("results/df_species.RDS")
# 
# df_sig_species  <- df_species %>% 
#   dplyr::select(Species) %>% 
#   distinct %>% 
#   mutate(sig_Species = Species %in% sig_Species) %>%
#   left_join(aldex_results, by = "Species") %>% 
#   mutate(effect = case_when(effect < 0  & sig_Species ~ effect_function(effect),
#                             effect > 0 & sig_Species ~ effect_function(effect),
#                             TRUE ~ "n.s."),
#          vertex = Species,
#          label = vertex,
#          type = "Species") %>% 
#   dplyr::select(-Species, -sig_Species, -p)


# Species -------------------------------------------------------------------

library(ALDEx2)

seqtab <- readRDS("results/seqtab.species.RDS")
reads <- t(seqtab[1:162, ])
reads <- reads[rowSums(reads > 0) > 10, ]  
set.seed(1337)
aldex_glm <- aldex.clr(reads, mm, mc.samples = 512, denom="all", useMC = TRUE)
aldex_effect <- aldex.glm.effect(aldex_glm, useMC = TRUE)


aldex_ttest <- ALDEx2::aldex.clr(reads,
                                 conds = substr(colnames(reads), 7, 9),
                                 mc.samples = 512, useMC = TRUE)
aldex_results <- ALDEx2::aldex.ttest(aldex_ttest, paired.test = T, verbose = T, hist.plot = T) %>% 
  tibble::rownames_to_column("Species") %>% 
  dplyr::select(Species, p = wi.eBH) %>% 
  mutate(effect = aldex_effect$method$effect)

saveRDS(aldex_results, "results/aldex_Species.RDS")

aldex_results <- readRDS("results/aldex_Species.RDS")

sig_Species <- aldex_results$Species[aldex_results$p < 0.05]

df_species <- readRDS("results/df_species.RDS")

df_sig_species  <- df_species %>% 
  dplyr::select(Species) %>% 
  distinct %>% 
  mutate(sig_Species = Species %in% sig_Species) %>%
  left_join(aldex_results, by = "Species") %>% 
  mutate(effect = case_when(effect < 0  & sig_Species ~ effect_function(effect),
                            effect > 0 & sig_Species ~ effect_function(effect),
                            TRUE ~ "n.s."),
         vertex = Species,
         label = vertex,
         type = "Species") %>% 
  dplyr::select(-Species, -sig_Species, -p)


# Genus -------------------------------------------------------------------

seqtab <- readRDS("results/seqtab.genus.RDS")
reads <- t(seqtab[1:162, ])
reads <- reads[rowSums(reads > 0) > 10, ]  
set.seed(1337)
aldex_glm <- aldex.clr(reads, mm, mc.samples = 512, denom="all", useMC = TRUE)
aldex_effect <- aldex.glm.effect(aldex_glm)


aldex_ttest <- ALDEx2::aldex.clr(reads,
                                 conds = substr(colnames(reads), 7, 9),
                                 mc.samples = 512, useMC = TRUE)


aldex_results <- ALDEx2::aldex.ttest(aldex_ttest, paired.test = T, verbose = T, hist.plot = T) %>% 
  tibble::rownames_to_column("Genus") %>% 
  dplyr::select(Genus, p = wi.eBH) %>% 
  mutate(effect = aldex_effect$method$effect)

saveRDS(aldex_results, "results/aldex_Genus.RDS")
aldex_results <- readRDS("results/aldex_Genus.RDS")

sig_Genus <- aldex_results$Genus[aldex_results$p < 0.05]

df_genus <- readRDS("results/df_genus.RDS")

df_sig_genus  <- df_genus %>% 
  dplyr::select(Genus) %>% 
  distinct %>% 
  mutate(sig_Genus = Genus %in% sig_Genus) %>%
  left_join(aldex_results, by = "Genus") %>% 
  mutate(effect = case_when(effect < 0  & sig_Genus ~ effect_function(effect),
                            effect > 0 & sig_Genus ~ effect_function(effect),
                            TRUE ~ "n.s."),
         vertex = Genus,
         label = vertex,
         type = "Genus") %>% 
  dplyr::select(-Genus, -sig_Genus, -p)




# Family ------------------------------------------------------------------

seqtab <- readRDS("results/seqtab.family.RDS")
reads <- t(seqtab[1:162, ])
reads <- reads[rowSums(reads > 0) > 10, ]  
set.seed(1337)
aldex_glm <- aldex.clr(reads, mm, mc.samples = 512, denom="all", useMC = TRUE)
aldex_effect <- aldex.glm.effect(aldex_glm)


aldex_ttest <- ALDEx2::aldex.clr(reads,
                                 conds = substr(colnames(reads), 7, 9),
                                 mc.samples = 512, useMC = TRUE)
aldex_results <- ALDEx2::aldex.ttest(aldex_ttest, paired.test = T, verbose = T, hist.plot = T) %>% 
  tibble::rownames_to_column("Family") %>% 
  dplyr::select(Family, p = wi.eBH) %>% 
  mutate(effect = aldex_effect$method$effect)

saveRDS(aldex_results, "results/aldex_Family.RDS")
aldex_results <- readRDS("results/aldex_Family.RDS")

sig_Family <- aldex_results$Family[aldex_results$p < 0.05]

df_family <- readRDS("results/df_family.RDS")

df_sig_family  <- df_family %>% 
  dplyr::select(Family) %>% 
  distinct %>% 
  mutate(sig_Family = Family %in% sig_Family) %>%
  left_join(aldex_results, by = "Family") %>% 
  mutate(effect = case_when(effect < 0  & sig_Family ~ effect_function(effect),
                            effect > 0 & sig_Family ~ effect_function(effect),
                            TRUE ~ "n.s."),
         vertex = Family,
         label = vertex,
         type = "Family") %>% 
  dplyr::select(-Family, -sig_Family, -p)




# Class -------------------------------------------------------------------

seqtab <- readRDS("results/seqtab.class.RDS")
reads <- t(seqtab[1:162, ])
reads <- reads[rowSums(reads > 0) > 10, ]  
set.seed(1337)
aldex_glm <- aldex.clr(reads, mm, mc.samples = 512, denom="all", useMC = TRUE)
aldex_effect <- aldex.glm.effect(aldex_glm)


aldex_ttest <- ALDEx2::aldex.clr(reads,
                                 conds = substr(colnames(reads), 7, 9),
                                 mc.samples = 512, useMC = TRUE)
aldex_results <- ALDEx2::aldex.ttest(aldex_ttest, paired.test = T, verbose = T, hist.plot = T) %>% 
  tibble::rownames_to_column("Class") %>% 
  dplyr::select(Class, p = wi.eBH) %>% 
  mutate(effect = aldex_effect$method$effect)

saveRDS(aldex_results, "results/aldex_Class.RDS")
aldex_results <- readRDS("results/aldex_Class.RDS")
sig_Class <- aldex_results$Class[aldex_results$p < 0.05]

df_class <- readRDS("results/df_class.RDS")

df_sig_class  <- df_class %>% 
  dplyr::select(Class) %>% 
  distinct %>% 
  mutate(sig_Class = Class %in% sig_Class) %>%
  left_join(aldex_results, by = "Class") %>% 
  mutate(effect = case_when(effect < 0  & sig_Class ~ effect_function(effect),
                            effect > 0 & sig_Class ~ effect_function(effect),
                            TRUE ~ "n.s."),
         vertex = Class,
         label = vertex,
         type = "Class") %>% 
  dplyr::select(-Class, -sig_Class, -p)





# Order -------------------------------------------------------------------


seqtab <- readRDS("results/seqtab.order.RDS")
reads <- t(seqtab[1:162, ])
reads <- reads[rowSums(reads > 0) > 10, ]  
set.seed(1337)
aldex_glm <- aldex.clr(reads, mm, mc.samples = 512, denom="all", useMC = TRUE)
aldex_effect <- aldex.glm.effect(aldex_glm)


aldex_ttest <- ALDEx2::aldex.clr(reads,
                                 conds = substr(colnames(reads), 7, 9),
                                 mc.samples = 512, useMC = TRUE)
aldex_results <- ALDEx2::aldex.ttest(aldex_ttest, paired.test = T, verbose = T, hist.plot = T) %>% 
  tibble::rownames_to_column("Order") %>% 
  dplyr::select(Order, p = wi.eBH) %>% 
  mutate(effect = aldex_effect$method$effect)

saveRDS(aldex_results, "results/aldex_Order.RDS")

aldex_results <- readRDS("results/aldex_Order.RDS")

sig_Order <- aldex_results$Order[aldex_results$p < 0.05]

df_order <- readRDS("results/df_order.RDS")

df_sig_order  <- df_order %>% 
  dplyr::select(Order) %>% 
  distinct %>% 
  mutate(sig_Order = Order %in% sig_Order) %>%
  left_join(aldex_results, by = "Order") %>% 
  mutate(effect = case_when(effect < 0  & sig_Order ~ effect_function(effect),
                            effect > 0 & sig_Order ~ effect_function(effect),
                            TRUE ~ "n.s."),
         vertex = Order,
         label = vertex,
         type = "Order") %>% 
  dplyr::select(-Order, -sig_Order, -p)




# Phylum ------------------------------------------------------------------


seqtab <- readRDS("results/seqtab.phylum.RDS")
reads <- t(seqtab[1:162, ])
reads <- reads[rowSums(reads > 0) > 10, ]  
set.seed(1337)
aldex_glm <- aldex.clr(reads, mm, mc.samples = 512, denom="all", useMC = TRUE)
aldex_effect <- aldex.glm.effect(aldex_glm)


aldex_ttest <- ALDEx2::aldex.clr(reads,
                                 conds = substr(colnames(reads), 7, 9),
                                 mc.samples = 512, useMC = TRUE)
aldex_results <- ALDEx2::aldex.ttest(aldex_ttest, paired.test = T, verbose = T, hist.plot = T) %>% 
  tibble::rownames_to_column("Phylum") %>% 
  dplyr::select(Phylum, p = wi.eBH) %>% 
  mutate(effect = aldex_effect$method$effect)

saveRDS(aldex_results, "results/aldex_Phylum.RDS")
aldex_results <- readRDS("results/aldex_Phylum.RDS")

sig_Phylum <- aldex_results$Phylum[aldex_results$p < 0.05]

df_phylum <- readRDS("results/df_phylum.RDS")

df_sig_phylum  <- df_phylum %>% 
  dplyr::select(Phylum) %>% 
  distinct %>% 
  mutate(sig_Phylum = Phylum %in% sig_Phylum) %>%
  left_join(aldex_results, by = "Phylum") %>% 
  mutate(effect = case_when(effect < 0  & sig_Phylum ~ effect_function(effect),
                            effect > 0 & sig_Phylum ~ effect_function(effect),
                            TRUE ~ "n.s."),
         vertex = Phylum,
         label = vertex,
         type = "Phylum") %>% 
  dplyr::select(-Phylum, -sig_Phylum, -p)





# Domain ------------------------------------------------------------------

seqtab <- readRDS("results/seqtab.domain.RDS")
reads <- t(seqtab[1:162, ])
reads <- reads[rowSums(reads > 0) > 10, ]  
set.seed(1337)
aldex_glm <- aldex.clr(reads, mm, mc.samples = 512, denom="all", useMC = TRUE)
aldex_effect <- aldex.glm.effect(aldex_glm)


aldex_ttest <- ALDEx2::aldex.clr(reads,
                                 conds = substr(colnames(reads), 7, 9),
                                 mc.samples = 512, useMC = TRUE)
aldex_results <- ALDEx2::aldex.ttest(aldex_ttest, paired.test = T, verbose = T, hist.plot = T) %>% 
  tibble::rownames_to_column("Domain") %>% 
  dplyr::select(Domain, p = wi.eBH) %>% 
  mutate(effect = aldex_effect$method$effect)


saveRDS(aldex_results, "results/aldex_Domain.RDS")

aldex_results <- readRDS("results/aldex_Domain.RDS")

sig_Domain <- aldex_results$Domain[aldex_results$p < 0.05]

df_domain <- readRDS("results/df_domain.RDS")

df_sig_domain  <- df_domain %>% 
  dplyr::select(Domain) %>% 
  distinct %>% 
  mutate(sig_Domain = Domain %in% sig_Domain) %>%
  left_join(aldex_results, by = "Domain") %>% 
  mutate(effect = case_when(effect < 0  & sig_Domain ~ effect_function(effect),
                            effect > 0 & sig_Domain ~ effect_function(effect),
                            TRUE ~ "n.s."),
         vertex = Domain,
         label = vertex,
         type = "Domain") %>% 
  dplyr::select(-Domain, -sig_Domain, -p)



saveRDS(sig_ASV, "results/sig_ASV.RDS")
saveRDS(sig_Species, "results/sig_Species.RDS")
saveRDS(sig_Genus, "results/sig_Genus.RDS")
saveRDS(sig_Family, "results/sig_Family.RDS")
saveRDS(sig_Class, "results/sig_Class.RDS")
saveRDS(sig_Order, "results/sig_Order.RDS")
saveRDS(sig_Phylum, "results/sig_Phylum.RDS")
saveRDS(sig_Domain, "results/sig_Domain.RDS")



df_sig <- bind_rows(df_sig_species,
                    df_sig_genus,
                    df_sig_family,
                    df_sig_order,
                    df_sig_class,
                    df_sig_phylum,
                    df_sig_domain)

saveRDS(df_sig, "results/df_sig.RDS")

