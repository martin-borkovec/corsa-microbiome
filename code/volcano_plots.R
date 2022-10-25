aldex_results <- 
  
  rbind(mutate(readRDS("results/aldex_ASVs.RDS"), rank = "ASV") %>% rename(Taxon = ASV),
        mutate(readRDS("results/aldex_Species.RDS"), rank = "Species") %>% rename(Taxon = Species),
        mutate(readRDS("results/aldex_Genus.RDS"), rank = "Genus") %>% rename(Taxon = Genus),
        mutate(readRDS("results/aldex_Family.RDS"), rank = "Family") %>% rename(Taxon = Family),
        mutate(readRDS("results/aldex_Order.RDS"), rank = "Order") %>% rename(Taxon = Order),
        mutate(readRDS("results/aldex_Class.RDS"), rank = "Class") %>% rename(Taxon = Class),
        mutate(readRDS("results/aldex_Phylum.RDS"), rank = "Phylum") %>% rename(Taxon = Phylum)) %>% 
  mutate(rank = factor(rank, levels = c("ASV", "Species", "Genus", "Family", "Order", "Class", "Phylum")))



ggplot(data=aldex_results, aes(x=effect, y=-log(p), fill = p < 0.05) ) + geom_point(shape = 21) +
  facet_wrap(~rank, ncol = 2) +
  theme_bw()
