library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)

get_ICC_inter <- function(rank, plot = TRUE, size_range = c(1, 2), stroke_size = 0.5) {
  
  data <- readRDS(paste0("results/df_", tolower(rank),".RDS")) %>% 
    filter(!volunteer)
  
  if (rank == "ASV") {

    data <- data %>% 
      group_by(ASV) %>% 
      mutate(prevalence = mean(abundance > 0)) %>% 
      ungroup %>% 
      filter(prevalence > 0.05)
    
  }
  
  
  
  ICCS <- data.frame("taxon" = character(),
                     "ICC" = numeric(0),
                     "lower bound" = numeric(0),
                     "upper bound"= numeric(0),
                     check.names = FALSE)
  
  # names(ICCS)[1] <- rank
  
  for (taxon_i in unique(data[[rank]])) {
    
    ICCS <- rbind(ICCS, 
                  cbind(data.frame(taxon = taxon_i),
                        ICC(
                          cbind(data %>% 
                                  filter(!!sym(rank) == taxon_i) %>% 
                                  filter(method == "FIT") %>% 
                                  pull(clr),
                                data %>%
                                  filter(!!sym(rank) == taxon_i) %>% 
                                  filter(method == "Norgen") %>% 
                                  pull(clr))
                        )$results[3, c(2,7,8)]))
  }
  
  
  
  ICC <- data %>% 
    filter(abundance > 0) %>%
    filter(!volunteer) %>% 
    rename(taxon = !!sym(rank)) %>% 
    group_by(taxon) %>% 
    summarise(sum_log_abundance = sum(log(abundance))) %>%
    # summarise(sum_log_abundance = mean(clr)) %>%
    left_join(ICCS) %>% 
    mutate(rank = rank,
           type = "inter") %>% 
    left_join(select(df_asv, taxon = !!sym(rank), Phylum = Phylum) %>%  distinct) %>% 
    mutate(Phylum.col = case_when(Phylum == "Firmicutes" ~ "Firmicutes",
                                  Phylum == "Bacteroidota" ~ "Bacteroidota",
                                  Phylum == "Actinobacteriota" ~ "Actinobacteriota",
                                  TRUE ~ "other"))
  
  if(plot) {
    return(
      # ggplot(ICC) +
      #   geom_errorbar(aes(x = sum_log_abundance, ymin = `lower bound`, ymax = `upper bound`), alpha = 0.25) +
      #   geom_point(aes(x = sum_log_abundance, y = ICC)) +
      #   geom_hline(yintercept = 0.9)
      
      ICC %>% 
        
        ggplot(aes(col = Phylum.col, fill = Phylum.col)) +
        geom_hline(yintercept = 0.9, linetype = "dashed") +
        geom_errorbar(aes(x = sum_log_abundance, ymin = `lower bound`, ymax = `upper bound`), alpha = 0.5, size = 0.2) +
        xlab("Summed log abundance") +
        geom_point(aes(x = sum_log_abundance, y = ICC, size = sum_log_abundance), shape = 21, color = "black", stroke = stroke_size) +
        scale_size_continuous(range = size_range, guide = "none") +
        theme_bw() +
        theme(legend.position = "top")
      
    )
  }
  
  ICC
  
  
}



