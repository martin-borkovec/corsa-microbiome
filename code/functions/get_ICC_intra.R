library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)

get_ICC_intra <- function(rank, sample_type, plot = TRUE) {
  data <- readRDS(paste0("results/df_", tolower(rank),".RDS")) %>% 
    filter(method == sample_type) %>% 
    filter(volunteer) %>% 
    mutate(sample = substr(LNR, 5, 5)) 
  
  
  ICCs <- data.frame("taxon" = character(),
                       "ICC" = numeric(0),
                       "lower bound" = numeric(0),
                       "upper bound"= numeric(0),
                       check.names = FALSE)
  
  # names(ICCs)[1] <- rank
  
  for (taxon_i in unique(data[[rank]])) {
    
    ICCs <- rbind(ICCs,
                  cbind(data.frame(taxon = taxon_i),
                        ICC(
                          cbind(data %>% 
                                  filter(!!sym(rank) == taxon_i) %>% 
                                  filter(sample == 1) %>% 
                                  pull(clr),
                                data %>% 
                                  filter(!!sym(rank) == taxon_i) %>% 
                                  filter(sample == 2) %>% 
                                  pull(clr),
                                data %>% 
                                  filter(!!sym(rank) == taxon_i) %>% 
                                  filter(sample == 3) %>% 
                                  pull(clr)))$results[2, c(2,7,8)]
                
          ))
    
    
  }
  
  
  
  ICC <- data %>% 
    filter(abundance > 0) %>% 
    rename(taxon = !!sym(rank)) %>% 
    group_by(taxon) %>% 
    summarise(sum_log_abundance = sum(log(abundance))) %>% 
    left_join(ICCs) %>% 
    mutate(rank = rank,
           type = sample_type)
  
  if(plot) {
    return(
      ggplot(ICC) +
        geom_errorbar(aes(x = sum_log_abundance, ymin = `lower bound`, ymax = `upper bound`), alpha = 0.25) +
        geom_point(aes(x = sum_log_abundance, y = ICC)) +
        geom_hline(yintercept = 0.9))
  }
  
  ICC
  
  
}



