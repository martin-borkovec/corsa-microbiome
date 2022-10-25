library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)

get_ICC_inter_vol <- function(rank, plot = TRUE) {
  
  data <- readRDS(paste0("results/df_", tolower(rank),".RDS")) %>% 
    filter(volunteer)
  
  
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
                                    filter(method == "FIT") %>% 
                                    group_by(patID) %>%
                                    summarise(clr = mean(clr)) %>% 
                                    pull(clr),
                                  data %>%
                                    filter(!!sym(rank) == taxon_i) %>% 
                                    filter(method == "Norgen") %>% 
                                    group_by(patID) %>%
                                    summarise(clr = mean(clr)) %>% 
                                    pull(clr))
                          )$results[6, c(2,7,8)]))
  }
  
  
  
  ICC <- data %>% 
    filter(abundance > 0) %>% 
    filter(volunteer) %>% 
    rename(taxon = !!sym(rank)) %>% 
    group_by(taxon) %>% 
    summarise(sum_log_abundance = sum(log(abundance))) %>% 
    left_join(ICCs) %>% 
    mutate(rank = rank,
           type = "inter")
  
  if(plot) {
    return(
      ggplot(ICC) +
        geom_errorbar(aes(x = sum_log_abundance, ymin = `lower bound`, ymax = `upper bound`), alpha = 0.25) +
        geom_point(aes(x = sum_log_abundance, y = ICC)) +
        geom_hline(yintercept = 0.9))
  }
  
  ICC
  
  
}



