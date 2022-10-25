make_scatterplot <- function(rank, plot = TRUE, size_range = c(1, 2), stroke_size = 0.5) {
  
  data <- readRDS(paste0("results/df_", tolower(rank),".RDS")) %>% 
    filter(!volunteer) %>% 
    left_join(distinct(select(df_asv, !!rank, Phylum)))
  
  if (rank == "ASV") {
    
    data <- data %>% 
      group_by(ASV) %>% 
      mutate(prevalence = mean(abundance > 0)) %>% 
      ungroup %>% 
      filter(prevalence > 0.05)
    
  }
  
  
  plot_dat <- data %>% 
    mutate("Taxon" = !!sym(rank)) %>% 
    filter(!is.na(Taxon)) %>% 
    # filter(abundance > 0) %>% 
    group_by(Taxon, method) %>%
    # summarise(mean_clr = sum(log(abundance)),
      summarise(mean_clr = mean(clr),
              Phylum = unique(Phylum)) %>%
    ungroup %>% 
    mutate(Phylum.col = case_when(Phylum == "Firmicutes" ~ "Firmicutes",
                                  Phylum == "Bacteroidota" ~ "Bacteroidota",
                                  Phylum == "Actinobacteriota" ~ "Actinobacteriota",
                                  TRUE ~ "others"),
           rank = rank) %>% 
    pivot_wider(c(Taxon, Phylum.col, rank), names_from = method, values_from = mean_clr)
    
    
    if (plot) {
      return(
        ggplot(plot_dat) +
          geom_point(aes(FIT, Norgen, fill = Phylum.col, size = (FIT + Norgen) /2),
                     alpha = 1, shape = 21, stroke = stroke_size) +
          geom_abline(slope = 1, intercept = 0) +
          labs(x = "FIT mean CLR abundance",
               y = "Norgen mean CLR abundance") +
          scale_size_continuous(range = size_range, guide = "none") +
          theme_bw() +
          NULL
      )
    }
  
  plot_dat
  
}
