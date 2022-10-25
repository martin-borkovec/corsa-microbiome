plot_sequences <- function(rank, plot = TRUE) {
  
plot_dat <- inner_join(
    df_asv %>%
      group_by(patID, method) %>%
      filter(abundance > 0) %>% 
      summarise(pat_sequences = n()) 
    ,
    df_asv %>% 
      group_by(patID, method) %>%
      filter(abundance > 0) %>% 
      filter(!is.na(!!sym(rank))) %>% 
      # mutate(pat_sequences = n()) %>% 
      # summarise(rank_reads = length(unique(!!sym(rank)))),
      summarise(rank_sequences = n()),
    by = c("patID", "method")) %>% 
    mutate(classified = rank_sequences / pat_sequences,
           rank = rank,
           type = "sequences") %>% 
  select(method, classified, rank, type)
    
  
  
  
  if(plot) {
    return(
      ggplot(plot_dat) +
        geom_boxplot(aes(x = method, y = classified, fill = method)) +
        ylim(0,1) +
        theme_bw() +
        theme(legend.position = "top") +
        scale_fill_manual(values = cols[1:2])
    )
  }
  
  plot_dat
  
}