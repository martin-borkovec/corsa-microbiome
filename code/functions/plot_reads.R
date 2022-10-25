plot_reads <- function(rank, plot = TRUE) {
  
  data <- readRDS(paste0("results/df_", tolower(rank),".RDS")) %>% 
    filter(!volunteer)
  
  plot_dat <- inner_join(
    df_asv %>%
      filter(!volunteer) %>% 
      group_by(patID, method) %>% 
      mutate(pat_reads = sum(abundance)) %>% 
      summarise(asv_reads = unique(pat_reads))
    ,
    
    data %>% 
      group_by(patID, method) %>% 
      mutate(pat_reads = sum(abundance)) %>% 
      summarise(rank_reads = unique(pat_reads)),
    by = c("patID", "method")) %>% 
    mutate(classified = rank_reads / asv_reads,
           rank = rank,
           type = "reads") %>% 
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