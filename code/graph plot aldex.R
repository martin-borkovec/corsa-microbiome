
source("code/load_all.R")

# asv_abundance <- readRDS("results/.RDS")

{
  df_asv <- readRDS("results/df_asv.RDS")
  df_asv <- df_asv %>%  
    mutate(Genus = ifelse(is.na(Genus), paste0(ASV, "_G"), Genus),
           Family = ifelse(is.na(Family), paste0(ASV, "_F"), Family),
           Order = ifelse(is.na(Order), paste0(ASV, "_O"), Order),
           Class = ifelse(is.na(Class), paste0(ASV, "_C"), Class),
           Phylum = ifelse(is.na(Phylum), paste0(ASV, "_P"), Phylum),
           Domain = ifelse(is.na(Domain), paste0(ASV, "_D"), Domain)) %>%
    mutate(Genus = ifelse(Genus == "Incertae Sedis", "Incertae Sedis_G", Genus),
           Family = ifelse(Family == "Incertae Sedis", "Incertae Sedis_F", Family),
           Order = ifelse(Order == "Incertae Sedis", "Incertae Sedis_O", Order),
           Class = ifelse(Class == "Incertae Sedis", "Incertae Sedis_C", Class),
           Phylum = ifelse(Phylum  == "Incertae Sedis", "Incertae Sedis_P", Phylum)) %>%
    # Genus = ifelse(is.na(Genus), paste0(ASV, Genus), Genus),
    # Genus = ifelse(is.na(Genus), paste0(ASV, Genus), Genus))
    # head(n=500) %>% 
    identity
  
}


sig_Species <- readRDS("results/sig_Species.RDS")
sig_Genus <- readRDS("results/sig_Genus.RDS")
sig_Family <- readRDS("results/sig_Family.RDS")
sig_Order <- readRDS("results/sig_Order.RDS")
sig_Class <- readRDS("results/sig_Class.RDS")
sig_Phylum <- readRDS("results/sig_Phylum.RDS")
sig_Domain <- readRDS("results/sig_Domain.RDS")



plot_domain <- unique(
  c(
    df_asv$Domain[df_asv$Phylum %in% sig_Phylum],
    df_asv$Domain[df_asv$Order %in% sig_Order],
    df_asv$Domain[df_asv$Class %in% sig_Class],
    df_asv$Domain[df_asv$Family %in% sig_Family],
    df_asv$Domain[df_asv$Genus %in% sig_Genus],
    df_asv$Domain[df_asv$Species %in% sig_Species]))

plot_phylum <- unique(
  c(
    # df_asv$Phylum[df_asv$Domain %in% plot_domain],
    df_asv$Phylum[df_asv$Phylum %in% sig_Phylum],
    df_asv$Phylum[df_asv$Order %in% sig_Order],
    df_asv$Phylum[df_asv$Class %in% sig_Class],
    df_asv$Phylum[df_asv$Family %in% sig_Family],
    df_asv$Phylum[df_asv$Genus %in% sig_Genus],
    df_asv$Phylum[df_asv$Species %in% sig_Species]))

plot_class <- unique(
  c(
    # df_asv$Class[df_asv$Domain %in% plot_domain],
    #   df_asv$Class[df_asv$Phylum %in% plot_phylum],
    df_asv$Class[df_asv$Class %in% sig_Class],
    df_asv$Class[df_asv$Order %in% sig_Order],
    df_asv$Class[df_asv$Family %in% sig_Family],
    df_asv$Class[df_asv$Genus %in% sig_Genus],
    df_asv$Class[df_asv$Species %in% sig_Species]))

plot_order <- unique(
  c(
    # df_asv$Order[df_asv$Domain %in% plot_domain],
    # df_asv$Order[df_asv$Phylum %in% plot_phylum],
    # df_asv$Order[df_asv$Class %in% plot_class],
    df_asv$Order[df_asv$Order %in% sig_Order],
    df_asv$Order[df_asv$Family %in% sig_Family],
    df_asv$Order[df_asv$Genus %in% sig_Genus],
    df_asv$Order[df_asv$Species %in% sig_Species]))

plot_family <- unique(
  c(
    # df_asv$Family[df_asv$Domain %in% plot_domain],
    # df_asv$Family[df_asv$Phylum %in% plot_phylum],
    # df_asv$Family[df_asv$Class %in% plot_class],
    # df_asv$Family[df_asv$Order %in% plot_order],
    df_asv$Family[df_asv$Family %in% sig_Family],
    df_asv$Family[df_asv$Genus %in% sig_Genus],
    df_asv$Family[df_asv$Species %in% sig_Species]))

plot_genus <- unique(
  c(
    # df_asv$Genus[df_asv$Domain %in% plot_domain],
    # df_asv$Genus[df_asv$Phylum %in% plot_phylum],
    # df_asv$Genus[df_asv$Class %in% plot_class],
    # df_asv$Genus[df_asv$Order %in% plot_order],
    # df_asv$Genus[df_asv$Family %in% plot_family],
    df_asv$Genus[df_asv$Genus %in% sig_Genus],
    df_asv$Genus[df_asv$Species %in% sig_Species]))


plot_species <- unique(
  c(
    # df_asv$Genus[df_asv$Domain %in% plot_domain],
    # df_asv$Genus[df_asv$Phylum %in% plot_phylum],
    # df_asv$Genus[df_asv$Class %in% plot_class],
    # df_asv$Genus[df_asv$Order %in% plot_order],
    # df_asv$Genus[df_asv$Family %in% plot_family],
    # df_asv$Genus[df_asv$Genus %in% sig_Genus],
    df_asv$Species[df_asv$Species %in% sig_Species]))

# plot_ASV <- unique(
#   c(df_sig_asv$vertex[df_sig_asv$Genus %in% plot_genus],
#     df_sig_asv$vertex[df_sig_asv$Family %in% plot_family],
#     df_sig_asv$vertex[df_sig_asv$Order %in% plot_order]))

plot_vertices <- c(plot_domain,
                   plot_phylum,
                   plot_class,
                   plot_order,
                   plot_family,
                   plot_genus,
                   plot_species)






edges <- rbind(
  # matrix(c("root", "root",
  #          "Archaea", "Bacteria"), ncol = 2),
  select(df_asv, Domain, Phylum) %>%
    distinct %>% as.matrix,
  select(df_asv, Phylum, Class) %>%
    distinct %>% as.matrix,
  select(df_asv, Class, Order) %>%
    distinct %>% as.matrix,
  select(df_asv, Order, Family) %>%
    distinct %>% as.matrix,
  
  select(df_asv, Family, Genus) %>%
    distinct %>% as.matrix,
  
  select(df_asv, Genus, Species) %>%
    distinct %>% as.matrix
)

edges <- edges[edges[, 2] %in% plot_vertices &
                 edges[, 1] %in% plot_vertices, ]

edges <- data.frame(parent = edges[, 1], child = edges[, 2], draw = TRUE)

# names(edges) <- c("", "child")

# plot_order[!plot_order %in% edges$parent]

# childless_orders <- plot_order[!plot_order %in% edges$parent]
childless_families <- plot_family[!plot_family %in% edges$parent]
childless_genera <- plot_genus[!plot_genus %in% edges$parent]

edges <- rbind(edges, 
               # data.frame(parent = c(childless_orders,
               #                       paste0(childless_orders, "F"),
               #                       paste0(childless_orders, "G")), 
               #            child = c(paste0(childless_orders, "F"),
               #                      paste0(childless_orders, "G"),
               #                      paste0(childless_orders, "S")),
               #            draw = FALSE),
               
               
               
               
               data.frame(parent = c(childless_families,
                                     paste0(childless_families, "G")), 
                          child = c(paste0(childless_families, "G"),
                                    paste0(childless_families, "S")),
                          draw = FALSE),
               
               
               
               
               data.frame(parent = c(childless_genera), 
                          child = c(paste0(childless_genera, "S")),
                          draw = FALSE))




# 
# edges <- edges %>% 
#   add_row(Domain = "Gastranaerophilales", Phylum = paste0("Gastranaerophilales", 1), draw = FALSE) %>% 
#   add_row(Domain = paste0("Gastranaerophilales", 1), Phylum = paste0("Gastranaerophilales", 2), draw = FALSE) %>% 
#   add_row(Domain = paste0("Gastranaerophilales", 2), Phylum = paste0("Gastranaerophilales", 3), draw = FALSE) %>% 
  # identity
# 

# plot_Genus <- unique(c(sig_Genus, df_asv$Genus[df_asv$ASV %in% sig_ASV]))

# unique(edges[,1], edges[,2])

df_sig <- readRDS("results/df_sig.RDS")

df_sig$effect <- as.numeric(df_sig$effect)

vertices <- data.frame(vertex = unique(c(edges[,1], edges[,2]))) %>% 
  left_join(df_sig, by = "vertex") %>%
  group_by(type) %>% 
  mutate(label_id = case_when(vertex == "root" ~ "",
                              type == "ASV" ~ paste0("  ", label, "  "),
                              grepl("ASV_", vertex) & grepl("G", vertex) ~ " G:unk.  ",
                              grepl("ASV_", vertex) & grepl("F", vertex) ~ " F:unk.  ",
                              grepl("ASV_", vertex) & grepl("O", vertex) ~ " O:unk.  ",
                              grepl("ASV_", vertex) & grepl("C", vertex) ~ " C:unk.  ",
                              grepl("ASV_", vertex) & grepl("P", vertex) ~ " P:unk.  ",
                              grepl("ASV_", vertex) & grepl("D", vertex) ~ " D:unk.  ",
                              # type == "Genus" ~ vertex,
                              TRUE ~ paste0("  ", substr(type, 1, 1), formatC(1:length(type), width = 2, flag = "0"), ":",substr(vertex, 1, 4), "  "))) %>% 
  ungroup




# vertices$effect[vertices$effect == "n.s."] <- NA

vertices$effect_num <- vertices$effect
vertices$effect[is.na(vertices$effect)] <- 0
vertices$effect <- cut(vertices$effect, breaks = c(-0.568, -0.355, -0.142, -0.05, 0.05, 0.142, 0.355, 0.568))

paste(levels(vertices$effect))


saveRDS(vertices, "results/vertices.RDS")

# limits = c("(-0.65,-0.35]", "(-0.35,-0.05]", "(-0.05,0.05]", "(0.05,0.35]", "(0.35,0.65]"),


# summary(vertices$effect)
# boxplot(vertices$effect)
# plot(density(vertices$effect %>% na.omit()))
# 
# sort(abs(vertices$effect))


# source("code/graph adjustments.R")

gr <- igraph::graph_from_data_frame(d = edges, vertices = dplyr::select(ungroup(vertices), vertex, label_id, effect, type))

# 
# 
# ragg::agg_png("plots/presentation/abundance_shift.png",
#               res=300, width=3600, height=3000)
# 
# p_gr <- 
#   ggraph(gr, layout = "dendrogram", circular = TRUE) + 
#   
#   annotate("path",
#            x=1/6*cos(seq(0,2*pi,length.out=100)),
#            y=1/6*sin(seq(0,2*pi,length.out=100)),
#            col = "grey90") +
#   
#   annotate("path",
#            x=2/6*cos(seq(0,2*pi,length.out=100)),
#            y=2/6*sin(seq(0,2*pi,length.out=100)),
#            col = "grey90") +
#   
#   annotate("path",
#            x=3/6*cos(seq(0,2*pi,length.out=100)),
#            y=3/6*sin(seq(0,2*pi,length.out=100)),
#            col = "grey90") +
#   
#   annotate("path",
#            x=4/6*cos(seq(0,2*pi,length.out=100)),
#            y=4/6*sin(seq(0,2*pi,length.out=100)),
#            col = "grey90") +
#   
#   annotate("path",
#            x=5/6*cos(seq(0,2*pi,length.out=100)),
#            y=5/6*sin(seq(0,2*pi,length.out=100)),
#            col = "grey90") +
#   
#   annotate("path",
#            x=6/6*cos(seq(0,2*pi,length.out=100)),
#            y=6/6*sin(seq(0,2*pi,length.out=100)),
#            col = "grey90") +
#   
#   
#   geom_edge_elbow(aes(filter = draw == "TRUE"),
#                   edge_colour = "grey50") + 
#   
#   geom_node_point(aes(filter = !is.na(type)),
#                   col = "white",
#                   size = 5
#   ) +
#   
#   geom_node_point(aes(fill = effect,
#                       # alpha = effect,
#                       filter = !is.na(type)
#                       # size = type
#   ),
#   col = "grey20",
#   shape = 21,
#   size = 5
#   ) +
#   # geom_node_text(aes(label = label_id,
#   # geom_node_text(aes(label = round(-((-node_angle(x, y)+90)%%180)+90),
#   geom_node_text(aes(label = label_id,
#                      filter = !is.na(type),
#                      angle =  -((-node_angle(x, y)+90)%%180)+90,
#                      # angle =  -((-node_angle(x, y)+90)%%180)+100,
#                      
#                      # col = change
#   ),
#   hjust = "outward",
#   vjust = 0,
#   # nudge_y = 0.01,
#   fontface = "bold",
#   size = 3
#   ) +
#   coord_fixed() +
#   scale_x_continuous(expand = c(0.075, 0.075)) +
#   scale_y_continuous(expand = c(0.075, 0.075)) +
#   theme_bw() +
#   theme(line = element_blank(),
#         axis.title = element_blank(),
#         rect = element_blank(),
#         axis.text = element_blank(),
#         legend.position = "right") +
#   theme(legend.title = element_text(face = "bold")) +
#   # scale_fill_manual("Bacterial abundance in \nFIT compared to Norgen",
#   scale_fill_manual("Bacterial abundance",
#                     limits = c( "(-0.568,-0.355]", "(-0.355,-0.142]", "(-0.142,-0.05]",  "(-0.05,0.05]",  "(0.05,0.142]",    "(0.142,0.355]", "(0.355,0.568]"),
#                     # labels = c("much higher", "higher", "slightly higher", "not significant",  "slightly lower", "lower", "much lower"),
#                     labels = c("FIT much higher", "FIT higher", "FIT slightly higher", "not significantly different", "Norgen slightly higher", "Norgen higher", "Norgen much higher"),
#                     # values = c(cols[6], cols[1], "lightskyblue", "white", "#FFFF99", cols[2], "goldenrod3")) +
#                     values = c(cols[6], cols[1], "lightskyblue", "white", "#FFFF99", cols[2], "darkgoldenrod4")) +
#   theme(legend.title = element_text(face = "bold")) +
# NULL
# 
# p_gr 
# dev.off()
# 






