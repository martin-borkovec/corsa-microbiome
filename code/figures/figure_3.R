source("code/load_all.R")
source("code/PCA volunteers.R")
source("code/graph plot aldex.R")

p_gr <- ggraph(gr, layout = "dendrogram", circular = TRUE) + 
  
  annotate("path",
           x=1/6*cos(seq(0,2*pi,length.out=100)),
           y=1/6*sin(seq(0,2*pi,length.out=100)),
           col = "grey90") +
  
  annotate("path",
           x=2/6*cos(seq(0,2*pi,length.out=100)),
           y=2/6*sin(seq(0,2*pi,length.out=100)),
           col = "grey90") +
  
  annotate("path",
           x=3/6*cos(seq(0,2*pi,length.out=100)),
           y=3/6*sin(seq(0,2*pi,length.out=100)),
           col = "grey90") +
  
  annotate("path",
           x=4/6*cos(seq(0,2*pi,length.out=100)),
           y=4/6*sin(seq(0,2*pi,length.out=100)),
           col = "grey90") +
  
  annotate("path",
           x=5/6*cos(seq(0,2*pi,length.out=100)),
           y=5/6*sin(seq(0,2*pi,length.out=100)),
           col = "grey90") +
  
  annotate("path",
           x=6/6*cos(seq(0,2*pi,length.out=100)),
           y=6/6*sin(seq(0,2*pi,length.out=100)),
           col = "grey90") +
  
  
  geom_edge_elbow(aes(filter = draw == "TRUE"),
                  edge_colour = "grey50", edge_width = 0.3) + 
  
  geom_node_point(aes(filter = !is.na(type)),
                  col = "white",
                  size = 2
  ) +
  
  geom_node_point(aes(fill = effect,
                      filter = !is.na(type)
  ),
  col = "grey20",
  shape = 21,
  size = 2
  ) +
  geom_node_text(aes(label = label_id,
                     filter = !is.na(type),
                     angle =  -((-node_angle(x, y)+90)%%180)+90,
  ),
  hjust = "outward",
  vjust = 0,
  fontface = "bold",
  size = 1.7
  ) +
  coord_fixed() +
  scale_x_continuous(expand = c(0.075, 0.075)) +
  scale_y_continuous(expand = c(0.075, 0.075)) +
  theme_bw(base_size = 7) +
  theme(line = element_blank(),
        axis.title = element_blank(),
        rect = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.5, 0.96)) +
  theme(legend.title = element_text(face = "bold")) +
  guides(fill = guide_legend(title.position = "top", nrow = 2, keyheight = 0.5, keywidth = 1))+
  
  scale_fill_manual("Bacterial abundance",
                    limits = c( "(-0.568,-0.355]", "(-0.355,-0.142]", "(-0.142,-0.05]",  "(-0.05,0.05]",  "(0.05,0.142]",    "(0.142,0.355]", "(0.355,0.568]"),
                    labels = c("FIT much higher", "FIT higher", "FIT slightly higher", "No significant difference", "Norgen slightly higher", "Norgen higher", "Norgen much higher"),
                    values = c(cols[6], cols[1], "lightskyblue", "white", "#FFFF99", cols[2], "darkgoldenrod4")) +
  
  theme(legend.title = element_text(face = "bold"))

p_gr
  
cairo_pdf(file = "plots/Figure3.pdf", height = 7.85, width = 6.7)

ggpubr::ggarrange(p_gr + coord_fixed(ylim = c(-0.8, 1)),
ggpubr::ggarrange(p2_pca, p3_pca, common.legend = T),
nrow = 2, heights = c(3, 1.2), labels = "AUTO", font.label = label_par)

dev.off()

