source("code/SSQs per method.R")
source("code/SSQs tern plot.R")

p_tern_ssq <- tern_dat %>% 
  select(ASV, Subject = subject, `Sample type` = method, Residual = residual) %>% 
  pivot_longer(-ASV) %>%
  ggplot() +
  geom_boxplot(aes(x = name, y = value, fill = name), col = "black", outlier.shape = 21, outlier.stroke = 0, outlier.size = 0.75, size = 0.1) +
  scale_fill_manual("Component", values = c("grey20", "seagreen", rgb2hex(213, 94, 0))) +
  ylab("Proportion explained") +
  xlab("Variance component") +
  theme_bw(base_size = 7) +
  theme(legend.position = "none") +
  theme(legend.title = element_text(face = "bold")) + 
  NULL






library(ggpubr)

cairo_pdf(file = "plots/Figure4.pdf", height = 7.85, width = 6.7)

ggtern::grid.arrange(

p_tern +
  theme(legend.position = c(0.15, 0.9)) +
  geom_text_viewport(label = "A", x = -1, y = 1, clip = "off", fontface = "bold", size = 3.5),



grid::nullGrob(),
nrow = 2,
ncol = 1,
heights = c(7, 2.5))  

print(ggarrange(p_tern_ssq, p_SSQ  + theme(legend.position = "bottom"), labels = c("B", "C"), font.label = label_par, widths = c(3,2)),
      vp = grid::viewport(y = 0.18, height = 0.36))



dev.off()

library(cowplot)


# print(bb, vp = grid::viewport(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
#                               width = unit(0.5, "npc"), height = unit(0.5, "npc")))

# 
# 
# arrangeGrob(p_tern, p_tern_ssq) %>% 
# grid::grid.draw()
# 






