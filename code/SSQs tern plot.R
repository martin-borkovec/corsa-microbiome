library(dplyr)

aldex_ASVs <- readRDS("results/aldex_ASVs_list.RDS")

df_asv <- readRDS("results/df_asv.RDS")
df_asv_V <- df_asv %>% 
  filter(volunteer) %>% 
  filter(ASV %in% aldex_ASVs) %>% 
  group_by(ASV) %>% 
  mutate(prevalence = mean(abundance > 0)) %>% 
  mutate(reads = sum(abundance)) %>% 
  ungroup %>% 
  filter(prevalence >= 3/30)

asvs <- unique(df_asv_V$ASV)

SSQs <- data.frame(ASV = character(),
                   total = numeric(),
                   subject = numeric(),
                   method = numeric(),
                   residual = numeric(),
                   p = numeric())



for (asv in asvs) {
  lm1 <- df_asv_V %>% 
    filter(ASV == asv) %>% 
    lm(formula = clr ~ patID + method)
  
  SSQs <- rbind(SSQs,
                data.frame(ASV = asv,
                           total = sum(anova(lm1)[,2]),
                           subject = anova(lm1)[1,2] / sum(anova(lm1)[,2]),
                           method = anova(lm1)[2,2] / sum(anova(lm1)[,2]),
                           residual = anova(lm1)[3,2] / sum(anova(lm1)[,2]),
                           p = anova(lm1)[2,5]))
  
}


sig_ASV <- readRDS("results/sig_ASV.RDS")


SSQs$sig <- SSQs$ASV %in% sig_ASV

# boxplot(SSQs$p  ~ SSQs$sig)
# 
# boxplot(SSQs$method)
# boxplot(SSQs$subject)
# 
# boxplot(SSQs$method, SSQs$subject, SSQs$residual)
# 
# 
# 
# summary(SSQs$method)
# quantile(SSQs$method, probs = 0.98)
# 
# table(SSQs$sig)
# 
# df_asv_V %>% 
#   filter(ASV == "ASV_1410") %>% 
#   # pull(abundance) %>% 
#   # filter(Genus %in% sig_Genus) %>%
#   # filter(ASV %in% sig_ASV) %>%
#   # ggplot(aes(y = log(abundance+1)/log(sample_reads), x = patID)) +
#   ggplot(aes(y = clr, x = patID)) +
#   geom_line(aes(group = patID)) +
#   # scale_shape(limits = rev) +
#   # scale_shape_manual(values = c(21, 16)) +
#   geom_point(aes(col = method, shape = matching), size = 1) + 
#   # facet_wrap(~ASV, ncol = 2, nrow =25 ) +
#   facet_wrap(~ASV) +
#   theme(legend.position = "top")


library(ggtern)


tern_dat <- SSQs %>% 
  left_join(
    
    df_asv_V %>% 
      group_by(ASV) %>% 
      summarise(prevalence = unique(prevalence),
                reads = unique(reads),
                Genus = unique(Genus),
                Family = unique(Family)
                
                  ),
    by = "ASV") %>% 
  filter(prevalence >= 3/30)


# ragg::agg_png("plots/SSQ.png",
#               # width = 2000,
#               # height = 1000,
#               # pointsize = 12,
#               # res = 500)
#               res=300, width=2500, height=1600, pointsize=12)

p_tern <- ggtern(data=tern_dat,
       aes(subject, method, residual, fill = sig)) + 
  theme_custom(base_size = 12,
               col.T = "seagreen",
               col.L = rgb2hex(213, 94, 0),
               col.R = "grey20") +
  theme_notitles() +
  labs(x = "Subject",
       y = "Sample type",
       z = "Residual",
       fill = "Effect of sample type \nin patient-samples") +
  theme_nomask() +
  scale_fill_manual(values = c("lightgoldenrod", "darkmagenta"), label = c("Non-significant", "Significant")) +
  theme(legend.title = element_text(face = "bold", size = 7),
        legend.text = element_text(size = 6),
        tern.axis.vshift = 0.15,
        tern.panel.background = element_rect(fill = "white"), 
        tern.panel.grid.minor = element_line(color = "gray90"),
        axis.text = element_text(size = 6),
        tern.axis.arrow.show = TRUE,
        legend.key = element_rect("white"),
        legend.key.height = unit(3, "mm")
        ) +
  # theme_tropical() +
  # guides(fill = guide_legend()) +
  
  geom_point(shape = 21, alpha = 1, size = 1, stroke = 0.2) +
  # coord_tern(expand = FALSE) +
  # geom_point(shape = 21) +
  # labs(title="Title") +
  # tern_limits(T= 0.6, L=1, R=0.6) +
  NULL

p_tern

# dev.off()

# 
# ggtern(data=tern_dat %>%  filter(prevalence > 1/30, residual < 0.2, subject > 0.80),
#        aes(subject, method, residual, fill = p < 0.05)) + 
#   theme_bvbw() +
#   theme_nomask() +
#   # theme_tropical() +
#   geom_point(shape = 21, alpha = 0.75, size = 3) +
#   labs(title="Zoom") +
#   tern_limits(T= 0.2, L=1, R=0.2) +
#   NULL
# 
# 
# ggplot(tern_dat) + 
#   geom_boxplot(aes(x = 1, y = round(prevalence, 2)), varwidth = T)
# 
# 
# ggplot(tern_dat) + 
#   geom_boxplot(aes(x = factor(round(prevalence, 2)), y = method), varwidth = T)
# 
# ggplot(tern_dat) + 
#   geom_boxplot(aes(x = factor(round(prevalence, 2)), y = subject), varwidth = T)
# 
# ggplot(tern_dat) + 
#   geom_boxplot(aes(x = factor(round(prevalence, 2)), y = residual), varwidth = T)
# 
# 
# 
# 
# SSQs$subject + SSQs$method + SSQs$residual

