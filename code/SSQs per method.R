library(ggplot2)
library(dplyr)

df_asv <- readRDS("results/df_asv.RDS")

df_asv_V_Nor <- df_asv %>% 
  filter(volunteer) %>% 
  filter(method == "Norgen") %>% 
  group_by(ASV) %>% 
  mutate(prevalence = mean(abundance > 0)) %>% 
  mutate(reads = sum(abundance)) %>% 
  ungroup %>% 
  filter(prevalence > 2/15)

df_asv_V_FIT <- df_asv %>% 
  filter(volunteer) %>% 
  filter(method == "FIT") %>% 
  group_by(ASV) %>% 
  mutate(prevalence = mean(abundance > 0)) %>% 
  mutate(reads = sum(abundance)) %>% 
  ungroup %>% 
  filter(prevalence > 2/15)


asvs_Nor <- unique(df_asv_V_Nor$ASV)
asvs_FIT <- unique(df_asv_V_FIT$ASV)

length(asvs_Nor)
length(asvs_FIT)

mean(asvs_Nor %in% asvs_FIT)

SSQs_Nor <- data.frame(ASV = character(),
                   total = numeric(),
                   subject = numeric(),
                   residual = numeric(),
                   p = numeric())


for (asv in asvs_Nor) {
  lm1 <- df_asv_V_Nor %>% 
    filter(ASV == asv) %>% 
    lm(formula = clr ~ patID)
  
  SSQs_Nor <- rbind(SSQs_Nor,
                data.frame(ASV = asv,
                           total = sum(anova(lm1)[,2]),
                           subject = anova(lm1)[1,2] / sum(anova(lm1)[,2]),
                           residual = anova(lm1)[2,2] / sum(anova(lm1)[,2]),
                           p = anova(lm1)[1,5]))
}


SSQs_FIT <- data.frame(ASV = character(),
                       total = numeric(),
                       subject = numeric(),
                       residual = numeric(),
                       p = numeric())


for (asv in asvs_FIT) {
  lm1 <- df_asv_V_FIT %>% 
    filter(ASV == asv) %>% 
    lm(formula = clr ~ patID)
  
  SSQs_FIT <- rbind(SSQs_FIT,
                    data.frame(ASV = asv,
                               total = sum(anova(lm1)[,2]),
                               subject = anova(lm1)[1,2] / sum(anova(lm1)[,2]),
                               residual = anova(lm1)[2,2] / sum(anova(lm1)[,2]),
                               p = anova(lm1)[1,5]))
}



# SSQs


# cols <- ggsci::pal_jco()(10)

# ragg::agg_png("plots/Subject SSQ.png",
#               # width = 2000,
#               # height = 1000,
#               # pointsize = 12,
#               # res = 500)
#               res=300, width=2500, height=1600, pointsize=12)

p_SSQ <- rbind(cbind(method = "FIT", SSQs_FIT), cbind(method = "Norgen", SSQs_Nor)) %>% 
  ggplot() +
  # geom_boxplot(aes(x = method, fill = method, y = subject), outlier.shape = 21, outlier.stroke = 0 
  #              ) + 
  geom_split_violin(aes(x = 1, fill = method, y = subject), size = 0.1) + 
  scale_fill_manual("Sample type", values = cols[1:2]) +
  labs(x = "",
       y = "Proportion explained by subject") +
  theme_bw(base_size = 7) +
  theme(legend.title = element_text(face = "bold"),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x  = element_blank(),
        legend.key.size = unit(3, "mm")) + 
  # geom_hline(yintercept = 0.62) 
  NULL



#p_SSQ

# dev.off()

# wilcox.test(SSQs_FIT$subject,SSQs_Nor$subject)
# boxplot(df_asv_V_FIT$prevalence, df_asv_V_Nor$prevalence)

# 
# asvs_set <- SSQs_FIT$ASV[SSQs_FIT$subject < 0.63 & SSQs_FIT$subject > 0.61]
# #
# df_asv_V_FIT %>%
#   filter(abundance > 0) %>%
#   filter(ASV == asvs_set[7]) %>%
#   select(sampleID, prevalence)
# 
# 
rbind(cbind(method = "FIT", SSQs_FIT), cbind(method = "Norgen", SSQs_Nor)) %>%
  group_by(method) %>%
  summarise(mean(subject),
            median(subject))
