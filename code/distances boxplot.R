dist_mat <- as.matrix(dist.clr)

dist_mat[!upper.tri(dist_mat)] <- NA


dat_p <- dist_mat %>% 
  data.frame() %>% 
  tibble::rownames_to_column("sampleA") %>% 
  pivot_longer(-sampleA, names_to = "sampleB", values_to = "distance") %>% 
  filter(!is.na(distance)) %>% 
  # filter(!(grepl("V", sampleA) | grepl("V", sampleB))) %>%
  mutate(volA = substr(sampleA,1, 4),
         volB = substr(sampleB,1, 4)) %>%
  filter(!((grepl("V", sampleA) | grepl("V", sampleB)) & (volA == volB))) %>%
  select(-volA, -volB) %>%
  mutate(subjectA = substr(sampleA,1, 5),
         subjectB = substr(sampleB,2, 6),
         group = ifelse(subjectA == subjectB, "FIT-Nor", "FIT/Nor-FIT/Nor"),
         cohort = "Patients")


# ggplot +
#   geom_boxplot(aes(y = distance, x = )) +
#   scale_x_discrete(labels = c("Inter subejct", "Inter method")) +
#   theme_bw()




dat_v <- dist_mat %>% 
  data.frame() %>% 
  tibble::rownames_to_column("sampleA") %>% 
  pivot_longer(-sampleA, names_to = "sampleB", values_to = "distance") %>% 
  filter(!is.na(distance)) %>% 
  # filter((grepl("V", sampleA) & grepl("V", sampleB))) %>% 
  mutate(patA = substr(sampleA,1, 5),
         patB = substr(sampleB,2, 6)) %>% 
  filter((grepl("V", sampleA) | grepl("V", sampleB)) & (patA != patB)) %>%
  select(-patA, -patB) %>% 
  mutate(subjectA = substr(sampleA,1, 4),
         subjectB = substr(sampleB,1, 4),
         methodA = substr(sampleA,7, 9),
         methodB = substr(sampleB,7, 9),
         group = case_when(subjectA != subjectB ~ "FIT/Nor-FIT/Nor",
                           methodA != methodB ~ "FIT-Nor",
                           methodA == "FIT" & methodB == "FIT" ~ "FIT-FIT",
                           methodA == "Nor" & methodB == "Nor" ~ "Nor-Nor"),
         cohort = "Volunteers") %>% 
  select(-methodA, -methodB)

# ggplot(dat_v) +
#   geom_boxplot(aes(y = distance, x = group)) +
#   scale_x_discrete(limits = c("inter subject", "inter method", "FIT", "Nor")) +
#   theme_bw()


p_distances <- rbind(dat_p, dat_v) %>% 
  mutate(fill = interaction(group, cohort),
         fill = ifelse(fill %in% c("FIT/Nor-FIT/Nor.Patients", "FIT/Nor-FIT/Nor.Volunteers"), "FIT/Nor-FIT/Nor", as.character(fill)),
         cohort = case_when(group == "FIT/Nor-FIT/Nor" ~ "Inter-subject",
                            cohort == "Patients" ~ "Intra-patient",
                            cohort == "Volunteers" ~ "Intra-volunteer"),
         group = factor(group,
                        levels = c("FIT/Nor-FIT/Nor",
                                          "FIT-Nor",
                                          "FIT-FIT",
                                          "Nor-Nor"),
                        labels = c("Any combination",
                                   "FIT to Norgen",
                                   "FIT to FIT",
                                   "Norgen to Norgen"))) %>% 
  # pull(fill) %>% unique %>% paste
  ggplot() +
  geom_boxplot(aes(y = distance, x = group, fill = fill, alpha = I(group == "FIT-Nor")),
               outlier.size = 1,
               outlier.shape = 21,
               outlier.stroke = 0,
               
               size = 0.125) +
  scale_fill_manual(values = c("grey40", cols[1:5]),
                    limits =  c("FIT/Nor-FIT/Nor",
                                "FIT-FIT.Volunteers",
                                "Nor-Nor.Volunteers",
                                "FIT-Nor.Patients",
                                "FIT-Nor.Volunteers")
  ) +
  # scale_color_manual(values = c("grey40", cols[1:4]),
  #                    limits =  c("FIT/Nor-FIT/Nor",
  #                                "FIT-FIT.Volunteers",
  #                                "Nor-Nor.Volunteers",
  #                                "FIT-Nor.Patients",
  #                                "FIT-Nor.Volunteers")) +
  scale_alpha_manual(values = c(1, 1)) +
  theme_bw(base_size = 8) +
  ylab("Distance") +
  theme(axis.title.x = element_blank()) +
  facet_grid(~cohort, scales = "free_x", space = "free_x")


# 
 p_distances          
# 
# 
# 
# wilcox.test(dat_v$distance[dat_v$group == "intra FIT-FIT"],
# dat_v$distance[dat_v$group == "intra Nor-Nor"])
# 
# wilcox.test(dat_v$distance[dat_v$group == "intra FIT-FIT"],
# dat_v$distance[dat_v$group == "intra FIT-Nor"])
# 
# wilcox.test(dat_v$distance[dat_v$group == "intra FIT-Nor"],
# dat_v$distance[dat_v$group == "intra Nor-Nor"])
# 
# 
# kruskal.test(dat_v$distance[dat_v$group %in% c("intra FIT-Nor", "intra Nor-Nor")],
#              dat_v$group[dat_v$group %in% c("intra FIT-Nor", "intra Nor-Nor")])
# 
p_distances$data %>%
  group_by(cohort, group) %>%
  summarise(max(distance))

# 
