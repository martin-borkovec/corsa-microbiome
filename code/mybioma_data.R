
ASV_raw_counts0 <- read.delim("data/myBioma/Batch_1/raw_results/ASV_raw_counts.tsv", skip = 1, fill = FALSE)

ASV_raw_counts <- ASV_raw_counts0 %>% 
  tibble::column_to_rownames("X.OTU.ID") %>%
  t %>% 
  as.data.frame

colnames(ASV_raw_counts) <- paste0("ASV_", seq_len(ncol(ASV_raw_counts)))
sample.names <- rownames(ASV_raw_counts)
sample.names <- gsub("x", "_", sample.names)
sample.names[1:162] <- substr(sample.names[1:162], 2, 10)
sample.names[-(1:162)] <- substr(sample.names[-(1:162)], 1, 9)
rownames(ASV_raw_counts) <- sample.names
  
saveRDS(ASV_raw_counts, "data/ASV_raw_counts.RDS")


sum(ASV_raw_counts[grepl("FIT", row.names(ASV_raw_counts)), !is.na(taxonomy_map$Genus)]) / sum(ASV_raw_counts[grepl("FIT", row.names(ASV_raw_counts)), ])
sum(ASV_raw_counts[!grepl("FIT", row.names(ASV_raw_counts)), !is.na(taxonomy_map$Genus)]) / sum(ASV_raw_counts[!grepl("FIT", row.names(ASV_raw_counts)), ])
sum(ASV_raw_counts[, !is.na(taxonomy_map$Genus)]) / sum(ASV_raw_counts)

sum(ASV_raw_counts[grepl("FIT", row.names(ASV_raw_counts)), !is.na(taxonomy_map$Species)]) / sum(ASV_raw_counts[grepl("FIT", row.names(ASV_raw_counts)), ])
sum(ASV_raw_counts[!grepl("FIT", row.names(ASV_raw_counts)), !is.na(taxonomy_map$Species)]) / sum(ASV_raw_counts[!grepl("FIT", row.names(ASV_raw_counts)), ])
sum(ASV_raw_counts[, !is.na(taxonomy_map$Species)]) / sum(ASV_raw_counts)





sequences <- ShortRead::readFasta("data/myBioma/Batch_1/raw_results/sequences.fasta")
sequences_map <- data.frame(ASV_ID = colnames(ASV_raw_counts), ASV = as.character(sequences@sread))

saveRDS(sequences_map, "data/sequences_map.RDS")

taxonomy_map <- read.delim("data/myBioma/Batch_1/raw_results/taxonomy_map.tsv", fill = FALSE)
names(taxonomy_map) <- c("ASV", "Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")
taxonomy_map$ASV <- paste0("ASV_", seq_len(ncol(ASV_raw_counts)))
saveRDS(taxonomy_map, "data/taxonomy_map.RDS")
