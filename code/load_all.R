library(openxlsx)
library(dplyr)
library(tidyr)
library(ggraph)
library(ggplot2)
library(ggpubr)


assign("df_asv", readRDS("results/df_asv.RDS"), envir = globalenv())
assign("df_species", readRDS("results/df_species.RDS"), envir = globalenv())
assign("df_genus", readRDS("results/df_genus.RDS"), envir = globalenv())
assign("df_family", readRDS("results/df_family.RDS"), envir = globalenv())
assign("df_order", readRDS("results/df_order.RDS"), envir = globalenv())
assign("df_class", readRDS("results/df_class.RDS"), envir = globalenv())
assign("df_phylum", readRDS("results/df_phylum.RDS"), envir = globalenv())
assign("df_domain", readRDS("results/df_domain.RDS"), envir = globalenv())


assign("seqtab.clr", readRDS("results/seqtab.clr.RDS"), envir = globalenv())
assign("seqtab.clr_species", readRDS("results/seqtab.clr_species.RDS"), envir = globalenv())
assign("seqtab.clr_genus", readRDS("results/seqtab.clr_genus.RDS"), envir = globalenv())
assign("seqtab.clr_family", readRDS("results/seqtab.clr_family.RDS"), envir = globalenv())
assign("seqtab.clr_order", readRDS("results/seqtab.clr_order.RDS"), envir = globalenv())
assign("seqtab.clr_class", readRDS("results/seqtab.clr_class.RDS"), envir = globalenv())
assign("seqtab.clr_phylum", readRDS("results/seqtab.clr_phylum.RDS"), envir = globalenv())

label_par <- list(size = 10, color = "black", face = "bold", family = NULL)

sapply(list.files("code/functions/", full.names = TRUE), source)

cols <- ggsci::pal_jco()(10)
cols[3] <- ggsci::pal_jama()(7)[5]

cols_phyla <- ggsci::pal_d3()(5)[c(3,4,5,2)]
