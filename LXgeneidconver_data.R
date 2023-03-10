
setwd("D:/Desktop/Råå¼å/LXgeneidconver")
library(openxlsx)

rat_genes_ENSEMBL <- read.xlsx("rat_genes_ENSEMBL.xlsx")
rat_genes_ENTREZID <- read.xlsx("rat_genes_ENTREZID.xlsx")
rat_genes_SYMBOL <- read.xlsx("rat_genes_SYMBOL.xlsx")



usethis::use_data(rat_genes_ENSEMBL,overwrite = T)
usethis::use_data(rat_genes_ENTREZID,overwrite = T)
usethis::use_data(rat_genes_SYMBOL,overwrite = T)


rm(list=ls())

data(rat_genes_ENSEMBL)
data(rat_genes_ENTREZID)
data(rat_genes_SYMBOL)

