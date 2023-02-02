\name{LXgeneidconver}
\alias{LXgeneidconver}
\title{LXgeneidconver}
\usage{
LXgeneidconver(gene_list,gene_type,species)
}
\description{
The R package can be used to convert gene id or symbol.
}
\examples{
install.packages("devtools")
library(devtools)

install_github("gluck4668/LXgeneidconver")

library(LXgeneidconver)
??LXgeneidconver
#---------------------------------------
data(rat_genes_SYMBOL)
data(rat_genes_ENSEMBL)
data(rat_genes_ENTREZID)

#---------------------------------------

setwd("D:/Desktop/R_example/LXgeneidconver_example")

gene_list <- "rat_genes_SYMBOL.xlsx"


gene_type <- " SYMBOL " # Here, it should be "SYMBOL","ENSEMBL",or "ENTREZID".

species= " mouse "  # The species should be "human", "mouse", or "rat"


LXgeneidconver(gene_list,gene_type,species)

}
