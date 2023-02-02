
#-----------------------------------------------------------------------
LXgeneidconver <- function(gene_list,gene_type,species){
  
  
###################################################################
#list all the packages that have been installed
all_packages <- data.frame(installed.packages())

#To judge whether a package was installed. If not, it will be installed.
pack <- data.frame(c("devtools","BiocManager","ggnewscale","R.utils",
                     "roxygen2","xfun", "ggsci","openxlsx","dplyr","psych","ggplot2",
                     "ggrepel","RColorBrewer", "ggthemes","rticles","grid","patchwork","Hmisc","pak") )

# To judge whether a package was included in the all_packages: %in%
pack$type <- pack[,1] %in% all_packages$Package

for (i in 1:nrow(pack)){
  if(pack[i,2]==FALSE)
    install.packages(pack[i,1],update = F,ask = F)
}
rm(i)

# 批量library
packages <- as.character(pack[,1])

for(i in packages){
  library(i, character.only = T)
}
rm(i)


#-----------------

if("tidyverse" %in% all_packages$Package==FALSE)
  pak::pak("tidyverse/tidyverse")
library(tidyverse)


#-----------------
BiocManager_pack <- data.frame(c("DOSE","clusterProfiler","do","enrichplot",
                                 "pathview","BiocParallel","GO.db","KEGGREST",
                                 "org.Hs.eg.db","org.Mm.eg.db","org.Rn.eg.db"))
# human: "org.Hs.eg.db"
# mouse: "org.Mn.eg.db"
# rat: "org.Rn.eg.db"

BiocManager_pack$type <- BiocManager_pack[,1] %in% all_packages$Package

for (i in 1:nrow(BiocManager_pack)){
  if(BiocManager_pack[i,2]==FALSE)
    BiocManager::install(BiocManager_pack[i,1],update = F,ask = F)
}

# 批量library
Bio_packages <- as.character(BiocManager_pack[,1])
for(i in Bio_packages){
  library(i, character.only = T)
}
rm(i)

#-------------------------------

#--------------------------------------
#创建KEGG本地数据库
if("KEGG.db" %in% all_packages$Package == F)
{install_github("YuLab-SMU/createKEGGdb")
  library(createKEGGdb)
  
  #选择创建几个常见物种的kegg注释包: 人"hsa"，小鼠"mmu",大鼠"rno";
  kegg_db <-c( "hsa", "mmu", "rno")
  createKEGGdb::create_kegg_db(kegg_db)
  
  #安装这个包(默认的包的路径在当前工作目录，根据实际情况修改路径)
  install.packages("KEGG.db_1.0.tar.gz",repos=NULL,type="source")
  
  #载入自己创建的KEGG.db包；
  library(KEGG.db)
  
  file.remove("KEGG.db_1.0.tar.gz")
  #使用本地数据（KEGG.db）进行富集分析
  # 在 enrichKEGG ( use_internal_data= T)
}

#--------------------------------------

spe <- c("human","mouse","rat")
species <- tolower(species) %>% trimws() # 去掉字符串空格

if(species %in% spe ==FALSE)
{spec_txt <- paste0("The species that you writed was ","'",species,"'",", which was error. It should be human, mouse, or rat.")
stop(spec_txt)}

#----------------------

type <- c("SYMBOL","ENSEMBL","ENTREZID")
g_type <- toupper(gene_type) %>% trimws() # 去掉字符串空格

if(g_type %in% type ==FALSE)
{type_txt <- paste0("The gene_type that you writed was ","'",gene_type,"'",", which was error. It should be SYMBOL,ENSEMBL,or ENTREZID.")
stop(type_txt)}

#---------------------------------------

gene_list <- read.xlsx(gene_list)
colnames(gene_list)[1] <- "gene_id"
table(duplicated(gene_list$gene_id))
df <- dplyr::distinct(gene_list,gene_id,.keep_all = T)

gene_type <- toupper(gene_type) %>% trimws() # 去掉字符串空格

  
###--------------gene_type=="ENSEMBL"------------###
if(gene_type=="ENSEMBL")
{

   if(species=="mouse")
     {gene_ENTREZID<-bitr(df[,1],fromType="ENSEMBL",toType=c("ENTREZID","ENSEMBL","SYMBOL"),OrgDb=org.Mm.eg.db)
      write.xlsx(gene_ENTREZID, "gene_ENSEMBL_conversion (mouse).xlsx")
     }

   if(species=="rat")
     {gene_ENTREZID<-bitr(df[,1],fromType="ENSEMBL",toType=c("ENTREZID","ENSEMBL","SYMBOL"),OrgDb=org.Rn.eg.db)
      write.xlsx(gene_ENTREZID, "gene_ENSEMBL_conversion (rat).xlsx")}

   if(species=="human")
     {gene_ENTREZID<-bitr(df[,1],fromType="ENSEMBL",toType=c("ENTREZID","ENSEMBL","SYMBOL"),OrgDb=org.Hs.eg.db)
     write.xlsx(gene_ENTREZID, "gene_ENSEMBL_conversion (human).xlsx")}
}
########################################################


###--------------gene_type=="SYMBOL"------------###
if(gene_type=="SYMBOL"){
  
  if(species=="mouse")
  {gene_ENTREZID<-bitr(df[,1],fromType="SYMBOL",toType=c("ENTREZID","ENSEMBL","SYMBOL"),OrgDb=org.Mm.eg.db)
  write.xlsx(gene_ENTREZID, "gene_SYMBOL_conversion (mouse).xlsx")
  }
  
  if(species=="rat")
  {gene_ENTREZID<-bitr(df[,1],fromType="SYMBOL",toType=c("ENTREZID","ENSEMBL","SYMBOL"),OrgDb=org.Rn.eg.db)
  write.xlsx(gene_ENTREZID, "gene_SYMBOL_conversion (rat).xlsx")}
  
  if(species=="human")
  {gene_ENTREZID<-bitr(df[,1],fromType="SYMBOL",toType=c("ENTREZID","ENSEMBL","SYMBOL"),OrgDb=org.Hs.eg.db)
  write.xlsx(gene_ENTREZID, "gene_SYMBOL_conversion (human).xlsx")}
}
########################################################

###--------------gene_type=="ENTREZID"------------###
if(gene_type=="ENTREZID"){
  
  if(species=="mouse")
  {gene_ENTREZID<-bitr(df[,1],fromType="ENTREZID",toType=c("ENTREZID","ENSEMBL","SYMBOL"),OrgDb=org.Mm.eg.db)
  write.xlsx(gene_ENTREZID, "gene_ENTREZID_conversion (mouse).xlsx")
  }
  
  if(species=="rat")
  {gene_ENTREZID<-bitr(df[,1],fromType="ENTREZID",toType=c("ENTREZID","ENSEMBL","SYMBOL"),OrgDb=org.Rn.eg.db)
  write.xlsx(gene_ENTREZID, "gene_ENTREZID_conversion (rat).xlsx")}
  
  if(species=="human")
  {gene_ENTREZID<-bitr(df[,1],fromType="ENTREZID",toType=c("ENTREZID","ENSEMBL","SYMBOL"),OrgDb=org.Hs.eg.db)
  write.xlsx(gene_ENTREZID, "gene_ENTREZID_conversion (human).xlsx")}
}
########################################################

text <- paste0("The gene ",gene_type," was successfully converted to a gene table including ENTREZID, ENSEMBL, and SYMBOL.")   

print(text)

}
