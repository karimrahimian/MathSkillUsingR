if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
#BiocManager::install("remotes")
#BiocManager::install("BioinformaticsFMRP/TCGAbiolinks")
#BiocManager::install("TCGAbiolinks")
#BiocManager::install("biomaRt")
#BiocManager::install("XML")
#BiocManager::install("rvest")
#BiocManager::install("SummarizedExperiment")

library(TCGAbiolinks)
library(SummarizedExperiment)
library(dplyr)
library(DT)

#GDCquery(project = "TCGA-ACC",
#                  data.category = "Transcriptome Profiling",
#                  data.type = "Gene Expression Quantification", 
##                  workflow.type = "STAR - Counts",
#                  )
#query <- GDCquery(project = "TARGET-AML",
#                  data.category = "Transcriptome Profiling",
#                  data.type = "miRNA Expression Quantification",
#                  workflow.type = "BCGSC miRNA Profiling",
#                  barcode = c("TARGET-20-PARUDL-03A-01R","TARGET-20-PASRRB-03A-01R"))
#tcga_maf <- GDCquery(project = "TCGA-ACC", 
#                     data.category = "Simple Nucleotide Variation", # Simple nucleotide variation if legacy
#                     data.type = "Masked Somatic Mutation",
#                     access = "open", 
#                     legacy = F,
#                     sample.type = "Primary Tumor")

query <- GDCquery(project = "TCGA-ACC",
                  data.category =  "Copy number variation",
                  legacy = TRUE,
                  file.type = "hg19.seg",
                  barcode = c("TCGA-OR-A5LR-01A-11D-A29H-01", "TCGA-OR-A5LJ-10A-01D-A29K-01"))

GDCdownload(query, directory = "TCGA_biolinks",method = 'api')
tcga_maf <- GDCprepare(tcga_maf)

