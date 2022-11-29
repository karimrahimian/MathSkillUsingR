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
query_snp <- GDCquery(project = "TCGA-ACC",
                      data.category = "Simple Nucleotide Variation",
                      data.type = "Masked Somatic Mutation",
                      workflow.type = "Aliquot Ensemble Somatic Variant Merging and Masking",
                      )

GDCdownload(query_snp, files.per.chunk = 100,directory = "inputs")

#LUAD_snp <- GDCprepare(query_snp, save = TRUE, save.filename = "LUAD_snp.rda")

#
#rror in GDCquery(project = "TCGA-ACC", data.category = "Simple Nucleotide Variation",  : 
#                   Please set a valid workflow.type argument from the list below:
#                   => MuSE Annotation
#                 => MuTect2 Annotation
#                 => MuSE
#                 => VarScan2 Annotation
##                => Pindel Annotation
#                 => Pindel
#                 => VarScan2
#                 => MuTect2
#                 => Aliquot Ensemble Somatic Variant Merging and Maskin#