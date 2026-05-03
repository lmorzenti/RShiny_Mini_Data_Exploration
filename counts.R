library(shiny)
library(bslib)
library(ggplot2)
library(colourpicker)

#################
#This component allows the user to choose different gene 
#filtering thresholds and assess their effects using diagnostic
#plots of the counts matrix.
#################

# pre-work # understanding the data I am working with from GEO: 
counts <- read.delim("GSE64810_mlhd_DESeq2_norm_counts_adjust.txt")
head(counts)
# X   C_0002     C_0003     C_0004     C_0005     C_0006
# 1 ENSG00000000003.10 319.4965 307.335295 258.906483 232.927082 229.053828
# 2  ENSG00000000005.5   0.0000   1.982808   6.943852   1.982358   2.974725
dim(counts)
# [1] 28087    70
colnames(counts)
# [1] "X"      "C_0002" "C_0003" "C_0004" "C_0005" "C_0006" "C_0008" "C_0009"
# [9] "C_0010" "C_0011" "C_0012" "C_0013" "C_0014" "C_0015" "C_0016" "C_0017"
ncol(counts)

# remove gene names from the counts to more cleanly work
gene_ids <- counts$X
counts_only <- counts[,-1]

var(as.numeric(counts_only[1, ]))

### part 1 ### tab w/ text/table summarizing effect of filtering
  # include: number of samples, total number of genes, number and 
  # % of genes passing current filter. number and % not passing

# a: filter by at least X percentile of variance 

filter_by_variance <- function(counts_only, percentile) {
  gene_variances <- apply(counts_only, 1, var)
  threshold <- quantile(gene_variances, percentile/100)
  keep <- gene_variances >= threshold
  filtered_varience <- counts_only[keep, ]
  return (filtered_varience)
}

# b: filter to include genes with at least X samples that are non-zero

filter_by_zeros <- function(counts_only, min_nonzero_count) {
  sample_counts <- apply(counts_only, 1, function(x) sum(x>0))
  keep <- sample_counts >= min_nonzero_count
  filtered_zeros <- counts_only[keep, ]
  return (filtered_zeros)
}

# c: make summary table

filtered_sum_table <- function(counts_only, ) {
  
  
}



### part 2 ### tab with scatter plots 



### part 3 ### tab with a clustered heatmap of counts remaining after filtering


#part 4 - tab with a scatter plot of PCA projections




