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

### part 1 ### tab w/ text/table summarizing effect of filtering
  # include: number of samples, total number of genes, number and 
  # % of genes passing current filter. number and % not passing

# a: filter by at least X percentile of variance 

filter_by_variance <- function(counts_only, percentile) {
  gene_variances <- apply(counts_only, 1, var)
  threshold <- quantile(gene_variances, percentile/100)
  keep <- gene_variances >= threshold
  filtered_variance <- counts_only[keep, ]
  return (filtered_variance)
}

# b: filter to include genes with at least X samples that are non-zero

filter_by_zeros <- function(counts_only, min_nonzero_count) {
  sample_counts <- apply(counts_only, 1, function(x) sum(x>0))
  keep <- sample_counts >= min_nonzero_count
  filtered_zeros <- counts_only[keep, ]
  return (filtered_zeros)
}

# c: make summary table

filter_counts <- function(counts_only, percentile, min_nonzero_count) {
  after_variance <- filter_by_variance(counts_only, percentile)
  after_both <- filter_by_zeros(after_variance, min_nonzero_count)
  return (after_both)
}

#test
filtered <- filter_counts(counts_only, 75, 10)

filtered_sum_table <- function(counts_only, filtered) {
  data.frame(
    `Number of Samples` = ncol(counts_only),
    `Total Number of Genes` = nrow(counts_only),
    `Number of genes passing filter` = nrow(filtered),
    `Percent of genes passing filter` = (nrow(filtered)/nrow(counts_only))*100,
    `Number of genes not passing the filter` = nrow(counts_only) - nrow(filtered),
    `Percent of genes not passing the filter` = ((nrow(counts_only) - nrow(filtered))/nrow(counts_only))*100
  )
}

#test
filtered_sum_table(counts_only, filtered)

### part 2 ### tab with scatter plots 

gene_stats <- data.frame(
  median = apply(counts_only, 1, median),
  variance = apply(counts_only, 1, var),
  num_zeros = apply(counts_only, 1, function(x) sum(x>0)),
  passing_filter = rownames(counts_only) %in% rownames(filtered)
)

medcount_vs_var <- function(gene_stats) {
  p <- ggplot(gene_stats, aes(x=median, y=variance, color=passing_filter)) + 
    geom_point() +
    scale_color_manual(values = c("TRUE" = "darkblue", "FALSE" = "lightpink")) +
    scale_x_log10() +
    scale_y_log10() 
  return(p)
}

medcount_vs_numofzero <- function(gene_stats) {
  m <- ggplot(gene_stats, aes(x=median, y=num_zeros, color=passing_filter)) + 
    geom_point() +
    scale_color_manual(values = c("TRUE" = "darkblue", "FALSE" = "lightpink")) +
    scale_x_log10() +
    scale_y_log10()  
  
  return(m)
}

# test
medcount_vs_var(gene_stats)
medcount_vs_numofzero(gene_stats)

### part 3 ### tab with a clustered heatmap of counts remaining after filtering
install.packages("pheatmap")
library(pheatmap)

make_heatmap <- function(filtered, log_transform) {
  if (log_transform) {
    mat <- log10(filtered + 1)
  } else {
    mat <- filtered
  }
  p<- pheatmap(mat,
                show_rownames = FALSE,
                show_colnames = TRUE,
                cluster_rows = TRUE,
                cluster_cols = TRUE,
                fontsize_col = 5
  )
  return(p)
}

make_heatmap(filtered, TRUE)

#part 4 - tab with a scatter plot of PCA projections




