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

pca_result <- prcomp(t(filtered), scale. = TRUE)
summary(pca_result)
# look at this
head(pca_result$x)
# proportion of variance for each PC
variance_explained <- summary(pca_result)$importance[2,]
variance_explained[1]  # PC1
variance_explained[2]  # PC2
variance_explained

#sample for the sake of this portion
clean_metadata <- metadata[, c(
  "title",
  "diagnosis:ch1",
  "age of death:ch1",
  "age of onset:ch1",
  "pmi:ch1",
  "rin:ch1",
  "mrna-seq reads:ch1"
)]

#remove the :ch1 from the names 
colnames(clean_metadata) <- c(
  "sample", "diagnosis", "age_of_death", 
  "age_of_onset", "pmi", "rin", "mrna_seq_reads"
)

pca_coords <- as.data.frame(pca_result$x)
pca_coords$diagnosis <- clean_metadata$diagnosis

# step 3: include the % variance in each component
variance_explained <- summary(pca_result)$importance[2,]
x_label <- paste0("PC1 (", round(variance_explained[1] * 100, 2), "%)")
y_label <- paste0("PC2 (", round(variance_explained[2] * 100, 2), "%)")

ggplot(pca_coords, aes(x = PC1, y = PC2, color = diagnosis)) +
  geom_point() +
  labs(x = x_label, y = y_label)

head(pca_coords)

# Final step: make this into an actual function to use

make_pca_plot <- function(filtered, clean_metadata, pc_x=1, pc_y=2) {
  
  pca_result <- prcomp(t(filtered), scale. = TRUE)
  
  # second step 
  pca_coords <- as.data.frame(pca_result$x)
  pca_coords$diagnosis <- clean_metadata$diagnosis
  
  # plot it
  variance_explained <- summary(pca_result)$importance[2,]
  x_label <- paste0("PC", pc_x, " (", round(variance_explained[pc_x] * 100, 2), "%)")
  y_label <- paste0("PC", pc_y, " (", round(variance_explained[pc_y] * 100, 2), "%)")
  
  ggplot(pca_coords, aes(x = .data[[paste0("PC", pc_x)]], y = .data[[paste0("PC", pc_y)]], color = diagnosis)) +
    geom_point() +
    labs(x = x_label, y = y_label)
  
}

# test the above function 

make_pca_plot(filtered, clean_metadata, 1, 2)
make_pca_plot(filtered, clean_metadata, 1, 3)
make_pca_plot(filtered, clean_metadata, 2, 3)

