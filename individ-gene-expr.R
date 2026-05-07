library(shiny)
library(bslib)
library(ggplot2)
library(colourpicker)
library(dplyr)
library(tidyr)

install.packages("ggbeeswarm")
library(ggbeeswarm)


# preparing the data to be plotted 

prepare_gene_data <- function(counts, clean_metadata, gene_name) {
  counts_long <- pivot_longer(
    counts,
    cols = -X,
    names_to="sample",
    values_to="expression"
  )
  
  counts_long <- left_join(
    counts_long,
    clean_metadata,
    by="sample"
  )
  
  gene_data <- counts_long[counts_long$X == gene_name, ]
  return(gene_data)
}

  #test
gene_data <- prepare_gene_data(counts, clean_metadata, "ENSG00000069011.10")
nrow(gene_data)

# plot creation 

plot_individual_geneexpression <- function(gene_data, plot_type) {
  p <- ggplot(gene_data, aes(x = diagnosis, y = expression, fill = diagnosis)) +
    theme_classic() +
    labs(
      title=paste("Expression of", gene_data$X[1]),
      x = "Diagnosis",
      y = "Normalized Expression"
    ) +
    scale_fill_manual(
      values=c("Huntington's Disease" = "darkblue",
               "Neurologically normal" = "lightblue")
    )
  
  if (plot_type == "boxplot") p<-p + geom_boxplot()
  else if (plot_type == "violinplot") p<-p + geom_violin()
  else if (plot_type == "barplot") p<-p + geom_col()
  else if (plot_type == "beeswarm") p<-p + geom_beeswarm()
  else stop("Invalid plot type")
  
  return(p)
}

#tests
plot_individual_geneexpression(gene_data, "boxplot")
plot_individual_geneexpression(gene_data, "violinplot")
plot_individual_geneexpression(gene_data, "barplot")
plot_individual_geneexpression(gene_data, "beeswarm")
