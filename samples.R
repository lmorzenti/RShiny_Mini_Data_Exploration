library(shiny)
library(bslib)
library(dyplr)
library(ggplot2)
library(colourpicker)
library(DT)
library(GEOquery)

install.packages("DT")
BiocManager::install("GEOquery")
df <- iris # for testing

# Summary Function

# part 1 - This is to make the summary tables
make_summary_table <- function(df) {
  do.call(rbind, lapply(names(df), function(col) {
    x <- df[[col]]
    data.frame(
      `Column Name` = col,
      `Type` = class(x),
      `Mean(sd) or Distinct Values` = if (is.numeric(x))
        sprintf("%.1f (+/- %.1f)", mean(x, na.rm=TRUE), sd(x, na.rm=TRUE))
      else
        paste(unique(x), collapse = ", "),
      check.names = FALSE
    )
  }))
}

# part 2 - the interactive statistics table 

make_data_table <- function(df) {
  display <- DT::datatable(df)
  return(display)
}

# part 3 - plots of continuous that allow different groups for plotting
numeric_cols <- names(df)[sapply(df, is.numeric)]
numeric_cols


#exploring the data for myself -- this is the differential expression data madefrom DeSeq2
df <- read.delim("GSE64810_mlhd_DESeq2_diffexp_DESeq2_outlier_trimmed_adjust.txt")
head(df)
colnames(df)
dim(df)


gse <- getGEO("GSE64810")
metadata <- pData(gse[[1]])
head(metadata)
colnames(metadata)

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

colSums(is.na(clean_metadata))

#check the results 
head(clean_metadata)

#save results to use:
write.csv(clean_metadata, "huntington_metadata.csv", row.names = FALSE)

# ensure everything is numeric
clean_metadata$age_of_death <- as.numeric(clean_metadata$age_of_death)
clean_metadata$rin <- as.numeric(clean_metadata$rin)
clean_metadata$pmi <- as.numeric(clean_metadata$pmi)
clean_metadata$mrna_seq_reads <- as.numeric(clean_metadata$mrna_seq_reads)

class(clean_metadata$age_of_death)
summary(clean_metadata$age_of_death)


# try this on your real data to see what you're aiming for

make_sample_plot <- function(df, col_name, group_by) {
  ggplot(df, aes(x = .data[[col_name]], fill = .data[[group_by]])) +
    theme_classic() + 
    geom_density(alpha = 0.5)
}

# test the fxn
make_sample_plot(clean_metadata, "diagnosis", "diagnosis")


test <- read.csv("huntington_counts.csv")
colnames(test)[1:4] 
head(test[, 1])  
