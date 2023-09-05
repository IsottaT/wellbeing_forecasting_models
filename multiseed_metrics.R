# Load the data
multi_seed_resume <- readRDS('models_multiSeedResume.RDS')

# Define a function to extract and calculate statistics for a given metric
extract_metric_stats <- function(data, metric_name) {
  train <- lapply(data, function(x) x[[paste0(metric_name, "_accuracy")]][1])
  test <- lapply(data, function(x) x[[paste0(metric_name, "_accuracy")]][2])
  mean_train <- mean(unlist(train))
  sd_train <- sd(unlist(train))
  mean_test <- mean(unlist(test))
  sd_test <- sd(unlist(test))
  return(list(mean_train = mean_train, sd_train = sd_train, mean_test = mean_test, sd_test = sd_test))
}

# Define a list of metric names
metric_names <- c("LR", "LASSO", "RF")

# Initialize an empty list to store metric statistics
metric_stats <- list()

# Extract and calculate statistics for each metric
for (metric_name in metric_names) {
  metric_stats[[metric_name]] <- extract_metric_stats(multi_seed_resume, metric_name)
}

# Create a data frame for the table
metrics_table <- data.frame(matrix(nrow = 6, ncol = length(metric_names) * 4))
colnames(metrics_table) <- rep(metric_names, each = 4)
rownames(metrics_table) <- c("accuracy train", "accuracy test", "AUC train", "AUC test", "F1 train", "F1 test")

# Fill in the data frame with metric statistics
for (i in 1:length(metric_names)) {
  col_start <- (i - 1) * 4 + 1
  col_end <- i * 4
  metrics_table[, col_start:col_end] <- do.call(cbind, metric_stats[[metric_names[i]]])
  colnames(metrics_table)[col_start:col_end] <- c(
    paste(metric_names[i], "acc train mean"),
    paste(metric_names[i], "acc train sd"),
    paste(metric_names[i], "acc test mean"),
    paste(metric_names[i], "acc test sd")
  )
}

# Write the metrics table to an Excel file
writexl::write_xlsx(metrics_table, 'metrics_table_inline.xlsx')

# Create a summary data frame for means and standard deviations
summary_df <- data.frame(
  Metric = rep(c("Accuracy Train", "Accuracy Test", "AUC Train", "AUC Test", "F1 Train", "F1 Test"), length(metric_names)),
  Model = rep(metric_names, each = 6),
  Mean = unlist(lapply(metric_stats, function(x) x$mean_train)),
  SD = unlist(lapply(metric_stats, function(x) x$sd_train))
)

# Write the summary data frame to an Excel file
writexl::write_xlsx(summary_df, 'metrics_table.xlsx')
