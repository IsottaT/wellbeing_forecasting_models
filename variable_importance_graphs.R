library(ggplot2)
library(ggpubr)
library(pROC)

# Define a function to generate variable importance plots
generate_variable_importance_plot <- function(model, model_name, data) {
  var_importance <- data.frame(
    variable = rownames(coef(model)),
    importance = abs(coef(model)[-1, "Estimate"])
  )
  var_importance$importance <- var_importance$importance / sum(var_importance$importance) * 100
  var_importance <- tail(arrange(var_importance, desc(importance)), 10)
  
  ggplot(var_importance, aes(x = variable, y = importance)) +
    geom_point(size = 3) +
    geom_segment(aes(x = variable, xend = variable, y = 0, yend = importance)) +
    ggtitle(paste("Variable Importance -", model_name, "model")) +
    xlab("") +
    theme_bw() +
    ylab("Importance") +
    scale_fill_discrete(name = "Variable Name") +
    theme(
      axis.text.y = element_text(angle = 45, hjust = 0.8, size = 13),
      axis.text.x = element_text(size = 14),
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 14),
      legend.position = "none"
    ) +
    coord_flip() +
    scale_y_continuous(minor_breaks = seq(0, 100, 5), breaks = seq(0, 100, 5))
}

# Define a function to generate ROC curves
generate_roc_curve <- function(model, model_name, data) {
  roc_obj <- roc(data$outcome_bool, as.vector(predict(model, newdata = data, type = "response")))
  plot.roc(roc_obj, print.auc = TRUE, main = paste("ROC -", model_name, "model"))
}

# Load your data and models here

# Generate variable importance plots and ROC curves for each model
models <- list(
  list(model = LR, model_name = "LR", data = test_norm),
  list(model = LASSO, model_name = "LASSO", data = test_norm_dummy),
  list(model = RF, model_name = "RF", data = test_norm)
)

for (model_info in models) {
  generate_variable_importance_plot(model_info$model, model_info$model_name, model_info$data)
  generate_roc_curve(model_info$model, model_info$model_name, model_info$data)
}
