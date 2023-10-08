library(ggplot2)

# Define the function
plot_rmse <- function(mse_values) {
  # Convert the MSE values to a data frame
  mse_data <- data.frame(
    Ticker = names(mse_values),
    MSE = mse_values
  )
  
  # Generate a bar plot using ggplot2
  p <- ggplot(mse_data, aes(x = Ticker, y = MSE)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "MSE per Ticker",
         x = "Ticker",
         y = "MSE") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  # Display the plot
  print(p)
}

plot_pred_real <- function(pred, real) {
  # Create data frames
  df_pred <- data.frame(Ticker = names(pred), Sharpe = pred, Type = "Predicted")
  df_real <- data.frame(Ticker = names(real), Sharpe = real, Type = "Real")
  
  # Combine them
  pred_real <- rbind(df_pred, df_real)
  
  # Generate a bar plot
  p <- ggplot(pred_real, aes(x = Ticker, y = Sharpe, fill = Type)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = "Weekly Return: Predicted vs Real",
         x = "Ticker",
         y = "Weekly return") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    scale_fill_manual(values = c("Predicted" = "skyblue", "Real" = "darkblue"))
  
  # Return the plot object
  return(p)
}






