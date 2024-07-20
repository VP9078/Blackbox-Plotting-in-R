
###################################################
#### 1. Plot summary metrics for Blackbox data ####
###################################################


library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)

summary_load <- function () {
  file_path <- file.choose
  SCI_Baseline <<- data.frame(read_xlsx(path = file_path, sheet = "SCI Baseline"))
  SHAM_Baseline <<- data.frame(read_xlsx(path = file_path, sheet = "SHAM Baseline"))
  
  assign(paste0("SCI_Baseline_Summary"),
         paste0("SCI_Baseline") %>%
           get() %>%
           colMeans(), envir = .GlobalEnv)
  
  assign(paste0("SHAM_Baseline_Summary"),
         paste0("SHAM_Baseline") %>%
           get() %>%
           colMeans(), envir = .GlobalEnv)
  
  
  days <<- c(1,7,14,21,28,35,42)
  
  for (i in 1:length(days)) {
    assign(paste0("SCI_Day_", days[i]), data.frame(read_xlsx(path = file_path, sheet = paste0("SCI ", days[i], " days post SCI"))), envir = .GlobalEnv)
    
    assign(paste0("SCI_Day_", days[i], "_Summary"),
           paste0("SCI_Day_", days[i]) %>%
             get() %>%
             colMeans(), envir = .GlobalEnv)
  
    assign(paste0("SHAM_Day_", days[i]), data.frame(read_xlsx(path = file_path, sheet = paste0("SHAM ", days[i], " days post SCI"))), envir = .GlobalEnv)
    
    assign(paste0("SHAM_Day_", days[i], "_Summary"),
           paste0("SHAM_Day_", days[i]) %>%
             get() %>%
             colMeans(), envir = .GlobalEnv)
  }
  
  column_names <<- names(SCI_Baseline_Summary)
  
}



plot_summary_data <- function(colname) {
  
  # Initialize the necessary variables
  SCI_index <- grep(colname, names(SCI_Baseline_Summary))
  SCI_data <- list(SCI_Baseline_Summary[SCI_index])
  
  SHAM_index <- grep(colname, names(SHAM_Baseline_Summary))
  SHAM_data <- list(SHAM_Baseline_Summary[SHAM_index])
  
  time_pts <- c("Baseline")
  
  # Loop through the days to populate SCI_data and SHAM_data
  for (i in 1:length(days)) {
    SCI_day_summary <- get(paste0("SCI_Day_", days[i], "_Summary"))
    SCI_index <- grep(colname, names(SCI_day_summary))
    SCI_data[[i+1]] <- SCI_day_summary[SCI_index]
    
    SHAM_day_summary <- get(paste0("SHAM_Day_", days[i], "_Summary"))
    SHAM_index <- grep(colname, names(SHAM_day_summary))
    SHAM_data[[i+1]] <- SHAM_day_summary[SHAM_index]
    
    time_pts[i+1] <- paste0("Day ", days[i])
  }
  
  # Combine the data into a data frame
  SCI_SHAM_df <- data.frame(time_pts = factor(time_pts, levels = time_pts), SHAM_data = unlist(SHAM_data), SCI_data = unlist(SCI_data))
  
  # Create the plot
  p <- ggplot(SCI_SHAM_df, aes(x = time_pts)) +
    geom_line(aes(y = SCI_data, color = "SCI"), group = 1, linewidth = 1.2) +
    geom_line(aes(y = SHAM_data, color = "SHAM"), group = 1, linewidth = 1.2) +
    labs(
      x = "Time Points",
      y = "Average Value",
      title = paste0("SCI vs SHAM ", colname, " plot")
    ) +
    theme_minimal() +  # Use a minimal theme for a cleaner look
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16),  # Title font size and alignment
      axis.text = element_text(size = 12),  # Axis text size
      axis.title = element_text(size = 14, face = "bold"),  # Axis title font size and weight
      legend.title = element_text(size = 12, face = "bold"),  # Legend title font size and weight
      legend.text = element_text(size = 11)  # Legend text size
    ) +
    scale_color_manual(values = c("SCI" = "blue", "SHAM" = "red"), name = "Condition") +  # Custom color scale
    guides(color = guide_legend(title = "Group"))  # Legend title
  
  # Assign the plot to a variable
  assign(paste0("SCI_SHAM_", colname, "_plot"), p, envir = .GlobalEnv)
  
  # Print the plot
  print(p)
}

summary_plot_all <- function() {
  for (i in 1:length(column_names)) {
    plot_summary_data(column_names[i])
  }
}
