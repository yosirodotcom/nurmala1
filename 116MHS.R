install.packages("pacman")
pacman::p_load(dplyr, tidyr,writexl, rcompanion, knitr, likert, ggplot2, magrittr, readr, stringr, grid, shadowtext, wordcloud, googlesheets4, vcd, corrplot, DescTools, extrafont, showtext, scales)

df_mhs <- read_sheet("https://docs.google.com/spreadsheets/d/1csPhVFCB2aEhKi-q4ubRmH0lbA-BccmFrZCGmugxV9U/edit?gid=787379130#gid=787379130", sheet = "df_mhs")
col_mhs <- read_sheet("https://docs.google.com/spreadsheets/d/1csPhVFCB2aEhKi-q4ubRmH0lbA-BccmFrZCGmugxV9U/edit?gid=33675846#gid=33675846", sheet = "col_mhs")



# Prepare Data

df_mhs <- df_mhs %>%
  mutate(
    Y = case_when(
      Y == 'Kurang Mahir ( TOEIC 350, IELTS 3.5, TOEFL 433)' ~ 'Lower Intermediate\n( TOEIC 350, IELTS 3.5, TOEFL 433)',
      Y == 'Dasar (TOEIC 255, IELTS 2.5, TOEFL 347)' ~ 'Basic\n(TOEIC 255, IELTS 2.5, TOEFL 347)',
      Y == 'Sedang (TOEIC 500, IELTS 5.0, TOEFL 477)' ~ 'Upper Intermediate\n(TOEIC 500, IELTS 5.0, TOEFL 477)',
      Y == 'Mahir (TOEIC 685, IELTS 6.5, TOEFL 550)' ~ 'Advanced\n(TOEIC 685, IELTS 6.5, TOEFL 550)'
  )
)


list_level1_col <- list('X1','X3','X4','X5','X7')
for (i in list_level1_col) {
  df_mhs <- df_mhs %>%
    mutate(
      !!sym(i) := case_when(
        !!sym(i) == 'Sangat Setuju' ~ 'Strongly Agree',
        !!sym(i) == 'Setuju' ~ 'Agree',
        !!sym(i) == 'Netral' ~ 'Neutral',
        !!sym(i) == 'Ragu-ragu' ~ 'Neutral',
        !!sym(i) == 'Tidak Setuju' ~ 'Disagree',
        !!sym(i) == 'Sangat Tidak Setuju' ~ 'Strongly Disagree'
      )
  )
}







# StopWord

stopwerd <-  function(data, str_kolom){
  if (!str_kolom %in% names(data)) {
    stop(paste("The specified column '", str_kolom, "' was not found in the dataframe.", sep = ""))
  }

  # Correctly extract the column data
  text_data_from_column <- data[[str_kolom]]

  # Check if the extracted column data is empty or unprocessable
  if (is.null(text_data_from_column) || 
      length(text_data_from_column) == 0 || 
      all(is.na(text_data_from_column)) || 
      all(text_data_from_column == "")) {
    stop("The column is empty or contains no processable text.") # This matches your error
  }

  # Example of how to start your processing:
  text_list <- unlist(strsplit(text_data_from_column, "\\W+"))
  text_list <- tolower(text_list)
  text_list <- trimws(text_list)
  text_list <- text_list[!is.na(text_list) & text_list != ""]
  
  text_df_intermediate <- data.frame(table(text = text_list))

  # Fetch stopwords from the local file
  # Make sure "stopwords-id.txt" is in your R working directory or provide the full path.
  # Example: local_stopw_file <- "C:/Users/YourName/Documents/R_Projects/stopwords-id.txt"
  local_stopw_file <- "stopwords-id.txt" 

  stopw <- tryCatch({
    readLines(local_stopw_file, warn = FALSE) 
    }, error = function(e) {
      message(paste("Could not read stopwords from local file:", local_stopw_file))
      message("Original error: ", e$message)
      message("Please ensure the file exists at the specified location and is readable.")
      message("Proceeding without stopwords or using a fallback list if defined elsewhere.")
      c("di", "ke", "dari", "yang", "dan", "ini", "itu", "adalah", "dengan", "untuk") 
    })

  text_df_intermediate <- text_df_intermediate[!is.element(text_df_intermediate$text, stopw), ]
  text_df_intermediate <- text_df_intermediate[text_df_intermediate$Freq > 6, ] # Your Freq filter

  # --- End of data preparation ---


  # 1. Store current margin settings and set new ones for the word cloud plot
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(mar = c(0, 0, 0, 0))

  # 2. Define your color gradient
  start_color <- "#555555"
  end_color <- "lightgrey"

  color_palette_func <- colorRampPalette(c(start_color, end_color))

  # 3. Generate the word cloud
  if (nrow(text_df_intermediate) > 0 && "Freq" %in% names(text_df_intermediate) && "text" %in% names(text_df_intermediate) && sum(text_df_intermediate$Freq) > 0) {
    
    # Determine the number of unique words to plot (respecting min.freq if it were > 1)
    # For min.freq = 1, this is just the number of rows in text_df
    num_words_to_plot <- nrow(text_df_intermediate) 
    
    # Ensure we have at least one color, even if only one word
    num_gradient_colors <- max(1, num_words_to_plot) 
    gradient_colors <- color_palette_func(num_gradient_colors)

    wordcloud(
      words = text_df_intermediate$text,
      freq = text_df_intermediate$Freq,
      min.freq = 1,           # If you increase this, num_words_to_plot logic might need adjustment
      random.order = FALSE,
      ordered.colors = TRUE,  # This is TRUE, so colors vector length matters
      rot.per = 0.25,
      colors = gradient_colors, # Now 'colors' will match the length of words
      scale = c(10,2)
    )
  } else {
    plot.new()
    try(title(main = "No processable words for word cloud", 
              sub = "Check your input 'df$C11' and stopword list.", 
              cex.main = 1, cex.sub = 0.8), silent = TRUE)
    message("Warning: Not enough valid words to generate a word cloud after filtering.")
    message("Please check the content of 'df$C11' and your stopword processing.")
  }

  # Original parameters are restored by on.exit(par(opar))
}

stopwerd(data = df_mhs, str_kolom = "C11")



analyze_chi_square <- function(data, dependent_var, independent_vars, filename = "chi_square_analysis_results.xlsx") {
  
  data <- data %>%
    mutate(!!sym(dependent_var) := case_when(
      .data[[dependent_var]] == 'Lower Intermediate\n( TOEIC 350, IELTS 3.5, TOEFL 433)' ~ 'Lower Intermediate',
      .data[[dependent_var]] == 'Basic\n(TOEIC 255, IELTS 2.5, TOEFL 347)'                 ~ 'Basic',
      .data[[dependent_var]] == 'Upper Intermediate\n(TOEIC 500, IELTS 5.0, TOEFL 477)' ~ 'Upper Intermediate',
      .data[[dependent_var]] == 'Advanced\n(TOEIC 685, IELTS 6.5, TOEFL 550)'              ~ 'Advanced',
      TRUE ~ as.character(.data[[dependent_var]])
    ))
  # Create an empty list to store the results
  results_list <- list()
  
  # Loop through each independent variable
  for (var in independent_vars) {
    # Ensure the columns exist in the data frame
    if (!var %in% names(data) || !dependent_var %in% names(data)) {
      warning(paste("Variable '", var, "' or '", dependent_var, "' not found. Skipping.", sep=""))
      next # Skip to the next iteration
    }
    
    # Create a contingency table
    contingency_table <- table(data[[var]], data[[dependent_var]])
    
    # Perform the Chi-Square test
    # A tryCatch block handles cases where the test cannot be performed
    test_result <- tryCatch({
      # The change is adding the simulate.p.value argument right here
      chisq.test(contingency_table, simulate.p.value = TRUE)
      
    }, error = function(e) NULL)
    
    # The rest of the code remains unchanged
    if (is.null(test_result)) {
      warning(paste("Chi-square test failed for variable '", var, "'. Skipping.", sep=""))
      next
    }
    # Calculate Cramér's V
    v <- cramerV(contingency_table, ci = FALSE)
    
    # Identify significant cell contributions
    significant_cells_text <- ""
    if (test_result$p.value <= 0.05) {
      residuals <- test_result$stdres
      significant_cells <- which(abs(residuals) > 2, arr.ind = TRUE)
      
      if (nrow(significant_cells) > 0) {
        cell_names <- apply(significant_cells, 1, function(cell_indices) {
          row_name <- rownames(residuals)[cell_indices[1]]
          col_name <- colnames(residuals)[cell_indices[2]]
          resid_val <- round(residuals[cell_indices[1], cell_indices[2]], 2)
          paste0(row_name, " & ", col_name, " (resid: ", resid_val, ")")
        })
        significant_cells_text <- paste(cell_names, collapse = "; ")
      }
    }
    
    # Interpret effect size
    df <- min(nrow(contingency_table) - 1, ncol(contingency_table) - 1)
    v_interpretation <- case_when(
      v < 0.10 / sqrt(df) ~ "Negligible",
      v < 0.30 / sqrt(df) ~ "Small",
      v < 0.50 / sqrt(df) ~ "Medium",
      TRUE ~ "Large"
    )
    
    # Store all results for the current variable
    results_list[[var]] <- data.frame(
      "Independent Variable" = var,
      "X-squared" = round(test_result$statistic, 2),
      "df" = test_result$parameter,
      "p-value" = round(test_result$p.value, 4),
      "p-value Conclusion" = ifelse(test_result$p.value <= 0.05, "Significant", "Not Significant"),
      "Cramér's V" = round(v, 4),
      "Effect Size" = v_interpretation,
      "Significant Contributions" = significant_cells_text,
      check.names = FALSE
    )
  }
  
  # Combine the list of results into a single data frame
  results_table <- bind_rows(results_list)
  
  # Save the final table to an Excel file
  write_xlsx(results_table, path = filename)
  
  # Print a confirmation message
  print(paste("Analysis complete. Results saved to", filename))
  
  # Return the results table
  return(results_table)
}

# 1. Define the variables you want to test
my_independent_vars <- c('X1','X3','X4','X5','X7')
my_dependent_var <- 'Y'
my_output_filename <- "116_Relationship_Analysis.xlsx"

# 2. Call the function to run the analysis and save the results
final_results <- analyze_chi_square(
  data = df_mhs, 
  dependent_var = my_dependent_var, 
  independent_vars = my_independent_vars,
  filename = my_output_filename
)

# 3. Display the final results table in the console using kable
kable(final_results, caption = "Comprehensive Chi-Square Test Results")


