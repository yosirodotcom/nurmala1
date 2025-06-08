install.packages("pacman")
pacman::p_load(dplyr, tidyr,writexl, rcompanion, knitr, likert, ggplot2, magrittr, readr, stringr, grid, shadowtext, wordcloud, googlesheets4, vcd, corrplot, DescTools, extrafont, showtext, scales)

df_mhs <- read_sheet("https://docs.google.com/spreadsheets/d/1Ab0iTaBz9IIBTP8CFFXlODIP1A4HBm5u2K-NuUBCbqI/edit?resourcekey=&gid=1244913331#gid=1244913331", sheet = "df_mhs")
col_mhs <- read_sheet("https://docs.google.com/spreadsheets/d/1Ab0iTaBz9IIBTP8CFFXlODIP1A4HBm5u2K-NuUBCbqI/edit?resourcekey=&gid=1089213745#gid=1089213745", sheet = "col_mhs")



# Prepare Data

## select column contain word "A" or "B" or "D"
df_mhs <- df_mhs %>% select(contains(c("C", "D")))
# filter df_mhs by D1 == "D3 Administrasi Bisnis" and D2 == "IV D" or D1 != "D3 Administrasi Bisnis"
df_mhs1 <- df_mhs %>% filter(D1 == "D3 Administrasi Bisnis", D2 == "IV D")
df_mhs2 <- df_mhs %>% filter(D1 != "D3 Administrasi Bisnis")
# h stack df_mhs1 and df_mhs2
df_mhs <- rbind(df_mhs1, df_mhs2)

df_mhs <- df_mhs %>%
  mutate(
    D3 = case_when(
      D3 == 'Kurang Mahir ( TOEIC 350, IELTS 3.5, TOEFL 433)' ~ 'Lower Intermediate\n( TOEIC 350, IELTS 3.5, TOEFL 433)',
      D3 == 'Dasar (TOEIC 255, IELTS 2.5, TOEFL 347)' ~ 'Basic\n(TOEIC 255, IELTS 2.5, TOEFL 347)',
      D3 == 'Sedang (TOEIC 500, IELTS 5.0, TOEFL 477)' ~ 'Upper Intermediate\n(TOEIC 500, IELTS 5.0, TOEFL 477)',
      D3 == 'Mahir (TOEIC 685, IELTS 6.5, TOEFL 550)' ~ 'Advanced\n(TOEIC 685, IELTS 6.5, TOEFL 550)'
  )
)

df_mhs <- df_mhs %>%
  mutate(
    D1 = case_when(
      D1 == 'D3 Administrasi Bisnis' ~ 'D3 Business Administration',
      D1 == 'D3 Akuntansi' ~ 'D3 Accounting',
      D1 == 'D3 Teknik Informatika' ~ 'D3 Information Technology',
      D1 == 'D4 Teknik Mesin Konversi Energi' ~ 'D4 Energy Conversion Engineering',
      D1 == 'D3 Teknologi Penangkapan Ikan' ~ 'D3 Fishing Technology',
      D1 == 'D4 Pengolahan Hasil Perkebunan Terpadu' ~ 'D4 Integrated Plantation Product Processing',
      D1 == 'D4 Arsitektur Bangunan Gedung' ~ 'D4 Building Architecture',
      D1 == 'D4 Akuntansi Sektor Publik' ~ 'D4 Public Sector Accounting',
      D1 == 'D4 Administrasi Negara' ~ 'D4 Public Administration',
      D1 == 'D4 Teknologi Rekayasa Sistem Elektronika' ~ 'D4 Electronic Systems Engineering Technology',
      D1 == 'D3 Teknik Mesin' ~ 'D3 Mechanical Engineering',
      D1 == 'D3 Teknik Elektronika' ~ 'D3 Electronic Engineering',
      TRUE ~ D1 
    )
  )

list_level1_col <- list('C4', 'C5', 'C6', 'C7', 'C8', 'C9', 'C10')
for (i in list_level1_col) {
  df_mhs <- df_mhs %>%
    mutate(
      !!sym(i) := case_when(
        !!sym(i) == 'Sangat setuju' ~ 'Strongly Agree',
        !!sym(i) == 'Setuju' ~ 'Agree',
        !!sym(i) == 'Netral' ~ 'Neutral',
        !!sym(i) == 'Tidak setuju' ~ 'Disagree',
        !!sym(i) == 'Sangat tidak setuju' ~ 'Strongly Disagree'
      )
  )
}

list_level2_col <- list('C1', 'C3', 'C12', 'C13')
for (i in list_level2_col) {
  df_mhs <- df_mhs %>%
    mutate(
      !!sym(i) := case_when(
        !!sym(i) == 'Selalu' ~ 'Always',
        !!sym(i) == 'Sering' ~ 'Frequently',
        !!sym(i) == 'Kadang-kadang' ~ 'Sometimes',
        !!sym(i) == 'Jarang' ~ 'Rarely',
        !!sym(i) == 'Tidak pernah' ~ 'Never'
      )
  )
}


# Functions
likert_bar <- function(dat, str_kolom, level){

  dat[[str_kolom]] <- factor(dat[[str_kolom]], levels=level)

  df_summary <- dat %>%
    group_by(!!sym(str_kolom)) %>%
    summarise(frequency = n(), .groups = 'drop') %>%
    tidyr::complete(!!sym(str_kolom), fill = list(frequency = 0)) %>%
    mutate(prob = frequency / nrow(dat)) %>%
    mutate(!!sym(str_kolom) := factor(!!sym(str_kolom), levels = level)) %>%
    filter(!is.na(!!sym(str_kolom))) %>%
    # Arrange by frequency in ascending order and reorder factor levels
    arrange(frequency) %>%
    mutate(!!str_kolom := factor(!!sym(str_kolom), levels = unique(!!sym(str_kolom))))

  keterangan <- col_mhs %>%
    filter(nama == str_kolom) %>%
    pull(keterangan)

  # Create gradient colors for the discrete levels
  n_levels <- length(level)
  bar_colors <- colorRampPalette(c("#555555", "lightgrey"))(n_levels)
  names(bar_colors) <- level

  p <- ggplot(df_summary, aes(x = !!sym(str_kolom), y = frequency, fill = !!sym(str_kolom))) +
    geom_bar(stat = "identity", width = 0.6) +
    geom_text(aes(label = paste0(frequency, " (", round(prob * 100, 2), "%)")),
              hjust = -0.1,
              vjust = 0.5, # Adjust this value for desired height above bar
              color = "black",
              size = 5,
              family = "Times New Roman") + # Removed position_stack
    labs(title = "",
        x = "\nResponse", y = "Frequency\n") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, 
                                  hjust = 0.5, # Changed from 1 to 0.5 for center alignment
                                  family = "Times New Roman", 
                                  color = "black", size = 15),
          axis.text.y = element_text(family = "Times New Roman", color = "black", size = 15),          
          axis.title.x = element_text(family = "Times New Roman", color = "black", size = 17),
          axis.title.y = element_text(family = "Times New Roman", color = "black", size = 17),
          plot.title = element_text(family = "Times New Roman", color = "black"),
          legend.position = "none",
          panel.grid.major = element_line(color = "#F5F5F5"),
          panel.grid.minor = element_line(color = "#F5F5F5")) +
    scale_fill_manual(values = bar_colors) +
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.32))) +
    coord_flip()

  p

}
# Likert Distribution
likert_bar(str_kolom = "C1", level=level2_en)
likert_bar(str_kolom = "C2", level=level3_en)
likert_bar(str_kolom = "C3", level=level2_en)
likert_bar(str_kolom = "C4", level=level1_en)
likert_bar(str_kolom = "C5", level=level1_en)
likert_bar(str_kolom = "C6", level=level1_en)
likert_bar(str_kolom = "C7", level=level1_en)
likert_bar(str_kolom = "C8", level=level1_en)
likert_bar(str_kolom = "C9", level=level1_en)
likert_bar(str_kolom = "C10", level=level1_en)
likert_bar(str_kolom = "C12", level=level2_en)
likert_bar(str_kolom = "C13", level=level2_en)
likert_bar(str_kolom = "C14", level=level3_en)


# Demografi
df_summary <- df_mhs %>%  
  group_by(D1) %>%
  summarise(frequency = n()) %>%
  mutate(prob = frequency / sum(frequency))
ggplot(df_summary, aes(x = reorder(D1, frequency), y = frequency, fill = frequency)) +
  geom_col() +
  geom_text(aes(label = paste0(frequency, " (", round(prob * 100, 1), "%)")),
            hjust = -0.1, 
            color = "black",      # Warna sudah hitam
            family = "Times New Roman", # Tambahkan font family
            size = 6) +
  coord_flip() +
  scale_fill_gradient(low = "#2A3439", high = "lightgrey") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.32))) +
  labs(x = NULL, 
       y = "Frequency", 
       title = "Respondent Distribution by Major") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(
      hjust = 1,
      vjust = 0.5,
      size = 18,
      color = "black",          # Warna sudah hitam
      family = "Times New Roman"  # Tambahkan font family
    ),
    axis.text.x = element_text(
      size = 15,
      color = "black",          # Warna sudah hitam
      family = "Times New Roman"  # Tambahkan font family
    ),
    axis.title.y = element_text( # Style untuk judul sumbu y ("Frequency")
      color = "black",
      family = "Times New Roman",
      size = 18, # Sesuaikan ukuran jika perlu
      margin = margin(r = 10) # Tambahkan margin jika perlu
    ),
    axis.title.x = element_text( # Style untuk judul sumbu x (meskipun NULL saat ini)
      color = "black",
      family = "Times New Roman",
      size = 11 # Sesuaikan ukuran jika perlu
    ),
    plot.title = element_text( # Style untuk judul plot
      color = "black",
      family = "Times New Roman",
      size = 20, # Sesuaikan ukuran jika perlu
      hjust = 0.5 # Pusatkan judul plot
    ),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 100)
  )


df_summary <- df_mhs %>%  
  group_by(D3) %>%
  summarise(frequency = n()) %>%
  mutate(prob = frequency / sum(frequency))
ggplot(df_summary, aes(x = reorder(D3, frequency), y = frequency, fill = frequency)) +
  geom_col() +
  geom_text(aes(label = paste0(frequency, " (", round(prob * 100, 1), "%)")),
            hjust = -0.1, 
            color = "black",      # Warna sudah hitam
            family = "Times New Roman", # Tambahkan font family
            size = 6) +
  coord_flip() +
  scale_fill_gradient(low = "#2A3439", high = "lightgrey") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.32))) +
  labs(x = NULL, 
       y = "Frequency", 
       title = "English Proficiency Distribution") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(
      hjust = 1,
      vjust = 0.5,
      size = 18,
      color = "black",          # Warna sudah hitam
      family = "Times New Roman"  # Tambahkan font family
    ),
    axis.text.x = element_text(
      size = 15,
      color = "black",          # Warna sudah hitam
      family = "Times New Roman"  # Tambahkan font family
    ),
    axis.title.y = element_text( # Style untuk judul sumbu y ("Frequency")
      color = "black",
      family = "Times New Roman",
      size = 18, # Sesuaikan ukuran jika perlu
      margin = margin(r = 10) # Tambahkan margin jika perlu
    ),
    axis.title.x = element_text( # Style untuk judul sumbu x (meskipun NULL saat ini)
      color = "black",
      family = "Times New Roman",
      size = 11 # Sesuaikan ukuran jika perlu
    ),
    plot.title = element_text( # Style untuk judul plot
      color = "black",
      family = "Times New Roman",
      size = 20, # Sesuaikan ukuran jika perlu
      hjust = 0.5 # Pusatkan judul plot
    ),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 100)
  )






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
my_independent_vars <- c('C1', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9', 'C10', 'C12', 'C13')
my_dependent_var <- 'D1'
my_output_filename <- "Prodi_Relationship_Analysis.xlsx"

# 2. Call the function to run the analysis and save the results
final_results <- analyze_chi_square(
  data = df_mhs, 
  dependent_var = my_dependent_var, 
  independent_vars = my_independent_vars,
  filename = my_output_filename
)

# 3. Display the final results table in the console using kable
kable(final_results, caption = "Comprehensive Chi-Square Test Results")


