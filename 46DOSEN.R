pacman::p_load(dplyr, tidyr, likert, ggplot2, magrittr, readr, stringr, grid, shadowtext, wordcloud, googlesheets4, vcd, corrplot, DescTools, extrafont, showtext, rlang)

df_dosen <- read_sheet("https://docs.google.com/spreadsheets/d/1SUKklelRoL8JoekQ0SOmXTskqfs6EvHfRo7qQkRVKys/edit?gid=221341234#gid=221341234", sheet = "df_dosen")
col_dosen <- read_sheet("https://docs.google.com/spreadsheets/d/1SUKklelRoL8JoekQ0SOmXTskqfs6EvHfRo7qQkRVKys/edit?gid=2031418127#gid=2031418127", sheet = "col_dosen")

# Prepare Data
list_level_col <- list('B')
known_likert_levels <- c('Sangat Setuju', 'Setuju', 'Netral', 'Tidak Setuju', 'Sangat Tidak Setuju')
for (i in list_level_col) {
  unique_values_in_col <- unique(na.omit(df_dosen[[i]]))
  unmatched_values <- unique_values_in_col[!unique_values_in_col %in% known_likert_levels]
    
  if (length(unmatched_values) > 0) {    
    unmatched_string <- paste(unmatched_values, collapse = ", ")
    message(paste0("Info: Nilai tidak cocok ditemukan di kolom '", i, "'. Nilai tersebut adalah: '", unmatched_string, "'"))
  }  
  
  df_dosen <- df_dosen %>%
    mutate(
      !!sym(i) := case_when(
        !!sym(i) == 'Sangat Setuju' ~ 'Strongly\nAgree',
        !!sym(i) == 'Setuju' ~ 'Agree',
        !!sym(i) == 'Netral' ~ 'Neutral',
        !!sym(i) == 'Tidak Setuju' ~ 'Disagree',
        !!sym(i) == 'Sangat Tidak Setuju' ~ 'Strongly\nDisagree',
        TRUE ~ as.character(!!sym(i)) 
      )
    )
}

level1_en <- c("Strongly\nAgree", "Agree", "Neutral", "Disagree", "Strongly\nDisagree")
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

  keterangan <- col_dosen %>%
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

split_comma_column_str <- function(dat, split_col_name, factor_list) {
  # Split the comma-separated values and create separate rows
  result <- dat %>%
    separate_rows(all_of(split_col_name), sep = ", ") %>%
    # Remove any leading/trailing whitespace and quotes
    mutate(across(all_of(split_col_name), ~ trimws(gsub("^'|'$", "", .)))) %>%
    # Remove empty values if any
    filter(.data[[split_col_name]] != "") %>%
    # Filter to only include values that match our factor list
    filter(.data[[split_col_name]] %in% factor_list)

  return(result)
}



# Pie Chart
## Slide 2
df_dosen_summary <- df_dosen %>%
  group_by(A) %>%
  summarise(frequency = n()) %>%
  mutate(percentage = frequency / sum(frequency))

ggplot(df_dosen_summary, aes(x = "", y = percentage, fill = A)) +

  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(
    aes(label = paste0(A, "\n", round(percentage * 100), "%")),
    position = position_stack(vjust = 0.5),
    size = 10,
    family = "Times New Roman", # Sets the font
    color = "black"            # Sets ALL text color to black
  ) +
  labs(title = NULL) +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("lightgrey", "#555555"))



# Likert

## Slide 3
likert_bar(dat=df_dosen, str_kolom = "B", level=level1_en)

## Slide 4
level8 = c("Produk Lokal/Tradisional", "Industri Kreatif", "Bisnis Digital", "Sistem Nilai/ Kepercayaan/Keyakinan", "MICE", "language", "Jasa (service)", "Food and Beverage", "Culture", "Brand")
dat = split_comma_column_str(df_dosen, "C", level8)
dat <- dat %>%
  mutate(
    C := case_when(
      C == "Produk Lokal/Tradisional" ~ "Local Product/Traditional",
      C == "Industri Kreatif" ~ "Creative Industry",
      C == "Bisnis Digital" ~ "Digital Business",
      C == "Sistem Nilai/ Kepercayaan/Keyakinan" ~ "System of Values/Trust/Faith",
      C == "MICE" ~ "MICE",
      C == "language" ~ "Language",
      C == "Jasa (service)" ~ "Service",
      C == "Food and Beverage" ~ "Food and Beverage",
      C == "Culture" ~ "Culture",
      C == "Brand" ~ "Brand",
      TRUE ~ as.character(C) 
    )
  )

level8_en <- c("Local Product/Traditional", "Creative Industry", "Digital Business", "System of Values/Trust/Faith", "MICE", "Language", "Service", "Food and Beverage", "Culture", "Brand")
likert_bar(dat, "C", level8_en)

## Slide 10
level9 = c("Teks tertulis (Reading)", "Teks audiovisual (Video)", "Teks audio/lisan (Listening)")
dat = split_comma_column_str(df_dosen, "I", level9)
dat <- dat %>% 
  mutate(
    I := case_when(
      I == "Teks tertulis (Reading)" ~ "Textual (Reading)",
      I == "Teks audiovisual (Video)" ~ "Visual (Video)",
      I == "Teks audio/lisan (Listening)" ~ "Auditory (Listening)",
      TRUE ~ as.character(I)
    )
)
level9_en <- c("Textual (Reading)", "Visual (Video)", "Auditory (Listening)")
likert_bar(dat, "I", level9_en)

# Stopword
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
  text_df_intermediate <- text_df_intermediate[text_df_intermediate$Freq > 2, ] # Your Freq filter

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
      scale = c(8,1.5)
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
## Slide 5
stopwerd(df_dosen, "D")
## Slide 6
stopwerd(df_dosen, "E")
## Slide 7
stopwerd(df_dosen, "F")
## Slide 8
stopwerd(df_dosen, "G")
## Slide 9
stopwerd(df_dosen, "H")


# Correlation Analysis for A and B
## Slide 11
## Table contingency
table(df_dosen$A, df_dosen$B)
## Chi-square test
chisq.test(df_dosen$A, df_dosen$B)


# Slide 1
likert_bar(df_dosen, "N", unique(df_dosen$N))
