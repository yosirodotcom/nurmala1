install.packages("pacman")
pacman::p_load(dplyr, tidyr, likert, ggplot2, magrittr, readr, stringr, grid, shadowtext, wordcloud, googlesheets4, vcd, corrplot, DescTools, extrafont, showtext)

df_mhs <- read_sheet("https://docs.google.com/spreadsheets/d/1Ab0iTaBz9IIBTP8CFFXlODIP1A4HBm5u2K-NuUBCbqI/edit?resourcekey=&gid=1244913331#gid=1244913331", sheet = "df_mhs")
col_mhs <- read_sheet("https://docs.google.com/spreadsheets/d/1Ab0iTaBz9IIBTP8CFFXlODIP1A4HBm5u2K-NuUBCbqI/edit?resourcekey=&gid=1089213745#gid=1089213745", sheet = "col_mhs")



# Prepare Data

## select column contain word "A" or "B" or "D"
df_mhs <- df_mhs %>% select(contains(c("A", "B", "D")))
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

list_level1_col <- list('A1','A3','A5')
level1_en <- c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree")
for (i in list_level1_col) {
  df_mhs <- df_mhs %>%
    mutate(
      !!sym(i) := case_when(
        !!sym(i) == 'Sangat setuju' ~ 'Strongly Agree',
        !!sym(i) == 'Setuju' ~ 'Agree',
        !!sym(i) == 'Ragu-ragu' ~ 'Neutral',
        !!sym(i) == 'Tidak setuju' ~ 'Disagree',
        !!sym(i) == 'Sangat tidak setuju' ~ 'Strongly Disagree'
      )
  )
}

list_level2_col <- list('A4','B1')
for (i in list_level2_col) {
  df_mhs <- df_mhs %>%
    mutate(
      !!sym(i) := case_when(
        !!sym(i) == 'Sangat setuju' ~ 'Strongly Agree',
        !!sym(i) == 'Setuju' ~ 'Agree',
        !!sym(i) == 'Netral' ~ 'Neutral',
        !!sym(i) == 'Neutral' ~ 'Neutral',
        !!sym(i) == 'Tidak setuju' ~ 'Disagree',
        !!sym(i) == 'Sangat tidak setuju' ~ 'Strongly Disagree'
      )
  )
}

# level3_en <- c("Perspective of life or system of values.", "Can be an architecture of a building.", "Can be a local product, including textiles.", "Can be a song, a story, or a poem.", "Everything is true")
# df_mhs <- df_mhs %>%
#   mutate(
#     !!sym('A2') := case_when(
#       !!sym('A2') == "Pandangan hidup atau sistem nilai." ~ "Perspective of life or system of values.",
#       !!sym('A2') == "Dapat berupa arsitektur sebuah bangunan." ~ "Can be an architecture of a building.",
#       !!sym('A2') == "Dapat berupa produk lokal termasuk tekstil." ~ "Can be a local product, including textiles.",
#       !!sym('A2') == "Dapat berupa nyanyian, dongeng, atau pantun." ~ "Can be a song, a story, or a poem.",
#       !!sym('A2') == "Semuanya benar" ~ "Everything is true"
#     )
# )




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
