install.packages("pacman")
pacman::p_load(dplyr, tidyr,writexl, rcompanion, knitr, likert, ggplot2, magrittr, readr, stringr, grid, shadowtext, wordcloud, googlesheets4, vcd, corrplot, DescTools, extrafont, showtext, scales)


# URL Google Sheets
url_pre <- "https://docs.google.com/spreadsheets/d/1WLUwyiRo7__7J664WS2obbOwerb3qfWletPLnD4euXE/edit?gid=124491331#gid=1244913331"
url_post <- "https://docs.google.com/spreadsheets/d/15A83iF86op1m31WSOio9CSI6Z9_UyhGMnjdh2QZ-DuA/edit?gid=1594826291#gid=1594826291"

# Membaca data dari Google Sheets
df_mhs_pre <- read_sheet(url_pre, sheet = "responses")
df_mhs_post <- read_sheet(url_post, sheet = "responses")

# --- 3. PERSIAPAN DAN PEMBERSIHAN DATA ---

# Memisahkan kolom berdasarkan jenis responsnya
kolom_persetujuan <- c("Q4", "Q5", "Q6", "Q7", "Q8")
kolom_frekuensi <- c("Q1", "Q3", "Q10", "Q11")
kolom_biner <- c("Q2")

# Daftar semua kolom yang akan dianalisis (tanpa Q12)
kolom_analisis <- c(kolom_persetujuan, kolom_frekuensi, kolom_biner)

# Fungsi untuk memetakan respons persetujuan (Sangat Tidak Setuju -> Sangat Setuju)
map_persetujuan_ke_numerik <- function(column) {
  case_when(
    column == "Sangat Tidak Setuju" ~ 1,
    column == "Tidak Setuju"        ~ 2,
    column == "Netral"              ~ 3,
    column == "Setuju"              ~ 4,
    column == "Sangat Setuju"       ~ 5,
    .default = NA_real_
  )
}

# Fungsi untuk memetakan respons frekuensi (Tidak Pernah -> Selalu)
map_frekuensi_ke_numerik <- function(column) {
  case_when(
    column == "Tidak Pernah"   ~ 1,
    column == "Jarang"         ~ 2,
    column == "Kadang-kadang"  ~ 3,
    column == "Sering"         ~ 4,
    column == "Selalu"         ~ 5,
    .default = NA_real_
  )
}

# Fungsi untuk memetakan respons biner (Ya/Tidak)
map_biner_ke_numerik <- function(column) {
  case_when(
    column == "Tidak" ~ 1,
    column == "Ya"    ~ 2,
    .default = NA_real_
  )
}

# Fungsi untuk menerapkan semua pemetaan ke data frame
konversi_data <- function(df) {
  df %>%
    mutate(across(all_of(kolom_persetujuan), map_persetujuan_ke_numerik)) %>%
    mutate(across(all_of(kolom_frekuensi), map_frekuensi_ke_numerik)) %>%
    mutate(across(all_of(kolom_biner), map_biner_ke_numerik))
}

# Terapkan fungsi konversi ke kedua data frame
df_mhs_pre <- konversi_data(df_mhs_pre)
df_mhs_post <- konversi_data(df_mhs_post)

# Memastikan data berpasangan memiliki jumlah baris yang sama
min_rows <- min(nrow(df_mhs_pre), nrow(df_mhs_post))
df_mhs_pre <- df_mhs_pre[1:min_rows, ]
df_mhs_post <- df_mhs_post[1:min_rows, ]


# --- 4. MELAKUKAN UJI STATISTIK (TWO-SIDED) ---
# Inisialisasi list untuk menyimpan hasil
hasil_analisis <- list()

# Loop untuk setiap kolom dalam daftar analisis
for (kolom in kolom_analisis) {
  data_pre <- df_mhs_pre[[kolom]]
  data_post <- df_mhs_post[[kolom]]
  
  # Uji Wilcoxon Signed-Rank
  wilcox_result <- tryCatch({
    wilcox.test(data_pre, data_post, paired = TRUE, exact = FALSE, correct = FALSE)
  }, error = function(e) {
    list(statistic = NA, p.value = NA)
  })
  
  # Uji Paired T-test
  ttest_result <- tryCatch({
    t.test(data_pre, data_post, paired = TRUE)
  }, error = function(e) {
    list(statistic = NA, p.value = NA)
  })
  
  # Menyimpan hasil dari kedua tes
  hasil_analisis[[kolom]] <- data.frame(
    Question = kolom,
    V_statistic = wilcox_result$statistic,
    p_value_wilcox = wilcox_result$p.value,
    t_statistic = ttest_result$statistic,
    p_value_ttest = ttest_result$p.value
  )
}

# Menggabungkan semua hasil menjadi satu data frame
tabel_hasil <- bind_rows(hasil_analisis)

# Menambahkan kolom yang diformat dan kolom signifikansi
tabel_hasil <- tabel_hasil %>%
  rowwise() %>%
  mutate(
    p_wilcox_formatted = if (is.na(p_value_wilcox)) "N/A" else pvalue(p_value_wilcox, accuracy = 0.001, add_p = TRUE),
    Significance_Wilcox = case_when(
      is.na(p_value_wilcox) ~ "N/A",
      p_value_wilcox < 0.05 ~ "Significant",
      TRUE ~ "Not Significant"
    ),
    p_ttest_formatted = if (is.na(p_value_ttest)) "N/A" else pvalue(p_value_ttest, accuracy = 0.001, add_p = TRUE),
    Significance_TTest = case_when(
      is.na(p_value_ttest) ~ "N/A",
      p_value_ttest < 0.05 ~ "Significant",
      TRUE ~ "Not Significant"
    )
  ) %>%
  ungroup()

# --- 5. MENAMPILKAN HASIL GABUNGAN (TWO-SIDED) ---
print(kable(tabel_hasil[, c("Question", "V_statistic", "p_wilcox_formatted", "Significance_Wilcox", "t_statistic", "p_ttest_formatted", "Significance_TTest")], 
      caption = "Perbandingan Hasil Uji Wilcoxon Signed-Rank dan Paired T-test (Two-Sided)",
      col.names = c("Pertanyaan", "Wilcoxon (V)", "P-Value (W)", "Signifikansi (W)", "T-test (t)", "P-Value (t)", "Signifikansi (t)")))


# --- 6. ANALISIS HIPOTESIS KHUSUS (LESS) UNTUK Q10 & Q11 ---
cat("\n\n--- Analisis Hipotesis Khusus ---\n")
kolom_hipotesis_less <- c("Q10", "Q11")
hasil_analisis_less <- list()

for (kolom in kolom_hipotesis_less) {
  data_pre <- df_mhs_pre[[kolom]]
  data_post <- df_mhs_post[[kolom]]
  
  # Uji Wilcoxon dengan hipotesis "less"
  wilcox_result_l <- tryCatch({
    wilcox.test(data_pre, data_post, paired = TRUE, alternative = "less", exact = FALSE, correct = FALSE)
  }, error = function(e) { list(statistic = NA, p.value = NA) })
  
  # Uji T-test dengan hipotesis "less"
  ttest_result_l <- tryCatch({
    t.test(data_pre, data_post, paired = TRUE, alternative = "less")
  }, error = function(e) { list(statistic = NA, p.value = NA) })
  
  hasil_analisis_less[[kolom]] <- data.frame(
    Question = kolom,
    V_statistic = wilcox_result_l$statistic,
    p_value_wilcox = wilcox_result_l$p.value,
    t_statistic = ttest_result_l$statistic,
    p_value_ttest = ttest_result_l$p.value
  )
}

tabel_hasil_less <- bind_rows(hasil_analisis_less)

tabel_hasil_less <- tabel_hasil_less %>%
  rowwise() %>%
  mutate(
    p_wilcox_formatted = if (is.na(p_value_wilcox)) "N/A" else pvalue(p_value_wilcox, accuracy = 0.001, add_p = TRUE),
    Significance_Wilcox = case_when(
      is.na(p_value_wilcox) ~ "N/A",
      p_value_wilcox < 0.05 ~ "Significant",
      TRUE ~ "Not Significant"
    ),
    p_ttest_formatted = if (is.na(p_value_ttest)) "N/A" else pvalue(p_value_ttest, accuracy = 0.001, add_p = TRUE),
    Significance_TTest = case_when(
      is.na(p_value_ttest) ~ "N/A",
      p_value_ttest < 0.05 ~ "Significant",
      TRUE ~ "Not Significant"
    )
  ) %>%
  ungroup()

# --- 7. MENAMPILKAN HASIL UJI HIPOTESIS KHUSUS ---
print(kable(tabel_hasil_less[, c("Question", "V_statistic", "p_wilcox_formatted", "Significance_Wilcox", "t_statistic", "p_ttest_formatted", "Significance_TTest")],
      caption = "Hasil Uji Hipotesis One-Sided (Post < Pre) untuk Q10 & Q11",
      col.names = c("Pertanyaan", "Wilcoxon (V)", "P-Value (W)", "Signifikansi (W)", "T-test (t)", "P-Value (t)", "Signifikansi (t)")))
