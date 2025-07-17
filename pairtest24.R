pacman::p_load(dplyr, knitr, googlesheets4, scales)


# URL Google Sheets
url_pre <- "https://docs.google.com/spreadsheets/d/1WLUwyiRo7__7J664WS2obbOwerb3qfWletPLnD4euXE/edit?gid=1244913331#gid=1244913331"
url_post <- "https://docs.google.com/spreadsheets/d/15A83iF86op1m31WSOio9CSI6Z9_UyhGMnjdh2QZ-DuA/edit?gid=1594826291#gid=1594826291"

# Membaca data dari Google Sheets
df_mhs_pre <- read_sheet(url_pre, sheet = "responses")
df_mhs_post <- read_sheet(url_post, sheet = "responses")

# --- 3. PERSIAPAN DAN PEMBERSIHAN DATA ---

# Daftar kolom yang akan dianalisis secara langsung
kolom_analisis <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q10", "Q11")

# **PERBAIKAN KUNCI: Mengubah data teks menjadi numerik**
# Fungsi untuk memetakan jawaban teks ke angka.
# !!! PENTING: Sesuaikan teks di bawah ini agar cocok dengan data Anda !!!
map_likert_to_numeric <- function(column) {
  case_when(
    column == "Sangat Tidak Setuju" ~ 1,
    column == "Tidak Setuju"        ~ 2,
    column == "Netral"              ~ 3,
    column == "Setuju"              ~ 4,
    column == "Sangat Setuju"       ~ 5,
    # Tambahkan pemetaan lain jika perlu
    # Baris di bawah ini akan mengubah nilai lain yang tidak cocok menjadi NA (Not Available)
    .default = NA_real_ 
  )
}

# Terapkan fungsi konversi ke semua kolom yang dianalisis
df_mhs_pre <- df_mhs_pre %>%
  mutate(across(all_of(kolom_analisis), map_likert_to_numeric))

df_mhs_post <- df_mhs_post %>%
  mutate(across(all_of(kolom_analisis), map_likert_to_numeric))

# Memastikan data berpasangan memiliki jumlah baris yang sama
min_rows <- min(nrow(df_mhs_pre), nrow(df_mhs_post))
df_mhs_pre <- df_mhs_pre[1:min_rows, ]
df_mhs_post <- df_mhs_post[1:min_rows, ]


# --- 4. MELAKUKAN UJI WILCOXON SIGNED-RANK ---
# Inisialisasi list untuk menyimpan hasil
hasil_analisis <- list()

# Loop untuk setiap kolom dalam daftar analisis
for (kolom in kolom_analisis) {
  # Mengambil data pre dan post untuk kolom saat ini
  data_pre <- df_mhs_pre[[kolom]]
  data_post <- df_mhs_post[[kolom]]
  
  # Melakukan uji Wilcoxon Signed-Rank
  test_result <- tryCatch({
    wilcox.test(data_pre, data_post, paired = TRUE, exact = FALSE, correct = FALSE)
  }, error = function(e) {
    list(statistic = NA, p.value = NA)
  })
  
  # Menyimpan hasil ke dalam list
  hasil_analisis[[kolom]] <- data.frame(
    Question = kolom,
    V_statistic = test_result$statistic,
    p_value = test_result$p.value
  )
}

# Menggabungkan semua hasil menjadi satu data frame
tabel_hasil <- bind_rows(hasil_analisis)

# Menambahkan kolom signifikansi dan memformat p-value
tabel_hasil <- tabel_hasil %>%
  rowwise() %>%
  mutate(
    Significance = case_when(
      is.na(p_value) | !is.numeric(p_value) ~ "N/A",
      p_value < 0.05 ~ "Significant",
      TRUE ~ "Not Significant"
    ),
    p_value_formatted = if (is.na(p_value) || !is.numeric(p_value)) {
        "N/A"
      } else {
        pvalue(p_value, accuracy = 0.001, add_p = TRUE)
      }
  ) %>%
  ungroup()

# --- 5. MENAMPILKAN HASIL ---
# Menampilkan tabel hasil dalam format yang rapi menggunakan knitr::kable
kable(tabel_hasil[, c("Question", "V_statistic", "p_value_formatted", "Significance")], 
      caption = "Wilcoxon Signed-Rank Test Results: Pre-test vs. Post-test",
      col.names = c("Question", "V Statistic", "P-Value", "Significance (α=0.05)"))
