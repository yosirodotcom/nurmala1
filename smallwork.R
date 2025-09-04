pacman::p_load(dplyr, tidyr, knitr, stringr, googlesheets4, scales, gtsummary)

url <- 'https://docs.google.com/spreadsheets/d/1zJBf7jlJtkTd9CtDnKyKehSLjIT0n72V5pqJuqQCcq4/edit?gid=1948573793#gid=1948573793'
df <- read_sheet(url, sheet = "data")

df %>%
  select(Q2.10a) %>%
  gtsummary::tbl_summary()

#combine list of df$Q1.1.other seperated by ";"
paste(df$`Q1.2.other`[!is.na(df$`Q1.2.other`)], collapse = ";")

df1 <- df %>%
  mutate(Q1.8 = na_if(Q1.8, "Lainnya")) %>%
  mutate(Q1.8.combine = coalesce(Q1.8, Q1.8.other))

summary_table <- df1 %>%
  # Step 1: Count combinations of A and B
  count(Q1.8.combine, Q1.5) %>%
  
  # Step 2: Create the formatted string for each row.
  # We use ifelse() to handle "respondent" vs "respondents".
  mutate(
    summary_string = paste0(
      Q1.5, ":", n, " ", ifelse(n == 1, "respondent", "respondents")
    )
  ) %>%

  # Step 3: Group by A only, and combine the formatted strings
  group_by(Q1.4.combine) %>%
  summarise(
    Q1.5 = paste(summary_string, collapse = ", ")
  )

# Print the final result
writexl::write_xlsx(summary_table, "summary_table.xlsx") 


# Likert Summary
summarize_survey <- function(data, cols_to_summarize, answer_levels) {
  
  summary_table <- data %>%
    # 1. Pilih hanya kolom yang relevan untuk diproses
    select(all_of(cols_to_summarize)) %>%
    
    # 2. Ubah data dari format lebar ke panjang
    pivot_longer(
      cols = everything(),
      names_to = "Question",
      values_to = "Jawaban"
    ) %>%
    
    # Hapus baris jika ada jawaban NA (penting untuk akurasi persentase)
    filter(!is.na(Jawaban)) %>%
    
    # 3. Hitung total responden per pertanyaan (denominator untuk persentase)
    add_count(Question, name = "total_per_question") %>%
    
    # 4. KUNCI PERBAIKAN: Ubah 'Jawaban' menjadi factor dengan level yang ditentukan.
    # Ini akan memastikan semua kategori jawaban (bahkan yang tidak dipilih) tetap ada.
    mutate(Jawaban = factor(Jawaban, levels = answer_levels)) %>%
    
    # 5. Hitung frekuensi untuk setiap kombinasi. .drop = FALSE akan menjaga kombinasi 
    # dengan hitungan 0 (misal, jika tidak ada yang menjawab "Selalu" untuk Q1).
    count(Question, total_per_question, Jawaban, .drop = FALSE) %>%
    
    # 6. Hitung persentase dan buat string format "Jumlah (Persen%)"
    mutate(
      # Hindari pembagian dengan nol jika sebuah pertanyaan tidak punya jawaban sama sekali
      percentage = if_else(total_per_question == 0, 0, (n / total_per_question) * 100),
      summary_string = paste0(n, " (", sprintf("%.1f", percentage), "%)")
    ) %>%
    
    # 7. Pilih kolom yang relevan untuk hasil akhir
    select(Question, Jawaban, summary_string) %>%
    
    # 8. Pivot kembali ke format lebar. Tidak perlu 'values_fill' karena 
    # langkah `count(.drop=FALSE)` sudah memastikan semua sel akan terisi.
    pivot_wider(
      names_from = Jawaban,
      values_from = summary_string
    )
    
  return(summary_table)
}

colQ2.1 <- c("Q2.9a", "Q2.9b", "Q2.9c", "Q2.9d", "Q2.9e", "Q2.9f", "Q2.9g", "Q2.9h")
levelQ2.1 <- c("Tidak Pernah", "Jarang", "Kadang-kadang", "Sering", "Selalu")
hasil_ringkasan <- summarize_survey(
  data = df,
  cols_to_summarize = colQ2,
  answer_levels = levelQ2
)

colQ2.2 <- c("Q2.10a", "Q2.10b", "Q2.10c", "Q2.10d", "Q2.10e", "Q2.10f", "Q2.10g", "Q2.10h", "Q2.10i")
levelQ2.2 <- c("Sangat Tidak Setuju", "Tidak Setuju", "Netral", "Setuju", "Sangat Setuju")
hasil_ringkasan <- summarize_survey(
  data = df,
  cols_to_summarize = colQ2.2,
  answer_levels = levelQ2.2
)

writexl::write_xlsx(hasil_ringkasan, "hasil_ringkasan.xlsx")
