install.packages("pacman")
pacman::p_load(dplyr, tidyr,writexl, rcompanion, knitr, likert, ggplot2, magrittr, readr, stringr, grid, shadowtext, wordcloud, googlesheets4, vcd, corrplot, DescTools, extrafont, showtext, scales)

df_mhs <- read_sheet("https://docs.google.com/spreadsheets/d/15A83iF86op1m31WSOio9CSI6Z9_UyhGMnjdh2QZ-DuA/edit?gid=1594826291#gid=1594826291", sheet = "responses")
col_mhs <- read_sheet("https://docs.google.com/spreadsheets/d/15A83iF86op1m31WSOio9CSI6Z9_UyhGMnjdh2QZ-DuA/edit?gid=489536093#gid=489536093", sheet = "colnames")


# Menentukan level jawaban sesuai urutan
level_jawaban1 <- c("Tidak Pernah", "Jarang", "Kadang-kadang", "Sering", "Selalu")
level_jawaban2 <- c("Sangat Tidak Setuju", "Tidak Setuju", "Netral", "Setuju", "Sangat Setuju")

# Daftar kolom yang akan dianalisis
kolom_analisis1 <- c("Q1", "Q3", "Q10", "Q11")
kolom_analisis2 <- c("Q4", "Q5", "Q6", "Q7", "Q8", "Q9")

df_mhs <- df_mhs %>%
  mutate(across(all_of(kolom_analisis2), ~factor(., levels = level_jawaban2)))

# ------------------------------------------------------------------------------
# 4. Membuat Tabel Statistik Deskriptif
# ------------------------------------------------------------------------------
# Kita akan mengubah data dari format 'wide' ke 'long' agar lebih mudah dianalisis.
# Kemudian, kita hitung frekuensi dan proporsi untuk setiap jawaban.

summary_table <- df_mhs %>%
  # Memilih kolom yang relevan
  select(all_of(kolom_analisis2)) %>%
  # Mengubah format data menjadi 'long'
  pivot_longer(
    cols = everything(), 
    names_to = "pertanyaan", 
    values_to = "jawaban"
    ) %>%
  # Menghitung jumlah untuk setiap kombinasi pertanyaan dan jawaban.
  # PERUBAHAN: Menambahkan .drop = FALSE agar semua level jawaban (termasuk yang 
  # jumlahnya 0) tetap dipertahankan dalam tabel.
  count(pertanyaan, jawaban, .drop = FALSE, name = "jumlah") %>%
  # Mengelompokkan berdasarkan pertanyaan untuk menghitung proporsi
  group_by(pertanyaan) %>%
  # Menghitung proporsi dan format persentase
  mutate(
    proporsi = jumlah / sum(jumlah),
    persentase = scales::percent(proporsi, accuracy = 0.1)
  ) %>%
  # Mengatur ulang urutan kolom agar lebih rapi
  select(pertanyaan, jawaban, jumlah, persentase, proporsi)

# Menampilkan tabel hasil
cat("--- Tabel Statistik Deskriptif ---\n\n")
print(as.data.frame(summary_table))


grey_palette <- c("#242323", "#404040", "#696969", "#A9A9A9", "#D3D3D3")


grafik_proporsi <- ggplot(summary_table, aes(x = jawaban, y = proporsi, fill = jawaban)) +
  # Membuat bar chart
  geom_col(show.legend = FALSE) +
  
  # Menambahkan label persentase di atas setiap bar
  geom_text(
    aes(label = persentase), 
    vjust = -0.5, # Posisi vertikal teks (sedikit di atas bar)
    size = 3
  ) +
  
  # Membuat plot terpisah untuk setiap pertanyaan
  facet_wrap(~ pertanyaan, ncol = 2) +
  
  # Mengatur format sumbu Y menjadi persentase
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, max(summary_table$proporsi) * 1.2)) +
  
  # PERUBAHAN: Menggunakan palet warna abu-abu yang sudah dibuat
  scale_fill_manual(values = grey_palette) +
  
  # Memberi judul dan label pada grafik
  labs(
    title = "Proporsi Respon Kuesioner per Pertanyaan",
    subtitle = "Visualisasi jawaban untuk Q1, Q3, Q10, dan Q11",
    x = "Kategori Jawaban",
    y = "Proporsi"
  ) +
  
  # Mengatur tema dan tampilan grafik
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1), # Memutar label sumbu X agar tidak tumpang tindih
    strip.text = element_text(face = "bold", size = 12), # Mengatur teks judul panel (Q1, Q3, dll)
    panel.spacing = unit(2, "lines")
  )

# Menampilkan grafik
print(grafik_proporsi)
