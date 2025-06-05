pacman::p_load(dplyr, tidyr, likert, ggplot2, magrittr, readr, stringr, grid, shadowtext, wordcloud, googlesheets4, vcd, corrplot, DescTools, extrafont, showtext)

df_dosen <- read_sheet("https://docs.google.com/spreadsheets/d/1SUKklelRoL8JoekQ0SOmXTskqfs6EvHfRo7qQkRVKys/edit?gid=221341234#gid=221341234", sheet = "df_dosen")
col_dosen <- read_sheet("https://docs.google.com/spreadsheets/d/1SUKklelRoL8JoekQ0SOmXTskqfs6EvHfRo7qQkRVKys/edit?gid=2031418127#gid=2031418127", sheet = "col_dosen")

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
