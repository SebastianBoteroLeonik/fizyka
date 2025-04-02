library(dplyr)
library(ggplot2)
library(tidyr)

df <- read.csv2("lab4/pomiary_predkosci.csv")

# Wykres
# Brakuje niepewności
df_long <- df |>
  pivot_longer(-distance, names_to = "medium", values_to = "time")

df_long |>
  ggplot(aes(x = time, y = distance, group = medium)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  geom_label(
    data =  
      df_long |> arrange(distance) |>
      group_by(medium) |> summarise(lt = last(time), ld = last(distance)),
    mapping = aes(x = lt, y = ld, label = medium),
    nudge_x = -2, nudge_y = -1
  ) +
  labs(
    x = "czas [s]",
    y = "droga [cm]",
    title = "Wykres drogi od czasu", 
    subtitle = "dla kulek spadających w różnych ośrodkach"
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_line(colour = "black")
  )  +
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(0, NA))

ggsave("lab4/wykres.pdf", device = cairo_pdf,
       width = 10, height = 6)

lm(df$distance ~ df$gliceryna)
lm(df$distance ~ df$olej)
