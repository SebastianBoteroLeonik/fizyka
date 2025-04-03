library(dplyr)
library(ggplot2)
library(tidyr)

df <- read.csv2("lab4/pomiary_predkosci.csv")

# Wykres
time_uncertainty <- sqrt((0.01^2)/3 + (0.3^2)/3)
distance_uncertainty <- 0.1/sqrt(3)

df_long <- df |>
  pivot_longer(-distance, names_to = "medium", values_to = "time")

df_long |>
  ggplot(aes(x = time, y = distance, group = medium)) +
  geom_point() +
  geom_errorbar(aes(ymin = distance - distance_uncertainty,
                    ymax = distance + distance_uncertainty), width = 0) +
  geom_errorbarh(aes(xmin = time - time_uncertainty,
                    xmax = time + time_uncertainty), height = 0) +
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

ggsave("lab4/s(t).pdf", device = cairo_pdf,
       width = 10, height = 6)

lm(df$distance ~ df$gliceryna)
lm(df$distance ~ df$olej)



#sformatowanie tabelki z danymi
df |>
  mutate(no_in_series = 1:dim(df)[1], .before = 1) |>
  select(no_in_series, distance, gliceryna, olej) |>
  write.table("lab4/odl_i_czasy.txt",
              col.names = c("Lp","s", "tg", "to"),
              quote = FALSE, dec = ",", sep = "\t", row.names = FALSE)

diameters <- read.table("lab4/srednice.txt", header = TRUE,
                        dec = ",")


# średnia średnica kulki i niepewność
mean_d <- mean(diameters$d)
mean_d_uncertainty <- sqrt(var(diameters$d) + (0.01^2)/3)
round_uncertainty(mean_d_uncertainty, mean_d_uncertainty)
round_uncertainty(mean_d, mean_d_uncertainty)

# gęstość kulki
m10 <- 1.13
m <- m10/10
u_m10 <- 0.01/sqrt(3)
u_m <- u_m10/10
r <- mean_d / 2
V <- 4*pi*r^3/3
ro <- m/V * 1000

u_r <- mean_d_uncertainty/2
u_V <-  2*pi*r^2*u_r
u_ro <- sqrt(
  (u_m/V)^2 + (m*u_V/V^2)^2
) * 1000
