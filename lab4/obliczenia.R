library(dplyr)
library(ggplot2)
library(tidyr)

df <- read.csv2("lab4/pomiary_predkosci.csv")
#df <- read.csv2("pomiary_predkosci.csv")

round_uncertainty <- function(val, uncer) {
  round(val, -floor(log10(uncer)) + 1)
}
# Wykres
time_uncertainty <- sqrt((0.01^2)/3 + (0.3^2)/3)
round_uncertainty(time_uncertainty, time_uncertainty)
distance_uncertainty <- 0.1/sqrt(3)
round_uncertainty(distance_uncertainty, distance_uncertainty)

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

# ggsave("lab4/s(t).pdf", device = cairo_pdf,
#        width = 10, height = 6)

reg_gli <- lm(df$distance ~ df$gliceryna)
reg_oil <- lm(df$distance ~ df$olej)

v_gr_gli <- reg_gli$coefficients[2]
v_gr_oil <- reg_oil$coefficients[2]

u_v_gr_gli <- sqrt(diag(vcov(reg_gli)))[2]
u_v_gr_oil <- sqrt(diag(vcov(reg_oil)))[2]

round_uncertainty(v_gr_gli, u_v_gr_gli)
round_uncertainty(u_v_gr_gli, u_v_gr_gli)
round_uncertainty(v_gr_oil, u_v_gr_oil)
round_uncertainty(u_v_gr_oil, u_v_gr_oil)

#sformatowanie tabelki z danymi
# df |>
#   mutate(no_in_series = 1:dim(df)[1], .before = 1) |>
#   select(no_in_series, distance, gliceryna, olej) |>
#   write.table("lab4/odl_i_czasy.txt",
#               col.names = c("Lp","s", "tg", "to"),
#               quote = FALSE, dec = ",", sep = "\t", row.names = FALSE)

diameters <- read.table("lab4/srednice.txt", header = TRUE,
                        dec = ",")
# diameters <- read.table("srednice.txt", header = TRUE,
#                         dec = ",")


# średnia średnica kulki i niepewność
mean_d <- mean(diameters$d)
mean_d_uncertainty <- sqrt(var(diameters$d)/length(diameters$d) + (0.01^2)/3)
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

round_uncertainty(u_r, u_r)
round_uncertainty(r, u_r)
round_uncertainty(u_V, u_V)
round_uncertainty(V, u_V)
round_uncertainty(u_ro, u_ro)
round_uncertainty(ro, u_ro)

#lepkość
g <- 100*9.80665
ro_gli <- 1.261
ro_oil <- 0.867
R <- 40
u_R <- 0.3
visc_gli <- (2 * (r/10)^2 * g * (ro - ro_gli))/(9 * v_gr_gli * (1 + 2.4 * (r/R))) * 1/10
visc_oil <- (2 * (r/10)^2 * g * (ro - ro_oil))/(9 * v_gr_oil * (1 + 2.4 * (r/R))) * 1/10

u_visc_gli <- visc_gli * sqrt(
  ((2/r - 2.4/(R + 2.4*r))*u_r)^2 + (u_ro / (ro - ro_gli))^2 +
    (u_v_gr_gli/v_gr_gli)^2 + (2.4*u_R/(R*(R + 2.4*r)))^2
)
u_visc_oil <- visc_oil * sqrt(
  ((2/r - 2.4/(R + 2.4*r))*u_r)^2 + (u_ro / (ro - ro_oil))^2 +
    (u_v_gr_oil/v_gr_oil)^2 + (2.4*u_R/(R*(R + 2.4*r)))^2
)

round_uncertainty(u_visc_gli, u_visc_gli)
round_uncertainty(visc_gli, u_visc_gli)
round_uncertainty(u_visc_oil, u_visc_oil)
round_uncertainty(visc_oil, u_visc_oil)

tau_gli <- m / (6 * pi * visc_gli * r)
tau_oil <- m / (6 * pi * visc_oil * r)

u_tau_gli <- sqrt((u_m / (6 * pi * visc_gli * r))^2 + (tau_gli / visc_gli * u_visc_gli)^2 +
                   (tau_gli / r * u_r)^2)
u_tau_oil <- sqrt((u_m / (6 * pi * visc_oil * r))^2 + (tau_oil / visc_oil * u_visc_oil)^2 +
                    (tau_oil / r * u_r)^2)

round_uncertainty(u_tau_gli, u_tau_gli)
round_uncertainty(tau_gli, u_tau_gli)
round_uncertainty(u_tau_oil, u_tau_oil)
round_uncertainty(tau_oil, u_tau_oil)
