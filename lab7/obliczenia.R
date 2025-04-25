library(dplyr)
library(ggplot2)

round_uncertainty <- function(val, uncer) {
  round(val, -floor(log10(uncer)) + 1)
}

df_zal <- sin(read.csv2("lab7/zalamanie.csv")*pi/180)

lm(df_zal$alpha ~ df_zal$beta)

df_pol <- read.csv2("lab7/polaryzacja.csv")

df_pol_proc <- df_pol |>
  mutate(
    lp = 1:dim(df_pol)[1],
    theta_rad = theta*pi/180,
    .before = 1,
  ) |>
  mutate(
    cos4 = cos(theta_rad)^4,
    u_cos4 = abs(4 * cos(theta_rad)^3 * sin(theta_rad) * (0.82 * pi /180)),
    u_µA = 0.025 * range
  ) |>
  mutate(
    cos4 = round_uncertainty(cos4, u_cos4),
    u_cos4 = round_uncertainty(u_cos4, u_cos4),
    µA = round_uncertainty(µA, u_µA),
    u_µA = round_uncertainty(u_µA, u_µA)
  )

df_pol_proc |>
  select(lp, theta, cos4, u_cos4, µA, range, u_µA) |>
  write.table(file = "lab7/malus.txt",
              col.names = c("Lp", "theta", "cos4", "u(cos4)", "I", "zakres", "u(I)"),
              quote = FALSE, dec = ",", sep = "\t", row.names = FALSE,
              na = "")

I0 <- df_pol[1,2]

df_pol |>
  mutate(csq = cos(theta*pi/180)^2,
         rest = range%%3,
         w = csq*(1-csq),
         # err = (rest*range/50) + ((1-rest)*range/30)
         err = 1000,
         Ilin = I0 * csq,
         Isq = I0 * csq ^2
  ) |>
  # print()
  ggplot(aes(x = csq, y = µA)) +
  geom_point() +
  # scale_y_sqrt() +
  # geom_errorbar(aes(ymin = µA - err, ymax = µA + err)) +
  # geom_smooth(method = "lm") +
  # geom_abline(intercept = reg$coefficients[1], slope = reg$coefficients[2])
  geom_line(aes(y = Ilin), colour = "#0000DD") +
  geom_line(aes(y = Isq), colour = "#119911")



reg <- lm(df_pol2$µA ~ df_pol2$csq, weights = df_pol2$w^2)

loess(df_pol2$µA ~ df_pol2$csq)
