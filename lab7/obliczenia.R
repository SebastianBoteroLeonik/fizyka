library(dplyr)
library(ggplot2)
library(latex2exp)

round_uncertainty <- function(val, uncer) {
  round(val, -floor(log10(uncer)) + 1)
}

deg_to_rad <- function(deg) {
  deg * pi / 180
}

deg_uncert <- deg_to_rad(0.82)

linreg_and_uncertainty <- function(x, y) {
reg <- lm(y ~ x)
cf <- reg$coefficients[2]
u_cf <- sqrt(diag(vcov(reg)))[2]
cat("coefficient:", round_uncertainty(cf, u_cf), "\n")
cat("uncertainty:", round_uncertainty(u_cf, u_cf), "\n")
}

df_zal <- read.csv2("zalamanie.csv")
df_zal_proc <- df_zal |>
  mutate(
    lp = 1:dim(df_zal)[1],
    .before = 1
  ) |>
  mutate(
    alpha_rad = deg_to_rad(alpha),
    beta_rad = deg_to_rad(beta),
  ) |>
  mutate(
    sin_alpha = sin(alpha_rad),
    u_sin_alpha = abs(cos(alpha_rad) * deg_uncert),
    sin_beta = sin(beta_rad),
    u_sin_beta = abs(cos(beta_rad) * deg_uncert)
  ) |>
  mutate(
    sin_alpha = round_uncertainty(sin_alpha, u_sin_alpha),
    u_sin_alpha = round_uncertainty(u_sin_alpha, u_sin_alpha),
    sin_beta = round_uncertainty(sin_beta, u_sin_beta),
    u_sin_beta = round_uncertainty(u_sin_beta, u_sin_beta),
  ) 

df_zal_proc |>
  select(-alpha_rad, -beta_rad) |>
  write.table(file = "sinusy.txt",
              col.names = c("Lp", "alfa", "beta", "sina", "usina", "sinb", "usinb"),
              quote = FALSE, dec = ",", sep = "\t", row.names = FALSE,
              na = "")

df_zal_proc |>
  ggplot(aes(x = sin_beta, y = sin_alpha)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  geom_errorbarh(aes(xmin = sin_beta - u_sin_beta, xmax = sin_beta + u_sin_beta)) +
  geom_errorbar(aes(ymin = sin_alpha - u_sin_alpha, ymax = sin_alpha + u_sin_alpha)) +
  labs(
    y = TeX("\\sin(\\alpha)"),
    x = TeX("\\sin(\\beta)")
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_line(colour = "black", size = 0.15)
  )  
# ggsave("lab7/Snellius.pdf", device = cairo_pdf,
#        width = 10, height = 6)

linreg_and_uncertainty(df_zal_proc$sin_beta, df_zal_proc$sin_alpha)

df_zal_proc |>
  mutate(ab = abs(alpha + beta - 90)) |>
  slice_min(order_by = ab) |>
  mutate(
    tan_alpha = tan(alpha_rad),
    u_tan_alpha = abs(deg_uncert/(cos(alpha_rad)^2))
    ) |>
  mutate(
    tan_alpha = round_uncertainty(tan_alpha, u_tan_alpha),
    u_tan_alpha = round_uncertainty(u_tan_alpha, u_tan_alpha)
  ) |>
  select(alpha, beta, tan_alpha, u_tan_alpha)

tibble(alpha_gr = 42.5) |>
  mutate(
    alpha_rad = deg_to_rad(alpha_gr),
  ) |>
  mutate(
    n = 1/sin(alpha_rad),
    u_n = abs(deg_to_rad(2) * cos(alpha_rad) / (sin(alpha_rad)^2))
  ) |>
  mutate(
    n = round_uncertainty(n, u_n),
    u_n = round_uncertainty(u_n, u_n)
  ) |>
  select(-alpha_rad)


df_pol <- read.csv2("polaryzacja.csv")

df_pol_proc <- df_pol |>
  mutate(
    lp = 1:dim(df_pol)[1],
    theta_rad = deg_to_rad(theta),
    .before = 1,
  ) |>
  mutate(
    cos4 = cos(theta_rad)^4,
    u_cos4 = abs(4 * cos(theta_rad)^3 * sin(theta_rad) * deg_uncert),
    u_µA = 0.025 * range
  ) |>
  mutate(
    cos4 = round_uncertainty(cos4, u_cos4),
    u_cos4 = round_uncertainty(u_cos4, u_cos4),
    µA = round_uncertainty(µA, u_µA),
    u_µA = round_uncertainty(u_µA, u_µA)
  )

# df_pol_proc |>
#   select(lp, theta, cos4, u_cos4, µA, range, u_µA) |>
#   write.table(file = "malus.txt",
#               col.names = c("Lp", "theta", "cos4", "u(cos4)", "I", "zakres", "u(I)"),
#               quote = FALSE, dec = ",", sep = "\t", row.names = FALSE,
#               na = "")

df_pol_proc |>
  ggplot(aes(x = cos4, y = µA)) +
  geom_point() +
  geom_errorbarh(aes(xmin = cos4 - u_cos4, xmax = cos4 + u_cos4)) +
  geom_errorbar(aes(ymin = µA - u_µA, ymax = µA + u_µA)) +
  geom_smooth(method = "lm", se = FALSE, colour = "black"
              , formula = "y ~ 0 + x"
              ) +
  labs(x = TeX("$\\cos^4(\\Delta \\theta)$"),
       y = TeX("$I$ $[\\mu A]$")) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_line(colour = "black", size = 0.15)
  )  
ggsave("malus.pdf", device = cairo_pdf,
       width = 10, height = 6)

# linreg_and_uncertainty(df_pol_proc$cos4, df_pol_proc$µA)
{
reg <- lm(df_pol_proc$µA ~ 0 + df_pol_proc$cos4)
summary(reg)
cf <- reg$coefficients
u_cf <- sqrt(diag(vcov(reg)))
cat("coefficient:", round_uncertainty(cf, u_cf), "\n")
cat("uncertainty:", round_uncertainty(u_cf, u_cf), "\n")
}

df_pol[1,2]

# df_pol_proc |>
#   mutate(I0_pred = µA/cos4) |>
#   filter(I0_pred < 2000) |>
#   ggplot(aes(x = I0_pred)) +
#   geom_density()

# 
# df_pol |>
#   mutate(csq = cos(theta*pi/180)^2,
#          rest = range%%3,
#          w = csq*(1-csq),
#          # err = (rest*range/50) + ((1-rest)*range/30)
#          err = 1000,
#          Ilin = I0 * csq,
#          Isq = I0 * csq ^2
#   ) |>
#   # print()
#   ggplot(aes(x = csq, y = µA)) +
#   geom_point() +
#   # scale_y_sqrt() +
#   # geom_errorbar(aes(ymin = µA - err, ymax = µA + err)) +
#   # geom_smooth(method = "lm") +
#   # geom_abline(intercept = reg$coefficients[1], slope = reg$coefficients[2])
#   geom_line(aes(y = Ilin), colour = "#0000DD") +
#   geom_line(aes(y = Isq), colour = "#119911")
# 
# 
# 
# reg <- lm(df_pol2$µA ~ df_pol2$csq, weights = df_pol2$w^2)
# 
# loess(df_pol2$µA ~ df_pol2$csq)
