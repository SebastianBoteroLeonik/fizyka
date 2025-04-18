library(dplyr)
library(ggplot2)
library(tidyr)

df_cu <- read.csv2("lab6/cu.csv")
df_pb <- read.csv2("lab6/pb.csv")
df_al <- read.csv2("lab6/al.csv")

round_uncertainty <- function(val, uncer) {
  round(val, -floor(log10(uncer)) + 1)
}

calc_log_and_uncert <- function(df) {
  df |>
    mutate(
      no = number,
      x = thickness,
      N = counts,
      u_N = sqrt(N),
      lnN = log(N),
      u_lnN = 1/u_N
    ) |>
    mutate(
      N = round_uncertainty(N, u_N),
      u_N = round_uncertainty(u_N, u_N),
      lnN = round_uncertainty(lnN, u_lnN),
      u_lnN = round_uncertainty(u_lnN, u_lnN),
    ) |>
    select(no, x, N, u_N, lnN, u_lnN)
}

save_table <- function(df, filename) {
df |>  
    write.table(file = paste("lab6/", filename, sep = ""),
                col.names = c("no", "d", "N", "uN", "lnN", "ulnN"),
                quote = FALSE, dec = ",", sep = "\t", row.names = FALSE,
                na = "")
}

proc_df_cu <- calc_log_and_uncert(df_cu)
proc_df_pb <- calc_log_and_uncert(df_pb)
proc_df_al <- calc_log_and_uncert(df_al)

save_table(proc_df_cu, "miedz.txt")
save_table(proc_df_pb, "olow.txt")
save_table(proc_df_al, "aluminium.txt")

log_plot <- function(df, title_suffix) {
  df |>
  ggplot(aes(x = x, y = lnN)) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  geom_point() +
  geom_errorbar(aes(ymin = lnN - u_lnN, ymax = lnN + u_lnN),
                width = 0.08, size = 0.5) +
  labs(
    y = "ln(N)",
    title = paste("Wykres logarytmu liczby zliczeń od grubości płytki",
                  title_suffix)
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_line(colour = "black", size = 0.15)
  )  
}

log_plot(proc_df_cu, "dla miedzi")
ggsave("lab6/dopasowanie_miedz.pdf", device = cairo_pdf,
       width = 10, height = 6)
log_plot(proc_df_pb, "dla ołowiu")
ggsave("lab6/dopasowanie_olow.pdf", device = cairo_pdf,
       width = 10, height = 6)
log_plot(proc_df_al, "dla aluminium")
ggsave("lab6/dopasowanie_aluminium.pdf", device = cairo_pdf,
       width = 10, height = 6)

bind_rows(
  proc_df_cu,
  proc_df_pb,
  proc_df_al,
  .id = "element"
) |>
  mutate(element = case_match(element,
                              "1" ~ "miedź",
                              "2" ~ "ołów",
                              "3" ~ "aluminium")) |>
  ggplot(aes(x = x, y = N, shape = element)) +
  geom_point(size = 1.8) +
  geom_errorbar(aes(ymin = N - u_N, ymax = N + u_N),
                width = 0.1, size = 0.7) +
  labs(
    title = "Wykres liczby zliczeń od grubości płytki dla wszystkich pomiarów",
    shape = "materiał płytki"
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_line(colour = "black", size = 0.2),
  ) +
  scale_shape_manual(values = c(0, 1, 2))

ggsave("lab6/zestawienie.pdf", device = cairo_pdf,
       width = 10, height = 6)

linreg_and_uncertainty <- function(df) {
reg <- lm(df$lnN ~ df$x)
cf <- -reg$coefficients[2]
u_cf <- sqrt(diag(vcov(reg)))[2]
cat("coefficient:", round_uncertainty(cf, u_cf), "\n")
cat("uncertainty:", round_uncertainty(u_cf, u_cf), "\n")
}

linreg_and_uncertainty(proc_df_cu)
linreg_and_uncertainty(proc_df_pb)
linreg_and_uncertainty(proc_df_al)
