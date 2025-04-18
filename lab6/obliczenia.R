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
    select(no , x, N, u_N, lnN, u_lnN)
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

View(proc_df_cu)

proc_df_cu |>
  ggplot(aes(x = x, y = lnN)) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  geom_point() +
  geom_errorbar(aes(ymin = lnN - u_lnN, ymax = lnN + u_lnN),
                width = 0) +
  labs(
    y = "ln(N)",
    title = "Wykres logarytmu zliczeń od grubości płytki"
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_line(colour = "black")
  )  
