# x1 <- c(
#   0.4,
#   4.7,
#   9,
#   13.3,
#   17.6,
#   21.8,
#   26.1,
#   30.4,
#   34.7,
#   39,
#   43.3,
#   47.6
# )
# 
# x2 <- c(
#   2.9,
# 6.7,
# 10.5,
# 14.3,
# 18,
# 21.8,
# 25.6,
# 29.4,
# 33.2,
# 36.9,
# 40.7,
# 44.5,
# 48.2
# )
# 
# x<-x2
# 
# y <- 1:length(x)
# 
# reg<-lm(x~y)
# reg$coefficients[2]
# summary(reg)
# 
# data_frame(x = x, y= y) |>
#   ggplot(aes(x = x, y = y)) +
#   geom_point() +
#   geom_smooth(method = "lm")

library(dplyr)
library(ggplot2)
library(tidyr)

df <- read.csv2("lab3/Pomiary.csv")
plot_df <- df |>
  mutate(no_in_series = 1:dim(df)[1]) |>
  pivot_longer(-no_in_series, names_to = "series") |>
  mutate(series = substring(series, 2)) |>
  filter(!is.na(value))
plot_df |>
  ggplot(aes(y = no_in_series, x = value, colour = series)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_errorbarh(aes(xmin = value - 0.1, xmax = value + 0.1), height = 0) +
  labs(
    y = "numer porządkowy pomiaru w serii",
    x = "zmierzona wartość (cm)"
  ) + 
  theme(
    legend.position = "none"
  ) +
  geom_label(
    data = (
      plot_df |>
        group_by(series) |> 
        summarise(last = last(value), last_no = last(no_in_series))
    ),
    aes(x = last, y = last_no, label = series),
    nudge_x = 1.5
  )

ggsave("lab3/wykres.pdf", device = cairo_pdf,
       width = 10, height = 6)

# Wyliczenia ze wzorów 4-6

data_frame(series_no = 1:length(df),
           freq = as.numeric(substring(colnames(df), 2))) |>
  mutate(
    period = round(1e6/(freq)),
         freq = 1e6/period,
         range = ifelse(freq<6000,
                        50,
                        20),
         period_uncertainty = sqrt(2*(range/(5*sqrt(3)))^2),
  ) |>
  select(-freq, freq) |>
  mutate(
    freq_uncertainty = 1e6*period_uncertainty/(period^2)
  ) |>
  # mutate(podz = period/range)
  write.table("lab3/okresy_i_czest.txt",
              col.names = c("seria","Ts","z(Ts)","u(Ts)","f","u(f)"),
              quote = FALSE, dec = ",", sep = "\t", row.names = FALSE)

df |>
  mutate(no_in_series = 1:dim(df)[1], .before = 1) |>
  write.table("lab3/odczyty_x.txt",
              col.names = c("Lp","s1","s2","s3","s4","s5","s6","s7","s8","s9"),
              quote = FALSE, dec = ",", sep = "\t", row.names = FALSE,
              na = "")
  