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
df |>
  mutate(no_in_series = 1:dim(df)[1]) |>
  pivot_longer(-no_in_series, names_to = "series") |>
  mutate(series = substring(series, 2)) |>
  ggplot(aes(y = no_in_series, x = value, colour = series)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_errorbarh(aes(xmin = value - 0.1, xmax = value + 0.1), height = 0) +
  labs(
    y = "numer porządkowy pomiaru w serii",
    x = "zmierzona wartość (cm)",
    colour = "Seria (wg.\nczęstotliwości w Hz)"
  )
