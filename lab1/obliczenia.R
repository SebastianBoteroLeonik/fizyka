setwd("~/Documents/STUDIA/Semestr 4/Fizyka/")
df <- read.csv("R4.txt", sep = ";")
colnames(df) <- c("I", "z_A", "u_A", "V", "z_V", "u_V")
library(ggplot2)
library(dplyr)
library(tikzDevice)

# tikz(filename = "r4_plot.tex")
ggplot(df, aes(x = I, y = V)) +
  geom_smooth(method = "lm") +
  geom_point() +
  geom_errorbar(aes(ymin = V-u_V, ymax = V+u_V), width = 0) +
  geom_errorbarh(aes(xmin = I-u_A, xmax = I+u_A), height = 0) +
  scale_x_continuous(labels = (\(x) x*1000)) +
  labs(
    x = "I (mA)",
    y = "U (V)"
  )
# dev.off()
# ggsave(device = "tex",
#        filename = "r4_plot")

?lm
reg <- lm(df$V ~ df$I)

ggplot(df, aes(x = I, y = V)) +
  geom_smooth(method = "lm") +
  geom_abline(slope = reg$coefficients[2], intercept = reg$coefficients[1]) +
  geom_point() +
  geom_errorbar(aes(ymin = V-u_V, ymax = V+u_V), width = 0) +
  geom_errorbarh(aes(xmin = I-u_A, xmax = I+u_A), height = 0) +
  scale_x_continuous(labels = (\(x) x*1000)) +
  labs(
    x = "I (mA)",
    y = "U (V)"
  )

df |>
  mutate(u_R = sqrt(
    (u_V/I)^2 + ((V*u_A)/(I^2))^2
  )) |>
  View()
