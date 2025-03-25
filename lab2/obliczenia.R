s1 <- read.csv("lab2/SERIA1.txt", sep = "\t", dec = ",")
s2 <- read.csv("lab2/SERIA2.txt", sep = "\t", dec = ",")

library(dplyr)
library(ggplot2)
library(latex2exp)

transform <- function(s) {
  s |>
    arrange(UH) |>
    mutate(ln_I = log(Ia/first(Ia)),
           u_ln_I = sqrt(
             (uBIa/Ia)^2 + (first(uBIa)/first(Ia))^2
           )
    ) |>
    select(ln_I, u_ln_I, UH, uBUH)
}

t1 <- transform(s1)
t2 <- transform(s2)

reg1 <- lm(t1$ln_I ~ t1$UH)
reg2 <- lm(t2$ln_I ~ t2$UH)

df <- bind_rows(t1, t2, .id = "series")
df |>
  # filter(series==1) |>
  ggplot(aes(x = UH, y = ln_I, colour = series)) +
  geom_point() +
  geom_errorbarh(aes(xmin = UH - uBUH, xmax = UH + uBUH)) +
  geom_errorbar(aes(ymin = ln_I - u_ln_I, ymax = ln_I + u_ln_I)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = TeX("$U_H$"),
    y = TeX("$\\ln{\\frac{I_a}{I_{a,0}}}$"),
    color = "seria"
  )

ggsave("lab2/dopasowanie.pdf", device = cairo_pdf,
       width = 10, height = 6)

s1 |>
  arrange(UH) |>
  mutate(ln_I = log(Ia/first(Ia)),
         u_ln_I = sqrt(
           (uBIa/Ia)^2 + (first(uBIa)/first(Ia))^2
         ),
           (uBIa/Ia)^2 , (first(uBIa)/first(Ia))^2
         
         
  ) |>
  View()

e <- 1.602176634*10^(-19)
me <- 9.10938291*10^(-32)

s1 |>
  ggplot(aes(x = sqrt(UH), y = Ia)) +
  geom_line() +
  labs(
    y = TeX("$ I_a$ $(\\mu A)$"),
    x = TeX("$\\sqrt{U_H}$ $(\\sqrt{V})$"),
    title = "Wykres zależności prądu anodowego\nod pierwiastku napięcia hamującego"
  ) +
  theme(title = element_text(family = "ArialMT"))

ggsave("lab2/task4_plot.pdf", device = cairo_pdf,
       width = 10, height = 6)
  # geom_line(data = s2, aes(x = sqrt(UH), y = Ia))
  # geom_smooth()

