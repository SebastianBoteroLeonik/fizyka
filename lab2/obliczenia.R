s1 <- read.csv("lab2/SERIA1.txt", sep = "\t", dec = ",")
s2 <- read.csv("lab2/SERIA2.txt", sep = "\t", dec = ",")

library(dplyr)
library(ggplot2)

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
  geom_smooth(method = "lm", se = FALSE)


s1 |>
  arrange(UH) |>
  mutate(ln_I = log(Ia/first(Ia)),
         u_ln_I = sqrt(
           (uBIa/Ia)^2 + (first(uBIa)/first(Ia))^2
         ),
           (uBIa/Ia)^2 , (first(uBIa)/first(Ia))^2
         
         
  ) |>
  View()
