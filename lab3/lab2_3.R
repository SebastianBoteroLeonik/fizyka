s1 <- read.csv("SERIA1.txt", sep = "\t", dec = ",")
s2 <- read.csv("SERIA2.txt", sep = "\t", dec = ",")

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
    ) 
  #|>
    #select(ln_I, u_ln_I, UH, uBUH)
}

t1 <- transform(s1)
t2 <- transform(s2)

reg1 <- lm(t1$ln_I ~ t1$UH)
summary(reg1)
reg2 <- lm(t2$ln_I ~ t2$UH)
summary(reg2)

df <- bind_rows(t1, t2, .id = "seria")
df |>
  filter(UH != 0) |>
  #mutate(UH = -UH) |>
  ggplot(aes(x = UH, y = Ia, colour = seria)) +
  geom_point(aes(shape = seria), size = 2) +
  geom_errorbarh(aes(xmin = UH - uBUH, xmax = UH + uBUH)) +
  geom_errorbar(aes(ymin = Ia - uBIa, ymax = Ia + uBIa)) +
  coord_cartesian(xlim = c(0, 1.5)) +
  labs(
    x = TeX("$U_H$ $(V)$"),
    y = TeX("$I_a$ $(\\mu A)$"))
  # geom_errorbarh(aes(xmin = UH - uBUH, xmax = UH + uBUH)) +
  # geom_errorbar(aes(ymin = ln_I - u_ln_I, ymax = ln_I + u_ln_I)) +
  # geom_smooth(method = "lm", se = FALSE)


df |>
  # filter(series==1) |>
  ggplot(aes(x = UH, y = ln_I, colour = seria)) +
  geom_point(aes(shape = seria), size = 2) +
  geom_errorbarh(aes(xmin = UH - uBUH, xmax = UH + uBUH)) +
  geom_errorbar(aes(ymin = ln_I - u_ln_I, ymax = ln_I + u_ln_I)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = TeX("$U_H$ $(V)$"),
    y = TeX("$\\ln{\\frac{I_a}{I_{a,0}}}$"),
    color = "seria"
  )



s1 |>
  arrange(UH) |>
  mutate(ln_I = log(Ia/first(Ia)),
         u_ln_I = sqrt(
           (uBIa/Ia)^2 + (first(uBIa)/first(Ia))^2
         ),
           (uBIa/Ia)^2 , (first(uBIa)/first(Ia))^2
  ) 



# Lab 3
calc_kappa <- function(first_obv, last_obv, num_obv, freq, temp = 293){
  cat((2*(1/100*(last_obv - first_obv)/num_obv))^2 * freq^2 * 4.81 * 1e-26 *
    1/(1.3806 * 1e-23 * temp))
}
kappas <- c(1.401161, 1.400417, 1.415298, 1.404684, 1.461697, 1.456019,
            1.474255, 1.462029, 1.484825)
kappa <- mean(kappas)
err = sd(kappas)*sqrt(1/9)
kappa + 3*sd(kappas)

calc_u_kappa <- function(uf, first_obv, last_obv, num_obv, f, ul = 0.16, t = 293, ut=0.013){
  M = 4.81 * 1e-26
  k = 1.3806 * 1e-23
  #cat(
    sqrt(
      (2*f*(2*(1/100*(last_obv - first_obv)/num_obv))^2*M/(k*t)* uf)^2 +
        (2*(2*(1/100*(last_obv - first_obv)/num_obv))*f^2*M/(k*t)* 1/100*ul)^2 +
        (-(2*(1/100*(last_obv - first_obv)/num_obv))^2*f^2*M/(k*t^2)* ut)^2
      )
    #)
  
}


u_kappas <- c(
  calc_u_kappa(130,0.4,47.6,11,4000),
  calc_u_kappa(170,2.9,48.2,12,4550),
  calc_u_kappa(200,0,48.3,14,5000),
  calc_u_kappa(250,1.6,48,15,5560),
  calc_u_kappa(120,1.5,47.5,16,6100),
  calc_u_kappa(140,1.4,49.9,18,6490),
  calc_u_kappa(160,0.5,48,19,7040),
  calc_u_kappa(190,1.2,49.8,21,7580),
  calc_u_kappa(210,0.2,48.4,22,8060)
  )
