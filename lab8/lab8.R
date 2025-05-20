

round_uncertainty <- function(val, uncer) {
  round(val, -floor(log10(uncer)) + 1)
}

equtaion_one_for_d <- function(m, alpha) {
  lambda <- 589.3
  d <- m * lambda / sin(alpha) 
  u_d <- abs(-m * lambda * 0.00076 * 1/sin(alpha) * 1/ tan(alpha))
  c(round_uncertainty(d, u_d), round_uncertainty(u_d, u_d))
}

equtaion_one_for_d(2, 0.81449)
equtaion_one_for_d(1, 0.36826)
equtaion_one_for_d(1, 0.36477)
equtaion_one_for_d(2, 0.69697)
d_i <- c(1620.3, 1637.0, 1651.9, 1836.1)
d <- mean(d_i)

u_d <- sqrt(sd(d_i)^2 + 3.3^2)
round_uncertainty(d, u_d)
round_uncertainty(u_d, u_d)

R <- 589.6 / (589.6 - 589)

equtaion_one_for_lamdba <- function(alpha, m=1) {
  d <- 1690
  lambda <- sin(alpha) * d / m 
  u_lambda<- 1/m*sqrt(sin(alpha)^2*100^2 + d^2*cos(alpha)^2*0.00076)
  paste0(paste(round_uncertainty(lambda, u_lambda), round_uncertainty(u_lambda, u_lambda), 
               sep='('), ')')
}

equtaion_one_for_lamdba(0.33568)
equtaion_one_for_lamdba(0.36477)
equtaion_one_for_lamdba(0.36681)
equtaion_one_for_lamdba(0.37059)
equtaion_one_for_lamdba(0.38514)
equtaion_one_for_lamdba(0.38921)
equtaion_one_for_lamdba(0.39735)
equtaion_one_for_lamdba(0.40201)
equtaion_one_for_lamdba(0.40841)
equtaion_one_for_lamdba(0.37728)



