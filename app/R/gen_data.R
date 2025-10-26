# ANOVAlab — gen_data.R
# Generación de datos para 1F y 2F (plan factorial equilibrado).

# Generadores compatibles con los controles
gen_oneway_from_delta <- function(k, n, delta, sigma){
  base <- 0
  means <- base + seq(-(k-1)/2, (k-1)/2, length.out = k) * delta
  groups <- rep(paste0("G", seq_len(k)), each = n)
  y <- unlist(lapply(means, function(m) rnorm(n, m, sigma)))
  data.frame(y = y, grupo = factor(groups))
}

gen_twoway_from_sliders <- function(I, J, n, effA, effB, intAB, sigma){
  A <- factor(rep(paste0("A",1:I), each = J*n))
  B <- factor(rep(rep(paste0("B",1:J), each = n), times = I))
  a_eff <- seq(-(I-1)/2, (I-1)/2, length.out = I) * effA
  b_eff <- seq(-(J-1)/2, (J-1)/2, length.out = J) * effB
  AB <- outer(a_eff, b_eff, function(a,b) a + b + intAB*(a*b)/max(1, max(abs(a_eff))*max(abs(b_eff))))
  y <- numeric(I*J*n); idx <- 1
  for(i in 1:I) for(j in 1:J){
    y[idx:(idx+n-1)] <- rnorm(n, AB[i,j], sigma); idx <- idx + n
  }
  data.frame(y=y, A=A, B=B)
}