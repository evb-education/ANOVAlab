# ANOVAlab — anova_twoway.R
# 2F: ajuste ANOVA, violines, segmentos con modos (SCR/SCE/Ambos/Ninguno),
#     LSD (A y B a la vez) e interacción (modos).
# Mantiene nombres esperados por el módulo y añade alias defensivos.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(broom)
})

# ============================= AJUSTE ANOVA ==============================

anova_twoway_fit <- function(dat, alpha = 0.05){
  if (is.null(dat)) return(NULL)  # defensa mínima
  dat <- dplyr::mutate(dat, A = factor(A), B = factor(B))
  
  fit  <- aov(y ~ A * B, data = dat)
  tidy <- broom::tidy(fit)
  mse  <- dplyr::filter(tidy, term == "Residuals")$meansq
  dfR  <- dplyr::filter(tidy, term == "Residuals")$df
  gm   <- mean(dat$y)
  
  means_A  <- dat %>% group_by(A)   %>% summarise(media = mean(y), n = n(), .groups = "drop")
  means_B  <- dat %>% group_by(B)   %>% summarise(media = mean(y), n = n(), .groups = "drop")
  means_AB <- dat %>% group_by(A,B) %>% summarise(media = mean(y), n = n(), .groups = "drop")
  
  sc_long <- tidy %>%
    dplyr::filter(term %in% c("A","B","A:B","Residuals")) %>%
    dplyr::transmute(
      Componente = dplyr::recode(term, A = "SCA", B = "SCB",
                                 `A:B` = "SCA×B", Residuals = "SCR"),
      valor      = sumsq
    )
  
  anova_table <- tidy %>%
    dplyr::transmute(
      `Fuente`             = dplyr::recode(term, A = "A", B = "B",
                                           `A:B` = "A×B", Residuals = "Residual"),
      `Suma de Cuadrados`  = sumsq,
      `Grados de libertad` = df,
      `Cuadrado medio`     = meansq,
      `F`                  = statistic,
      `p-valor`            = p.value
    )
  
  list(
    fit = fit, mse = mse, dfR = dfR, grand = gm, alpha = alpha,
    means_A = means_A, means_B = means_B, means_AB = means_AB,
    sc_long = sc_long, anova_table = anova_table, dat = dat
  )
}

# ============================ UTILIDADES LSD =============================

.lsd_bounds <- function(means_df, groups_df, mse, dfR, alpha){
  n_eff <- groups_df %>%
    group_by(across(everything())) %>%
    summarise(n_eff = n(), .groups = "drop")
  key_col <- setdiff(names(n_eff), "n_eff")
  out <- dplyr::left_join(means_df, n_eff, by = key_col)
  out$lsd  <- qt(1 - alpha/2, dfR) * sqrt(mse / out$n_eff)
  out$ymin <- out$media - out$lsd
  out$ymax <- out$media + out$lsd
  out
}

# ============================ REPRESENTACIONES ===========================

# ---- 1) VIOLINES A×B (estilo 1F: sin segmentos) ----
plot_twoway_violins <- function(dat, fit){
  gm <- fit$grand
  
  dat <- dat %>%
    dplyr::mutate(AB = interaction(A, B, sep = "×", drop = TRUE))
  
  # medias por combinación A×B
  mu_ab <- dat %>%
    dplyr::group_by(AB, B) %>%
    dplyr::summarise(media = mean(y), .groups = "drop")
  
  ggplot() +
    geom_violin(data = dat,
                aes(x = AB, y = y, fill = B),
                width = .85, alpha = .35, color = NA) +
    geom_point(data = mu_ab,
               aes(x = AB, y = media),
               size = 3, color = "black") +
    geom_text(data = mu_ab,
              aes(x = AB, y = media, label = sprintf("%.2f", media)),
              vjust = -1, color = "black") +
    geom_hline(yintercept = gm, linetype = "dashed", color = "black") +
    labs(x = "Combinaciones de variantes / niveles de A y B",
         y = "Respuesta",
         title = "Gráfico de Violines A×B con medias de cada grupo y μ") +
    theme_minimal()
}
plot_twoway_violins_labeled <- function(dat, fit) plot_twoway_violins(dat, fit)

# ---- 2) SEGMENTOS A×B (modo: SCR/SCE/Ambos/Ninguno; estilo 1F) ----
# mode = "both" (por defecto) | "scr" | "sce" | "none"
plot_twoway_segments <- function(dat, fit, mode = c("both","scr","sce","none")){
  mode <- match.arg(mode)
  gm <- fit$grand
  
  # Etiqueta A×B y posición numérica estable (para jitter y eje)
  dat2 <- dat %>%
    dplyr::mutate(AB = interaction(A, B, sep = "×", drop = TRUE))
  ab_levels <- levels(dat2$AB)
  dat2 <- dat2 %>% dplyr::mutate(pos = match(AB, ab_levels))
  
  # Medias por combinación A×B (guardando AB y pos)
  mu_ab <- dat2 %>%
    dplyr::group_by(AB, pos) %>%
    dplyr::summarise(media = mean(y), .groups = "drop")
  
  # Datos con jitter y unión de medias (para trazar residuos)
  set.seed(1234)
  J <- dat2 %>%
    dplyr::left_join(mu_ab, by = c("AB","pos")) %>%
    dplyr::mutate(
      x_jit = pos + stats::runif(dplyr::n(), min = -0.08, max = 0.08)
    )
  
  # Texto docente coherente con UT1 (más claro y compacto)
  # SCR: distancia de cada observación a la media de su combinación A×B (residual)
  # SCE: distancia de la media de cada combinación A×B a la media general (explicada total)
  #     En 2F: SCE = SCA + SCB + SCA×B
  nota <- paste(
    "Segmentos rojos: distancias de cada observación a la media de su combinación de variantes / niveles de A×B. Usados en el cálculo de SCR",
    "Segmentos negros: distancias de las medias de las combinaciones de variantes / niveles A×B a la media general. Usados en el cálculo de SCE.",
    "Donde SCE = SCA + SCB + SCA×B.",
    sep = "\n"
  )
  y_top <- max(dat$y, na.rm = TRUE)
  
  g <- ggplot()
  
  # (1) puntos crudos con jitter (negros semitransparentes) — siempre
  g <- g + geom_point(data = J,
                      aes(x = x_jit, y = y, color = B),
                      size = 1.2, alpha = 0.35, show.legend = TRUE)
  
  # (2) segmento residual (punto -> media A×B) — ROJO (según modo)
  if (mode %in% c("both","scr")) {
    g <- g + geom_segment(data = J,
                          aes(x = x_jit, xend = pos, y = y, yend = media),
                          linewidth = 0.6, color = "red", alpha = 0.9)
  }
  
  # (3) media A×B (punto ○ blanco con borde negro) — siempre
  g <- g + geom_point(data = mu_ab,
                      aes(x = pos, y = media),
                      size = 3.2, shape = 21, fill = "white",
                      color = "black", stroke = 0.9)
  
  # (4) media A×B -> μ — NEGRO (según modo)
  if (mode %in% c("both","sce")) {
    g <- g + geom_segment(data = mu_ab,
                          aes(x = pos, xend = pos, y = media, yend = gm),
                          color = "black", linewidth = 1.1)
  }
  
  # (5) μ (línea discontinua) + etiqueta — siempre
  g <- g + geom_hline(yintercept = gm, linetype = "dashed", color = "black") +
    annotate("label", x = max(mu_ab$pos), y = gm,
             label = sprintf("Media general = %.2f", gm),
             vjust = -0.6, label.size = NA, fill = NA, color = "black")
  
  # (6) Nota docente (siempre)
  g <- g + annotate("text", x = min(mu_ab$pos) + 0.2, y = y_top,
                    label = nota, hjust = 0, vjust = 1, size = 3.2)
  
  # (7) Eje X y etiquetas
  g <- g + scale_x_continuous(breaks = mu_ab$pos, labels = mu_ab$AB) +
    labs(x = "Combinaciones de variantes / niveles de A y B",
         y = "Respuesta", color = "B",
         title = "Segmentos A×B: elige SCR / SCE / Ambos / Ninguno") +
    theme_minimal()
  
  g
}
plot_twoway_segments_labeled <- function(dat, fit, mode = c("both","scr","sce","none")){
  plot_twoway_segments(dat, fit, mode = mode)
}

# ---- 3) INTERACCIÓN ----
plot_interaction_only <- function(fit){
  gm <- fit$grand
  ggplot(fit$means_AB, aes(B, media, group = A, color = A)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 2.6) +
    geom_text(aes(label = sprintf("%.2f", media)), vjust = -0.9, show.legend = FALSE) +
    geom_hline(yintercept = gm, linetype = "dashed", color = "black") +
    labs(x = "B", y = "Media", color = "A", title = "Gráfico de interacción") +
    theme_minimal()
}
# Interacción con ±LSD en cada combinación A×B
plot_interaction_lsd <- function(fit){
  gm <- fit$grand; mse <- fit$mse; dfR <- fit$dfR; alpha <- fit$alpha
  byAB <- fit$dat %>% group_by(A,B) %>% summarise(n_eff = n(), .groups = "drop")
  dfc  <- dplyr::left_join(fit$means_AB, byAB, by = c("A","B")) %>%
    dplyr::mutate(lsd = qt(1 - alpha/2, dfR) * sqrt(mse/n_eff),
                  ymin = media - lsd, ymax = media + lsd)
  ggplot(dfc, aes(B, media, color = A, group = A)) +
    geom_line() + geom_point(size = 2.6) +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = .12, linewidth = 0.7) +
    geom_text(aes(label = sprintf("%.2f", media)), vjust = -0.9, show.legend = FALSE) +
    geom_hline(yintercept = gm, linetype = "dashed", color = "black") +
    labs(x = "B", y = "Media", color = "A", title = "Interacción con LSD (verticales)") +
    theme_minimal()
}
# Modo de interacción: "lines" / "lsd" / "none"
plot_interaction_mode <- function(fit, mode = c("lines","lsd","none")){
  mode <- match.arg(mode)
  switch(mode,
         lines = plot_interaction_only(fit),
         lsd   = plot_interaction_lsd(fit),
         none  = NULL
  )
}
# Aliases compatibles
plot_interaction         <- function(fit) plot_interaction_only(fit)
plot_interaction_labeled <- function(fit) plot_interaction_only(fit)

# ---- 4) LSD de factores (A y B a la vez) ----
plot_LSD_marginals <- function(fit, show = TRUE, ...){
  if(!show) return(NULL)
  gm <- fit$grand; mse <- fit$mse; dfR <- fit$dfR; alpha <- fit$alpha
  dfA <- .lsd_bounds(fit$means_A, fit$dat["A"], mse, dfR, alpha) %>%
    dplyr::mutate(x = as.character(A), Faceta = "Factor A")
  dfB <- .lsd_bounds(fit$means_B, fit$dat["B"], mse, dfR, alpha) %>%
    dplyr::mutate(x = as.character(B), Faceta = "Factor B")
  dfx <- dplyr::bind_rows(dfA, dfB)
  
  ggplot(dfx, aes(x, media)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = .15, linewidth = 0.7) +
    geom_text(aes(label = sprintf("%.2f", media)), vjust = -1) +
    geom_hline(yintercept = gm, linetype = "dashed", color = "black") +
    facet_wrap(~Faceta, scales = "free_x") +
    labs(x = NULL, y = "Media", title = "LSD por factor (A y B)") +
    theme_minimal()
}
# Aliases que puede invocar el módulo
plot_LSD_main        <- function(fit, show = TRUE, ...)                                    plot_LSD_marginals(fit, show = show)
plot_LSD_main_factor <- function(fit, lsd_on = c("Factor A","Factor B"), show = TRUE, ...) plot_LSD_marginals(fit, show = show)
plot_LSD_marginal    <- function(fit, apply_to = c("Factor A","Factor B"), show = TRUE, ...) plot_LSD_marginals(fit, show = show)
plot_LSD_A           <- function(fit, show = TRUE, ...)                                    plot_LSD_marginals(fit, show = show)
plot_LSD_B           <- function(fit, show = TRUE, ...)                                    plot_LSD_marginals(fit, show = show)

# ---- 5) SC apiladas horizontales ----
plot_sc_twoway_stacked_horizontal <- function(df, scale = c("Valor","Porcentaje"), caption = NULL){
  scale <- match.arg(scale)
  g <- ggplot(df, aes(x = "Total", y = valor, fill = Componente)) +
    geom_col(width = .6) + coord_flip() +
    theme_minimal() +
    labs(x = NULL, y = if (scale == "Valor") "Suma de Cuadrados Total (SCT)" else "Porcentaje",
         caption = caption) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  if (scale == "Porcentaje"){
    tot <- sum(df$valor)
    g <- g + aes(y = valor / tot) +
      scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"))
  }
  g
}

# ============================ COMPATIBILIDAD ==============================

# Si el módulo busca nombres alternativos, apuntamos a las versiones correctas:
if (!exists("plot_twoway_violins_labeled"))   plot_twoway_violins_labeled   <- function(dat, fit) plot_twoway_violins(dat, fit)
if (!exists("plot_twoway_segments_labeled"))  plot_twoway_segments_labeled  <- function(dat, fit, mode = c("both","scr","sce","none")) plot_twoway_segments(dat, fit, mode = mode)