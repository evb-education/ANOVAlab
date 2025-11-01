# ANOVAlab — anova_oneway.R
# ANOVA 1F + LSD + gráficos (violines y medias). Terminología: media general,
# efectos simples, intervalos LSD (UT1).

library(dplyr)
library(ggplot2)
library(tidyr)

# --- Violines con etiquetas + μ ---
plot_oneway_violins_labeled <- function(dat, fit){
  gm <- fit$grand
  ggplot(dat, aes(grupo, y, color = grupo, fill = grupo)) +
    geom_violin(alpha=.25) +
    geom_point(position = position_jitter(width=.06), alpha=.6) +
    stat_summary(fun=mean, geom="point", size=3, color="black") +
    stat_summary(fun=mean, geom="text",
                 aes(label=sprintf("%.2f", after_stat(y))), vjust=-1, color="black") +
    geom_hline(yintercept = gm, linetype="dashed", color="black") +
    annotate("label", x = Inf, y = gm,
             label = paste0("Media general = ", sprintf("%.2f", gm)),
             vjust = -0.4, hjust = 1.02, size = 3, fill = "white") +
    labs(x="Grupo", y="Respuesta", title="Diagrama de violines por variantes / niveles (grupos)") +
    theme_minimal()
}

# Tabla resumen
anova_oneway_fit <- function(dat){
  dat <- mutate(dat, grupo = factor(grupo))
  fit <- aov(y ~ grupo, data = dat)
  tidy <- broom::tidy(fit)
  
  mse  <- filter(tidy, term=="Residuals")$meansq
  dfR  <- filter(tidy, term=="Residuals")$df
  gm   <- mean(dat$y)
  means <- dat %>% group_by(grupo) %>% summarise(media = mean(y), n = n(), .groups="drop")
  
  sc_long <- tibble(
    Componente = c("SCE","SCR"),
    valor = c(filter(tidy, term=="grupo")$sumsq,
              filter(tidy, term=="Residuals")$sumsq)
  )
  
  anova_table <- tidy %>%
    dplyr::transmute(
      `Fuente` = dplyr::recode(term,
                               grupo     = "Entre grupos",
                               Residuals = "Residual"),
      `SC`  = sumsq,
      `gl`  = as.integer(df),
      `CM`  = meansq,
      `F`   = statistic,
      `p-valor` = p.value
    )
  
  # Fila Total (SCT y gl total = N-1)
  total_row <- tibble::tibble(
    `Fuente` = "Total",
    `SC`     = sum(tidy$sumsq, na.rm = TRUE),   # SCT
    `gl`     = sum(tidy$df,    na.rm = TRUE),   # N - 1
    `CM`     = NA_real_,
    `F`      = NA_real_,
    `p-valor`= NA_real_
  )
  
  anova_table <- dplyr::bind_rows(anova_table, total_row)
  
  list(fit=fit, mse=mse, dfR=dfR, grand=gm, means=means, sc_long=sc_long,
       anova_table=anova_table)
}

# --- SC apilada HORIZONTAL ---
plot_sc_oner_stacked_horizontal <- function(df, scale=c("Valor","Porcentaje"), caption=NULL){
  scale <- match.arg(scale)
  g <- ggplot(df, aes(x="Total", y=valor, fill=Componente)) +
    geom_col(width = .6) + coord_flip() +
    theme_minimal() + labs(x=NULL, y="SCT = SCE + SCR", caption=caption) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  if(scale=="Porcentaje"){
    tot <- sum(df$valor)
    g <- g + aes(y = valor/tot) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  }
  g
}

# --- Segmentos con jitter horizontal explícito (segmentos rojos VISIBLES) ---
plot_oneway_segments_jitter <- function(dat, fit, mode = "Ambos"){
  gm <- fit$grand
  mu_g <- dat %>% dplyr::group_by(grupo) %>% dplyr::summarise(media = mean(y), .groups="drop")
  
  # jitter fijo para que punto y segmento compartan x
  set.seed(1234)
  J <- dat %>%
    dplyr::mutate(xj = as.numeric(grupo) + runif(dplyr::n(), -0.12, 0.12)) %>%
    dplyr::left_join(mu_g, by="grupo")
  
  show_scr <- mode %in% c("Ambos","SCR (residual)")
  show_sce <- mode %in% c("Ambos","SCE (entre grupos)")
  
  ggplot(J, aes(x=xj, y=y, color = grupo)) +
    geom_point(alpha=.7) +
    (if (show_scr) geom_segment(aes(x=xj, xend=xj, y=y, yend=media),
                                linewidth=0.6, color="firebrick", alpha=.9) else NULL) +
    # Punto de media de grupo + etiqueta de su valor
    stat_summary(data = mu_g, aes(x = as.numeric(grupo), y = media),
                 fun=mean, geom="point", size=3, color="black", inherit.aes = FALSE) +
    geom_text(data = mu_g,
              aes(x = as.numeric(grupo), y = media, label = sprintf("%.2f", media)),
              vjust = -1, color = "black", inherit.aes = FALSE) +
    (if (show_sce) geom_segment(data=mu_g,
                                aes(x=as.numeric(grupo), xend=as.numeric(grupo),
                                    y=media, yend=gm),
                                inherit.aes = FALSE, color="black", linewidth=1.1) else NULL) +
    geom_hline(yintercept = gm, linetype="dashed", color="black") +
    annotate("label", x = Inf, y = gm,
             label = paste0("Media general = ", sprintf("%.2f", gm)),
             vjust = -0.4, hjust = 1.02, size = 3, fill = "white") +
    scale_x_continuous(breaks = unique(as.numeric(dat$grupo)),
                       labels = levels(dat$grupo)) +
    labs(
      x = "Grupo",
      y = "Respuesta",
      title = paste(
        "Segmentos rojos: distancias de cada observación a la media de cada variante/nivel (grupo). Usados en al cálculo de la SCR.",
        "Segmentos negros: distancias de la media de cada variante/nivel (grupo) a la media general. Usados en al cálculo de la SCE.",
        sep = "\n"
      )
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", lineheight = 1.1)
    )
}

