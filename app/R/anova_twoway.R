# ANOVAlab — anova_twoway.R
# ANOVA 2F, medias marginales, medias por celda, LSDs para efectos simples y simples condicionados.

library(dplyr)
library(ggplot2)
library(tidyr)

anova_twoway_fit <- function(dat, alpha = 0.05){
  dat <- mutate(dat, A=factor(A), B=factor(B))
  fit <- aov(y ~ A*B, data = dat)
  tidy <- broom::tidy(fit)
  mse <- filter(tidy, term=="Residuals")$meansq
  dfR <- filter(tidy, term=="Residuals")$df
  gm  <- mean(dat$y)
  
  means_A  <- dat %>% group_by(A)   %>% summarise(media=mean(y), n=n(), .groups="drop")
  means_B  <- dat %>% group_by(B)   %>% summarise(media=mean(y), n=n(), .groups="drop")
  means_AB <- dat %>% group_by(A,B) %>% summarise(media=mean(y), n=n(), .groups="drop")
  
  sc_long <- tidy %>% filter(term %in% c("A","B","A:B","Residuals")) %>%
    transmute(Componente = recode(term, A="SCA", B="SCB", `A:B`="SCA×B", Residuals="SCR"),
              valor = sumsq)
  
  anova_table <- tidy %>%
    transmute(
      `Fuente` = recode(term, A="A", B="B", `A:B`="A×B", Residuals="Residual"),
      `Suma de Cuadrados` = sumsq,
      `Grados de libertad` = df,
      `Cuadrado medio` = meansq,
      `F` = statistic,
      `p-valor` = p.value
    )
  
  list(fit=fit, mse=mse, dfR=dfR, grand=gm,
       means_A=means_A, means_B=means_B, means_AB=means_AB,
       sc_long=sc_long, anova_table = anova_table,
       alpha=alpha, dat=dat)
}

plot_sc_twoway_stacked_horizontal <- function(df, scale=c("Valor","Porcentaje"), caption=NULL){
  scale <- match.arg(scale)
  g <- ggplot(df, aes(x="Total", y=valor, fill=Componente)) +
    geom_col(width = .6) + coord_flip() +
    theme_minimal() + labs(x=NULL, y="Suma de Cuadrados", caption=caption) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  if(scale=="Porcentaje"){
    tot <- sum(df$valor)
    g <- g + aes(y = valor/tot) + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  }
  g
}

plot_twoway_violins_labeled <- function(dat, fit){
  gm <- fit$grand
  ggplot(dat, aes(interaction(A,B, sep="×"), y, fill=B)) +
    geom_violin(alpha=.25, color="black") +
    geom_point(position = position_jitter(width=.06), alpha=.6) +
    stat_summary(fun=mean, geom="point", size=3, color="black") +
    stat_summary(fun=mean, geom="text",
                 aes(label=sprintf("%.2f", after_stat(y))), vjust=-1, color="black") +
    geom_hline(yintercept = gm, linetype="dashed", color="black") +
    annotate("label", x = Inf, y = gm,
             label = paste0("Media general = ", sprintf("%.2f", gm)),
             vjust = -0.4, hjust = 1.02, size = 3, fill = "white") +
    labs(x="Poblaciones (A×B)", y="Respuesta", title="Violines A×B (poblaciones)", fill="B") +
    theme_minimal()
}

# --- Segmentos A×B (solo etiquetas de MEDIAS) ---
plot_twoway_segments <- function(dat, fit){
  gm <- fit$grand
  mu_ab <- dat %>% dplyr::group_by(A,B) %>% dplyr::summarise(media = mean(y), .groups="drop")
  
  set.seed(1234)
  J <- dat %>%
    dplyr::mutate(xj = as.numeric(interaction(A,B, drop = TRUE)) + runif(dplyr::n(), -0.12, 0.12)) %>%
    dplyr::left_join(mu_ab, by=c("A","B"))
  
  ggplot(J, aes(x=xj, y=y, color = B)) +
    geom_point(alpha=.7) +
    geom_segment(aes(x=xj, xend=xj, y=y, yend=media),
                 linewidth=0.6, color="red", alpha=.9) +   # SCR (rojo)
    # Punto de media de celda + etiqueta de su valor
    stat_summary(data = mu_ab,
                 aes(x = as.numeric(interaction(A,B, drop = TRUE)), y = media),
                 fun=mean, geom="point", size=3, color="black", inherit.aes = FALSE) +
    geom_text(data = mu_ab,
              aes(x = as.numeric(interaction(A,B, drop = TRUE)), y = media,
                  label = sprintf("%.2f", media)),
              vjust = -1, color = "black", inherit.aes = FALSE) +
    # Segmento media de celda → media general
    geom_segment(data = mu_ab,
                 aes(x = as.numeric(interaction(A,B, drop = TRUE)),
                     xend = as.numeric(interaction(A,B, drop = TRUE)),
                     y = media, yend = gm),
                 inherit.aes = FALSE, color="black", linewidth=1.1) +
    geom_hline(yintercept = gm, linetype="dashed", color="black") +
    scale_x_continuous(breaks = unique(as.numeric(interaction(dat$A, dat$B, drop = TRUE))),
                       labels = levels(interaction(dat$A, dat$B, sep="×", drop = TRUE))) +
    labs(x="Poblaciones (A×B)", y="Respuesta",
         title="Descomposición geométrica A×B: SCR (punto→media) y media de celda→μ",
         caption="SCR (rojo): distancia del dato a su media de celda · Negro: distancia de la media de celda a la media general (μ).") +
    theme_minimal()
}

plot_interaction_labels_only <- function(fit){
  gm <- fit$grand
  ggplot(fit$means_AB, aes(B, media, group = A, color = A)) +
    geom_line(linewidth = 0.7, position = position_dodge(width=.35)) +
    geom_point(size=2.6, position = position_dodge(width=.35)) +
    geom_text(aes(label=sprintf("%.2f", media)),
              vjust=-0.9, position = position_dodge(width=.35), show.legend = FALSE) +
    geom_hline(yintercept = gm, linetype="dashed", color="black") +
    annotate("label", x = Inf, y = gm,
             label = paste0("Media general = ", sprintf("%.2f", gm)),
             vjust = -0.4, hjust = 1.02, size = 3, fill = "white") +
    labs(x="Niveles de B", y="Media", color="A", title="Gráfico de interacción A×B") +
    theme_minimal()
}

.lsd_bounds <- function(means_df, groups_df, mse, dfR, alpha){
  n_eff <- groups_df %>% group_by(across(everything())) %>%
    summarise(n_eff = n(), .groups = "drop")
  key_col <- setdiff(names(n_eff), "n_eff")
  out <- left_join(means_df, n_eff, by = key_col)
  out$lsd  <- qt(1 - alpha/2, dfR) * sqrt(mse / out$n_eff)
  out$ymin <- out$media - out$lsd
  out$ymax <- out$media + out$lsd
  out
}

plot_LSD_main <- function(fit, lsd_on=c("Factor A","Factor B"), show=TRUE){
  if(!show) return(NULL)
  lsd_on <- match.arg(lsd_on)
  gm <- fit$grand; mse <- fit$mse; dfR <- fit$dfR; alpha <- fit$alpha
  
  if (lsd_on == "Factor B"){
    dfB <- .lsd_bounds(fit$means_B, fit$dat["B"], mse, dfR, alpha)
    ggplot(dfB, aes(B, media)) +
      geom_point(size=3) +
      geom_errorbar(aes(ymin=ymin, ymax=ymax), width=.15, color="black") +
      geom_text(aes(label=sprintf("%.2f", media)), vjust=-1) +
      geom_hline(yintercept = gm, linetype="dashed", color="black") +
      labs(x="B", y="Media", title="LSD en efectos simples (B)") +
      theme_minimal()
  } else {
    dfA <- .lsd_bounds(fit$means_A, fit$dat["A"], mse, dfR, alpha)
    ggplot(dfA, aes(A, media)) +
      geom_point(size=3) +
      geom_errorbar(aes(ymin=ymin, ymax=ymax), width=.15, color="black") +
      geom_text(aes(label=sprintf("%.2f", media)), vjust=-1) +
      geom_hline(yintercept = gm, linetype="dashed", color="black") +
      labs(x="A", y="Media", title="LSD en efectos simples (A)") +
      theme_minimal()
  }
}

plot_LSD_simple <- function(fit, lsd_on=c("Factor A","Factor B"), show=FALSE){
  if(!show) return(NULL)
  lsd_on <- match.arg(lsd_on)
  gm <- fit$grand; mse <- fit$mse; dfR <- fit$dfR; alpha <- fit$alpha
  dodge <- position_dodge(width=.35)
  
  if (lsd_on == "Factor B"){
    byA <- fit$dat %>% group_by(A, B) %>% summarise(n_eff = n(), .groups="drop")
    dfc <- left_join(fit$means_AB, byA, by=c("A","B")) %>%
      mutate(lsd = qt(1 - alpha/2, dfR) * sqrt(mse/n_eff),
             ymin = media - lsd, ymax = media + lsd)
    ggplot(dfc, aes(B, media, color=A, group=A)) +
      geom_line(position = dodge) +
      geom_point(size=2.6, position = dodge) +
      geom_errorbar(aes(ymin=ymin, ymax=ymax), width=.12, position = dodge) +
      geom_text(aes(label=sprintf("%.2f", media)), vjust=-0.9, position = dodge, show.legend = FALSE) +
      geom_hline(yintercept = gm, linetype="dashed", color="black") +
      labs(x="B", y="Media", color="A", title="Gráfico de interacción (LSD)") +
      theme_minimal()
  } else {
    byB <- fit$dat %>% group_by(B, A) %>% summarise(n_eff = n(), .groups="drop")
    dfc <- left_join(fit$means_AB, byB, by=c("A","B")) %>%
      mutate(lsd = qt(1 - alpha/2, dfR) * sqrt(mse/n_eff),
             ymin = media - lsd, ymax = media + lsd)
    ggplot(dfc, aes(B, media, color=A, group=A)) +
      geom_line(position = dodge) +
      geom_point(size=2.6, position = dodge) +
      geom_errorbar(aes(ymin=ymin, ymax=ymax), width=.12, position = dodge) +
      geom_text(aes(label=sprintf("%.2f", media)), vjust=-0.9, position = dodge, show.legend = FALSE) +
      geom_hline(yintercept = gm, linetype="dashed", color="black") +
      labs(x="B", y="Media", color="A", title="Gráfico de interacción (LSD)") +
      theme_minimal()
  }
}