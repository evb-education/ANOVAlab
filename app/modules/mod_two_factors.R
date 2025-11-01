# modules/mod_two_factors.R

# Fallback local por si no se cargó app/R/gen_data.R
if (!exists("gen_data_twoway")) {
  gen_data_twoway <- function(I = 3, J = 3, n = 10,
                              effA = 1, effB = 1, intAB = 0, sigma = 1,
                              seed = 123) {
    set.seed(seed)
    A <- factor(rep(LETTERS[seq_len(I)], each = J * n), levels = LETTERS[seq_len(I)])
    B <- factor(rep(letters[seq_len(J)], times = I, each = n), levels = letters[seq_len(J)])
    a_i  <- effA * seq(-1, 1, length.out = I)
    b_j  <- effB * seq(-1, 1, length.out = J)
    ab_ij <- intAB * outer(a_i, b_j)
    mu_mat <- outer(a_i, b_j, `+`) + ab_ij
    mu_vec <- as.vector(t(mu_mat))
    y <- unlist(lapply(mu_vec, function(mu) stats::rnorm(n, mean = mu, sd = sigma)))
    data.frame(y = y, A = A, B = B)
  }
}

mod_two_factors_ui <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Controles con los textos que mostraste en la captura
      numericInput(ns("I"), "Variantes / niveles Factor 1 (A)", value = 3, min = 2, step = 1),
      numericInput(ns("J"), "Variantes / niveles Factor 1 (B)", value = 3, min = 2, step = 1),
      numericInput(ns("n"), "nº de observaciones por tratamiento (nij).Todos los tratamientos tienen el mismo nº de observaciones.", value = 10, min = 1, step = 1),
      
      sliderInput(ns("effA"),  "Efecto a detectar del Factor 1 (A)", min = 0,  max = 3,  value = 1, step = 0.1),
      sliderInput(ns("effB"),  "Efecto a detectar del Factor 1 (B)", min = 0,  max = 3,  value = 1, step = 0.1),
      sliderInput(ns("intAB"), "Efecto conjunto a detectar. Interacción (A×B)", min = -3, max = 3, value = 0, step = 0.1),
      sliderInput(ns("sigma"), "σ (Desviación típica poblacional (Homocedasticidad))", min = 0.1, max = 3, value = 1, step = 0.1),
      
      tags$hr(),
      checkboxInput(ns("show_lsd_factors"), "Mostrar LSD de factores (A y B)", value = TRUE),
      radioButtons(ns("interaction_mode"), "Interacción:",
                   choices  = c("Solo líneas" = "lines", "LSD verticales" = "lsd", "Ninguno" = "none"),
                   selected = "lines"),
      radioButtons(ns("segments_mode"), "Resaltar segmentos:",
                   choices = c("Ambos" = "both",
                               "SCR (residual)" = "scr",
                               "SCE (entre combinaciones)" = "sce",
                               "Ninguno" = "none"),
                   selected = "both")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("tabs"), type = "tabs",
        
        # ---- Pestaña DATOS ----
        tabPanel("Datos",
                 tableOutput(ns("tabla_datos"))
        ),
        
        # ---- Pestaña REPRESENTACIONES ----
        tabPanel("Representaciones",
                 # 1) Violines (ya primero)
                 tags$h3("Distribución de las observaciones por tratamiento (A×B)"),
                 plotOutput(ns("plot_violins")),
                 
                 # 2) Segmentos
                 tags$br(),
                 tags$h3("Representación geométrica de la descomposición de la variabilidad (A, B, A×B)"),
                 plotOutput(ns("plot_segments")),
                 
                 # 3) Tabla resumen ANOVA
                 tags$br(),
                 tags$h3("Descomposición numérica de la variabilidad: tabla resumen del ANOVA"),
                 tableOutput(ns("tabla_anova")),
                 
                 # 4) Barra apilada SCT
                 tags$br(),
                 tags$h3("Visualización de la descomposición de la variabilidad total"),
                 plotOutput(ns("plot_sc")),
                 
                 # 5) Interacción
                 tags$br(),
                 tags$h3("Gráfico de Interacción (A×B)"),
                 plotOutput(ns("plot_interaction")),
                 
                 # 6) LSD factores
                 tags$br(),
                 tags$h3("Intervalos LSD"),
                 plotOutput(ns("plot_lsd_factors"))
        )
      )
    )
  )
}

mod_two_factors_server <- function(id){
  moduleServer(id, function(input, output, session){
    
    # --- Datos simulados (balanceado) ---
    dat <- reactive({
      gen_data_twoway(
        I     = input$I,
        J     = input$J,
        n     = input$n,
        effA  = input$effA,
        effB  = input$effB,
        intAB = input$intAB,
        sigma = input$sigma
      )
    })
    
    # --- Ajuste ANOVA + resúmenes ---
    fit <- reactive({
      req(dat())
      anova_twoway_fit(dat(), alpha = 0.05)
    })
    
    # ==================== SALIDAS ====================
    
    # Pestaña "Datos"
    output$tabla_datos <- renderTable({
      req(dat())
      dat()
    })
    
    # Representaciones — 1) Violines
    output$plot_violins <- renderPlot({
      req(dat(), fit())
      plot_twoway_violins_labeled(dat(), fit())
    })
    
    # Representaciones — 2) Segmentos
    output$plot_segments <- renderPlot({
      req(dat(), fit())
      plot_twoway_segments_labeled(dat(), fit(), mode = input$segments_mode)
    })
    
    # Representaciones — 3) Tabla ANOVA
    output$tabla_anova <- renderTable({
      req(fit())
      fit()$anova_table
    }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 4)
    
    # Representaciones — 4) Barra apilada SCT
    output$plot_sc <- renderPlot({
      req(fit())
      plot_sc_twoway_stacked_horizontal(fit()$sc_long, scale = "Valor")
    })
    
    # Interacción (modo)
    output$plot_interaction <- renderPlot({
      req(fit())
      plot_interaction_mode(fit(), mode = input$interaction_mode)
    })
    
    # LSD factores (A y B simultáneo)
    output$plot_lsd_factors <- renderPlot({
      req(fit())
      plot_LSD_marginals(fit(), show = isTRUE(input$show_lsd_factors))
    })
    
  })
}