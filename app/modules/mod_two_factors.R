# ANOVAlab — mod_two_factors.R
# Dos factores (2F): ANOVA, violines por poblaciones (A×B), gráfico de interacción
# con media general y etiquetas de medias, y opción de intervalos LSD (efectos simples
# y, opcionalmente, efectos simples condicionados cuando hay interacción).

# 2 FACTORES — 2 pestañas: Datos | Representaciones
mod_two_factors_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        numericInput(ns("I"), "Variantes / niveles Factor 1 (A)", 3, min = 2),
        numericInput(ns("J"), "Variantes / niveles Factor 1 (B)", 3, min = 2),
        numericInput(ns("n"), "n° de observaciones por tratamiento (nij).Todos los tratamientos tienen el mismo n° de observaciones.", 10, min = 1),
        sliderInput(ns("effA"),  "Efecto a detectar del Factor 1 (A)", 0, 3, 1, step = 0.1),
        sliderInput(ns("effB"),  "Efecto a detectar del Factor 1 (B)", 0, 3, 1, step = 0.1),
        sliderInput(ns("intAB"), "Efecto conjunto a detectar. Interacción (A×B)", -3, 3, 0, step = 0.1),
        sliderInput(ns("sigma"), "σ (Desviación típica poblacional (Homocedasticidad)", 0.1, 3, 1, step = 0.1),
        checkboxInput(ns("show_lsd_main"), "Mostrar intervalos LSD (efectos simples)", TRUE),
        checkboxInput(ns("show_lsd_simple"), "LSD por efectos simples condicionados (si hay interacción)", FALSE),
        selectInput(ns("lsd_on"), "Aplicar LSD a:", c("Factor A","Factor B")),
        numericInput(ns("alpha"), "Nivel α", 0.05, min = 0.001, max = 0.2, step = 0.005),
        actionButton(ns("regen"), "Regenerar 2-factores")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Datos",
                   tableOutput(ns("tbl_data"))
          ),
          tabPanel("Representaciones",
                   # 1) Violines A×B
                   plotOutput(ns("plot_violins_ab"), height = "340px"),
                   tags$br(),
                   # 2) Segmentos A×B
                   plotOutput(ns("plot_segments_ab"), height = "340px"),
                   tags$br(),
                   # 3) Tabla ANOVA (terminología UD)
                   tableOutput(ns("tbl_anova_ud")),
                   # 3b) SC apilada (horizontal) justo debajo de la tabla
                   tags$br(),
                   plotOutput(ns("plot_sc_h"), height = "260px"),
                   tags$br(),
                   # 4) Interacción (líneas + etiquetas, sin LSD)
                   plotOutput(ns("plot_inter"), height = "360px"),
                   tags$br(),
                   # 5) LSD efectos simples (gráfico aparte)
                   plotOutput(ns("plot_lsd_main"), height = "340px"),
                   tags$br(),
                   # 6) Interacción (LSD) — simples condicionados
                   plotOutput(ns("plot_lsd_simple"), height = "360px")
          )
        )
      )
    )
  )
}

mod_two_factors_server <- function(id){
  moduleServer(id, function(input, output, session){
    
    # --- CARGA ROBUSTA DE FUNCIONES (soporta app lanzada desde raíz o desde app/) ---
    try({
      source(file.path("R","gen_data.R"),        local = TRUE)
      source(file.path("R","anova_twoway.R"),    local = TRUE)
    }, silent = TRUE)
    if (!exists("anova_twoway_fit")) {
      # fallback: estructura ANOVAlab/app/R/...
      source(file.path("app","R","gen_data.R"),     local = TRUE)
      source(file.path("app","R","anova_twoway.R"), local = TRUE)
    }
    # Comprobación final (si fallara, mensaje claro):
    stopifnot(
      exists("anova_twoway_fit"),
      exists("plot_twoway_violins_labeled"),
      exists("plot_sc_twoway_stacked_horizontal"),
      exists("plot_interaction_labels_only"),
      exists("plot_LSD_main"),
      exists("plot_LSD_simple"),
      exists("plot_twoway_segments")
    )
    
    ns <- session$ns
    
    dat <- reactiveVal(NULL)
    observeEvent(input$regen, {
      dat(gen_twoway_from_sliders(I = input$I, J = input$J, n = input$n,
                                  effA = input$effA, effB = input$effB,
                                  intAB = input$intAB, sigma = input$sigma))
    }, ignoreInit = TRUE)
    observeEvent(TRUE, { dat(gen_twoway_from_sliders(3,3,10,1,1,0,1)) }, once = TRUE)
    
    fit <- reactive({ req(dat()); anova_twoway_fit(dat(), alpha = input$alpha) })
    
    # Datos
    output$tbl_data  <- renderTable(dat(), striped = TRUE, bordered = TRUE)
    
    # Representaciones (orden que pediste)
    output$plot_violins_ab <- renderPlot({ plot_twoway_violins_labeled(dat(), fit()) })
    output$plot_segments_ab <- renderPlot({ plot_twoway_segments(dat(), fit()) })
    output$tbl_anova_ud <- renderTable({ fit()$anova_table }, digits = 4)
    output$plot_sc_h <- renderPlot({
      plot_sc_twoway_stacked_horizontal(
        fit()$sc_long, scale = "Valor",
        caption = "SCA (por A) · SCB (por B) · SCA×B (interacción) · SCR (residual)"
      )
    })
    output$plot_inter <- renderPlot({ plot_interaction_labels_only(fit()) })
    output$plot_lsd_main <- renderPlot({
      plot_LSD_main(fit(), lsd_on = input$lsd_on, show = isTRUE(input$show_lsd_main))
    })
    output$plot_lsd_simple <- renderPlot({
      plot_LSD_simple(fit(), lsd_on = input$lsd_on, show = isTRUE(input$show_lsd_simple))
    })
  })
}