# ANOVAlab — mod_one_factor.R
# Un factor (1F): generación/entrada de datos, ANOVA, gráficos con media general,
# etiquetas de medias y opción de intervalos LSD.

# 1 FACTOR — 2 pestañas: Datos | Representaciones
mod_one_factor_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        numericInput(ns("seed"), "Semilla", 123),
        numericInput(ns("k"), "Número de variantes / niveles (I). Grupos", 3, min = 2),
        numericInput(ns("n"), "nº de observaciones por variante / nivel (ni). Se asume que todas las muestras tienen el mismo número de observaciones.", 20, min = 2),
        sliderInput(ns("delta"), "δ (Tamaño de efecto a detectar)", 0, 3, 1, step = 0.1),
        sliderInput(ns("sigma"), "σ (Desviación Típica poblacional).  Es la misma para todas las poblaciones (homocedasticidad)", 0.1, 3, 1, step = 0.1),
        actionButton(ns("regen"), "Regenerar 1-factor")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Datos",
                   tableOutput(ns("tbl_data"))
          ),
          tabPanel("Representaciones",
                   # 1) Segmentos
                   radioButtons(ns("segm_mode"), "Resaltar segmentos:",
                                c("Ambos","SCR (residual)","SCE (entre grupos)"),
                                inline = TRUE),
                   plotOutput(ns("plot_segments"), height = "360px"),
                   # 2) Tabla ANOVA (terminología UD)
                   tags$br(), tableOutput(ns("tbl_anova")),
                   # 3) SC apilada (HORIZONTAL)
                   tags$br(), radioButtons(ns("scale_sc"), "Escala de barras:",
                                           c("Valor","Porcentaje"),
                                           inline = TRUE, selected = "Valor"),
                   plotOutput(ns("plot_sc_h"), height = "260px"),
                   # 4) Violines por grupos
                   tags$br(), plotOutput(ns("plot_violin"), height = "340px")
          )
        )
      )
    )
  )
}

mod_one_factor_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    source("R/gen_data.R", local = TRUE)
    source("R/anova_oneway.R", local = TRUE)
    
    dat <- reactiveVal(NULL)
    observeEvent(input$regen, {
      set.seed(input$seed)
      dat(gen_oneway_from_delta(k = input$k, n = input$n,
                                delta = input$delta, sigma = input$sigma))
    }, ignoreInit = TRUE)
    observeEvent(TRUE, { dat(gen_oneway_from_delta(3,20,1,1)) }, once = TRUE)
    
    fit <- reactive({ req(dat()); anova_oneway_fit(dat()) })
    
    # Datos
    output$tbl_data  <- renderTable(dat(), striped = TRUE, bordered = TRUE)
    
    # Representaciones
    output$plot_violin <- renderPlot({
      plot_oneway_violins_labeled(dat(), fit())
    })
    
    output$plot_segments <- renderPlot({
      plot_oneway_segments_jitter(dat(), fit(), mode = input$segm_mode)
    })
    
    output$tbl_anova <- renderTable(fit()$anova_table, digits = 4)
    
  
    output$plot_sc_h <- renderPlot({
      plot_sc_oner_stacked_horizontal(fit()$sc_long, scale = input$scale_sc,
                                      caption = "SCE (entre grupos) + SCR (residual) = SCT (total)")
    })
    
  })
}