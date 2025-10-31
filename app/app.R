# app/app.R
library(shiny)
library(bslib)
library(markdown)   # ← necesario para includeMarkdown("help.md")

# 1) Cargar TODAS las funciones auxiliares (ANOVA, gráficos, generadores, etc.)
#    desde app/R/ antes de cargar los módulos:
lapply(list.files("R", full.names = TRUE), source)

# 2) Cargar módulos de la app
source("modules/mod_one_factor.R",  encoding = "UTF-8")
source("modules/mod_two_factors.R", encoding = "UTF-8")

# 3) UI
ui <- page_navbar(
  title = "ANOVAlab",
  theme = bs_theme(bootswatch = "flatly"),  # opcional
  nav_panel("1 factor",   mod_one_factor_ui("onef")),
  nav_panel("2 factores", mod_two_factors_ui("twof")),
  nav_panel(
    "Ayuda",
    fluidPage(
      titlePanel("Ayuda de ANOVAlab"),
      tags$hr(),
      div(style = "max-width: 900px;", includeMarkdown("help.md"))
    )
  )
)

# 4) Server
server <- function(input, output, session) {
  mod_one_factor_server("onef")
  mod_two_factors_server("twof")
}

# 5) Lanzar app
shinyApp(ui, server)