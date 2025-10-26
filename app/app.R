# ANOVAlab — app.R
# UI reorganizada: dos pestañas separadas (1 factor / 2 factores),
# barras laterales "sticky" y resultados en tabs para evitar scroll largo.

library(shiny)
library(bslib)
library(markdown)

# Módulos
source("modules/mod_one_factor.R", encoding = "UTF-8")
source("modules/mod_two_factors.R", encoding = "UTF-8")

ui <- page_navbar(
  title = "ANOVAlab",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # --- pestañas principales ---
  nav("1 factor",  mod_one_factor_ui("onef")),
  nav("2 factores", mod_two_factors_ui("twof")),
  
  # --- pestaña de AYUDA ---
  nav("Ayuda",
      fluidPage(
        titlePanel("Ayuda de ANOVAlab"),
        tags$hr(),
        includeMarkdown(file.path("help.md")),
        tags$hr(),
        tags$div(
          class = "text-muted small",
          "Esta ayuda se genera automáticamente desde el archivo help.md (carpeta app/)."
        )
      )
  )
)

server <- function(input, output, session) {
  mod_one_factor_server("onef")
  mod_two_factors_server("twof")
}

shinyApp(ui, server)