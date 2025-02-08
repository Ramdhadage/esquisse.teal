library(shiny)
library(teal.widgets)
library(ggplot2)
library(teal.code)
devtools::load_all()
ui <- fluidPage(
  tags$h2("ggplot output"),
  select_geom_aes_ui(
    id = ns("geomaes"),
    n_geoms = 1,
    list_geoms = c(
      list(geomIcons()),
      rep_len(list(
        geomIcons(
          c("line", "step", "jitter", "point", "smooth", "density", "boxplot", "violin", "text", "label"),
          default = "select"
        )
      ), 1)
    )
  ),
  ggplot_output_ram(ns("plot"))
)

server <- function(input, output, session) {
  res_geom_aes_r <- select_geom_aes_server(
    id = "geomaes",
    data_r = reactive(apexcharter::temperatures),
    aesthetics_r = reactive(input$aesthetics),
    n_geoms = 1
  )
  aes_r <- reactive(res_geom_aes_r()$main$aes)
  aes_others_r <- reactive({
    others <- res_geom_aes_r()$others
    mappings <- others[grepl("aes", names(others))]
    lapply(mappings, make_aes)
  })
  geom_r <- reactive(res_geom_aes_r()$main$geom)
  geoms_others_r <- reactive({
    others <- res_geom_aes_r()$others
    geoms <- others[grepl("geom", names(others))]
    unlist(geoms, use.names = FALSE)
  })
  render_ggplot_ram("plot",{
    geom <- req(geom_r())
    aes_input <- make_aes(aes_r())

  })
}

shinyApp(ui, server)
