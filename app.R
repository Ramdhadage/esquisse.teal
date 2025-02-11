library(shiny)
library(teal.widgets)
library(ggplot2)
library(teal.code)
library(reactlog)
options(shiny.reactlog = TRUE)
devtools::load_all()
# options("teal.bs_theme" = bslib::bs_theme(5L))
# ls("package:datasets")[ls("package:datasets") %>%
#   purrr::map_lgl(\(x)  {
#     y <-  get(x)
#     !inherits(y, "ts")
#   })] %>% as.name()

initial_data <- teal.data::teal_data() %>% within({
  # iris <- iris
  cars <-  cars
})
reactlog_enable()
app <- teal::init(
  data = teal_data_module(
    ui = function(id) {
      ns <- NS(id)
      fluidPage(
        mainPanel(
          shiny::fileInput(ns("file"), "Upload a file"),
          actionButton(ns("submit"), "Submit"),
          DT::dataTableOutput(ns("preview"))
        )
      )
    },
    server = function(id) {
      moduleServer(id, function(input, output, session) {

        data <- eventReactive(input$submit, {
          if (!is.null(input$file)) {
            td <- within(initial_data,
                         filename <- read.csv(path),
                         path = input$file$datapath,
                         filename = tools::file_path_sans_ext(input$file$name))
            td
          } else {
            initial_data
          }
        })

        output$preview <- DT::renderDataTable({
          req(input$submit)
          if (!is.null(input$file)) {
            data()[["my_data"]]
          } else {
            data()[["cars"]]
          }
        })
        data
      })
    }
  ),
  modules = example_esquisse_module()
)

shinyApp(app$ui, app$server)



