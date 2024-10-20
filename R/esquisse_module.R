#' An example `teal` module
#'
#' `r lifecycle::badge("experimental")`
#'
#' @import teal teal.transform teal.data
#' @return A `teal` module which can be included in the `modules` argument to [init()].
#' @examples
#' app <- init(
#'   data = teal.data::teal_data(IRIS = iris, MTCARS = mtcars),
#'   modules = esquisse_module()
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#' @export
esquisse_module <- function(label = "esquisse teal module", datanames = "all") {
  checkmate::assert_string(label)
  ans <- module(
    label,
    server = function(id, data) {
      checkmate::assert_class(isolate(data()), "teal_data")
      moduleServer(id, function(input, output, session) {
        datanames_rv <- reactiveValues(
          data = iris,
          dataname = "iris")
        observeEvent(input$dataname, {
          datanames_rv$data <- as.data.frame(get(input$dataname, "package:datasets"))
          datanames_rv$dataname <- input$dataname
        })
        results <- esquisse_server(
          id = "esquisse",
          data_rv = datanames_rv
        )
      })
    },
    ui = function(id) {
      ns <- NS(id)
      names <- ls("package:datasets")[ls("package:datasets") %>%
                                        purrr::map_lgl(\(x)  {
                                          y <-  get(x)
                                          !inherits(y, "ts")
                                        })]
      teal.widgets::standard_layout(
        output = esquisse_ui(
          id = ns("esquisse"),
          controls = c("options", "labs", "axes", "geoms", "theme"),
          play_pause = FALSE,
          header = FALSE
        ),
        encoding = tags$div(
          selectInput(ns("dataname"), "Choose a dataset", choices = names)
        )
      )
    },
    datanames = datanames
  )
  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}
