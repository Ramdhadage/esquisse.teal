#' An example `teal` module
#'
#' `r lifecycle::badge("experimental")`
#' @import teal
#' @return A `teal` module which can be included in the `modules` argument to [init()].
#' @examples
#' app <- init(
#'   data = teal_data(IRIS = iris, MTCARS = mtcars),
#'   modules = example_module()
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#' @export
example_esquisse_module <- function(label = "example teal module", datanames = "all", transformers = list()) {
  checkmate::assert_string(label)
  module(
    label,
    server = function(id, data) {
      checkmate::assert_class(isolate(data()), "teal_data")
      moduleServer(id, function(input, output, session) {
        datanames_rv <- reactive(ls(teal.code::get_env((req(data())))))
        observeEvent(datanames_rv(), {
          selected <- input$dataname
          if (identical(selected, "")) {
            selected <- restoreInput(session$ns("dataname"), NULL)
          } else if (isFALSE(selected %in% datanames_rv())) {
            selected <- datanames_rv()[1]
          }
          updateSelectInput(
            session = session,
            inputId = "dataname",
            choices = datanames_rv(),
            selected = selected
          )
        })

        observe({
          req(input$dataname)
          results <- esquisse_server(
            id = "esquisse",
            data_rv = reactive(data()[[input$dataname]]),
            import_from = NULL,
            n_geoms = 1
          )
        })
      })
    },
    ui = function(id) {
      ns <- NS(id)
      teal.widgets::standard_layout(
        output = tags$div(
          class = "ggplot-geom-aes-container",
          select_geom_aes_ui(
            id = ns("esquisse-geomaes"),
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
          tags$div(
            class = "ggplot-output-sidebar-container",
            # play_pause_input(ns("play_plot"), show = TRUE),
            ggplot_output(
              id = ns("esquisse-plooooooot"),
              width = "1200px",
              height = "600px",
              downloads = downloads_labels()
            )
          )
        ),

        encoding = tags$div(
          teal.widgets::panel_group(
            teal.widgets::panel_item(
              title = "Data",
              collapsed = FALSE,
              # shiny::fileInput(ns("file"), "Upload a file"),
              selectInput(ns("dataname"), "Choose a dataset", choices = NULL)
            ),
            teal.widgets::panel_item(
              title = "Geometries",
              collapsed = FALSE,
              controls_ui(
                id = ns("esquisse-controls"),
                insert_code = FALSE,
                controls = c("geoms"),
                layout = "accordion",
                downloads = downloads_labels(),
                n_geoms = 1
              )
            ),
            teal.widgets::panel_item(
              title = "Options",
              collapsed = TRUE,
              controls_ui(
                id = ns("esquisse-controls"),
                insert_code = FALSE,
                controls = c("options"),
                layout = "accordion",
                downloads = downloads_labels(),
                n_geoms = 1
              )
            ),
            teal.widgets::panel_item(
              title = "Labels & Title",
              collapsed = TRUE,
              controls_ui(
                id = ns("esquisse-controls"),
                insert_code = FALSE,
                controls = c("labs"),
                layout = "accordion",
                downloads = downloads_labels(),
                n_geoms = 1
              )
            ),
            teal.widgets::panel_item(
              title = "Axes",
              collapsed = TRUE,
              controls_ui(
                id = ns("esquisse-controls"),
                insert_code = FALSE,
                controls = c("axes"),
                layout = "accordion",
                downloads = downloads_labels(),
                n_geoms = 1
              )
            ),
            teal.widgets::panel_item(
              title = "Theme",
              collapsed = TRUE,
              controls_ui(
                id = ns("esquisse-controls"),
                insert_code = FALSE,
                controls = c("theme"),
                layout = "accordion",
                downloads = downloads_labels(),
                n_geoms = 1
              )
            )
          ),
          teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
        )
      )
  },
  datanames = datanames
  )
}
