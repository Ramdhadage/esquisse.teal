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

        # observe({
           # req(!is.null(data()[[input$dataname]]))
          results <- esquisse_server(
            id = "esquisse",
            data_rv = reactive(data()[[input$dataname]]),
            import_from = NULL,
            n_geoms = 1
          )
        # })




        plot_code <- reactive({

          paste(get_code(data()),
                "data <-",
                as.name(bquote(.(input$dataname))), "\n",
                results$code_plot, sep = " ")
        })

        teal.widgets::verbatim_popup_srv(
          id = "rcode",
          verbatim_content = reactive(plot_code()),
          title = "Example Code"
        )
        # ggplotCall <- reactiveValues(code = "")
        # res_geom_aes_r <- select_geom_aes_server(
        #   id = "geomaes",
        #   data_r = reactive(data()[[input$dataname]]),
        #   aesthetics_r = reactive(input$aesthetics),
        #   n_geoms = 1
        # )
        #
        # aes_r <- reactive(res_geom_aes_r()$main$aes)
        # aes_others_r <- reactive({
        #   others <- res_geom_aes_r()$others
        #   mappings <- others[grepl("aes", names(others))]
        #   lapply(mappings, make_aes)
        # })
        # geom_r <- reactive(res_geom_aes_r()$main$geom)
        # geoms_others_r <- reactive({
        #   others <- res_geom_aes_r()$others
        #   geoms <- others[grepl("geom", names(others))]
        #   unlist(geoms, use.names = FALSE)
        # })
        # results <- render_ggplot_ram("plot",{
        #   geom <- req(geom_r())
        #   aes_input <- make_aes(aes_r())
        #   req(unlist(aes_input) %in% names(data()[[input$dataname]]))
        #   mapping <- build_aes(
        #     data = data()[[input$dataname]],
        #     .list = aes_input,
        #     geom = geom
        #   )
        #
        #   geoms <- potential_geoms(data()[[input$dataname]], mapping)
        #   req(geom %in% geoms)
        #   gg_call <- ggcall(
        #     data = input$dataname,
        #     mapping = mapping,
        #     geom = geom,
        #     # geom_args = geom_args,
        #     # scales = scales,
        #     # scales_args = scales_args,
        #     # labs = controls_rv$labs,
        #     # theme = controls_rv$theme$theme,
        #     # theme_args = controls_rv$theme$args,
        #     # coord = controls_rv$coord$fun,
        #     # coord_args = controls_rv$coord$args,
        #     facet = aes_r()$facet,
        #     facet_row = aes_r()$facet_row,
        #     facet_col = aes_r()$facet_col,
        #     # facet_args = controls_rv$facet
        #     # xlim = controls_rv$limits$xlim,
        #     # ylim = controls_rv$limits$ylim
        #   )
        #   ggplotCall$code <- deparse2(gg_call)
        #   ggplotCall$call <- gg_call
        #
        #   # ggplotCall$ggobj <- safe_ggplot(
        #   #   expr = expr((!!gg_call) %+% !!sym("esquisse_data")),
        #   #   data = setNames(list(data, data), c("esquisse_data", input$data))
        #   #   # show_notification = notify_warnings %||% input$notify_warnings  %||% "once"
        #   # )
        #   ggplotCall$call
        # })

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
            play_pause_input(ns("play_plot"), show = TRUE),
            ggplot_output(
              id = ns("esquisse-plooooooot"),
              width = "100%",
              height = "100%",
              downloads = downloads_labels()
            )
          )
        ),
        # output = tags$div(
        #   select_geom_aes_ui(
        #     id = ns("geomaes"),
        #     n_geoms = 1,
        #     list_geoms = c(
        #       list(geomIcons()),
        #       rep_len(list(
        #         geomIcons(
        #           c("line", "step", "jitter", "point", "smooth", "density", "boxplot", "violin", "text", "label"),
        #           default = "select"
        #         )
        #       ), 1)
        #     )
        #   ),
        #   ggplot_output_ram(ns("plot"))
        #
        # ),
        # output = esquisse_ui(
        #   id = ns("esquisse"),
        #   controls = c("labs", "axes", "geoms", "theme"),
        #   play_pause = FALSE,
        #   header = FALSE,
        #   layout_sidebar = TRUE,
        #   n_geoms = 1
        # ),
        encoding = tags$div(
          teal.widgets::panel_group(
            teal.widgets::panel_item(
              title = "Data",
              collapsed = FALSE,
              shiny::fileInput(ns("file"), "Upload a file"),
              selectInput(ns("dataname"), "Choose a dataset", choices = NULL)
            ),
            teal.widgets::panel_item(
              title = "Options",
              collapsed = FALSE,
              controls_ui(
                id = ns("esquisse-controls"),
                insert_code = FALSE,
                controls = c( "geoms"),
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
