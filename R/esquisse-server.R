
#' @param data_rv Either:
#'  * A [shiny::reactiveValues()] with a slot `data` containing a `data.frame`
#'    to use in the module and a slot `name` corresponding to the name of the `data.frame` used for the generated code.
#'  * A [shiny::reactive()] function returning a `data.frame`. See argument `name` for the name used in generated code.
#'  * A `data.frame` object.
#' @param name The default name to use in generated code. Can be a `reactive` function return a single character.
#' @param default_aes Default aesthetics to be used, can be a `character`
#'  vector or `reactive` function returning one.
#' @param import_from From where to import data, argument passed
#'  to [datamods::import_server()], use `NULL` to prevent the modal to appear.
#' @param drop_ids Argument passed to [datamods::filter_data_server]. Drop columns containing more than 90% of unique values, or than 50 distinct values.
#' @param notify_warnings See [safe_ggplot()]. If `NULL`, the user can make his or her own choice via the settings menu, default is to show warnings once.
#'
#'
#' @export
#'
#' @rdname esquisse-module
#' @order 2
#'
#' @importFrom shiny moduleServer reactiveValues observeEvent is.reactive
#'  renderPlot stopApp plotOutput showNotification isolate reactiveValuesToList
#'  is.reactivevalues
#' @importFrom ggplot2 ggplot_build ggsave %+%
#' @import ggplot2
#' @importFrom datamods import_modal import_server show_data
#' @importFrom rlang expr sym
esquisse_server <- function(id,
                            data_rv = NULL,
                            name = "data",
                            default_aes = c("fill", "color", "size", "group", "facet"),
                            import_from = c("env", "file", "copypaste", "googlesheets", "url"),
                            n_geoms = 8,
                            drop_ids = TRUE,
                            notify_warnings = NULL) {

  moduleServer(
    id = id,
    module = function(input, output, session) {
browser()
      ns <- session$ns
      ggplotCall <- reactiveValues(code = "")
      data_chart <- reactiveValues(data = NULL, name = NULL)
      # Settings modal (aesthetics choices)
      observeEvent(input$settings, {
        showModal(modal_settings(aesthetics = input$aesthetics))
      })

      if (is.reactivevalues(data_rv)) {
        observeEvent(data_rv$data, {
          data_chart$data <- data_rv$data
          data_chart$name <- data_rv$name %||% if (is.reactive(name)) {
            name()
          } else {
            name
          }
        }, ignoreInit = FALSE)
      } else if (is.reactive(data_rv)) {
        observeEvent(data_rv(), {
          data_chart$data <- data_rv()
          data_chart$name <- if (is.reactive(name)) {
            name()
          } else {
            name
          }
        }, ignoreInit = FALSE)
      } else if (is.data.frame(data_rv)) {
        data_chart$data <- data_rv
        data_chart$name <- if (is.character(name)) name
      }

      # Launch import modal if no data at start
      if (!is.null(import_from)) {
        observe({
          if (is.null(data_chart$data)) {
            datamods::import_modal(
              id = ns("import-data"),
              from = import_from,
              title = i18n("Import data to create a graph")
            )
          }
        })
      }

      # Launch import modal if button clicked
      observeEvent(input$launch_import_data, {
        datamods::import_modal(
          id = ns("import-data"),
          from = import_from,
          title = i18n("Import data to create a graph")
        )
      })
      # Data imported and update rv used
      data_imported_r <- datamods::import_server("import-data", return_class = "tbl_df")
      observeEvent(data_imported_r$data(), {
        data <- data_imported_r$data()
        data_chart$data <- data
        data_chart$name <- data_imported_r$name() %||% "imported_data"
      })

      # show data if button clicked
      show_data_server("show_data", reactive(controls_rv$data))

      # update variable modal
      updated_data <- update_vars_server("update_variable", reactive(data_chart$data))
      observeEvent(updated_data(), {
        data_chart$data <- updated_data()
      })

      # create column modal
      created_col <- create_col_server("create_col", reactive(data_chart$data))
      observeEvent(created_col(), {
        data_chart$data <- created_col()
      })

      # cut variable modal
      cutted_var <- cut_var_server("cut_var", reactive(data_chart$data))
      observeEvent(cutted_var(), {
        data_chart$data <- cutted_var()
      })

      # update factor modal
      updated_fct <- update_fct_server("up_fct", reactive(data_chart$data))
      observeEvent(updated_fct(), {
        data_chart$data <- updated_fct()
      })



      ### Geom & aesthetics selection
      res_geom_aes_r <- select_geom_aes_server(
        id = "geomaes",
        data_r = reactive(data_chart$data),
        aesthetics_r = reactive(input$aesthetics),
        n_geoms = n_geoms,
        default_aes = default_aes
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


      ### Module chart controls : title, xlabs, colors, export...
      controls_rv <- controls_server(
        id = "controls",
        data_r = reactive(data_chart$data),
        data_name = reactive({
          nm <- req(data_chart$name)
          if (is_call(nm)) {
            nm <- as_label(nm)
          }
          nm
        }),
        ggplot_rv = ggplotCall,
        geoms_r = reactive({
          c(geom_r(), geoms_others_r())
        }),
        n_geoms = n_geoms,
        active_geom_r <- reactive(res_geom_aes_r()$active),
        aesthetics_r = reactive({
          c(list(aes_r()), aes_others_r())
        }),
        width = reactive(rv_render_ggplot$plot_width),
        height = reactive(rv_render_ggplot$plot_height),
        drop_ids = drop_ids
      )


      rv_render_ggplot <- render_ggplot(
        id = "plooooooot",
        {
          browser()
          req(input$play_plot, cancelOutput = TRUE)
          req(data_chart$data)
          data <- req(controls_rv$data)
          req(controls_rv$inputs)
          geom <- req(geom_r())

          aes_input <- make_aes(aes_r())
          req(length(unlist(aes_input))> 0)
          req(unlist(aes_input) %in% names(data_chart$data))
          mapping <- build_aes(
            data = data_chart$data,
            .list = aes_input,
            geom = geom
          )

          geoms <- potential_geoms(data_chart$data, mapping)
          req(geom %in% geoms)

          if (isTruthy(setdiff(geoms_others_r(), "blank"))) {
            geom <- c(geom, geoms_others_r())
            mappings <- c(list(mapping), aes_others_r())
            geom_args <- lapply(
              X = seq_len(n_geoms), # n_geoms
              FUN = function(i) {
                match_geom_args(
                  geom[i],
                  controls_rv[[paste0("geomargs", i)]],
                  mapping = mappings[[i]],
                  add_mapping = i != 1 & length(mappings[[i]]) > 0,
                  exclude_args = names(combine_aes(mappings[[1]], mappings[[i]]))
                )
              }
            )
            blanks <- geom == "blank"
            geom <- geom[!blanks]
            geom_args[blanks] <- NULL

            scales_l <- dropNulls(lapply(
              X = seq_len(n_geoms),
              FUN = function(i) {
                mapping <- mappings[[i]]
                if (length(mapping) < 1) return(NULL)
                which_pal_scale(
                  mapping = mapping,
                  palette = controls_rv[[paste0("geomcolors", i)]]$colors,
                  data = data,
                  reverse = controls_rv[[paste0("geomcolors", i)]]$reverse
                )
              }
            ))
            scales_args <- unlist(lapply(scales_l, `[[`, "args"), recursive = FALSE)
            scales <- unlist(lapply(scales_l, `[[`, "scales"))
          } else {
            geom_args <- match_geom_args(
              geom,
              controls_rv$geomargs1,
              mapping = mapping,
              add_mapping = FALSE
            )
            scales <- which_pal_scale(
              mapping = mapping,
              palette = controls_rv$geomcolors1$colors,
              data = data,
              reverse = controls_rv$geomcolors1$reverse
            )
            scales_args <- scales$args
            scales <- scales$scales
          }

          if (isTRUE(controls_rv$transX$use)) {
            scales <- c(scales, "x_continuous")
            scales_args <- c(scales_args, list(x_continuous = controls_rv$transX$args))
          }

          if (isTRUE(controls_rv$transY$use)) {
            scales <- c(scales, "y_continuous")
            scales_args <- c(scales_args, list(y_continuous = controls_rv$transY$args))
          }

          data_name <- data_chart$name %||% "data"
          gg_call <- ggcall(
            data = data_name,
            mapping = mapping,
            geom = geom,
            geom_args = geom_args,
            scales = scales,
            scales_args = scales_args,
            labs = controls_rv$labs,
            theme = controls_rv$theme$theme,
            theme_args = controls_rv$theme$args,
            coord = controls_rv$coord$fun,
            coord_args = controls_rv$coord$args,
            facet = aes_r()$facet,
            facet_row = aes_r()$facet_row,
            facet_col = aes_r()$facet_col,
            facet_args = controls_rv$facet,
            xlim = controls_rv$limits$xlim,
            ylim = controls_rv$limits$ylim
          )

          ggplotCall$code <- deparse2(gg_call)
          ggplotCall$call <- gg_call
          ggplotCall$ggobj <- safe_ggplot(
            expr = expr((!!gg_call) %+% !!sym("esquisse_data")),
            data = setNames(list(data, data), c("esquisse_data", data_chart$name)),
            show_notification = notify_warnings %||% input$notify_warnings  %||% "once"
          )
          ggplotCall$ggobj$plot
        },
        filename = "esquisse-plot",
        width = reactive(controls_rv$width),
        height = reactive(controls_rv$height),
        use_plotly = reactive(controls_rv$plotly)
      )


      # Close addin
      observeEvent(input$close, shiny::stopApp())

      # Ouput of module (if used in Shiny)
      output_module <- reactiveValues(code_plot = NULL, code_filters = NULL, data = NULL)
      observeEvent(ggplotCall$code, {
        output_module$code_plot <- ggplotCall$code
      }, ignoreInit = TRUE)
      observeEvent(controls_rv$data, {
        output_module$code_filters <- controls_rv$code
        output_module$data <- controls_rv$data
      }, ignoreInit = TRUE)
      return(output_module)
    }
  )

}
