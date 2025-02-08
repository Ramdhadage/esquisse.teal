# Render ggplot -----------------------------------------------------------


#' @title Render \code{ggplot} module
#'
#' @description Display a plot on the client and allow to download it.
#'
#' @param id Module ID.
#' @param width,height Width / Height of the plot, in the server it has to be a [shiny::reactive()] function returning a new width/height for the plot.
#' @param downloads Labels for export options, use `downloads_labels()` or `NULL` to disable export options.
#' @param ... Parameters passed to [shiny::plotOutput()] (`ggplot_output`) or [shiny::renderPlot()] (`render_ggplot`).
#'
#' @return Server-side, a `reactiveValues` with the plot.
#' @export
#'
#' @name ggplot-output
#'
#' @importFrom shiny NS downloadLink actionButton plotOutput actionLink
#' @importFrom htmltools tags tagList
#' @importFrom shinyWidgets dropMenu
#' @import teal.widgets
#' @example examples/render-ggplot.R

ggplot_output_ram <- function(id) {
  ns <- NS(id)
  tagList(
    plot_with_settings_ui(
      id = ns("plot_with_settings")
    )
  )
}

render_ggplot_ram <- function(id,
                          expr,
                          ...,
                          env = parent.frame(),
                          quoted = FALSE
                          ) {
  moduleServer(
    id,
    function(input, output, session) {
      plot_r <- reactive({
        expr
      })

      plot_with_settings_srv(
        id = "plot_with_settings",
        plot_r = plot_r,
        height = c(400, 100, 1200),
        width = NULL
      )


    }
  )
}

