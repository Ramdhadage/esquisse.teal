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

data <- teal.data::teal_data() %>% within({
  iris <- iris
  # cars = cars
})
# reactlog_enable()
app <- teal::init(
  data = teal.data::teal_data() %>% within({
    iris <- iris
    # cars = cars
  }),
  modules = example_esquisse_module()
)

shinyApp(app$ui, app$server)



