devtools::load_all()
options("teal.bs_theme" = bslib::bs_theme(5L))

app <- init(
  data = teal.data::teal_data(iris = iris, cars = cars),
  modules = list(
    esquisse_module()
  )
)

shinyApp(app$ui, app$server)



