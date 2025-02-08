library(shiny)
library(survival)
library(survminer)
library(bslib)
library(dplyr)
library(DT)
library(colourpicker)

# Generate sample survival data
set.seed(123)
n <- 200
group <- factor(rep(c("Treatment", "Control"), each = n/2))
time <- c(rexp(n/2, 1/20), rexp(n/2, 1/15))
status <- rbinom(n, 1, 0.7)
default_data <- data.frame(time = time, status = status, group = group)
# Function to generate plot interpretation
generate_interpretation <- function(stats, groups) {
  # Create a structured summary of the analysis
  summary_text <- sprintf(
    "Based on the Kaplan-Meier analysis comparing %s groups:\n\n
    1. Median Survival Times:
    - %s: %.1f
    - %s: %.1f\n
    2. Hazard Ratio: %.2f (95%% CI: %.2f-%.2f)\n
    3. Statistical significance: p = %.3f\n\n
    Please interpret these results.",
    length(groups),
    groups[1], stats$median[1],
    groups[2], stats$median[2],
    stats$hr, stats$hr_ci[1], stats$hr_ci[2],
    stats$cox_p
  )

  # You would typically send this to an LLM API
  # For demonstration, we'll create a structured interpretation
  if(stats$cox_p < 0.05) {
    interpretation <- sprintf(
      "Key findings:\n
      1. There is a statistically significant difference between the groups (p = %.3f).\n
      2. The hazard ratio of %.2f indicates that the %s group has a %.0f%% %s risk compared to the %s group.\n
      3. Median survival times differ by %.1f time units between groups.\n
      4. The confidence interval (%.2f-%.2f) suggests this finding is reliable.",
      stats$cox_p,
      stats$hr,
      groups[2],
      abs(100 * (1 - stats$hr)),
      ifelse(stats$hr > 1, "higher", "lower"),
      groups[1],
      abs(diff(stats$median)),
      stats$hr_ci[1], stats$hr_ci[2]
    )
  } else {
    interpretation <- sprintf(
      "Key findings:\n
      1. There is no statistically significant difference between the groups (p = %.3f).\n
      2. Despite a hazard ratio of %.2f, the evidence is not strong enough to conclude a real difference exists.\n
      3. The wide confidence interval (%.2f-%.2f) suggests considerable uncertainty in the estimate.",
      stats$cox_p,
      stats$hr,
      stats$hr_ci[1], stats$hr_ci[2]
    )
  }

  return(interpretation)
}
ui <- page_sidebar(
  title = "Enhanced Kaplan-Meier Plot Builder",
  sidebar = sidebar(
    title = "Plot Controls",
    accordion(
      accordion_panel(
        "Data Input",
        fileInput("file", "Upload CSV file",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        selectInput("time_col", "Time Column:", NULL),
        selectInput("status_col", "Status Column:", NULL),
        selectInput("group_col", "Group Column:", NULL),
        actionButton("use_sample", "Use Sample Data", class = "btn-secondary")
      ),
      accordion_panel(
        "Plot Layers",
        checkboxGroupInput("layers", "Select Plot Layers:",
                           choices = list(
                             "Base KM Curves" = "base",
                             "Confidence Intervals" = "ci",
                             "Risk Table" = "risk",
                             "Censoring Points" = "censor",
                             "Number at Risk" = "risk_n"
                           ),
                           selected = "base"
        )
      ),
      accordion_panel(
        "Statistical Annotations",
        checkboxInput("show_pvalue", "Show p-value", FALSE),
        checkboxInput("show_median", "Show Median Survival", FALSE),
        checkboxInput("show_hr", "Show Hazard Ratio", FALSE)
      ),
      accordion_panel(
        "Appearance",
        selectInput("theme", "Select Theme",
                    choices = c("Classic" = "theme_classic",
                                "Minimal" = "theme_minimal",
                                "Light" = "theme_light"),
                    selected = "theme_classic"),
        uiOutput("color_pickers")
      )
    )
  ),
  layout_column_wrap(
    width = 1/2,
    heights_equal = "row",
    value_box(
      full_screen = TRUE,
      title = "What is this layer?",
      value = textOutput("layer_explanation"),
      showcase = icon("info-circle"),
      theme = "primary"
    ),
    card(
      full_screen = TRUE,
      card_header("Kaplan-Meier Survival Plot"),
      plotOutput("kmplot", height = "500px")
    ),
    card(
      full_screen = TRUE,
      card_header("AI Plot Interpretation"),
      actionButton("interpret", "Generate Interpretation", class = "btn-primary"),
      verbatimTextOutput("ai_interpretation")
    ),
    card(
      full_screen = TRUE,
      card_header("Data Preview"),
      DTOutput("data_preview")
    ),
    card(
      full_screen = TRUE,
      card_header("Statistical Summary"),
      verbatimTextOutput("stats_summary")
    )
  )
)

server <- function(input, output, session) {
  # Reactive values for data and analysis
  rv <- reactiveValues(
    data = default_data,
    colors = c("#E7B800", "#2E9FDF")
  )
  # New reactive for AI interpretation
  interpretation <- reactiveVal("")
  observeEvent(input$interpret, {
    req(stats())
    groups <- levels(rv$data$group)
    interpretation(generate_interpretation(stats(), groups))
  })

  output$ai_interpretation <- renderText({
    interpretation()
  })

  # Data handling

  observeEvent(input$file, {
    req(input$file)
    rv$data <- read.csv(input$file$datapath)
    updateSelectInput(session, "time_col", choices = names(rv$data), selected = names(rv$data)[1])
    updateSelectInput(session, "status_col", choices = names(rv$data), selected = names(rv$data)[2])
    updateSelectInput(session, "group_col", choices = names(rv$data), selected = names(rv$data)[3])
  })

  observeEvent(input$use_sample, {
    rv$data <- default_data
    updateSelectInput(session, "time_col", selected = "time")
    updateSelectInput(session, "status_col", selected = "status")
    updateSelectInput(session, "group_col", selected = "group")
  })

  # Dynamic color pickers based on groups
  output$color_pickers <- renderUI({
    req(rv$data)
    groups <- unique(rv$data$group)
    color_pickers <- lapply(seq_along(groups), function(i) {
      colourInput(
        inputId = paste0("color_", i),
        label = paste("Color for", groups[i]),
        value = rv$colors[i]
      )
    })
    do.call(tagList, color_pickers)
  })

  # Update colors when pickers change
  observe({
    req(rv$data)
    groups <- unique(rv$data$group)
    colors <- sapply(seq_along(groups), function(i) {
      input[[paste0("color_", i)]]
    })
    rv$colors <- colors
  })

  # Create survival object
  surv_obj <- reactive({
    req(rv$data)
    time_col <- if(!is.null(input$time_col) && input$time_col != "") input$time_col else "time"
    status_col <- if(!is.null(input$status_col)&& input$status_col != "" ) input$status_col else "status"
    group_col <- if(!is.null(input$group_col) && input$group_col != "") input$group_col else "group"
    formula <- as.formula(paste0("Surv(", time_col, ", ", status_col, ") ~ ", group_col))
    survfit_orignal <- survfit(formula = formula, data = rv$data)
    survfit_orignal$call$formula <- formula
    survfit_orignal
  })
  # Calculate additional statistics
  stats <- reactive({
    fit <- surv_obj()
    cox <- coxph(Surv(time, status) ~ group, data = rv$data)
    hr <- exp(coef(cox))
    hr_ci <- exp(confint(cox))

    list(
      median = summary(fit)$table[, "median"],
      hr = hr,
      hr_ci = hr_ci,
      cox_p = summary(cox)$waldtest["pvalue"]
    )
  })

  # Layer explanations
  output$layer_explanation <- renderText({
    selected_layers <- input$layers
    if(length(selected_layers) == 0) return("Select layers to start building the plot")

    last_selected <- tail(selected_layers, 1)
    switch(last_selected,
           "base" = "Base KM curves show the estimated survival probability over time for each group",
           "ci" = "Confidence intervals show the uncertainty around the survival estimates",
           "risk" = "Risk table shows the number of subjects at risk at different time points",
           "censor" = "Censoring points indicate when participants were lost to follow-up",
           "risk_n" = "Number at risk annotations provide context about the sample size over time"
    )
  })

  # Data preview
  output$data_preview <- renderDT({
    datatable(rv$data, options = list(pageLength = 5))
  })

  # Statistical summary
  output$stats_summary <- renderPrint({
    req(stats())
    cat("Statistical Summary:\n\n")
    cat("Median Survival Times:\n")
    print(stats()$median)
    cat("\nHazard Ratio (95% CI):\n")
    print(sprintf("%.2f (%.2f-%.2f)",
                  stats()$hr, stats()$hr_ci[1], stats()$hr_ci[2]))
    cat("\nLog-rank p-value:\n")
    print(stats()$cox_p)
  })

  # Reactive KM plot
  output$kmplot <- renderPlot({
    req(surv_obj())

    # Additional annotations
    annotations <- NULL
    if(input$show_median) {
      annotations <- list(
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray"),
        geom_text(aes(x = 0, y = 0.5), label = "Median", hjust = -0.1)
      )
    }
    # Base plot
    p <- ggsurvplot(
      surv_obj(),
      data = rv$data,
      risk.table = "risk" %in% input$layers,
      conf.int = "ci" %in% input$layers,
      censor = "censor" %in% input$layers,
      risk.table.y.text = "risk_n" %in% input$layers,
      pval = input$show_pvalue,
      pval.method = TRUE,
      ggtheme = get(input$theme)(),
      # palette = rv$colors
      title = "Kaplan-Meier Survival Curve",
      risk.table.height = 0.25,
      legend.labs = levels(rv$data$group),
      xlab = "Time",
      ylab = "Survival probability"
    )
    if(input$show_hr) {
      hr_text <- sprintf("HR = %.2f (95%% CI: %.2f-%.2f)",
                         stats()$hr, stats()$hr_ci[1], stats()$hr_ci[2])
      p$plot <- p$plot + annotate("text", x = max(rv$data$time)/2, y = 0.1,
                                  label = hr_text)
    }

    p$plot
  })
}

shinyApp(ui, server)
