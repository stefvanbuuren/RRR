
shinyServer(function(input, output, session) {

  ## Create data storage for this session
  e <- new.env()

  ## Auxiliary functions
  draw.plot <- function() {
    ntreated <- as.numeric(input$ntreated)
    if (input$disease == "fibrosis")
      main <- ifelse(input$outcome == "hist",
                     "Histological fibrosis score",
                     "Collagen content")
    if (input$disease == "other")
      main <- ifelse(input$otheroutcome == "out1",
                     "Outcome 1",
                     "Outcome 2")
    powertables <- powertables()
    powerplot(powertables, main = main, isPBS = TRUE)
  }

  powertables <- reactive({
    calculate.powertables(
      musd = musd(),
      alpha = ifelse(input$alpha == "0.05", 0.05, 0.01),
      power = switch(input$power, "0.80" = 0.8,
                     "0.90" = 0.9,
                     "0.50" = 0.5)
    )})

  musd <- reactive({
    if (input$disease == 'fibrosis')
      get.musd(input$outcome,
               input$hist.induced.mu, input$hist.induced.sd,
               input$hist.control.mu, input$hist.control.sd,
               as.numeric(input$hist.treated.pct),
               input$cola.induced.mu, input$cola.induced.sd,
               input$cola.control.mu, input$cola.control.sd,
               as.numeric(input$cola.treated.pct))
    else
      get.musd(input$otheroutcome,
               input$out1.induced.mu, input$out1.induced.sd,
               input$out1.control.mu, input$out1.control.sd,
               as.numeric(input$out1.treated.pct),
               input$out2.induced.mu, input$out2.induced.sd,
               input$out2.control.mu, input$out2.control.sd,
               as.numeric(input$out2.treated.pct))
  })

  create.table.sym <- reactive({
    powertables <- powertables()
    ntreated <- as.numeric(input$ntreated)
    get.table.symmetric(powertables, k = ntreated)
  })

  create.table.asym <- reactive({
    powertables <- powertables()
    ntreated <- as.numeric(input$ntreated)
    get.table(powertables, k = ntreated)
  })

  create.summary <- reactive({
    e$ntreated <- as.numeric(input$ntreated)
    e$alpha <- as.numeric(input$alpha)
    e$power1 <- as.numeric(input$power)
    e$musdval <- musd()
    e$table.sym <- create.table.sym()
    e$table.asym <- create.table.asym()
    includeRmd(file.path(path.package("RRR"), "md", "summary.Rmd"),
               envir = e)
  })

  ## Output elements
  output$tablesym <- renderTable(
    create.table.sym(),
    include.rownames = FALSE,
    caption = "Symmetric design",
    caption.placement = "top"
  )

  output$tableasym <- renderTable(
    create.table.asym(),
    include.rownames = FALSE,
    caption = "Asymmetric design",
    caption.placement = "top"
  )

  output$mainplot <- renderPlot(
    draw.plot(),
    width = 700,
    height = 700,
    res = 108
  )

  output$summary <- renderUI(create.summary())

  # Context-sensitive download buttons
  output$downloadGraph <- downloadHandler(
    filename = function() {
      paste('Graph-', Sys.Date(), '.pdf', sep = '')
    },
    content = function(file) {
      pdf(file, useDingbats = FALSE)
      draw.plot()
      dev.off()
    }
  )

  output$downloadTable <- downloadHandler(
    filename = function() {
      paste('Table-', Sys.Date(), '.txt', sep = '')
    },
    content = function(file) {
      write.table(create.table.sym(),
                  file = file, sep = "\t", quote = FALSE,
                  row.names = FALSE)
      write.table(create.table.asym(),
                  file = file, sep = "\t", quote = FALSE,
                  append = TRUE,
                  row.names = FALSE, col.names = FALSE)
    }
  )

  output$downloadSummary <- downloadHandler(
    filename = function() {
      paste('Summary-', Sys.Date(), '.html', sep = '')
    },
    content = function(file) {
      html <- create.summary()
      writeLines(html, con = file)
    },
    contentType = "application/html"
  )
}
)

