
# setwd("/Users/stefvanbuuren/Documents/Sync/W/My Documents/Diversen/dierproeven/Stoop/RRR/R")
library("RRR")
treated <- 0

shinyServer(function(input, output) {
    
    draw.plot <- function() {
        ntreated <<- as.numeric(input$ntreated)
        # iscontrol <- input$comparison == "control"
        iscontrol <- TRUE
        if (input$disease == "fibrosis") 
            main <- ifelse(input$outcome == "hist", 
                           "Histological fibrosis score", 
                           "Collagen content")
        if (input$disease == "other") 
            main <- ifelse(input$otheroutcome == "out1", 
                           "Outcome 1",
                           "Outcome 2")
        powertables <- powertables()
        powerplot(powertables, main = main, isPBS = iscontrol)
    }
    
    powertables <- reactive({
        if (input$disease == 'fibrosis') musd <- 
                get.musd(input$outcome,
                         input$hist.induced.mu, input$hist.induced.sd,
                         input$hist.control.mu, input$hist.control.sd,
                         as.numeric(input$hist.treated.pct),
                         input$cola.induced.mu, input$cola.induced.sd,
                         input$cola.control.mu, input$cola.control.sd,
                         as.numeric(input$cola.treated.pct))
        else musd <- 
                get.musd(input$otheroutcome,
                         input$out1.induced.mu, input$out1.induced.sd,
                         input$out1.control.mu, input$out1.control.sd,
                         as.numeric(input$out1.treated.pct),
                         input$out2.induced.mu, input$out2.induced.sd,
                         input$out2.control.mu, input$out2.control.sd,
                         as.numeric(input$out2.treated.pct))
        calculate.powertables(
            musd,
            alpha = ifelse(input$alpha == "0.05", 0.05, 0.01),
            power = switch(input$power, "0.80" = 0.8, 
                           "0.90" = 0.9,
                           "0.50" = 0.5)
        )})
    
    # musd <- reactive({
    #     if (input$disease == 'fibrosis') 
    #         get.musd(input$outcome,
    #                  input$hist.induced.mu, input$hist.induced.sd,
    #                  input$hist.control.mu, input$hist.control.sd,
    #                  as.numeric(input$hist.treated.pct),
    #                  input$cola.induced.mu, input$cola.induced.sd,
    #                  input$cola.control.mu, input$cola.control.sd,
    #                  as.numeric(input$cola.treated.pct)
    #         )
    #     else 
    #         get.musd(input$otheroutcome,
    #                  input$out1.induced.mu, input$out1.induced.sd,
    #                  input$out1.control.mu, input$out1.control.sd,
    #                  as.numeric(input$out1.treated.pct),
    #                  input$out2.induced.mu, input$out2.induced.sd,
    #                  input$out2.control.mu, input$out2.control.sd,
    #                  as.numeric(input$out2.treated.pct))
    # }) 
    
    create.pdf <- reactive({
        temp <- tempfile(fileext = ".pdf")
        
        pdf(file = temp, height = 7, width = 7, useDingbats = FALSE)
        draw.plot()
        dev.off()
        
        return(temp)
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
        ntreated <<- as.numeric(input$ntreated)
        alpha <<- ifelse(input$alpha == "0.05", 0.05, 0.01)
        power1 <<- switch(input$power, "80%" = 0.8, 
                          "90%" = 0.9,
                          "50%" = 0.5)
        musdval <<- 
            if (input$disease == 'fibrosis')
                get.musd(input$outcome,
                         input$hist.induced.mu, input$hist.induced.sd,
                         input$hist.control.mu, input$hist.control.sd,
                         as.numeric(input$hist.treated.pct),
                         input$cola.induced.mu, input$cola.induced.sd,
                         input$cola.control.mu, input$cola.control.sd,
                         as.numeric(input$cola.treated.pct)
                )
        else
            get.musd(input$otheroutcome,
                     input$out1.induced.mu, input$out1.induced.sd,
                     input$out1.control.mu, input$out1.control.sd,
                     as.numeric(input$out1.treated.pct),
                     input$out2.induced.mu, input$out2.induced.sd,
                     input$out2.control.mu, input$out2.control.sd,
                     as.numeric(input$out2.treated.pct))
        
        table.sym <<- create.table.sym()
        table.asym <<- create.table.asym()
        includeRmd(file.path(path.package("RRR"),"md","summary.Rmd"))
    })
    
    create.summary.print <- reactive({
        ntreated <<- as.numeric(input$ntreated)
        alpha <<- ifelse(input$alpha == "0.05", 0.05, 0.01)
        power1 <<- switch(input$power, "80%" = 0.8, 
                          "90%" = 0.9,
                          "50%" = 0.5)
        musdval <<- 
            if (input$disease == 'fibrosis')
                get.musd(input$outcome,
                         input$hist.induced.mu, input$hist.induced.sd,
                         input$hist.control.mu, input$hist.control.sd,
                         as.numeric(input$hist.treated.pct),
                         input$cola.induced.mu, input$cola.induced.sd,
                         input$cola.control.mu, input$cola.control.sd,
                         as.numeric(input$cola.treated.pct)
                )
        else
            get.musd(input$otheroutcome,
                     input$out1.induced.mu, input$out1.induced.sd,
                     input$out1.control.mu, input$out1.control.sd,
                     as.numeric(input$out1.treated.pct),
                     input$out2.induced.mu, input$out2.induced.sd,
                     input$out2.control.mu, input$out2.control.sd,
                     as.numeric(input$out2.treated.pct))
        
        table.sym <<- create.table.sym()
        table.asym <<- create.table.asym()
        includeRmd(file.path(path.package("RRR"),"md","summary.Rmd"), 
                   fragment.only = FALSE)
    })
    
    output$mainplot <- renderPlot(
        draw.plot(),
        width = 700,
        height = 700, 
        res = 108
    )
    
    # Generate an HTML table view of the data
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
    
    output$summary <- renderUI(create.summary())
    
    output$downloadPdf <- downloadHandler(
        filename = function() {paste('my-figure', sep = '.', 'pdf')},
        content = function(file) {
            pdffile <- create.pdf()
            
            # create temporary directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(from = pdffile, to = file, overwrite = TRUE)
            # file.rename(from = filename, to = file)
            
            #on.exit(unlink(pdffile))
            #bytes <- readBin(pdffile, "raw", file.info(pdffile)$size)
            #writeBin(bytes, filepath)
        }
    )
    
    
    
    output$downloadTable <- downloadHandler(
        filename = "table.txt",
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
        filename = "mysummary.html",
        content = function(file) {
            out <- create.summary.print()
            file.rename("summary.html", file)
        },
        contentType = "application/html"
    )
    
    
}
)

