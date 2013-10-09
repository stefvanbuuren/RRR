
# setwd("/Users/stefvanbuuren/Documents/Sync/W/My Documents/Diversen/dierproeven/Stoop/RRR/R")
library("RRR")

shinyServer(function(input, output) {
    
    draw.plot <- function() {
        ntreated <- as.numeric(input$ntreated)
        # iscontrol <- input$comparison == "control"
        iscontrol <- TRUE
        main <- ifelse(input$outcome == "hist", 
                       "Histology left lobe", 
                       "Collagen Accessory & Medial")
        powertables <- powertables()
        powerplot(powertables, main = main, isPBS = iscontrol)
    }
    
    powertables <- reactive({
        calculate.powertables(
            musd = musd(),
            alpha = ifelse(input$alpha == "0.05", 0.05, 0.01),
            power = switch(input$power, "80%" = 0.8, 
                           "90%" = 0.9,
                           "50%" = 0.5)
        )})
    
    musd <- reactive({
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
        }) 
    
    create.pdf <- reactive({
        temp <- tempfile(fileext=".pdf")
        
        pdf(file = temp, height = 7, width = 7)
        draw.plot()
        dev.off()
        
        return(temp)
    })
    
            
    output$mainplot <- renderPlot(
        draw.plot(), 
        width = 700,
        height = 700, 
        res = 108
    )

    # Generate an HTML table view of the data
    output$table <- renderTable({
        powertables <- powertables()
        ntreated <- as.numeric(input$ntreated)
        get.table(powertables, k = ntreated)
    }, NA.string = "0", include.rownames = FALSE)
    
#        output$table <- renderMarkdown(
#            file = file.path(path.package("RRR"),"md","table.Rmd")
#        )
    
    output$downloadPdf <- downloadHandler(
        filename =  file.path(getwd(),paste("output","pdf",sep=".")),
        content = function(filepath) {
            pdffile <- create.pdf()
            on.exit(unlink(pdffile))
            bytes <- readBin(pdffile, "raw", file.info(pdffile)$size)
            writeBin(bytes, filepath)
        }
    )
}
)

