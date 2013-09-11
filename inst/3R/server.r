
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
        get.musd(input$outcome,
                 input$hist.induced.mu, input$hist.induced.sd,
                 input$hist.control.mu, input$hist.control.sd,
                 input$hist.treated.mu, input$hist.treated.sd,
                 input$cola.induced.mu, input$cola.induced.sd,
                 input$cola.control.mu, input$cola.control.sd,
                 input$cola.treated.mu, input$cola.treated.sd
        )}) 
    
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
        # musd <- musd()
        powertables <- powertables()
        ntreated <- as.numeric(input$ntreated)
        get.table(powertables, k = ntreated)
    }, NA.string = "-", include.rownames = FALSE)
    
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

