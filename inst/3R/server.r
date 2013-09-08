
# setwd("/Users/stefvanbuuren/Documents/Sync/W/My Documents/Diversen/dierproeven/Stoop/RRR/R")
library("RRR")

shinyServer(function(input, output) {
    reset.globals()
    
    draw.plot <- function() {
        alpha <- ifelse(input$alpha == "0.05", 0.05, 0.01)
        power <- ifelse(input$power == "80%", 0.8, 0.9)
        ncompound <- as.numeric(input$ncompound)
        isPBS <- (input$comparison == "PBS")
        musd <- get.musd()

        if (isPBS)  z <- power.grid(mu1 = musd[1,1], sd1 = musd[1,2],
                                    mu2 = musd[2,1], sd2 = musd[2,2],
                                    percent = seq(100,10,-10), 
                                    alternative = "less", 
                                    alpha = alpha,
                                    power = power)
        else z <- power.grid(mu1 = musd[1,1], sd1 = musd[1,2],
                             mu2 = musd[3,1], sd2 = musd[3,2],
                             percent = 100, 
                             alternative = "less", 
                             alpha = alpha,
                             power = power)
        
        main <- ifelse(input$outcome == "hist", 
                       "Histology left lobe", 
                       "Collagen Accessory & Medial")
        powerplot(z, power = power, main = main, isPBS = isPBS)
    }
    
    get.musd <- function() {
        musd <- matrix(NA, nrow = 3, ncol = 2, 
                         dimnames = list(c("Induced","PBS","Compound"), c("mu","sd")))
        # cat(input)
        if (input$outcome == "hist") {
            musd[1,] <- c(input$hist.bleomycin.mu, input$hist.bleomycin.sd)
            musd[2,] <- c(input$hist.pbs.mu, input$hist.pbs.sd)
            musd[3,] <- c(input$hist.compound.mu, input$hist.compound.sd)
        }
        if (input$outcome == "cola") {
            musd[1,] <- c(input$cola.bleomycin.mu, input$cola.bleomycin.sd)
            musd[2,] <- c(input$cola.pbs.mu, input$cola.pbs.sd)
            musd[3,] <- c(input$cola.compound.mu, input$cola.compound.sd)
        }
        return(musd)
    }
    
    
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
        musd <- get.musd()
        alpha <- ifelse(input$alpha == "0.05", 0.05, 0.01)
        power <- ifelse(input$power == "80%", 0.8, 0.9)
        ncompound <- as.numeric(input$ncompound)
        get.table(musd, ncompound, alpha, power)
    })
    
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

