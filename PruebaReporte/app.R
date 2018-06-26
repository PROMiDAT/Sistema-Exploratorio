#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
shinyApp(
  
  ui = fluidPage(
      title = 'Download a PDF report',
      sidebarLayout(
        sidebarPanel(
          helpText(),
          selectInput('x', 'Build a regression model of mpg against:',
                      choices = names(mtcars)[-1]),
          radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'), inline = TRUE),
          downloadButton('downloadReport')
        ),
        mainPanel(
          plotOutput('regPlot')
        )
      )
    ),
  
  server = function(input, output) {
      
      regFormula <- reactive({
        as.formula(paste('mpg ~', input$x))
      })
      
      output$regPlot <- renderPlot({
        par(mar = c(4, 4, .1, .1))
        plot(regFormula(), data = mtcars, pch = 19)
      })
      
      output$downloadReport <- downloadHandler(
        filename = function() {
          paste('my-report', sep = '.', switch(
            input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
          ))
        },
        
        content = function(file) {
          #src <- normalizePath('report.Rmd')
          
          # temporarily switch to the temp dir, in case you do not have write
          # permission to the current working directory
          #owd <- setwd(tempdir())
          #on.exit(setwd(owd))
          file.copy("/Users/promidat05/Desktop/PROMIDAT-EXPLORATORIO-SHINY/", 'report.Rmd', overwrite = TRUE)
          
          library(rmarkdown)
          out <- render('/Users/promidat05/Desktop/PROMIDAT-EXPLORATORIO-SHINY/report.Rmd', switch(
            input$format,
            PDF = pdf_document(), HTML = html_document(), Word = word_document()
          ))
          file.rename(out, file)
        }
      )
      
    }
  
  
)