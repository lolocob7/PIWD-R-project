#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic
function(input, output, session) {
    output$trendPlot <- renderPlot({
        req(input$country)
        # Use the shared plotting function
        plot_unemployment_trend(unemployment_data, input$country)
    })
}
