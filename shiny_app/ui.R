#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application
fluidPage(

    # Application title
    titlePanel("Unemployment Data Analysis"),

    # Sidebar with a dropdown for Country selection
    sidebarLayout(
        sidebarPanel(
            selectInput("country",
                "Select Country:",
                choices = unique(unemployment_data$geo),
                selected = "Austria"
            )
        ),

        # Show the plot
        mainPanel(
            plotOutput("trendPlot")
        )
    )
)
