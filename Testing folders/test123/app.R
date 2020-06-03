#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage("Navbar page", id = "tabs",
                 tabPanel("Home",
                          actionButton("hideTab", "Hide 'Foo' tab"),
                          actionButton("showTab", "Show 'Foo' tab"),
                          actionButton("hideMenu", "Hide 'More' navbarMenu"),
                          actionButton("showMenu", "Show 'More' navbarMenu")
                 ),
                 tabPanel("Foo", "This is the foo tab"),
                 tabPanel("Bar", "This is the bar tab"),
                 navbarMenu("More",
                            tabPanel("Table", "Table page"),
                            tabPanel("About", "About page"),
                            "------",
                            "Even more!",
                            tabPanel("Email", "Email page")
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    observeEvent(input$hideTab, {
        hideTab(inputId = "tabs", target = "Foo")
    })
    
    observeEvent(input$showTab, {
        showTab(inputId = "tabs", target = "Foo")
    })
    
    observeEvent(input$hideMenu, {
        hideTab(inputId = "tabs", target = "More")
    })
    
    observeEvent(input$showMenu, {
        showTab(inputId = "tabs", target = "More")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
