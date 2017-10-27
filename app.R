library(dplyr)
library(ggthemes)
library(plotly)
library(shiny)

db <- readRDS("./data/data_after_import.Rds")

db <- db %>%
      mutate(CalYear    = as.factor(CalYear),
             Gender     = as.factor(Gender),
             Category   = as.factor(Category),
             Occupation = as.factor(Occupation))

ui <- fluidPage(

      sidebarLayout(

            sidebarPanel(
                  selectInput("var1", "Pick numeric var:",
                              choices = c("Age", "Bonus", "Poldur",
                                          "Density", "Exppdays")),

                  selectInput("var2", "Pick categoric var:",
                              choices = c("CalYear", "Gender",
                                          "Category", "Occupation")),

                  sliderInput("bins", "Pick number of bins:",
                              min = 10, max = 70, value = 70),

                  checkboxInput("fill", "Scale to 100%:",
                                value = FALSE)
            ),

            mainPanel(
                  plotlyOutput("distribution_plot"),
                  plotlyOutput("response_plot"))
      )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

      bins <- reactive({
            min(input$bins, length(unique(db[[input$var1]])))
      })

      position <- reactive({
            if (input$fill == TRUE){
                  return("fill")
            }
            return("stack")
      })

      output$distribution_plot <- renderPlotly({
            p <- ggplot(db, aes_string(x = input$var1, fill = input$var2)) +
                  geom_histogram(bins = bins(), position = position()) +
                  scale_fill_hc() +
                  theme_hc()
            ggplotly(p)
      })

      output$response_plot <- renderPlotly({
            p <- ggplot(db, aes_string(x = input$var1, y = "Numtppd",
                                       fill = input$var2, colour = input$var2)) +
                  geom_smooth() +
                  scale_fill_hc() +
                  theme_hc()
            ggplotly(p)
      })
}

# Run the application
shinyApp(ui = ui, server = server)