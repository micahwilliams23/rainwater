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
ui <- fluidPage(

    # Application title
    titlePanel("East Honolulu Rainwater Harvesting"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(
            sliderInput("width",
                        "Width:",
                        min = 1,
                        max = 40,
                        value = 4),
            sliderInput("height",
                        "Height:",
                        min = 1,
                        max = 40,
                        value = 3.33),
            sliderInput("rain",
                        "Annual Precipitation (inches):",
                        min = 1,
                        max = 20,
                        value = 5),
            # h5('Average annual precipitation in East Honolulu is 5.34 inches.'),
            position = 'right',
            width = 2
        ),

        # Show a plot of the generated distribution
        column(
            5,
            plotOutput("panelsize",
                      width = '500px',
                      height = '500px')),
        column(
           5,
           plotOutput("fill",
                      width = '500px',
                      height = '500px')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$panelsize <- renderPlot({
        
        l <- input$height
        w <- input$width
        
        ggplot() +
            
            geom_rect(aes(xmin = 0, xmax = w,
                          ymin = 0, ymax = l),
                      fill = '#aaaaaa8d') +
            
            geom_text(aes(x = w / 2, y = l + 1.1,
                          label = round(l,2))) +
            geom_text(aes(x = w + 1, y = l / 2,
                          label = round(w,2))) +
            
            labs(title = 'Harvesting Dimensions') +
            
            scale_x_continuous(limits = c(0,40),
                               name = 'width (ft.)') +
            scale_y_continuous(limits = c(0,40),
                               name = 'length (ft.)') +
            
            coord_equal() +
            theme_minimal()
    })
    
    output$fill <- renderPlot({
        
        l <- input$height
        w <- input$width
        
        fl <- input$rain / 12
        
        vol = l * w * fl * 7.480543 # gal / cu. ft.
        
        t.vol = 275
        
        ggplot() +
            
            geom_rect(aes(xmin = 0, xmax = 40,
                          ymin = 0, ymax = 40),
                      fill = '#aaaaaa00',
                      color = '#000000',
                      size = 2) +
            
            geom_rect(aes(xmin = 0, xmax = 40,
                          ymin = 0, ymax = min(c(40, 40 * vol / t.vol))),
                      fill = '#3333aa') +
            
            geom_text(aes(x = 20, y = min(c(20, 20 * vol / t.vol)),
                          label = paste(
                              round(vol / t.vol * 100,2),
                              '%', sep = '')),
                      color = 'white') +
        
            
            labs(title = 'Fill Level') +
            
            scale_x_continuous(limits = c(0,40)) +
            scale_y_continuous(limits = c(0,40)) +
            
            coord_equal() +
            theme_minimal() +
            
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_blank())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
