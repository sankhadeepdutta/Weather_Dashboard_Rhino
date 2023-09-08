box::use(shinydashboard[box], shiny[moduleServer, NS, plotOutput, renderPlot], ggplot2[ggplot, geom_bar, aes, geom_text, labs, xlim, theme_minimal, theme, element_blank, element_text, element_rect])

#' @export
ui <- function(id){
  ns <- NS(id)
  box(
    background = "black",
    width = 3, 
    closable = F,
    status = NULL,
    plotOutput(ns("forecast"))
  )
}

#' @export
server <- function(id, rain_data){
  moduleServer(id, function(input, output, session){
    output$forecast <- renderPlot({
      data <- tryCatch({
        rain_data
      }, error = function(e) {
        NULL
      })
      if (is.null(data)) {
        NULL
      }
      else{
        plot <- ggplot(data, aes(x = chance_of_rain,
                                 y = time,
                                 width = 0.3)) +
          geom_bar(stat = "identity",
                   show.legend = F,
                   fill = "white") +
          geom_text(
            aes(label = paste(chance_of_rain, "%")),
            hjust = -0.4,
            color = "white",
            size = 5
          ) +
          labs(x = NULL,
               y = NULL,
               title = "Chances of Rain (Today)") +
          xlim(0, 100) +
          theme_minimal() +
          theme(
            plot.background = element_rect(fill = "black"),  
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            plot.title = element_text(size = 16, hjust = 0, color = "white"),  
            axis.text.y = element_text(color = "white", size = 12), 
            axis.title = element_text(color = "white"),  
            legend.text = element_text(color = "white"), 
            legend.title = element_text(color = "white")
          )
        
        if (nrow(data) == 0) {
          plot + labs(title = "No more rain chances for today")
        }
        else{
          plot
        }
      }
    })
  })
}


