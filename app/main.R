box::use(
  shiny[HTML, NS, moduleServer, div, column, observe, reactiveVal, observeEvent, stopApp, textInput, icon, fluidRow],
  utils[URLencode],
  shinydashboard[dashboardBody, box, dashboardPage, dashboardHeader, dashboardSidebar],
  imola[gridPanel, flexPanel],
  shinyalert[shinyalert],
  shinyWidgets[actionBttn],
  httr[GET, add_headers, content],
  jsonlite[fromJSON],
  htmltools[tags],
  waiter[autoWaiter, spin_2]
)

box::use(app / view / weatherOverviewContent)
box::use(app / view / gridTemplate[two_row_custom, searchTemplate])
box::use(app / logic / APIconfig[api_base_url, headers])
box::use(app / view / tempForecast)
box::use(app / view / rainForecast)
box::use(app / view / currentTemp)
box::use(app / view / header)

dashboardBgColor <- tags$head(
  tags$style(
    HTML(".content-wrapper { background-color: #404258;}")
  )
)

headerText <- div(
  style = "text-align: center; font-family: 'Roboto', sans-serif; font-weight: bold; color: white; font-size: 40px; padding-bottom: 15px;",
  "WEATHER DASHBOARD"
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  dashboardPage(skin = "black",
    title = "Weather Dashboard",
    dashboardHeader(disable = T),
    dashboardSidebar(disable = T),
    dashboardBody(
      autoWaiter(html = spin_2(), color = "black"),
      dashboardBgColor,
      fluidRow(headerText),
      fluidRow(header$ui(ns("location"))),
      fluidRow(
      gridPanel(
        template = two_row_custom,
        content1 = weatherOverviewContent$ui(ns("weather_content")),
        content2 = box(
          background = "black",
          width = 12,
          flexPanel(
            direction = "column",
            textInput(
              ns("search"),
              label = "",
              placeholder = "Search Location",
              width = "100%"
            ),
            actionBttn(
              inputId = ns("search_btn"),
              label = "Search/Update",
              icon = icon("search"),
              style = "gradient",
              size = "sm",
              block = F,
              color = "primary"
            ),
            currentTemp$ui(ns("temp")),
            gap = "15px"
          )
        )
      )
    ),
    fluidRow(
        tempForecast$ui(ns("temp_forecast")),
        rainForecast$ui(ns("rain_forecast"))
    ))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ## Intimation about the limited API requests
    shinyalert(
      title = "Information",
      text = "Enter your desired location name and click on the 'Search/Update' button.\n Note: You can search for maximum of 3 locations only.",
      size = "s",
      closeOnEsc = F,
      closeOnClickOutside = F,
      html = FALSE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#3B71CA",
      animation = F
    )
    
    # API call count
    count_request = reactiveVal(0)
    
    # Increment count
    increment_count <- function() {
      count_request(count_request() + 1)
    }
    
    # Code execution flag
    run_code <- T
    
    # Required dataframes initialization
    current_data <- reactiveVal(NULL)
    
    forecast_data <- reactiveVal(NULL)
    
    rain_chance <- reactiveVal(NULL)
    
    jsonData <- reactiveVal(NULL)
    
    # Define search button functionality
    observeEvent(input$search_btn, {
      if (count_request() == 3) {
        ## Update flag
        run_code <- F
        
        ## Intimation about the limited API requests
        shinyalert(
          title = "Information",
          text = "You have reached your maximum limit for location searches. Press 'OK' to close the application.",
          size = "s",
          closeOnEsc = F,
          closeOnClickOutside = F,
          html = FALSE,
          type = "info",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#3B71CA",
          animation = F,
          callbackR = function() {
            stopApp()
          }
        )
      }
      
      if (input$search == "") {
        # Shiny modal for no location name
        shinyalert(
          title = "Information",
          text = "Enter a location name to search",
          size = "s",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          html = FALSE,
          type = "info",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#3B71CA",
          animation = F
        )
      }
      else{
        # Query parameters
        query_params <- list(q = input$search, days = "3")
        
        # Encode the query parameters
        encoded_params <-
          URLencode(names(query_params), reserved = TRUE)
        
        query_string <-
          paste(
            encoded_params,
            "=",
            URLencode(unlist(query_params)),
            sep = "",
            collapse = "&"
          )
        
        # Construct the complete URL
        api_url <- paste0(api_base_url, "?", query_string)
        
        response <- GET(api_url, add_headers(.headers = headers))
        
        # Check if the request was successful (status code 200)
        if (response$status_code == 200 & run_code == T) {
          # Record api call count
          increment_count()
          
          # Parse the JSON response
          json_data <-
            fromJSON(content(response, "text"), flatten = TRUE)
          
          jsonData(json_data)
          
          forecast_today <-
            as.data.frame(json_data$forecast$forecastday$hour[1])
          
          format_time <-
            strptime(json_data$location$localtime, format = "%Y-%m-%d %H:%M") |> format(format = "%Y-%m-%d %H:00")
          
          # Current weather details dataframe
          current_data(subset(forecast_today, time == format_time))
          
          forecast1 <-
            json_data$forecast$forecastday$hour[1] |> as.data.frame()
          forecast1 <- forecast1[forecast1$time >= format_time, ]
          forecast2 <-
            json_data$forecast$forecastday$hour[2] |> as.data.frame()
          forecast3 <-
            json_data$forecast$forecastday$hour[3] |> as.data.frame()
          
          # Temperature forecast dataframe
          forecast_data(rbind(forecast1, forecast2, forecast3))
          
          # Rain chance dataframe
          rain_chance_data <-
            forecast_today[forecast_today$chance_of_rain > 0 &
                             forecast_today$time >= format_time, ]
          rain_chance_data$time <-
            strptime(rain_chance_data$time, format = "%Y-%m-%d %H:%M") |> format(format = "%H:%M")
          rain_chance(rain_chance_data)
          
        }
        else if (response$status_code == 400 & run_code == T) {
          # Record api call count
          increment_count()
          
          # Shiny modal for error in API request
          shinyalert(
            title = "Oops!",
            text = "Unable to fetch data for the entered location.",
            size = "s",
            closeOnEsc = TRUE,
            closeOnClickOutside = TRUE,
            html = FALSE,
            type = "error",
            showConfirmButton = TRUE,
            showCancelButton = FALSE,
            confirmButtonText = "OK",
            confirmButtonCol = "#3B71CA",
            animation = F
          )
        }
        else if (run_code == F) {
          # Shiny modal after application stops
          shinyalert(
            title = "Bye!",
            text = "The application has stopped. You can close the window now.",
            size = "s",
            closeOnEsc = F,
            closeOnClickOutside = F,
            html = FALSE,
            type = "info",
            showConfirmButton = F,
            showCancelButton = FALSE,
            animation = F
          )
        }
        else {
          # Shiny modal for any other error
          shinyalert(
            title = "Oops!",
            text = "An unexpected error has occured. Please try again!",
            size = "s",
            closeOnEsc = TRUE,
            closeOnClickOutside = TRUE,
            html = FALSE,
            type = "error",
            showConfirmButton = TRUE,
            showCancelButton = FALSE,
            confirmButtonText = "OK",
            confirmButtonCol = "#3B71CA",
            animation = F
          )
        }
      }
    })
    
    observe({
      input$search_btn
      weatherOverviewContent$server("weather_content", jsonData(), current_data())
      tempForecast$server("temp_forecast", forecast_data())
      rainForecast$server("rain_forecast", rain_chance())
      currentTemp$server("temp", current_data())
      header$server("location", jsonData())
    })
  })
}
