library(shiny)

vessel_ui <- function(id){
  uiOutput(NS(id, "vessel"))
}

vessel_server <- function(id, df, vessel_type){
  
  moduleServer(id, function(input, output, session){
    
    output$vessel <- renderUI({
      
      
      
      choices <- df %>% 
        filter(ship_type == vessel_type()) %>% 
        distinct(SHIPNAME) %>% 
        arrange(SHIPNAME) %>% 
        collect() %>% 
        pull(SHIPNAME)
      
      selectizeInput(inputId = 'vessel_name', label = 'Select a Vessel', 
                                choices = choices, selected = choices[1], options = list(placeholder = 'Type vessel name'))
    })
    
  })
  
}


metric_demo <- function() {
  
  df1 <- data.frame(SHIPNAME=LETTERS[1:8], ship_type = rep(c("A", "B"), 4))
  
  ui <- fluidPage(
    fluidRow(
      shinyWidgets::pickerInput("vessel_type", "Vessel Type", choices = c("A", "B"), selected = "A"), 
      vessel_ui(id = "x1")
    )
  )
  
  
  server <- function(input, output, session) {
    
    vessel_server(id = "x1", df = df1, reactive({input$vessel_type}))
    
    
    output$text <- renderText({
      input$vessel_type
    })
  }
  
  shinyApp(ui, server)
  
}



