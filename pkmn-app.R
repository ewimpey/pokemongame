library(shiny)

#Setting Up the User Interface
ui <- fluidPage(
  titlePanel("Pokemon Trivia Game"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # Input the pokemon characteristics
      actionButton("button", "Get New Pokemon!"),
      textInput("pokemon_name", "What is the name of this pokemon?"),
      textInput("type1", "What is it's first type?"),
      textInput("type2", "What is it's second type (if any)?"),
      
      radioButtons("region", "What is it's original region?",
                         choices = list("Kanto", "Johto", "Hoenn", "Sinnoh", "Unova", "Kalos", "Alola", "Galar"),
                          ),
      actionButton("submit", "Submit Answers")
    ),
    # Show image, answers to questions
    mainPanel(
      imageOutput("img"),
      h4(textOutput("pname")),
      h4(textOutput("type")),
      h4(textOutput("region")),
      # Show points earned
      h2(textOutput("pts"))
    )
  )
)


server <- function(input, output){  
  library(highcharter)
  library(magick)
  data("pokemon")
  
  # using child's subject-matter-expertise
  regions <- c(rep("Kanto", 151), rep("Johto", 100), rep("Hoenn", 135), rep("Sinnoh", 108), rep("Unova", 155), rep("Kalos", 72), rep("Alola", 88), rep("Galar", 89))
  
  pokemon$region <- regions
  
  # render image
  show_pokepic <- function(random_number){
    t <- paste0("https:", pokemon[[random_number, 22]])
    par(mar=c(1,1,1,1))
    (image_read(t))
  }
  
  #sample a new pokemon each time button is clicked
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$button, {
    v$data <- sample(1:nrow(pokemon), 1) 
  })
  
  # Get image of pokemon
  output$img <- renderImage({
    tmpfile <- show_pokepic(v$data) %>%
      image_write(tempfile(fileext='jpg'), format = 'jpg')
    list(src = tmpfile, contentType = "image/jpeg")
  })
  
  #Get name of pokemon
  output$pname <- renderText({
    input$submit
    
    # Use isolate() wait until 'submit' button
    isolate({paste0("Name: ",pokemon[[v$data, 2]])})
  })
  
  # Get type(s) for pokemon
  output$type <- renderText({
    input$submit
    
    # Use isolate() wait until 'submit' button
    isolate({
      ifelse(!is.na(pokemon[[v$data, 8]]), 
             paste0("Types: ", pokemon[[v$data, 7]]," and ", pokemon[[v$data, 8]]),
             paste0("Type: ", pokemon[[v$data, 7]]))})
  })
  
  # Get region for pokmeon
  output$region <- renderText({
    input$submit
    
    # Use isolate() wait until 'submit' button
    isolate({(paste0("Region: ",pokemon[[v$data, 25]]))})
  })
  
  # Calculate points:
  
  
  # output the points after submitting
  output$pts <- renderText({
    input$submit
    
    # Use isolate() wait until 'submit' button
    isolate({
      pts = (tolower(input$pokemon_name) == tolower(pokemon[[v$data, 2]])) +
        (input$type1 != input$type2 & 
           input$type1 %in% c(pokemon[[v$data, 7]], pokemon[[v$data, 8]], "") &
           input$type2 %in% c(pokemon[[v$data, 7]], pokemon[[v$data, 8]], "")) +
        (input$region == pokemon[[v$data, 25]])
      
      paste0("You scored ", pts, " points!")})
  })
}
shinyApp(ui = ui, server = server)