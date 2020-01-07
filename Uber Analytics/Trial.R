require (shiny)
require (argonDash) # UI
ui <- argonDashPage ( # header ------------------------------- ----------------------------------
                      header = argonDashHeader (tags $ img (src = "https: // www .data university.de / wp-content / uploads / 2019/06 / logo-data-university-white-small.png "),
                                                background_img =" https://www.data-university.de/wp-content/ uploads / 2019/04 / data-university-header-1.jpg ",
                                                separator = TRUE,
                                                height = 400,
                                                mask = TRUE,
                                                opacity = 1),
# Body -------------- -------------------------------------------------- ----
body = argonDashBody(
  fluidPage(
    tags$style(HTML("{{color: # 124D71; font-size: 150%;}
                             a: hover {color: # 69D0D0;}
                             h1 {color: # 69D0D0; font-size: 250 %;} ")),

    fluidRow(column(width = 8,
                      tagList(
                        br(),
                        br(),
                        htmlOutput(" hi ")
                      )
    ),

    column(width = 3,
            align = " right ",
            br(),

            textInput("text",
                        label = "",
                      value = "What is your name"),
           actionButton( "goButton", "OK"))))))
# Simple logic server-
server <- function(input, output){
observeEvent(input$goButton,{
your_name <- ifelse(isolate(input $ text) == "What's your name?","",paste("", isolate (input $ text)))
output$hi <- renderText ({paste0 ("<h1> Hi",your_name,"Join the Data University!</ h1> </br>  <a href='https://www.data-university.de/tickets/'> But of course - click! - </a>")
})
})
}
# Run the app
shinyApp (ui = ui, server = server)
