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
   titlePanel("This is progress:"),
   
   # Sidebar with a slider input for number of bins 
   #sidebarLayout(
      # sidebarPanel(
      #    sliderInput("bins",
      #                "Number of bins:",
      #                min = 1,
      #                max = 50,
      #                value = 30)
      # ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Time Plot", plotlyOutput('TimePlot')),
          tabPanel("Time Table", tableOutput('TimeTable')))
          
        
            # tableOutput("TimeTable"),
            # plotOutput("TimePlot")
      )
  # )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    output$TimeTable <- renderTable({
      
      invalidateLater(1000, session)
      
      progressFunction(Sys.Date())
      
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    output$TimePlot <- renderPlotly({
      
     invalidateLater(1000, session)
      
      time <- progressFunction(Sys.Date())
      Year<- as.numeric(sub("%", "",time$Year))
      Quarter <- as.numeric(sub("%", "",time$Quarter))
      Month <- as.numeric(sub("%", "",time$Month))
      Day <- as.numeric(sub("%", "",time$Day))
      Hour <- as.numeric(sub("%", "",time$Hour))
      Minute <- as.numeric(sub("%", "",time$Minute))
      
      df<- as.data.frame(cbind(Year,Quarter,Month,Day,Hour,Minute))
      
      
      Time <- names(df)
      Growth <- as.numeric(df[1,])
                          data1<- data.frame(cbind(Time,Growth))
                          data1$Growth=as.numeric(levels(data1$Growth))[data1$Growth]

                          library(plotly)
                          yform <- list(categoryorder = "array",
                                        categoryarray = data1$Time,
                                        showgrid = T, showline = T, showticklabels = TRUE)

                          plot_ly(data = data1,
                                  x = ~Growth,
                                  y = ~Time,
                                  type = "bar", orientation = 'h',  text = paste(Growth,"%"), textposition = 'auto',
                                  marker = list(color = 'rgba(50, 171, 96, 0.6)',
                                                line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1)))%>%
                                  #mode = "lines+markers")
                            layout(title = "Time live progress",
                                   xaxis = list(range=c(0, 100),zeroline = F, showline = T, showticklabels = TRUE, showgrid = TRUE),
                                   yaxis = yform)
    })
  }
# Run the application 
shinyApp(ui = ui, server = server)
# # 
# p1 <- plot_ly(x = ~TotalCharges, y = ~reorder(ExpenseCode, TotalCharges), name = 'Total charges',
#               type = 'bar', orientation = 'h',  text = paste(round(TotalCharges/1000,2),"K"), textposition = 'auto',
#               marker = list(color = 'rgba(50, 171, 96, 0.6)',
#                             line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>%
#   layout(yaxis = list(showgrid = T, showline = T, showticklabels = TRUE, domain= c(0, 0.85)),
#          xaxis = list(zeroline = F, showline = T, showticklabels = TRUE, showgrid = TRUE))
# 

# p2 <- plot_ly(x = ~TotalDistance, y = ~reorder(ExpenseCode, TotalDistance), name = 'Total Distance',
#               type = 'bar',
#               text = paste(round(TotalDistance,2),"Kms"), textposition = 'auto',
#               marker = list(color ='rgba(222,45,38,0.6)',
#                             line = list(color = 'rgb(169,169,169)', width = 1))) %>%
#   layout(yaxis = list(showgrid = T, showline =T, showticklabels =F, domain= c(0, 0.85)),
#          xaxis = list(zeroline = FALSE, showline = T, showticklabels = TRUE, showgrid = TRUE)) 
# 
# 




#                    



# library(ggplot2)
#                     
#                     # Usual bar plot :
#                     ggplot(data1, aes(x = group, y = value ,fill = group )) + 
#                       geom_bar(width = 0.85, stat="identity")
#                     
# # Circular one
# ggplot(data1, aes(x = group, y = value ,fill = group)) + 
#   geom_bar(width = 0.85, stat="identity") +    
#   
#   # To use a polar plot and not a basic barplot
#   coord_polar(theta = "y") +    
#   
#   #Remove useless labels of axis
#   xlab("") + ylab("") +
#   
#   #Increase ylim to avoid having a complete circle
#   ylim(c(0,100)) + 
#   
#   #Add names(df) labels close to the bars :
#   geom_text(data = data1, hjust = 1, size = 3, aes(x = group, y = 0, label = group)) +
#   
#   #Remove useless legend, y axis ticks and y axis text
#   theme(legend.position = "none" , axis.text.y = element_blank() , axis.ticks = element_blank())
# devtools::install_github("ropensci/plotly")
# library(plotly)
# p <- plot_ly(
#   type = "scatterpolar",
#   r = data1$group,
#   theta = data1$value,
#   mode = 'markers'
#   
# )
# 
# p <- plot_ly(
#   type = 'scattercarpet',
#   r = c(0,1,2,2),
#   theta = c(0,45,90,0),
#   mode = 'markers'
# ) 
# 
# 

# 
# 
# 
# # make data
# data=data.frame(group=c("A ","B ","C ","D ") , value=c(33,62,56,67) )
# 
# # Usual bar plot :
# ggplot(data, aes(x = group, y = value ,fill = group )) + 
#   geom_bar(width = 0.85, stat="identity")
# 
# # Circular one
# ggplot(data, aes(x = group, y = value ,fill = group)) + 
#   geom_bar(width = 0.85, stat="identity") +    
#   
#   # To use a polar plot and not a basic barplot
#   coord_polar(theta = "y") +    
#   
#   #Remove useless labels of axis
#   xlab("") + ylab("") +
#   
#   #Increase ylim to avoid having a complete circle
#   ylim(c(0,75)) + 
#   
#   #Add group labels close to the bars :
#   geom_text(data = data, hjust = 1, size = 3, aes(x = group, y = 0, label = group)) +
#   
#   #Remove useless legend, y axis ticks and y axis text
#   theme(legend.position = "none" , axis.text.y = element_blank() , axis.ticks = element_blank())
# 
