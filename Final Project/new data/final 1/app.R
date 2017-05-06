#final 1
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(ggmap)

ui <- fluidPage(
   
   titlePanel("Final Project: Residential Fires with Fatalities"),
   

   sidebarLayout(
      sidebarPanel(
      checkboxGroupInput("years", "Plot(1). Choose the Year You Want to See:",
                         c("Year 2012" = "2012", 
                           "Year 2013" = "2013",
                           "Year 2014" = "2014",
                           "Year 2015" = "2015",
                           "All" = "4"
                           
                           ),
                           selected = "4"
      ),
      
      
    sliderInput("obs", "Plot(2). Adjust the sample size:",
      min = 0, max = 500, value = 250
    )
      
    
      
  ),
      

      mainPanel(
        tabsetPanel(
          tabPanel("Plot 1",
        
          h4("The following things include several plots and tables for the number of residential fires with fatalities.
              Click the tab above to change the things you want to see."),
          h3("Plot"),
          h4("The plot will change as you check the different values in the checkbox on the left side. 
              You can also change the plot by clicking the states under the 'region' on the right side of the plot."),
          h4("The x-axis is the states and the y-axis is the number of death from the residential fires in 
             total. Also, the plots are divided into 4 parts based on the years. In this way, it is more clear 
             for people to see that which state has the largest number of residential fires with fatalities and
             whether the number changes from 2012 to 2015. I also divide all the states into 4 regions: midwest, 
             northeast,south, west. The color represents different regions"),
          plotlyOutput("plot")),
          
          tabPanel("Table 1",
          h3("Table 1"),
          h4("The table is a summary for the residential fires with fatalities from year 2012 to 2015. The 
             column named 'Other' represents civilian calsualties, and 'Firefighter' column is the number
             of fire fighters who dead in the fire incidents. 'Total' is the total number of death in 
             that year"),
          tableOutput("table1"),
          h4("From the table we can see that the number of residential fires with fatalities actually increases
              during the 4 years. Also, in 2012 and 2015, there are firefighters who died in the incidents.")),
          
          tabPanel("Table 2",
        
          h3("Table 2"),
          h4("In table 2, I only present the first 15 rows from the original table 2 because there are more than
             6000 observations in the table. This table has the address, cause, and year for the residential fires."),
          
          tableOutput("table2")),
          
          
          tabPanel("Plot 2",
                   h4("The points on the plot represent the number of residential fires with fatalities.
                      The color of the points represents the causes for that fire incident. "),
                   
                   h3("Plot 2"),
                   plotOutput("plot2"))
      ))
   )
)
#########################################################################################################

server <- function(input, output) {
 
  output$table1 <- renderTable({
      casualtycount1 <- read.csv("casualtycount1.csv")
      table<- select(casualtycount1, OTH_DEATH, FF_DEATH, total, year)
      table1 <- table %>% group_by(year) %>% summarise(Other = sum(OTH_DEATH), Firefighter = sum(FF_DEATH), Total = sum(total))
      table1
  
  },  striped = TRUE, align = 'l'
    
  )
  
  
  addresstable <- read.csv("table2.csv")
  output$table2 <- renderTable({
        head(addresstable, 15)
  }, striped = TRUE, align = 'l')
     
###########################################################################################################  
  
  casualtycounts <- read.csv("casualtycounts.csv")
  datasetInput <- reactive({
    validate(
      need(input$years != "", "You need to select at least 1 year!!!")) 

                  if(input$years != 4){
                          dataset1 <- filter(casualtycounts, year == input$years)
                          return(dataset1)}
                  
                  else{return(casualtycounts)}
                  
     })     
 
   output$plot <- renderPlotly({
   g <- ggplot(datasetInput()) + geom_bar(aes(STATE, total, fill= region), stat="identity")+
        facet_grid(year ~ .)+ theme(plot.title = element_text(hjust = 0.5),
                                   axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
                                   ggtitle("Number of Residential Fires with Fatalities")
     g <- ggplotly(g)
     g
   })
   

   
   
      
   
   location <- read.csv("location.csv")
   location1 <- filter(location, lon != "NA", lat != "NA")    
   map<-get_map(location='united states', zoom=4, maptype = "terrain", source='google',
                api_key = "AIzaSyCSAQmkKjSMMdPJcYcHJ9smRDgUF1qut5g")
   
   sliderValues <- reactive({
     data1 <- sample_n(location1, input$obs, replace = TRUE)
     data1
   }) 
   
   
   
   output$plot2 <- renderPlot({
     g3 <- ggmap(map)+ geom_count(data = sliderValues(), aes(x=lon, y=lat,color=Cause))
     g3
   }, height = 600, width = 900 )
   
}

# Run the application 
shinyApp(ui = ui, server = server)

