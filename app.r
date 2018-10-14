library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(stringi)
library(lubridate)
library(plotly)
library(RColorBrewer)

bank_data <- read_csv("Final_data.csv")
bank_data$`Date received` <- as.Date(bank_data$`Date received`, "%m/%d/%y")
bank_data$Company = trimws(bank_data$Company)
bank_data$Product = trimws(bank_data$Product)
bank_data$region <-stri_trans_totitle(abbr2state(bank_data$State) )
bank_data$Sentiment<-rnorm(nrow(bank_data), mean=-0.5, sd=0.5, min=-1, max=0)

samp <- rnorm(1000000, -0.5, 0.25)
samp <- samp[samp >= -1 & samp <= 0]
samp<-as.data.frame(samp)
bank_data$Sentiment<-samp[1:272870,1]

bank_data$classification<-sample(4, size = nrow(bank_data), replace = TRUE)

for (i in 1:nrow(bank_data)){
if(bank_data$classification[i]==1){
  bank_data$classification[i]<-"Fraud"
} else if(bank_data$classification[i]==2) {
  bank_data$classification[i]<-"Policy"
} else if(bank_data$classification[i]==3) {
  bank_data$classification[i]<-"Fee"
}else if(bank_data$classification[i]==4) {bank_data$classification[i]<-"Unauthorized"}
}

source("US_heatmap.r")
source("US_heatmap_sentiment.r")
source("abbr2state.r")
source("coordinateToState.r")
source("wordcloud_diag.r")
# Define UI for application that draws a histogram
ui <- navbarPage("Complaints Dashboard",
                 tabPanel("Summary",
                          # Sidebar with a slider input for number of bins 
                          sidebarLayout(
                            sidebarPanel(
                              dateRangeInput("date", label = "Date", start = min(bank_data$`Date received`),
                                             end = max(bank_data$`Date received`)),
                              selectInput("product", label = "Product", choices = sort(unique(bank_data$Product)), multiple=TRUE
                              ),
                              selectInput("banks", label = "Banks", # selectize = TRUE, 
                                          choices = sort(unique(bank_data$Company)), multiple=TRUE
                              ) 
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(fluidRow(plotOutput("map",
                                                          click = "plot_click")),
                                      fluidRow(column(width = 6),
                                        column(width = 6,selectInput("banksForTime", label = "Select bank from left graph:", # selectize = TRUE, 
                                                           choices = sort(unique(bank_data$Company))))),
                                      fluidRow(
                                        column(width = 6, 
                                               plotlyOutput("top5")
                                        ),
                                        column(width = 6, 
                                               plotlyOutput("BankTimeLine")
                                        )
                                      )
                                      
                            )
                          )
                 ),
                 tabPanel("Sentiment Analysis",
                          # Sidebar with a slider input for number of bins 
                          sidebarLayout(
                            sidebarPanel(
                              dateRangeInput("S.date", label = "Date", start = min(bank_data$`Date received`),
                                             end = max(bank_data$`Date received`)),
                              selectInput("S.product", label = "Product", choices = sort(unique(bank_data$Product)), multiple=TRUE
                              ),
                              selectInput("S.banks", label = "Banks", # selectize = TRUE, 
                                          choices = sort(unique(bank_data$Company)), multiple=TRUE
                              ) 
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(fluidRow(plotOutput("mapSentiment",
                                                          click = "S.plot_click")),
                                      fluidRow(column(width = 6),
                                               column(width = 6,selectInput("S.banksForTime", label = "Select Bank From Left Graph:",  
                                                                            choices = sort(unique(bank_data$Company))))),
                                      fluidRow(
                                        column(width = 6, 
                                               plotlyOutput("S_top5")),
                                         column(width = 6,plotOutput("Wordcloud_plot"))
                                        ),
                                      fluidRow(),
                                      fluidRow(column(width = 12,plotOutput("Sentiment_density")))
                            ))
                          )
                 )
# Define server logic required to draw a histogram
server <- function(input, output) {
##Filter data tab1
  bank_filt <- reactive({
    filt_data <- bank_data %>%
      filter(Product %in% input$product 
             & Company %in% input$banks
             & `Date received`>= input$date[1]
             & `Date received`<=input$date[2]
      )
    return(filt_data)
  })
#map output   
  output$map <- renderPlot({
    US_heatmap(bank_filt())
  }) 
  
  State_val <- reactive({
    testPoints <- data.frame(x = input$plot_click$x, y = input$plot_click$y)
    states <- coordinateToState(testPoints)
    
    vals2 <- bank_filt()%>%
      filter(toupper(region) == toupper(states))
    return(vals2)
  })  
#Top5 banks with most complaints  
  output$top5 <- renderPlotly({
    validate(
      need(!is.null(State_val()) , "Please select data in map")
    )
    top5_banks <- State_val() %>% 
      select(classification,Company) %>% 
      group_by(classification,Company) %>% 
      summarize( complaints = n()) %>%
      arrange(desc(complaints)) %>% 
      mutate(rank = row_number()) %>% 
      filter(rank<=5) %>% 
      select(Company,classification, complaints)
    
    x <- list(
      title = "Banks"
    )
    
    y <- list(
      title = "Number of Complaints"
    )
    p <- plot_ly(top5_banks, x=~top5_banks$Company, y=~top5_banks$complaints, type = 'bar', color  = ~top5_banks$classification) %>% 
      layout(xaxis=x,
             yaxis=y,
             title = "Top 5 Banks by Complaints",
             dragmode =  "select",
             barmode = "stack")
    p
  })  
#select bank's month over month number of complaints  
  output$BankTimeLine <- renderPlotly({
    validate(
      need(!is.null(State_val()) , "Please select data in map")
    )
    bank_time <- State_val()%>%filter(Company==input$banksForTime)%>% mutate(month=floor_date(`Date received`, "month"))%>%
      select(Product, month) %>% 
      group_by(Product,month) %>% 
      summarize( complaints = n())
    
    x <- list(
      title = "Months"
    )
    
    y <- list(
      title = "Number of Complaints"
    )
    line_plot <- plot_ly(bank_time, x= ~bank_time$month, y = ~bank_time$complaints,
                           mode = 'lines', mode = 'lines+markers', type = 'scatter',
                           color  = ~bank_time$Product)%>%
        layout(xaxis=x,
             yaxis=y,
             title = "Complaints by Product",
             dragmode =  "select")
    line_plot
 })
##  
  bank_filt_S <- reactive({
    filt_data <- bank_data %>%
      filter(Product %in% input$S.product 
             & Company %in% input$S.banks
             & `Date received`>= input$S.date[1]
             & `Date received`<=input$S.date[2]
      )
    return(filt_data)
  })
  
  output$mapSentiment <- renderPlot({
    US_heatmap_sentiment(bank_filt_S())
  })
  
  State_val_S <- reactive({
    S.testPoints <- data.frame(x = input$S.plot_click$x, y = input$S.plot_click$y)
    S.states <- coordinateToState(S.testPoints)
    S.vals2 <- bank_filt_S()%>%
      filter(toupper(region) == toupper(S.states))
    return(S.vals2)
  })
  
  output$S_top5 <- renderPlotly({
    S_top5_banks <- State_val_S() %>% 
      select(Company,classification, Sentiment) %>% 
      group_by(classification,Company) %>% 
      summarize( complaints = sum(Sentiment)) %>%
                   arrange(desc(complaints)) %>% 
                   mutate(rank = row_number()) %>% 
                   filter(rank<=5) %>% 
                   select(Company,classification, complaints)
                 
                 xname <- list(title = "Banks")
                 
                 yname <- list(title = "Sentiment distribution")
                 
                 p <- plot_ly(S_top5_banks, x=~S_top5_banks$Company, y=~S_top5_banks$complaints, color  = ~S_top5_banks$classification, type = 'bar') %>% 
                   layout(xaxis=xname,
                          yaxis=yname,
                          title = "Top 5 Banks by Sentiments",
                          dragmode =  "select",
                          barmode = "stack")
                 p
  })


 output$Wordcloud_plot <- renderPlot({
    word_Cloud <- State_val_S()%>%filter(Company==input$S.banksForTime)%>%
     select(`Consumer complaint narrative`) %>%
      na.omit() %>%
      as.data.frame() 
    names(word_Cloud) <- "text"
    plotcloud<-wordcloud_diag(word_Cloud)
    plotcloud
})
 
output$Sentiment_density <- renderPlot({
   sentiment_Den <- State_val_S()%>%filter(Company==input$S.banksForTime)%>%
     select(Sentiment)%>%
     na.omit() %>%
     as.data.frame() 
   plotdensity<- ggplot(sentiment_Den, aes(Sentiment)) + geom_density(alpha = 0.2)+
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
     panel.background = element_blank(), axis.line = element_line(colour = "white"),
     plot.title = element_text(size=14, face="bold"))+
     ggtitle("Sentiment Distribution")
   plotdensity
})
}

# Run the application 
shinyApp(ui = ui, server = server)

