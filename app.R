#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(readr)
library(scales)
library(httr)
## Read in the data

# confirmed<-fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
confirmed<-fread("C:/corona/time_series_2019-ncov-Confirmed.csv")
# recovered<-fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
# fatalities<-fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

# recovered<-fread("C:/corona/time_series_2019-ncov-Confirmed.csv")
# fatalities<-fread("C:/corona/time_series_2019-ncov-Confirmed.csv")
preprocess<-function(input_file){
    ## Convert from wide format to long format
    
    input_file_long<-gather(input_file,date,confirmed_cases,colnames(input_file)[5:ncol(input_file)],-"Province/State")
    
    ## Replace "Country/Region" by country 
    colnames(input_file_long)[2]<-"Country"
    
    ## Process Dates
    input_file_long$date<-as.Date(input_file_long$date,format="%m/%d/%y")
    input_file_long$date<-as.POSIXct(input_file_long$date,format="%m/%d/%y")
    ## Get country list for X-axis
    
    return(input_file_long)    
}
confirmed<-preprocess(confirmed)
confirmed_country_list<-sort(unique(confirmed$Country))

recovered<-preprocess(recovered)
recovered_country_list<-sort(unique(recovered$Country))

fatalities<-preprocess(fatalities)
fatalities_country_list<-sort(unique(fatalities$Country))

# India<-confirmed_long %>% group_by(date) %>% group_by("Country") %>% filter(Country=="India") 


# ggplot(India,mapping=aes(India$date,India$confirmed_cases))+
#     geom_point()+
#     theme(text=element_text(size=10),axis.text.x=element_text(angle = 90,hjust=1))+
#           xlab("India")+
#              ylab("cases")+
#     theme_bw()+
#         scale_x_datetime(breaks = "1 day")
# dev.off()
# Define UI for application that draws a histogram


## Shiny code starts here-

ui <- fluidPage(

    # Application title
    titlePanel("Coronavirus cases by Country"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #Country List UI
            helpText("1. Choose Country", style ="color:#2C3F51;font-weight: bold;padding-bottom:13px;"),
            selectInput("country",
                        label = NULL,
                        choices = c("",confirmed_country_list),
                        selected = "All Countries"),
            hr(style = "border-color: #18BC9C;")
            
        ),
        mainPanel(
            tabsetPanel(id="tab",
                         tabPanel("Confirmed",plotOutput("confirmed_graph")),
                         tabPanel("Recovered",plotOutput("recovered_graph")),
                         tabPanel("Fatalities",plotOutput("fatalities_graph"))
           
        )
    )
))

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    plotData1=reactiveVal()
    plotData2=reactiveVal()
    plotData3=reactiveVal()
    
    observeEvent(input$country,print(input$country))
    observe({

        updateSelectInput(session,"Country")    
        # if (input$tab=="Confirmed"){
        #     req(is.null(plotData1()))
                selected_country<-confirmed %>% group_by(date) %>% group_by("Country") %>% filter(Country==input$country) 
                plotData1(ggplot(selected_country,mapping=aes(selected_country$date,selected_country$confirmed_cases,xlab(input$country)))+
                    geom_point()+
                    geom_line()+
                    xlab(input$country)+
                    ylab("Confirmed cases")+
                    theme_bw()+
                    theme(axis.text.x =element_text(size=10,angle = 90),
                          axis.text.y =element_text(size=20),
                          axis.title.x = element_text(size=20),
                          axis.title.y = element_text(size=20))+
                    
                    scale_x_datetime(breaks = "1 day"))
        # } 
        
         if (input$tab=="Recovered"){
            req(is.null(plotData2()))
            selected_country<-recovered %>% group_by(date) %>% group_by("Country") %>% filter(Country==input$country) 
            plotData2(ggplot(selected_country,mapping=aes(selected_country$date,selected_country$confirmed_cases,xlab(input$country)))+
                geom_point()+
                geom_line()+
                xlab(input$country)+
                ylab("Recovered cases")+
                theme_bw()+
                theme(axis.text.x =element_text(size=10,angle = 90),
                      axis.text.y =element_text(size=20),
                      axis.title.x = element_text(size=20),
                      axis.title.y = element_text(size=20))+
                scale_x_datetime(breaks = "1 day"))

        } else if (input$tab=="Fatalities"){    
            req(is.null(plotData3()))
            selected_country<-fatalities %>% group_by(date) %>% group_by("Country") %>% filter(Country==input$country) 
            
            
            plotData3(ggplot(selected_country,mapping=aes(selected_country$date,selected_country$confirmed_cases,xlab(input$country)))+
                geom_point()+
                geom_line()+
                xlab(input$country)+
                ylab("Fatalities")+
                theme_bw()+
                theme(axis.text.x =element_text(size=10,angle = 90),
                      axis.text.y =element_text(size=20),
                      axis.title.x = element_text(size=20),
                      axis.title.y = element_text(size=20))+
                
                scale_x_datetime(breaks = "1 day"))
        }
        
    })
    output$confirmed_graph=renderPlot({plot(plotData1())})
    output$recovered_graph=renderPlot({plot(plotData2())})
    output$fatalities_graph=renderPlot({plot(plotData3())})
}

# Run the application 
shinyApp(ui = ui, server = server)
