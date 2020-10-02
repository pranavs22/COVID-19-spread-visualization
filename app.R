#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Author: Pranav Sahasrabudhe
library(shiny)
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(readr)
library(shinycssloaders)
library(scales)
library(httr)
library(plotly)

###################################################
## Read in the data
###################################################
confirmed<-fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
recovered<-fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
fatalities<-fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

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


###################################################
## Shiny code starts here-
###################################################
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
                        selected = "US"),
            hr(style = "border-color: #18BC9C;"),
            actionButton("submit","Submit"),
        ),
        mainPanel(
            tabsetPanel(id="tab",
                        tabPanel("Confirmed",withSpinner(ui_element = plotlyOutput("confirmed_graph"),type = 6)),
                        tabPanel("Recovered",withSpinner(ui_element = plotlyOutput("recovered_graph"),type = 6)),
                        tabPanel("Fatalities",withSpinner(ui_element = plotlyOutput("fatalities_graph"),type = 6))
           
        )
    )
))

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    plotData1<-eventReactive(input$submit,{

        selected_country<-confirmed %>% group_by(date) %>% group_by("Country") %>% filter(Country==input$country)
        if (input$tab=="Confirmed"){
            p<-ggplot(selected_country,mapping=aes(selected_country$date,selected_country$confirmed_cases))+
                    geom_point()+
                    xlab(input$country)+
                    ylab("Confirmed cases")+
                    theme_bw()
            p<-ggplotly(p)
            p
            }
        })
        plotData2<-eventReactive(input$submit,{
            if (input$tab=="Recovered"){
                    # req(is.null(plotData2()))
                    selected_country<-recovered %>% group_by(date) %>% group_by("Country") %>% filter(Country==input$country)
                    p<-ggplot(selected_country,mapping=aes(selected_country$date,selected_country$confirmed_cases))+
                        geom_point()+
                        xlab(input$country)+
                        ylab("Recovered cases")+
                        theme_bw()
                    p<-ggplotly(p)
                    p
            }
        })
                    
    plotData3<-eventReactive(input$submit,{
        if (input$tab=="Fatalities"){
            selected_country<-fatalities %>% group_by(date) %>% group_by("Country") %>% filter(Country==input$country) 
            p<-ggplot(selected_country,mapping=aes(selected_country$date,selected_country$confirmed_cases,))+
                geom_point()+
                xlab(input$country)+
                ylab("Fatalities")+
                theme_bw()
            p<-ggplotly(p)
            p
        }
    })

    
    output$confirmed_graph<-renderPlotly({plotData1()})
    output$recovered_graph<-renderPlotly({plotData2()})
    output$fatalities_graph<-renderPlotly({plotData3()})
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
