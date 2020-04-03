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
download.file("https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv","C:/corona/Corona/data_file.csv",method="wininet")

confirmed<-fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

## Convert from wide format to long format

confirmed_long<-gather(confirmed,date,confirmed_cases,colnames(confirmed)[5:ncol(confirmed)],-"Province/State")

## Replace "Country/Region" by country 
colnames(confirmed_long)[2]<-"Country"

## Process Dates
confirmed_long$date<-as.Date(confirmed_long$date,format="%m/%d/%y")
confirmed_long$date<-as.POSIXct(confirmed_long$date,format="%m/%d/%y")

## Get country list for X-axis
country_list<-sort(unique(confirmed_long$Country))

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
                        choices = c("",country_list),
                        selected = "All Countries"),
            hr(style = "border-color: #18BC9C;")
            
        ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("CountryPlot",click = "plot_click")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$CountryPlot <- renderPlot(width=1000,height=800,{
        # generate bins based on input$bins from ui.R
        selected_country<-confirmed_long %>% group_by(date) %>% group_by("Country") %>% filter(Country==input$country) 
        
        
        ggplot(selected_country,mapping=aes(selected_country$date,selected_country$confirmed_cases,xlab(input$country)))+
            geom_point()+
            geom_line()+
            geom_density()+
            xlab(input$country)+
            ylab("Confirmed cases")+
            theme_bw()+
            theme(axis.text.x =element_text(size=10,angle = 90),
                  axis.text.y =element_text(size=20),
                  axis.title.x = element_text(size=20),
                  axis.title.y = element_text(size=20))+
            
            scale_x_datetime(breaks = "1 day")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
