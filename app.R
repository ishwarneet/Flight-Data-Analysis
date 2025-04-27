# Analyses of Flight Data

# Loading libraries
library(dplyr)
library(ggplot2)

# Loading the data
data_file_name = "./Airline Dataset.csv"
dataset <- read.csv(data_file_name,header=FALSE)

# Cleaning the data
colnames(dataset) <- dataset[1,]
dataset <- dataset[-1,]
colnames(dataset) <- gsub(" ","_",colnames(dataset))
# Removing na values
dataset <- na.omit(dataset)

#Formating the departure date column to required date format
dataset$Departure_Date <- gsub("-","/",dataset$Departure_Date)
dataset$Departure_Date <- as.Date(dataset$Departure_Date,"%m/%d/%Y")
dataset$Departure_Date <- format(dataset$Departure_Date,"%d/%m/%Y")


# Exploring the data
summary(dataset)
dim(dataset)


#Declaring the variables
typeOfAnalysis=c("Countries with most passengers",
                 "Countries with most flight departures",
                 "Busiest airports",
                 "Airports with most flights on time",
                 "Airports with most flights delayed or cancelled")

numberOfRecords = c(5,10,15,20)



#Creating Shiny App
ui <- fluidPage(
  titlePanel("Analysis of airline data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("typeData","Select the type of data to view:",choices = typeOfAnalysis),
      selectInput("recordsNumber","Select the number of records to view:",choices = numberOfRecords),
      dateInput("startDate","Select the start date: ",value = as.Date("01/01/2022","%d/%m/%Y"),format="dd/mm/yyyy"),
      dateInput("endDate","Select the end date: ",value = as.Date("31/10/2022","%d/%m/%Y"),format="dd/mm/yyyy")
    ),
    mainPanel(
      plotOutput("outputPlot"),
      textOutput("reactiveHeading")
    )
  )
)

server <- function(input,output){
  output$outputPlot <- renderPlot({
    start_date <- format(input$startDate,"%d/%m/%Y")
    end_date <- format(input$endDate,"%d/%m/%Y")
    numberOfRecordsVisible <- as.integer(input$recordsNumber)
    print(numberOfRecordsVisible)
    
    # When we want to view the nationality of most passengers
    # Filter for dates and then find the count of Nationality for each country
    if(input$typeData == typeOfAnalysis[1]){
      filtered <- dataset %>% filter(Departure_Date>=start_date & Departure_Date<=end_date) %>% 
        group_by(Nationality) %>% summarise(count = n()) %>% arrange(desc(count)) %>% head(numberOfRecordsVisible)
      
      ggplot(filtered,aes(x = Nationality, y = count)) + geom_col(fill = "blue")+
        labs(title = input$typeData, x = "Nationality", y = "Number of Passengers") +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
      
    }
    
    # Countries with most flight departures
    # filter for dates and then find the countries with highest number of flights
    else if(input$typeData == typeOfAnalysis[2]){
      filtered <- dataset %>% filter(Departure_Date>=start_date & Departure_Date<=end_date) %>% 
        group_by(Country_Name) %>% summarise(count = n()) %>% arrange(desc(count)) %>% head(numberOfRecordsVisible)
      
      ggplot(filtered,aes(x = Country_Name, y = count)) + geom_col(fill = "blue")+
        labs(title = input$typeData, x = "Country", y = "Number of Passengers") +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
    }
    
    # Busiest airport
    # Filter for date range, then group by airport name anf find the number of passengers for each airport
    else if(input$typeData == typeOfAnalysis[3]){
      filtered <- dataset %>% filter(Departure_Date>=start_date & Departure_Date<=end_date) %>% 
        group_by(Airport_Name) %>% summarise(count = n()) %>% arrange(desc(count)) %>% head(numberOfRecordsVisible)
      
      ordered <- arrange(filtered,desc(count))
      
      ggplot(filtered,aes(x = Airport_Name, y = count)) + geom_col(fill = "blue")+
        labs(title = input$typeData, x = "Airport", y = "Number of Flights") +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
    }
    
    # Airports with most flights on time
    # Filter based on date range, then find the count of flights for each country where the flight status is on time
    else if(input$typeData == typeOfAnalysis[4]){
      filtered <- dataset %>% filter(Departure_Date>=start_date & Departure_Date<=end_date) %>% 
        filter(Flight_Status == "On Time") %>% 
        group_by(Airport_Name) %>% summarise(count = n()) %>% arrange(desc(count)) %>% head(numberOfRecordsVisible)
      
      ggplot(filtered,aes(x = Airport_Name, y = count)) + geom_col(fill = "blue")+
        labs(title = input$typeData, x = "Airport", y = "Number of Flights") +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
    }
    
    # Airports with most flights delayed or cancelled
    # Filter based on dates, and then find the count of flights for each country which are delayed or cancelled
    else if(input$typeData == typeOfAnalysis[5]){
      filtered <- dataset %>% filter(Departure_Date>=start_date & Departure_Date<=end_date) %>% 
        filter(Flight_Status == "Delayed" | Flight_Status == "Cancelled") %>% 
        group_by(Airport_Name) %>% summarise(count = n()) %>% arrange(desc(count)) %>% head(numberOfRecordsVisible)
      
      ggplot(filtered,aes(x = Airport_Name, y = count)) + geom_col(fill = "blue")+
        labs(title = input$typeData, x = "Airport", y = "Number of Flights") +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
    }
  })
  
}

shinyApp(ui = ui, server = server)

