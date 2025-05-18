#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(clock)
library(curl)
library(rdrop2)
library(jsonlite)
source("utils/ura_data_utils.R")
source("utils/token_utils.R")

dir.create("data")

start_year <- 2018
start_month <- 1

end_year <- Sys.time() %>% get_year()
end_month <- Sys.time() %>% get_month() -1
#if today is January, revert back to december year before
if(end_month == 0){
  end_year <- end_year - 1
  end_month <- 12
}


dtoken <- readRDS("droptoken.rds")
dtoken$refresh()

condos <- get_condo_list(dtoken)

## Pre-checks
ura_update_check <- function(x = Sys.time()){
  if(get_day(x) > 16){
    return(TRUE)
  } else if(get_day(x) >= 15 & !(weekdays(x) %in% c("Saturday", "Sunday"))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
if(ura_update_check()){
  run_update <- TRUE
} else {
  run_update <- FALSE
}

# only keep records of one month (previous month)
retrieve_last_update_file(dtoken)
last_update <- read_csv("last_update.csv")
last_y <- last_update$y[1]
last_m <- last_update$m[1]
last_date <- year_month_day(last_y, last_m, 1)
update_date <- add_months(last_date, 1)
update_y <- update_date %>% get_year()
update_m <- update_date %>% get_month()
while(year_month_day(end_year, end_month, 1) >= update_date){
  if(year_month_day(end_year, end_month, 1) > update_date || run_update){
    update_ura_data(update_y, update_m, dtoken)
    update_date <- add_months(update_date, 1)
    update_y <- update_date %>% get_year()
    update_m <- update_date %>% get_month()
  } else {
    break
  }
}
# update last_update file
drop_path <- "ura_rental/data"
last_update_date <- add_months(update_date, -1)
last_update$y[1] <- last_update_date %>% get_year()
last_update$m[1] <- last_update_date %>% get_month()
write_csv(last_update, "last_update.csv")
drop_upload("last_update.csv", "ura_rental/data/", dtoken=dtoken, mode = "overwrite")



# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

    # Application title
    titlePanel("Trends: Rental Prices"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectizeInput("project",
                         label = "select a condominium",
                         choices = condos,
                         multiple = F,
                         options = list(maxItems = 1, placeholder = 'Select a project'),
                         selected = "THE SAIL @ MARINA BAY"),
          flowLayout(
            selectInput("ystart",
                        label = "start year",
                        choices = seq(start_year, end_year),
                        multiple = F,
                       # options = list(placeholder = "Select a starting date (year)"),
                        selected = "2022"),
            selectInput("mstart",
                        label = "start month",
                        choices = seq(1,12),
                        multiple = F,
                        #options = list(placeholder = "Select an starting date (month)"),                        
                        selected = 6)
          ),
          flowLayout(
            selectInput("yend",
                        label = "end year",
                        choices = seq(start_year, end_year),
                        multiple = F,
                        #options = list(placeholder = "Select an ending date (year)"),
                        selected = "2023"),
            selectInput("mend",
                        label = "end month",
                        choices = seq(1,12),
                        multiple = F,
                        #options = list(placeholder = "Select an ending date (month)"),                        
                        selected = 2)
          ),
          radioButtons("rentformat",
                       label = "rental price format",
                       choices = list("rent per month" = "rent",
                                      "rent per square feet" = "rent_psf"),
                       selected = "rent"),
          radioButtons("timescale",
                       label = "Time scale",
                       choices = list("Quarterly" = "quarter",
                                      "monthly" = "monthly"),
                       selected = "quarter")
          ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("rentalPlot")
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  get_df <- reactive({
    shiny::validate(
      need(year_month_day(as.integer(input$ystart), as.integer(input$mstart)) <= year_month_day(as.integer(input$yend), as.integer(input$mend)),
           'starting date must be <= ending date')
    )
    ystart <- as.integer(input$ystart)
    yend <- as.integer(input$yend)
    mstart <- as.integer(input$mstart)
    mend <- as.integer(input$mend)
    get_rental_data(ystart, yend, mstart, mend, dtoken = dtoken)
  })
  output$rentalPlot <- renderPlotly({
    shiny::validate(
      need(input$project, 'please select a condo')
      )
    all_df <- get_df() %>% rename("monthly" = "leaseDate")
    rentformat <- input$rentformat
    timescale <- input$timescale
    p <- ggplot(all_df %>%
                  filter(project == input$project),
                aes(x = .data[[timescale]],
                    y = .data[[rentformat]])) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(aes(color = noOfBedRoom), group = 1, width = .1, height = 0) +
      scale_color_brewer("# bedrooms", palette = "Set1") +
      labs(y = rentformat,
           x = "Lease Date") +
      ggtitle(input$project) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = 1))
    p})
  }
  )

# Run the application 
shinyApp(ui = ui, server = server)

