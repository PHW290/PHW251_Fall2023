#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shiny)
library(dplyr)
library(readr)
library(shiny)
library(lubridate)
library(data.table)
library(plotly)
library(htmltools)
library(DT)
library(purrr)


left_col_attr <- list(`Recent Cases` = c("today_cases_rep", "today_cases_rep_chart"),
                      `Confirmed Cases` = c("cases_epdate", "cases_epdate_chart"),
                      `Cases per 100k 7 day avg` = c("recent_cases_ep_rate", "recent_cases_ep_rate_chart"),
                      `Cases per 100k Cumulative` = c("cases_epdate_rate", "cases_epdate_rate_chart"),
                      `Recent Deaths` = c("today_deaths_rep", "today_deaths_rep_chart"),
                      `Confirmed Deaths` = c("deaths_DOD", "deaths_DOD_chart"),
                      `Deaths per 100k 7 day avg` = c("recent_deaths_dd_rate", "recent_deaths_dd_rate_chart"),
                      `Deaths per 100k Cumulatve` = c("deaths_DOD_rate", "deaths_DOD_rate_chart")
)

ui <- fluidPage(theme = shinytheme("united"),
                
                navbarPage("California Covid-19 Dashboard",
                           tabPanel("Cases",
                                    # fluidRow(radioGroupButtons(
                                    #     inputId = "source",
                                    #     label = "Choose Data",
                                    #     choices = c("Cases", "Testing", "Demographics"),
                                    #     individual = TRUE,
                                    #     checkIcon = list(
                                    #         yes = tags$i(class = "fa fa-circle", 
                                    #                      style = "color: steelblue"),
                                    #         no = tags$i(class = "fa fa-circle-o", 
                                    #                     style = "color: steelblue"))
                                    # )
                                    #),
                                    column(3,
                                           wellPanel(
                                             selectInput("select_metric", label = "Select metric", choices = names(left_col_attr), selected = "Recent Cases"),
                                             sliderInput("daterange",
                                                         "Epi curves date range:",
                                                         min = as.Date("2020-01-01","%Y-%m-%d"),
                                                         max = as.Date("2022-12-01","%Y-%m-%d"),
                                                         value=c(as.Date("2020-01-01"), as.Date("2022-12-01")),
                                                         timeFormat="%b %Y"),
                                             dataTableOutput("county_table"), style = "height:1000px; overflow-y: scroll;overflow-x: scroll;")
                                    ),
                                    column(9,
                                           wellPanel(
                                             uiOutput("stats_box")
                                           ),
                                           wellPanel(uiOutput("plot_title"),
                                                     plotlyOutput(outputId = "cases_plot", height = "300px")
                                           ),
                                           wellPanel(uiOutput("second_title"),
                                                     plotlyOutput(outputId = "deaths_plot", height = "300px")
                                           )
                                           
                                    ) #end column
                           ), #end tabPanel
                           tabPanel("Testing",
                                    fluidRow(h3("Still working on it", style = "color: green; font-weight:bold;" ))
                           ),
                           tabPanel("Demographics",
                                    fluidRow(h3("Still working on it", style = "color: orange; font-weight:bold;"))
                           )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  '^M.*csv'
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  cases_deaths <- read_csv("https://data.chhs.ca.gov/dataset/f333528b-4d38-4814-bebb-12db1f10f535/resource/046cdd2b-31e5-4d34-9ed3-b48cdbc4be7a/download/covid19cases_test.csv") %>%
    select(date, county = area, dof_pop = population, cases_epdate = cases, deaths_DOD = deaths, cases_repdate = reported_cases, deaths_repdate = reported_deaths) %>%
    filter(county %!in% c("California", "Out of state", "Unknown"))
  
  
  us_stats <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv?_sm_au_=iVVWPQ545LD7stpRGqtBJK3R1GQBC") %>%
    filter(date == max(as_date(date)))
  
  tDatDOF  <- cases_deaths %>% 
    distinct(county, dof_pop) 
  
  max_date <- max(as_date(cases_deaths$date), na.rm = T)
  
  county_alpha <- select(tDatDOF, county) %>% arrange(county) %>% pull()
  
  unit.scale <- function(x) round((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)), 6)
  
  chart_left <- function(bar_color = "lightblue", prop_value) {
    paste0("<span style=\"display: inline-block; direction: rtl; unicode-bidi: plaintext; border-radius: 4px; padding-right: 2px; font-size: 0px; background-color: lightblue; width: ", unit.scale(prop_value) * 100, "%\">", prop_value, "</span>")
  }
  
  rep_cases_deaths <- cases_deaths %>%
    mutate(recent_cases_ep = ifelse(date > as_date(max_date) - 14 & date <= as_date(max_date) - 7, cases_epdate / 7, 0),
           recent_deaths_dd = ifelse(date > as_date(max_date) - 28 & date <= as_date(max_date) - 7, deaths_DOD / 21, 0),
           today_cases_rep = ifelse(date == max_date, cases_repdate, 0),
           today_deaths_rep = ifelse(date == max_date, deaths_repdate, 0)
    ) %>%
    group_by(county) %>%
    summarize(across(where(is.numeric), ~sum(., na.rm = T))) %>%
    mutate(across(c(cases_epdate, deaths_DOD, recent_cases_ep, recent_deaths_dd), ~round((.x /  dof_pop) * 100000, 1), .names = "{.col}_rate"),
           across(!!unname(unlist(map(left_col_attr, 1))), 
                  ~chart_left(prop_value = round(.x, 0)), .names = "{.col}_chart")
    )
  
  output$county_table <- DT::renderDataTable({
    
    # Setting the table 
    rep_cases_deaths_tbl <- rep_cases_deaths %>%
      select(county, left_col_attr[[input$select_metric]][1])
    
    DT::datatable(data = rep_cases_deaths_tbl, selection=list(mode="single", target="row"), rownames = T,
                  options = list(dom = "t", 
                                 paging = FALSE,
                                 order = list(list(2, 'desc')),
                                 columnDefs = list(list(visible=FALSE, targets=c(0))),
                                 headerCallback = JS(
                                   "function(thead, data, start, end, display){",
                                   "  $(thead).remove();",
                                   "}")
                  )) %>%
      formatStyle(left_col_attr[[input$select_metric]][1],
                  background = styleColorBar(rep_cases_deaths[[left_col_attr[[input$select_metric]][1]]], 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')
    
  }, server = TRUE)
  
  
  selected_county <- reactive({
    county_alpha[input$county_table_rows_selected]
  })
  
  selected_county_text <- reactive({
    if (length(input$county_table_rows_selected) != 0){
      paste0(county_alpha[input$county_table_rows_selected], " county")
    } else {
      "Statewide"
    }
  })
  
  summary_data <- reactive({
    
    if (length(input$county_table_rows_selected) == 1){
      rep_cases_deaths %>%
        filter(county == county_alpha[input$county_table_rows_selected]) %>%
        select(cases = cases_epdate, deaths = deaths_DOD, cases_repdate, deaths_repdate, recent_cases_ep, recent_deaths_dd, recent_cases_ep_rate, recent_deaths_dd_rate) 
      # mutate(recent_cases_ep = ifelse(date > as_date(max_date) - 14 & date <= as_date(max_date) - 7, cases_epdate / 7, 0),
      #        recent_deaths_dd = ifelse(date > as_date(max_date) - 14 & date <= as_date(max_date) - 7, deaths_DOD / 7, 0),
      #        today_cases = ifelse(date == max_date, cases_repdate, 0),
      #        today_deaths = ifelse(date == max_date, deaths_repdate, 0)
      # )
    } else {
      rep_cases_deaths %>%
        summarize(across(c(cases_epdate, deaths_DOD, today_cases_rep, today_deaths_rep, recent_cases_ep, recent_deaths_dd, dof_pop), ~sum(.x, na.rm = T) )) %>%
        select(cases = cases_epdate, deaths = deaths_DOD, today_cases_rep, today_deaths_rep, recent_cases_ep, recent_deaths_dd, dof_pop) %>%
        mutate(across(c(recent_cases_ep, recent_deaths_dd), ~round((.x /  dof_pop) * 100000, 1), .names = "{.col}_rate"))
    }
  })
  
  plot_data <- reactive({
    
    if (length(input$county_table_rows_selected) == 1){
      cases_deaths %>%
        filter(county == county_alpha[input$county_table_rows_selected] & between(date, as_date(input$daterange[[1]]), as_date(input$daterange[[2]]))) %>%
        select(date, cases = cases_epdate, deaths = deaths_DOD) %>%
        mutate(roll_mean_cases = frollmean(cases, 7), roll_mean_deaths = frollmean(deaths, 7)) %>%
        filter(date >= as_date("2020-03-01"))
    } else {
      cases_deaths %>%
        filter(between(date, as_date(input$daterange[[1]]), as_date(input$daterange[[2]]))) %>%
        group_by(date) %>%
        summarize(cases = sum(cases_epdate), deaths = sum(deaths_DOD)) %>%
        mutate(roll_mean_cases = frollmean(cases, 7), roll_mean_deaths = frollmean(deaths, 7)) %>%
        filter(date >= as_date("2020-03-01"))
    }
  })
  # alpha <- c(petunia = "today_cases_rep")
  # select(rep_cases_deaths, county, today_cases_rep) %>%
  #     arrange(desc(.data[[alpha[["petunia"]]]]))
  output$plot_title <- renderUI({
    
    div(h4("Cases by Episode Date"),
        h5(selected_county_text())
    )
    
  })
  
  output$second_title <- renderUI({
    
    div(h4("Deaths by Date of Death"),
        h5(selected_county_text())
    )
    
  })
  
  #selected_county_text <- function() {"Statewide"}
  case_style <- c(bold = "color: #58ABCC; font-weight:bold;", notbold = "color: #58ABCC;")
  death_style <- c(bold = "color: #8A8D8E; font-weight:bold;", notbold = "color: #8A8D8E;")
  
  output$stats_box <- renderUI({
    
    case_change <- round((sum(summary_data()$recent_cases_ep, na.rm = T) / (sum(summary_data()$cases, na.rm = T) - sum(summary_data()$recent_cases_ep, na.rm = T)) * 100), 1)
    death_change <- round((sum(summary_data()$recent_deaths_dd, na.rm = T) / (sum(summary_data()$deaths, na.rm = T) - sum(summary_data()$recent_deaths_dd, na.rm = T)) * 100), 1)
    
    stats <- c(tot_cases = formatC(sum(summary_data()$cases, na.rm = T), format="f", big.mark=",", digits=0),
               today_cases = paste0(formatC(sum(summary_data()$recent_cases_ep, na.rm = T)), " (", ifelse(case_change >= 0, "+", ""), case_change, "%)"),
               case_rate = summary_data()$recent_cases_ep_rate,
               us_cases = formatC(us_stats$cases, format="f", big.mark=",", digits=0),
               tot_deaths = formatC(sum(summary_data()$deaths, na.rm = T), format="f", big.mark=",", digits=0),
               today_deaths = paste0(formatC(sum(summary_data()$recent_deaths_dd, na.rm = T)), " (", ifelse(death_change >= 0, "+", ""), death_change, "%)"),
               death_rate = summary_data()$recent_deaths_dd_rate,
               us_deaths = formatC(us_stats$deaths, format="f", big.mark=",", digits=0)
    )
    
    
    
    tagList(
      fluidRow(
        column(6,
               h5(paste0("Cases (", selected_county_text(), ")"), style = case_style[["bold"]]),
               hr(style = "color: #1F2121; font-weight:bold;"),
               column(6,
                      div(
                        h2(stats[["tot_cases"]], style = case_style[["bold"]]),
                        h5("Total Confirmed Cases", style = case_style[["notbold"]]),
                        br(),
                        h3(stats[["today_cases"]], style = case_style[["bold"]]),
                        h5(paste0("Average per day between ", max_date - 7, " and ", max_date - 14), style = case_style[["notbold"]])
                      )
               ),
               column(6,
                      div(
                        h2(stats[["case_rate"]], style = case_style[["bold"]]),
                        h5(" Recent cases per 100k (7 day average)", style = case_style[["notbold"]]),
                        br(),
                        h4(stats[["us_cases"]], style = case_style[["notbold"]]),
                        h5("US Total Cases", style = case_style[["notbold"]])
                      )
               )
        ),
        column(6,
               h5(paste0("Deaths (", selected_county_text(), ")"), style = death_style[["bold"]]),
               hr(style = "color: #1F2121; font-weight:bold;"),
               column(6,
                      div(
                        h2(stats[["tot_deaths"]], style = death_style[["bold"]]),
                        h5("Total Confirmed Deaths", style = death_style[["notbold"]]),
                        br(),
                        h3(stats[["today_deaths"]], style = death_style[["bold"]]),
                        h5(paste0("Average per day between ", max_date - 7, " and ", max_date - 28), style = death_style[["notbold"]])
                      )
               ),
               column(6,
                      div(
                        h2(stats[["death_rate"]], style = death_style[["bold"]]),
                        h5("Recent deaths per 100k (7-day average)", style = death_style[["notbold"]]),
                        br(),
                        h4(stats[["us_deaths"]], style = death_style[["notbold"]]),
                        h5("US Total Deaths", style = death_style[["notbold"]])
                      )
               )
        )
      )# end fluidRow
    ) # end tagList
    
    
    
  })
  
  output$cases_plot<- renderPlotly({
    
    fig <- plot_ly(data = plot_data()) %>%
      add_trace(x = ~date, y = ~cases, type = 'bar', name = 'Cases per day',
                marker = list(color = 'rgb(187, 216, 228)'),
                hoverinfo = "text",
                text = ~paste(cases, ' cases')) %>%
      add_trace(x = ~date, y = ~roll_mean_cases, type = 'scatter', mode = 'lines', name = 'Average Cases', 
                line = list(color = '#345B6B'),
                hoverinfo = "text",
                text = ~paste(roll_mean_cases, ' average cases')
      ) %>% 
      layout(legend = list(x = 0.1, y = 0.9))
    
    
  })
  
  output$deaths_plot<- renderPlotly({
    
    
    fig <- plot_ly(data = plot_data()) %>%
      add_trace(x = ~date, y = ~deaths, type = 'bar', name = 'Deaths per day',
                marker = list(color = '#C6C9CA'),
                hoverinfo = "text",
                text = ~paste(deaths, ' deaths')) %>%
      add_trace(x = ~date, y = ~roll_mean_deaths, type = 'scatter', mode = 'lines', name = 'Average Deaths', 
                line = list(color = '#43494B'),
                hoverinfo = "text",
                text = ~paste(roll_mean_deaths, ' average deaths')
      ) %>%
      layout(legend = list(x = 0.1, y = 0.9))
    
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
