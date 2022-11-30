library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinyjs)
library(leaflet)
library(sp)
library(DT)
library(openxlsx)
library(datasets)

data_standards <- read_rds("qs_data/qstable1_standards.rds") %>%
    mutate(label = paste0("QS",qualitystandardid," - ",qualitystandard)) 

data_statements <- read_rds("qs_data/qstable2_statements.rds") %>%
    mutate(standardid = as.integer(standardid),
           label=paste(standard_statementid, statement_short)) 

data_measures <- read_rds("qs_data/qstable3_measure.rds") %>%
    mutate(label = QS_measure_id)
    #mutate(label=paste(QS_measure_id, "(", measure_type, ")"))# %>%
    #left_join(read_rds("qs_data/qstable4_measure_detail.rds"), by = "QS_measure_id")

data_details <- read.csv("Details.csv")

data_measures_details <- read_rds("qs_data/qstable9_allmeasures.rds") %>%
    mutate(label = QS_measure_id)
    #mutate(label = paste0( "(",QS_measure_id, ")"))

data_data <- read_rds("data/smoking/data.rds") %>%
    mutate(Measure=str_trim(Measure))

#data_measure_detail <- read_rds("qs_data/qstable4_measure_detail.rds") %>%
#  mutate(label=paste(QS_measure_id, "(", measure_type, ")")) 


data_shapefile <- readRDS("shapefile.RDS")

#LDHC <- readRDS ("qs101.2_data\\LDHC_101.2.p1.rds") %>%
#filter(Geography %in% c("England")) %>%
#mutate(Percentage = (LDOB079/LDOB003A)*100)

#wb <- loadWorkbook("xl_worksheets/Action.xlsx")


ui <- dashboardPage(
    dashboardHeader(title="QSSIT"),
    dashboardSidebar(
        sidebarMenu(
            
            id="tabs",
            
            
            
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            conditionalPanel(condition = "input.tabs=='dashboard'",
                             selectizeInput("dashboard_standards","Quality Standards", choices=data_standards %>% 
                                                pull(label), multiple=TRUE),
                             selectizeInput("dashboard_statements","Quality Statements", choices=data_statements %>%
                                                pull(label)),  
                             selectizeInput("dashboard_measures","Quality Measures", choices=data_measures %>%
                                                pull(label))),
            
            
            menuItem("Actions", tabName = "actions", icon = icon("th")),
            
            conditionalPanel(condition = "input.tabs=='actions'",
                             selectizeInput("actions_standards","Quality Standards", choices=data_standards %>% 
                                                pull(label), multiple=TRUE),
                             selectizeInput("actions_statements","Quality Statements", choices=data_statements %>%
                                                pull(label), multiple=TRUE),
                             selectizeInput("actions_measures","Quality Measures", choices=data_measures %>%
                                                pull(label)))
            
            
        )
        
    ),
    dashboardBody(
        
        tabItems(
            tabItem(tabName = "dashboard",
                    column(width =4,
                           box(textOutput("standard_description"), title = "Quality standard title:", width = NULL, status = "primary"),
                           box(textOutput("statement_description"), title = "Selected statement text:", width = NULL, status = "primary"),
                           box(textOutput("measure_description"), title = "Selected measure text:", width = NULL, status = "primary")),
                    column(width =8,
                           box(leafletOutput("heatmap"), title = "Map:", width = NULL, status = "primary")),
                    
                    box(plotOutput("plotLDHC"), title = "Chart:", width = 12, status = "primary"),  
                    box(DTOutput("table"), title = "Table:", width = 12, status = "primary")), 
            
            
            tabItem(tabName = "actions",
                    #column(width =6,
                      #    box(textOutput("standard_description"), title = "Quality standard title:", width = NULL, status = "primary"),
                     #    box(textOutput("statement_description"), title = "Selected statement text:", width = NULL, status = "primary"),
                        #box(textOutput("measure_description"), title = "Selected measure text:", width = NULL, status = "primary"),
                    downloadButton("download_actionplan","Download Action Plan"),
                    downloadButton("download_monitoringchange","Download Monitoring Change Worksheet"))
            
        )
        
        
    )
)  


server <- function(input, output, session) {
    
    #Dashboard tab
    # filter dashboard statements when standard selected
    observeEvent(input$dashboard_standards, {
        # TODO: why does not get called when nothing is selected. 
        
        if(is.null(input$dashboard_standards)) {
            updateSelectizeInput("dashboard_statements", session=session, choices = data_statements %>%
                                     pull(label))      
        } else{
            updateSelectizeInput("dashboard_statements", session=session, choices = data_statements %>%
                                     inner_join(data_standards %>% filter(label %in% input$dashboard_standards), 
                                                by= c("standardid" = "qualitystandardid")) %>%
                                     pull(label.x)) 
        }
    })
    
    
    
    ###filter dashboard measures when statement selected on dashboard tab
    observeEvent(input$dashboard_statements, {
        # TODO: why does not get called when nothing is selected. 
        
        if(is.null(input$dashboard_statements)) {
            updateSelectizeInput("dashboard_measures", session=session, choices = data_measures %>%
                                     pull(label)) 
        } else{
            updateSelectizeInput("dashboard_measures", session=session, choices = data_measures %>%
                                     inner_join(data_statements %>% filter(label %in% input$dashboard_statements), 
                                                by= c("QS_statement_id" = "standard_statementid")) %>%
                                     pull(label.x)) 
        }
    })
    
    #Actions tab
    # filter actions statements when standard selected
    observeEvent(input$actions_standards, {
        # TODO: why does not get called when nothing is selected. 
        
        if(is.null(input$actions_standards)) {
            updateSelectizeInput("actions_statements", session=session, choices = data_statements %>%
                                     pull(label))      
        } else{
            
            updateSelectizeInput("actions_statements", session=session, choices = data_statements %>%
                                     inner_join(data_standards %>% filter(label %in% input$actions_standards), 
                                                by= c("standardid" = "qualitystandardid")) %>%
                                     pull(label.x))   
        }
        
    })
    
    ###filter action measures when statement selected on dashboard tab
    observeEvent(input$actions_statements, {
        # TODO: why does not get called when nothing is selected. 
        
        if(is.null(input$actions_statements)) {
            updateSelectizeInput("actions_measures", session=session, choices = data_measures %>%
                                     pull(label)) 
        } else{
            updateSelectizeInput("actions_measures", session=session, choices = data_measures %>%
                                     inner_join(data_statements %>% filter(label %in% input$actions_statements), 
                                                by= c("QS_statement_id" = "standard_statementid")) %>%
                                     pull(label.x)) 
        }
    }) 
    
    

   
    
    measure_data <- reactive({
        m <- data_details %>%
            filter(Measure == str_split (input$dashboard_measures, " ")[[1]][1])
        data_data %>%
            mutate(Measure= str_replace(Measure, m$Numerator[[1]], "Numerator"),
                   Measure= str_replace(Measure, m$Denominator[[1]], "Denominator")) %>%
            filter(Measure %in% c("Numerator", "Denominator")) %>%
            distinct() %>%
            pivot_wider(names_from = Measure, values_from = Value) %>%
            mutate(Percentage = Numerator/Denominator)
            
    })
    
    output$standard_description <- renderText({
        data_standards %>%
            filter(label %in% input$dashboard_standards) %>%
            pull(qualitystandard)
    })
    
    output$statement_description <- renderText({
        data_statements %>%
            filter(label %in% input$dashboard_statements) %>%
            pull(statement_long)
    })
    
    output$measure_description <- renderText({
        data_measures %>%
            filter(label %in% input$dashboard_measures) %>%
            pull(measure_text)
    })
    
    output$heatmap <- renderLeaflet({
        
       d <- measure_data() %>%
            filter(FinancialYear==max(FinancialYear),
                   Quarter==max(Quarter)) %>%
           select(ccg20nm=OrgName, Percentage) %>%
       mutate(Percentage = Percentage * 100)
       
       print(d)
       
       s <- data_shapefile
       
       s@data <- s %>%
           as.data.frame() %>%
           left_join(d, by = "ccg20nm")
       
       print(as.data.frame(s))
       
       pal <- colorBin("plasma", 
                       bins = 5, 
                       na.color = "#808080", 
                       domain = s$Percentage) 
       
        leaflet(s) %>% 
            addProviderTiles(provider="CartoDB.Positron") %>%
            addPolygons(weight = 2, fill=0.3, 
                        color = "#228096", 
                        label = ~lapply(paste0("Name: ",s$ccg20nm, 
                                               
                                               "<br>Percentage: ", s$Percentage),
                                        htmltools::HTML),
                        fillColor = ~pal(s$Percentage), 
                        layerId = ~s$ccg20cd) %>%
            addLegend("bottomright", pal = pal, values = s$Percentage,
                      title = "Women known to be smokers at time of delivery",
                      labFormat = labelFormat(suffix = "%"),
                      opacity = 1)
    }) 
    
    output$plotLDHC<-renderPlot({
        #   ggplot(data=LDHC, aes(x=Fyear, y=Percentage)) +
        #       geom_bar(stat='identity') +
        #      theme_minimal() +
        #      labs(title = "Learning disability health check, England, 2016-17 - 2020-21")
    })
    
    
    
    output$table <- DT::renderDT({
        DT::datatable(
            data_measures,
            selection = "none",
            extensions = 'Buttons',
            option = list(
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
            )
        ) 
        
        
        
        
    })
    
    #output$download_actionplan <- downloadHandler(    
    # filename = function() {      
    #     "actionplan.csv"},
    #   content = function(file) 
    #      {
    #     write.csv(data_standards, file, row.names = FALSE)})
    
    output$download_actionplan <- downloadHandler(    
        filename = function() {      
            "Action.xlsx"},
        content = function(file) 
        {
            
            data <- data_statements %>%
                filter(label %in% input$actions_statements) %>%
                left_join(data_standards , by = c("standardid"="qualitystandardid")) %>%
                select(standard_statementid, qualitystandard, statement_short)
            wb <- loadWorkbook("xl_worksheets/Action.xlsx")
            writeData(wb, "Action plan", data, colNames = FALSE, startRow = 7)
            saveWorkbook(wb, file, overwrite = TRUE)
            
        })
    
    # wont use data_measures_details in selection, just show in excel
    output$download_monitoringchange <- downloadHandler(    
        filename = function() {      
            "MonitoringChange.xlsx"},
        content = function(file) 
        {
            
            data <- data_measures_details %>%
                filter(label %in% input$actions_measures) %>%
                #left_join(data_measures_details , by = c("QS_measure_id"="QS_measure_id")) %>%
                select(QS_Number, QS_title, QS_statement_id,Statement_text, Measure_type, Measure_text,Numerator,Denominator)
            wb <- loadWorkbook("xl_worksheets/Monitoring_Change.xlsx")
            writeData(wb, "Monitoring_Change", data, colNames = FALSE, startRow = 100)
            saveWorkbook(wb, file, overwrite = TRUE)
            
        })
    
    
}

shinyApp(ui, server)

