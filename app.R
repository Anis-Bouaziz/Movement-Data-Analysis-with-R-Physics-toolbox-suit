

library(shiny)
library(ggplot2)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(withr)
library(DT)
library(shinyBS)
library(shinyjs)
library(shinycssloaders)
library(plotly)
library(magrittr)
library(RcppRoll)
library(groupdata2)
library(treemap)


  ## 1. header -------------------------------
  header <-
    dashboardHeader(
      titleWidth = 0,
      disable = FALSE,
      
      
      tags$li(
        class = "dropdown",
        id = "logo",
        style = "font-size:20px",
        
        tags$span(strong('R project'),
                  style = "font-size:20px;color:white;margin-right:30px"),
        tags$span(tags$img(src = 'IGM.png', height =
                             '54'))
      )
    )
  
  
  ## 2. siderbar ------------------------------
  siderbar <-
    dashboardSidebar(
      width = 300,
      sidebarMenu(
        id = 'sidebar',
        style = "position: relative; overflow: visible;",
        menuItem(
          "Presentation",
          tabName = 'presentation',
          icon = icon('book')
        ),
        menuItem(
          "Upload",
          tabName = 'Upload Data',
          icon = icon('upload'),
          startExpanded = F,
          menuSubItem(fileInput(
            "file1",
            "Choose CSV File",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ))
        ),
        
        menuItem(
          "Data",
          tabName = 'data',
          icon = icon('file-csv'),
          startExpanded = F,
          menuSubItem('DataFrame', tabName = "Data", icon = icon('table')),
          menuSubItem('Description', tabName = "info", icon = icon('info'))
        ),
        menuItem(
          "Data Visualization",
          tabName = 'data_viz',
          icon = icon('chart-bar') ,
          startExpanded = F,
          menuSubItem('Force-g', tabName = "Force-g", icon = icon('info')),
          menuSubItem(
            'Linear Accelerometer',
            tabName = "Linear_Accelerometer",
            icon = icon("cog", lib = "glyphicon")
          ),
          menuSubItem(
            'Gyroscope',
            tabName = "Gyroscope",
            icon =  icon("signal", lib = "glyphicon")
          )
        )
        
        ,
        
        menuItem(
          "Data Processing ",
          tabName = "Data_Processing",
          icon = icon('align-justify'),
          startExpanded = F,
          menuSubItem(
            'Feature Extraction 1',
            tabName = "extract_feature_long",
            icon = icon('align-right')
          ),
          menuSubItem(
            'Feature Extraction 2',
            tabName = "extract_feature_large",
            icon = icon('align-left')
          )
        ),
        
        menuItem(" Notebook",
                 tabName = "notebook",
                 icon = icon('code'))
        ,
        
        
        menuItem(" Github",
                 href = "https://github.com/Anis-Bouaziz",
                 icon = icon('github'))
      )
    )
  
  
  ## 3. body --------------------------------
  body <- dashboardBody(
    tags$head(tags$link(rel = "shortcut icon", href = "favicon.png")),
    setBackgroundImage(src = 'bg.png', shinydashboard = TRUE) ,
    style = ('color:white'),
    tags$head(tags$style(
      HTML(
        "#mytable thead {color:white;background-color:rgba(255,255,255,0.5);}"
      )
    )),
    tabItems(
      tabItem(
        tabName = "presentation",
        fluidRow(style = "display: flex !important; justify-content: center !important;",
                 
                 tags$img(src = "mvnt.jpg", style = "width:50%")),
        fluidRow(
          style = "display: flex !important; justify-content: center !important;",
          
          tags$div(
            style = 'padding:10px',
            h1("Data Analysis with R"),
            h3('M2 SIA'),
            h4('Mohamed Anis Bouaziz'),
            h4('Khalil Bagbag'),
            h4('Ramzi Raddaoui'),
            h3('Project intro to DS'),
            align = 'center'
          )
          
        )
      ),
      tabItem(
        tabName = "Data",
        h2("Force-g, Linear Accelerometer, Gyroscope
           ", align =
             'center'),
        DT::dataTableOutput("mytable")
      ),
      tabItem(tabName = "info",
              h2(
                "Physics Toolbox Sensor Suite",    align = "center", style = "color:white"
              ),
              fluidRow(
                style = "display: flex !important; justify-content: center !important;font-size:25px;",
                box(
                  width = 5,
                  background = "blue",
                  p(" This application uses the data measured by the sensors of the device to collect, save and export these data in a coma separated value (csv) file that can be shared. This data can be displayed on a graph versus time or digitally. Users can export the data for further analysis in a spreadsheet or plotting software. This application also generates tones, colors and a strobe. "),
                  div("Sensors ",br(),"
(1) G-meter - ratio of Fn/Fg (x, y, z and/or total)",br(),"
(2) Linear accelerometer - acceleration (x, y, and/or z)",br(),"
(3) Gyroscope - radial velocity (x, y, and/or z)",br()),
                  a(href ="https://play.google.com/store/apps/details?id=com.chrystianvieyra.physicstoolboxsuite&hl=fr&gl=US","Playstore"),
                  img(src = 'app.webp')
                )
              )),
      tabItem(
        tabName = "Force-g",
        tags$div(plotlyOutput("plot1", height = 800)),
        style = 'border:solid 3px;padding:15px'
        
        
      ),
      
      tabItem(
        tabName = "Linear_Accelerometer",
        tags$div(plotlyOutput("plot2", height = 800)),
        style = 'border:solid 3px;padding:15px'
        
        
      ),
      tabItem(
        tabName = "Gyroscope",
        tags$div(plotlyOutput("plot3", height = 800)),
        style = 'border:solid 3px;padding:15px'
        
        
      ),
      
      tabItem(
        tabName = "extract_feature_long",
        fluidRow(
          box(
            width = 10,
            title = 'extract_feature_long',
            status = "success",
            solidHeader = TRUE,
            plotOutput("plot4", brush = "plot_brush")
            
          ),
          style = "display: flex !important; justify-content: center !important;"
        ),
        fluidRow(
          style = "display: flex !important; justify-content: center !important;",
          box(
            width = 3,
            background = "green",
            sliderInput("t_thresh", "Time Threshhold:",
                        min = 0, max = 1, value = 0.5
            ),
            sliderInput("m_thresh", "Mean Threshhold:",
                        min = 0, max = 1, value = 0.2
            ),
            sliderInput("sd_thresh", "Standard Deviation Threshhold:",
                        min = 0, max = 1, value = 0.1
            )
          ),
          box(
            width = 7,
            background = "green",
            verbatimTextOutput("info1"),
            radioButtons("radio1", "Data type", c('X','O','N'), selected = 'X', inline = T),
            downloadButton("Download1", label = "Download Results ", class = NULL)
          )
        ) ,
        align = 'center'
      )
      ,
      tabItem(
        tabName = "extract_feature_large",
        fluidRow(
          style = "display: flex !important; justify-content: center !important;",
          box(
            width = 5,
            background = "green",
            verbatimTextOutput("info2"),
            radioButtons("radio2", "Data type", c('X','O','N'), selected = 'X', inline = T),
            downloadButton("Download2", label = "Download Results ", class = NULL)
          ),
          box(
              width = 2,background = "green",
              
              noUiSliderInput(
                inputId = "tthresh", label = "Time Threshhold:",
                min = 0, max = 1, step = 0.01,
                value = 0.1, margin = 10,
                orientation = "vertical",
                width = "300px", height = "400px"
              )
              
              
              ),
          box(width = 2,background = "green",
              noUiSliderInput(
            inputId = "mthresh", label = "Mean Threshhold:",
            min = 0, max = 1, step = 0.01,
            value = 0.1, margin = 10,
            orientation = "vertical",
            width = "300px", height = "400px"
          )),
          box(width = 2,background = "green",
              noUiSliderInput(
            inputId = "sdthresh", label = "Standard Deviation Threshhold::",
            min = 0, max = 1, step = 0.01,
            value = 0.1, margin = 10,
            orientation = "vertical",
            width = "300px", height = "400px"
          ))
          
        )
        
      ),
      tabItem(tabName = "notebook",
              fluidPage(htmlOutput("inc")))
      
      
      )
  )
  

  ui <-
    dashboardPage(title = "M2 SIA DS", header, siderbar, body , skin = "blue")
  
  server <- function(input, output, session) {
    df_upload <- reactive({
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      df <-
        read.csv(
          inFile$datapath,
          header = TRUE ,
          sep = ","
        )
      return(df)
    })
    
    output$mytable = renderDataTable(df <- df_upload() ,
                                     options = list(
                                       pageLength = 13,
                                       searching = FALSE ,
                                       lengthChange = FALSE
                                     ))
    output$plot1 <- renderPlotly({
      df <- df_upload()
      p <-
        plot_ly(
          x = df$gFx,
          y = df$gFy,
          z = df$gFz,
          type = "scatter3d",
          mode = "lines",
          line = list(width = 4, color = ~c, colorscale = list(c(0,'#BA52ED'), c(1,'#FCB040')))
        )
    })
    output$plot2 <- renderPlotly({
      df <- df_upload()
      p <-
        plot_ly(
          x = df$ax,
          y = df$ay,
          z = df$az,
          type = "scatter3d",
          mode = "lines",
          line = list(width = 4, color = ~c, colorscale = list(c(0,'#BA52ED'), c(1,'#FCB040')))
        )
    })
    output$plot3 <- renderPlotly({
      df <- df_upload()
      p <-
        plot_ly(
          x = df$wx,
          y = df$wy,
          z = df$wz,
          type = "scatter3d",
          mode = "lines",
          line = list(width = 4, color = ~c, colorscale = list(c(0,'#BA52ED'), c(1,'#FCB040')))
        )
    })
    output$plot4 <- renderPlot({
      res = extract_feature_long(df <- df_upload(), input$m_thresh, input$sd_thresh, input$t_thresh)
      ggplot(res %>% filter(pid > 0)) + geom_line(aes(
        x = time,
        y = gNsd,
        color = as.factor(pid),
        group = pid
      )) + geom_point(data = res %>% filter(pid == 00),
                      aes(x = time, y = gNsd),
                      color = "black")
    })
    output$info1 <- renderPrint({
      res <- extract_feature_long(df <- df_upload(), input$m_thresh, input$sd_thresh, input$t_thresh)
      brushedPoints(res, input$plot_brush, allRows = TRUE)
    })
    
    output$info2 <- renderPrint({
      res <- extract_feature_large(df <- df_upload(),input$mthresh, input$sdthresh, input$tthresh)
      brushedPoints(str(res, give.attr = F), input$plot_brush, allRows = TRUE)
    })
    
    output$Download1 <- downloadHandler(
      filename = function() {
        paste('BOUAZIZ_BAGBAG_RADDAOUI_OUTPUT_LONG_', input$radio1, '.csv', sep = '')
      },
      
      content = function(con) {
        write.csv(extract_feature_long(df <- df_upload(), input$m_thresh, input$sd_thresh, input$t_thresh), con)
      }
    )
    output$Download2 <-
      downloadHandler(
        filename = function() {
          paste('BOUAZIZ_BAGBAG_RADDAOUI_OUTPUT_LARGE_',input$radio2, '.csv', sep = '')
        },
        content = function(con) {
          write.csv(extract_feature_large(df <- df_upload(), input$mthresh, input$sdthresh, input$tthresh),
                    con)
        }
      )
    
    output$inc <- renderUI({
      includeHTML("Notebook.html")
    })
    extract_feature_long = function (data,
                                     threshold_mean,
                                     threshold_sd,
                                     threshold_t) {
      data$gN = abs((data$gFx) ^ 2 + ((data$gFy) ^ 2) + ((data$gFz) ^ 2) - 0.8)
      data$gNm = roll_meanl(data$gN, 10)
      data$gNsd = roll_sdl(data$gN, 10)
      data$X <- NULL
      data = drop_na(data)
      left = data[data$gNm <= threshold_mean |
                    data$gNsd <= threshold_sd, ]
      data = data[data$gNm > threshold_mean &
                    data$gNsd > threshold_sd, ]
      
      data$dt = c(0, diff(data$time))
      data$switch[data$dt > threshold_t] = 1
      data$switch[data$dt <= threshold_t] = 0
      data = group(
        data,
        n = rep(1, sum(data$switch)),
        method = 'l_starts',
        col_name = 'pid',
        starts_col = 'switch'
      )
      data$pid = as.numeric(data$pid)
      data$pid[data$switch == 1] = 0
      left$dt = 0
      left$switch = 0
      left$pid = 0
      
      
      res = bind_rows(data, left)
      return (res)
      
    }
    extract_feature_large = function (data,
                                      threeshold_mean,
                                      threeshold_sd,
                                      threeshold_t) {
      result = extract_feature_long(data, threeshold_mean, threeshold_sd, threeshold_t)
      result <- result %>% group_by(pid) %>%
        
        filter(and(sum(dt) <= 3, sum(dt) >= 1))  %>%
        mutate(localtime = time - min(time)) %>%
        mutate(tbin = floor(localtime * 2))
      
      result = result %>% group_by(pid, tbin) %>% summarise(
        mean_gFx = mean(gFx),
        mean_gFy = mean(gFy),
        mean_gFz = mean(gFz),
        mean_wx = mean(wx),
        mean_wy = mean(wy),
        mean_wz = mean(wz),
        mean_ax = mean(ax),
        mean_ay = mean(ay),
        mean_az = mean(az)
      ) %>% pivot_wider(
        pid,
        tbin,
        values_from = c(
          mean_gFx,
          mean_gFy,
          mean_gFz,
          mean_ax,
          mean_ay,
          mean_az,
          mean_wx,
          mean_wy,
          mean_wz
        ),
        values_fill = 0
      )
      
      return (result)
    }
  }
  

shinyApp(ui = ui, server = server)