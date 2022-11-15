library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyr)
library(stringr)
library(DT)
library(dplyr)
library(readr)
library(ggplot2)
library(shinyWidgets)
library(echarts4r)
library(lubridate)
library(googlesheets4)

# Data -----
gs4_deauth()
sheet_id <- "https://docs.google.com/spreadsheets/d/1hzXw4B6Tm1ro6qyaBMTlLdJ2YhdBHAI2dvxXIQQe-0Q/"
appData <- read_sheet(sheet_id)

#----------#

# UI -----
ui <- dashboardPage(
  skin = "black",
  ## Header -----
  dashboardHeader(
    ### Title -----
    title = "Load-Velocity App", titleWidth = 250,
    ### User Icon -----
    userOutput("user"),
    ### Github Icon Link -----
    tags$li(
      class="dropdown", 
      tags$a(
        # link reference
        href="https://github.com/project-greenhouse/Load-Velocity-App",
        # icon
        icon("github"), 
        # display text
        "Source Code", 
        target="_blank"
      )
    )
  ),
  ## Sidebar -----
  dashboardSidebar(
    #sidebarUserPanel("Greenhouse Sports Performance",
    #                 image = "logo.png"),
    sidebarMenu(
      ### Tab Items -----
      menuItem("Hisotrical Data", tabName = "tab_history", icon = icon("calendar")),
      menuItem("Player to Player", tabName = "tab_S2S", icon = icon("people-arrows")),
      menuItem("Power Profile", tabName = "tab_power", icon = icon("chart-simple")),
      # File upload
      fileInput(inputId = "inputData", 
                label = "Enter your data here:",
                buttonLabel = "upload",
                placeholder = ".csv files only"),
      # Download button
      downloadBttn(
        outputId = "dwnldTemplate",
        label = "Download Template",
        size = "xs",
        style = "bordered",
        color = "success"
      )
    )
  ),
  ## Body -----
  dashboardBody(
    tabItems(
      ### History Tab -----
      tabItem(
        tabName = "tab_history",
        fluidPage(
          fluidRow(
            column(
              width = 3,
              ### Historical Player filter -----
              selectInput(inputId = "HistPlayer",
                          label = "Select Player: ",
                          choices = c("First Player"),
                          selectize = FALSE
              )
            ),
            column(
              width = 6,
              ### Historical Exercise filter -----
              selectInput(inputId = "HistExercise",
                          label = "Select Exercise: ",
                          choices = c("Back Squat"),
                          selectize = FALSE
              )
            )
          ),
          fluidRow(
            ### Summary Tables Tab Box -----
            box(
              width = 12,
              #title = "Comparison Chart",
              echarts4rOutput(
                outputId = "histPlot"
              )
            ),
            tabBox(
              title = "Summary Tables",
              side = "right",
              selected = "sess",
              height = "500px",
              width = 12,
              #### All Reps Tab -----
              tabPanel(
                title = "All Reps", 
                value = "all", 
                icon = icon("chart-gantt"),
                dataTableOutput(
                  outputId = "histAll", 
                  height = "400px"
                )
              ),
              #### Set Average Tab -----
              tabPanel(
                title = "Set Average", 
                value = "avg", 
                icon = icon("vials"),
                dataTableOutput(
                  outputId = "histAvg", 
                  height = "400px"
                )
              ),
              #### Best Rep Tab -----
              tabPanel(
                title = "Best Rep", 
                value = "best", 
                icon = icon("code-pull-request"),
                dataTableOutput(
                  outputId = "histBest", 
                  height = "400px"
                )
              ),
              #### Session Summary Tab -----
              tabPanel(
                title = "Session Summary", 
                value = "sess", 
                icon = icon("diagram-next"),
                dataTableOutput(
                  outputId = "histSess", 
                  height = "400px"
                )
              )
            )
          ) 
        )
      ),
      ### S2S Tab -----
      tabItem(
        tabName = "tab_S2S",
        fluidPage(
          #### Row 1 -----
          fluidRow(
            ##### Exercise Select -----
            column(
              width = 4,
            ),
            column(
              width = 3,
              selectInput(
                inputId = "exrcsS2S",
                label = "Select Exercise",
                choices = c("Back Squat", "Bench Press"),
                selectize = FALSE
              )
            )
          ),
          #### Row 2 -----
          fluidRow(
            ##### Player 1 Column -----
            box(
              width = 3,
              title = "Session 1",
              background = "red",
              ###### Player 1 Select -----
              selectInput(
                inputId = "S2SselectP1",
                label = "Select Player:",
                choices = "First Player",
                selectize = FALSE
              ),
              ###### Date 1 Select -----
              selectInput(
                inputId = "S2SdateP1",
                label = "Select Date: ", 
                choices = "Select Date",
                selectize = FALSE
              ),
              ###### Comp 1 Outputs -----
              valueBoxOutput(
                outputId = "e1RM1",
                width = 6
              ),
              valueBoxOutput(
                outputId = "PowFactor1",
                width = 6
              ),
              valueBoxOutput(
                outputId = "PeakV1",
                width = 6
              ),
              valueBoxOutput(
                outputId = "PeakF1",
                width = 6
              ),
              ###### Comp 1 Summary -----
              DTOutput(
                outputId = "S2SSumm1"
              )
            ),
            ##### Comp Column -----
            box(
              width = 6,
              #title = "Comparison Chart",
              echarts4rOutput(
                outputId = "S2Splot"
              )
            ),
            ##### Player 2 Column -----
            box(
              width = 3,
              title = "Session 2",
              background = "olive",
              ###### Player 2 Select -----
              selectInput(
                inputId = "S2SselectP2",
                label = "Select Player:",
                choices = "First Player",
                selectize = FALSE
              ),
              ###### Date 2 Select -----
              selectInput(
                inputId = "S2SdateP2",
                label = "Select Date: ", 
                choices = "Select Date",
                selectize = FALSE
              ),
              ###### Comp 2 Outputs -----
              valueBoxOutput(
                outputId = "e1RM2",
                width = 6
              ),
              valueBoxOutput(
                outputId = "PowFactor2",
                width = 6
              ),
              valueBoxOutput(
                outputId = "PeakV2",
                width = 6
              ),
              valueBoxOutput(
                outputId = "PeakF2",
                width = 6
              ),
              ###### Comp 2 Summary -----
              DTOutput(
                outputId = "S2SSumm2"
              )
            )
          )
        )
      ),
      ### Power Profile Tab -----
      tabItem(
        tabName = "tab_power",
        fluidPage(
          #### Row 1 -----
          fluidRow(
            column(
              width = 3,
              ##### PP Player filter -----
              selectInput(inputId = "playerPP",
                          label = "Select Player: ",
                          choices = c("First Player"),
                          selectize = FALSE
              )
            ),
            column(
              width = 3,
              ##### PP Exercise filter -----
              selectInput(inputId = "exercisePP",
                          label = "Select Exercise: ",
                          choices = c("Back Squat", "Bench Press"),
                          selectize = FALSE
              )
            ),
            column(
              width = 3,
              ##### PP Date filter -----
              selectInput(inputId = "datePP",
                          label = "Select Date: ",
                          choices = c("MM-DD-YYYY"),
                          selectize = FALSE
              )
            )
          ),
          #### Row 2 -----
          fluidRow(
            box(
              title = "Summary Table and Recommendations",
              width = 4,
              ##### PP Outputs -----
              ###### PP Summary Table -----
              DTOutput(
                outputId = "PPtable"
              ),
              ###### PP Rec Text -----
              valueBoxOutput(
                outputId = "PPrec",
                width = 12
              ),
              ###### PP V-Zone Value -----
              #valueBoxOutput(
              #  outputId = "PPvZone",
              #  width = 12
              #),
              ###### PP Training Zone Value -----
              valueBoxOutput(
                outputId = "PPtZone",
                width = 12
              )
            ),
            box(
              #title = "Power Plot",
              width = 8,
              ##### PP Graph -----
              echarts4rOutput(
                outputId = "PPplot"
              )
            )
          )
        )
      )
    )
  )
)

# Server -----
server <- function(input, output, session) {
  
  ## Header -----
  ### User Output -----
  output$user <- renderUser({
    dashboardUser(
      name = "Greenhouse Sports Performance", 
      image = "GSPlogo.png", 
      title = "Lauren Green",
      subtitle = "Author", 
      footer = p("Together We Grow", class = "text-center"),
      fluidRow(
        # Website
        dashboardUserItem(
          width = 3,
          socialButton(
            href = "https://www.greenhousesp.com",
            icon = icon("home")
          )
        ),
        # Github
        dashboardUserItem(
          width = 3,
          socialButton(
            href = "https://github.com/project-greenhouse",
            icon = icon("square-github")
          )
        ),
        # Instagram
        dashboardUserItem(
          width = 3,
          socialButton(
            href = "https://www.instagram.com/greenhouse_sp/",
            icon = icon("square-instagram")
          )
        ),
        #YouTube
        dashboardUserItem(
          width = 3,
          socialButton(
            href = "https://www.youtube.com/@greenhouseperformance",
            icon = icon("square-youtube")
          )
        )
      )
    )
  })
  
  #----------#
  
  ## Sidebar -----
  ### Template Download -----
  output$dwnldTemplate <- downloadHandler(
    filename = function() {
      paste0("LV_Profile_Template_", Sys.Date(), ".csv")
    },
    content = function(con) {
      Date = c("mm-dd-yyyy")
      User = c("First Player")
      Exercise = c("Back Squat")
      Load = c("kg or lbs")
      Velocity = c("Mean Velocity (m/s)")
      Power = c("Mean Power W")
      
      template <- data.frame(Date, User, Exercise, Load, Velocity, Power)
      template <- as.data.frame(template)
      write.csv(template, con)
    },
    contentType = "text/csv"
  )
  
  #----------#
  
  ## History -----
  ###  Athlete Observe Function -----
  observe({
    
    # Filter users by data
    userFilter <- appData %>%
      select(User)
    
    # List of Athletes filtered from exercise select
    athList <- unique(userFilter)
    
    # Update P1 select input
    updateSelectInput(session,"HistPlayer",choices=athList)

  })
  
  ###  Exercise Observe Function -----
  observe({
    user <- input$HistPlayer
    
    # Filter users by data
    exerciseFilter <- appData %>%
      filter(User == user) %>%
      select(Exercise)
    
    # List of Athletes filtered from exercise select
    exrcsList <- unique(exerciseFilter)
    
    # Update P1 select input
    updateSelectInput(session,"HistExercise",choices=exrcsList)
  })
  
  ### History Data -----
  histDF <- reactive({
    # Input filters
    user <- input$HistPlayer
    exrcs <- input$HistExercise
    
    # Filter df
    histDF <- appData %>% 
      filter(User == user, Exercise == exrcs)
    
    return(histDF)
  })
  
  ### History Session DT output -----
  #### Session First Calc -----
  histSessCalc <- reactive({
    
    #hist data
    hdf <- histDF()
    
    # Sort data
    df <- hdf %>% 
      mutate("MVT" = ifelse(Exercise == "Back Squat", 0.3, ifelse(Exercise == "Bench Press", 0.17))) %>%
      group_by(Date, Load) %>%
      filter(aVelocity == max(aVelocity)) %>%
      select(Date, Load, MVT, aVelocity) %>%
      group_by(Date) %>%
      summarise(
        "MVT" = max(MVT),
        "yInt" = lm(aVelocity ~ Load)$coefficients[1],
        "slope" = lm(aVelocity ~ Load)$coefficients[2]) %>%
      mutate("Est1RM" = round(((MVT - yInt)/slope),0)) %>%
      arrange(Date) %>%
      select(Date, Est1RM)
  })
  
  #### Session Final Calc -----
  histSession <- reactive ({
    
    hdf <- histDF()
    df <- histSessCalc()
    
    df2 <- hdf %>% 
      mutate("MVT" = ifelse(Exercise == "Back Squat", 0.3, ifelse(Exercise == "Bench Press", 0.17))) %>%
      group_by(Date, Load) %>%
      filter(aVelocity == max(aVelocity)) %>%
      select(Date, Load, MVT, aVelocity) %>%
      left_join(y = df, by = "Date") %>%
      mutate("relLoad" = Load/Est1RM) %>%
      group_by(Date) %>%
      summarise(
        "MVT" = max(MVT),
        "yInt" = lm(aVelocity ~ Load)$coefficients[1],
        "slope" = lm(aVelocity ~ Load)$coefficients[2],
        "nSlope" = lm(aVelocity ~ relLoad)$coefficients[2]) %>%
      mutate("Est1RM" = round(((MVT - yInt)/slope),0),
             "PeakF" = ((0-yInt)/slope),
             "pFactor" = round((yInt*PeakF)/2,0),
             "deficit" = paste0(ifelse(nSlope > -1, " Velocity", " Force"))) %>%
      transmute("Date" = mdy(Date),
                "Power Factor" = pFactor,
                "Est 1RM (kg)" = Est1RM,
                "Peak Velocity (m/s)" = round(yInt,2),
                "Peak Force (kg)" = round(PeakF,0),
                "Slope" = round(nSlope,2),
                "Power Deficit" = deficit) %>%
      arrange(Date)
  })
  
  #### History Session DT Output -----
  output$histSess <- renderDT({
    
    df <- histSession()
    
    ## Color and values for table color formatting
    brksPF <- seq(100, 300, 1)
    clrsPF <- colorRampPalette(c("white", "#27737b"))(length(brksPF) + 1)
    brksRM <- seq(100, 300, 1)
    clrsRM <- colorRampPalette(c("white", "#c1232a"))(length(brksRM) + 1)
    
    return(
      datatable(
        df,
        selection = "none",
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        )
      ) %>%
        formatStyle("Power Factor", backgroundColor = styleInterval(brksPF, clrsPF)) %>%
        formatStyle("Est 1RM (kg)", backgroundColor = styleInterval(brksRM, clrsRM))
    )
  })
  
  ### History Session Avg DT output -----
  output$histAvg <- renderDT({
    
    # Data
    hdf <- histDF()
    # Sort data
    df <- hdf %>% group_by(Date, Exercise, Load) %>%
      summarise(
        "Reps" = max(Rep),
        "Avg Mean Velocity (m/s)" = round(mean(aVelocity),2),
        "SD Mean Velocity (m/s)" = round(sd(aVelocity),2),
        "Avg Mean Power (W)" = round(mean(aPower),2),
        "SD Mean Power (W)" = round(sd(aPower),2))
    
    # Create Table
    return(
      datatable(
        df,
        rownames = FALSE,
        extensions = c("RowGroup", "Scroller", "Buttons"),
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          rowGroup = list(dataSrc = 0),
          columnDefs = list(list(className = "dt-center", targets = "_all")),
          deferRender = TRUE,
          scrollY = 325,
          scroller = TRUE
        )
      ) 
    )
  })
  
  ### History Session Best DT output -----
  output$histBest <- renderDT({
    
    # Data
    hdf <- histDF()
    
    # Sort data
    df <- hdf %>% group_by(Date, Exercise, Load) %>%
      filter(aVelocity == max(aVelocity)) %>%
      summarise(
        "Rep" = max(Rep),
        "Avg Velocity (m/s)" = round(mean(aVelocity),2),
        "Peak Velocity (m/s)" = round(mean(pVelocity),2),
        "Avg Power (W)" = round(mean(aPower),0),
        "Peak Power (W)" = round(mean(pPower),0))
    
    # Create Table
    return(
      datatable(
        df,
        rownames = FALSE,
        extensions = c("RowGroup", "Scroller", "Buttons"),
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          rowGroup = list(dataSrc = 0),
          columnDefs = list(list(className = "dt-center", targets = "_all")),
          deferRender = TRUE,
          scrollY = 325,
          scroller = TRUE
        )
      ) 
    )
  })
  
  ### History All Reps DT output -----
  output$histAll <- renderDT({
    
    # Data
    hdf <- histDF()
    
    # Sort data
    df <- hdf %>%
      transmute("Date" = Date, 
                "Exercise" = Exercise, 
                "Set" = Set, 
                "Rep" = Rep, 
                "Load" = Load, 
                "Avg Velocity (m/s)" = aVelocity,
                "Peak Velocity (m/s)" = pVelocity,
                "Avg Power (W)" = aPower,
                "Peak Power (W)" = pPower) %>%
      group_by(Date, Exercise, Load)
    
    # Create Table
    return(
      datatable(
        df,
        rownames = FALSE,
        extensions = c("RowGroup", "Scroller", "Buttons"),
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          rowGroup = list(dataSrc = c(0,2)),
          columnDefs = list(list(className = "dt-center", targets = "_all")),
          deferRender = TRUE,
          scrollY = 325,
          scroller = TRUE
        )
      ) 
    )
  })
  
  ### History Plot Data -----
  histData <- reactive({
   
    # Data
    hdf <- histDF()
    
    # Sort data
    df <- hdf %>% 
      mutate("MVT" = ifelse(Exercise == "Back Squat", 0.3, ifelse(Exercise == "Bench Press", 0.17))) %>%
      group_by(Date, Load) %>%
      filter(aVelocity == max(aVelocity)) %>%
      select(Date, Load, MVT, aVelocity) %>%
      group_by(Date) %>%
      summarise(
        "MVT" = max(MVT),
        "yInt" = lm(aVelocity ~ Load)$coefficients[1],
        "slope" = lm(aVelocity ~ Load)$coefficients[2]) %>%
      mutate("Est1RM" = round(((MVT - yInt)/slope),0),
             "PeakF" = ((0-yInt)/slope),
             "pFactor" = round((yInt*PeakF)/2,0),
             "rDate" = mdy(Date)) %>% 
      arrange(rDate)
  })
  
  ### History Timeline Plot -----
  output$histPlot <- renderEcharts4r({
    
    avg <- list(
      type = "average",
      name = "AVG"
    )
    
    
    histData() |>
      e_charts(Date) |>
      e_bar(Est1RM, name = "Est. 1RM") |>
      e_bar(pFactor, name = "Power Factor") |>
      e_mark_line(data = avg) |>
      e_tooltip(trigger = "axis") |>
      e_toolbox_feature(feature = "saveAsImage") |>
      e_theme("infographic")
  })
  
  #----------#
  ## S2S -----
  ###  Athlete Observe Function -----
  observe({
    exrcs <- input$exrcsS2S
    
    # Filter users by exercise select
    exrcsFilter <- appData %>%
      filter(Exercise == exrcs) %>%
      select(User)
    
    # List of Athletes filtered from exercise select
    athList <- unique(exrcsFilter)
    
    # Update P1 select input
    updateSelectInput(session,"S2SselectP1",choices=athList)
    # Update P2 select input
    updateSelectInput(session,"S2SselectP2",choices=athList)
  })
  
  ###  Date 1 Observe Function -----
  observe({
    ath1 <- input$S2SselectP1
    ath2 <- input$S2SselectP2
    exrcs <- input$exrcsS2S
    
    # Filter dates from athlete 1 select
    P1Filter <- appData %>%
      filter(Exercise == exrcs, User == ath1) %>%
      select(Date)
    
    # List of dates for P1
    date1List <- unique(P1Filter)
    
    # Filter dates from athlete 1 select
    P2Filter <- appData %>%
      filter(Exercise == exrcs, User == ath2) %>%
      select(Date)
    
    # List of dates for P2
    date2List <- unique(P2Filter)
    
    # Update Date1 select input
    updateSelectInput(session,"S2SdateP1",choices=date1List)
    updateSelectInput(session,"S2SdateP2",choices=date2List)
  })
  
  ### S2S 1 Data -----
  dfP2P1 <- reactive({
    
    ath <- input$S2SselectP1
    exrcs <- input$exrcsS2S
    date <- input$S2SdateP1
    
    P2Pdf1 <- appData %>%
      filter(User == ath, Exercise == exrcs, Date == date) %>%
      group_by(Load) %>%
      filter(aVelocity == max(aVelocity)) %>%
      transmute("Load" = Load, 
                "MV" = round(aVelocity,2),
                "MP" = round(aPower,0))
    
    return(P2Pdf1)
  })
  
  #### S2S 1 Outputs -----
  P2Pout1 <- reactive({
    
    # P1 Data
    df <- dfP2P1()
    
    # Exercise 
    exrcs <- input$exrcsS2S
    
    # Set MVT
    mvt <- if(exrcs == "Back Squat") {
      0.3
    } else if(exrcs == "Bench Press") {
      0.17
    }
    
    P2Pdf1 <- df %>%
      select(Load, MV)
    
    # linear model
    lvFit <- lm(data = P2Pdf1, MV ~ Load)
    
    # y-intercept
    yInt <- as.double(lvFit$coefficients[1])
    
    # slope
    slope <- as.double(lvFit$coefficients[2])
    
    # estimate 1RM
    e1rm <- round((mvt - yInt)/slope,0)
    
    # peakF
    PeakF <- round((0-yInt)/slope,0)
    
    #peakV
    PeakV <- round(yInt,2)
    
    # AUC
    Pfactor <- round((PeakV*PeakF)/2,0)
    
    output1 <- list(Est1rm = e1rm, PowFact = Pfactor, PeakFo = PeakF, PeakVelo = PeakV)
    return(output1)
  })
  
  ##### S2S 1 e1RM -----
  output$e1RM1 <- renderValueBox({
    val <- P2Pout1()
    valueBox(
      value = val$Est1rm, 
      subtitle = "Est. 1RM (kg)", 
      icon = icon("dumbbell"),
      color = "red"
    )
  })
  
  ##### S2S 1 PowFactor -----
  output$PowFactor1 <- renderValueBox({
    val <- P2Pout1()
    valueBox(
      value = val$PowFact,
      subtitle = "Power Factor", 
      icon = icon("bolt"),
      color = "red"
    )
  })
  
  ##### S2S 1 PeakF -----
  output$PeakF1 <- renderValueBox({
    val <- P2Pout1()
    valueBox(
      value = val$PeakFo, 
      subtitle = "Peak Force (kg)", 
      icon = icon("arrows-up-to-line"),
      color = "red"
    )
  })
  
  ##### S2S 1 PeakVelo -----
  output$PeakV1 <- renderValueBox({
    val <- P2Pout1()
    valueBox(
      value = val$PeakVelo, 
      subtitle = "Peak Velocity (m/s)", 
      icon = icon("gauge-high"),
      color = "red"
    )
  })
  
  #### Summary Table 1 ----- 
  output$S2SSumm1 <- renderDT({
    
    df <- dfP2P1()
    
    df <- df %>%
      transmute(
        "Load" = Load,
        "Avg. Velocity (m/s)" = MV,
        "Avg. Power (w)" = MP
      )
    
    datatable(
      df, 
      rownames = FALSE,
      filter = "none",
      options = list(
        dom = 't',
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      )
    )
  })
  
  ### S2S 2 Data -----
  dfP2P2 <- reactive({
    
    ath <- input$S2SselectP2
    exrcs <- input$exrcsS2S
    date <- input$S2SdateP2
    
    P2Pdf2 <- appData %>%
      filter(User == ath, Exercise == exrcs, Date == date) %>%
      group_by(Load) %>%
      filter(aVelocity == max(aVelocity)) %>%
      transmute("Load" = Load, 
                "MV" = round(aVelocity,2),
                "MP" = round(aPower,0))
    
    return(P2Pdf2)
  })
  
  #### Summary Table 2 ----- 
  output$S2SSumm2 <- renderDT({
    # Column Names
    df <- dfP2P2()
    
    df <- df %>%
      transmute(
        "Load" = Load,
        "Avg. Velocity (m/s)" = MV,
        "Avg. Power (w)" = MP
      )
        
    datatable(
      df, 
      rownames = FALSE,
      filter = "none",
      options = list(
        dom = 't',
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      )
    )
  })
  
  #### S2S 2 Outputs -----
  P2Pout2 <- reactive({
    
    # P2 data
    df <- dfP2P2()
    
    # Exercise 
    exrcs <- input$exrcsS2S
    
    # Set MVT
    mvt <- if(exrcs == "Back Squat") {
      0.3
    } else if(exrcs == "Bench Press") {
      0.17
    }
    
    # Data for lm
    P2Pdf2 <- df %>%
      select(Load, MV)
    
    # linear model
    lvFit <- lm(data = P2Pdf2, MV ~ Load)
    
    # y-intercept
    yInt <- as.double(lvFit$coefficients[1])
    
    # slope
    slope <- as.double(lvFit$coefficients[2])
    
    # estimate 1RM
    e1rm <- round((mvt - yInt)/slope,0)
    
    # peakF
    PeakF <- round((0-yInt)/slope,0)
    
    #peakV
    PeakV <- round(yInt,2)
    
    # AUC
    Pfactor <- round((PeakV*PeakF)/2,0)
    
    # List of calculated outputs
    output2 <- list(Est1rm = e1rm, PowFact = Pfactor, PeakFo = PeakF, PeakVelo = PeakV)
    return(output2)
  })
  
  ##### S2S 2 e1RM -----
  output$e1RM2 <- renderValueBox({
    val <- P2Pout2()
    valueBox(
      value = val$Est1rm,
      subtitle = "Est. 1RM (kg)", 
      icon = icon("dumbbell"),
      color = "olive"
    )
  })
  
  ##### S2S 2 PowFactor -----
  output$PowFactor2 <- renderValueBox({
    val <- P2Pout2()
    valueBox(
      value = val$PowFact, 
      subtitle = "Power Factor", 
      icon = icon("bolt"),
      color = "olive"
    )
  })
  
  ##### S2S 2 PeakF -----
  output$PeakF2 <- renderValueBox({
    val <- P2Pout2()
    valueBox(
      value = val$PeakFo, 
      subtitle = "Peak Force (kg)", 
      icon = icon("arrows-up-to-line"),
      color = "olive"
    )
  })
  
  ##### S2S 2 PeakVelo -----
  output$PeakV2 <- renderValueBox({
    val <- P2Pout2()
    valueBox(
      value = val$PeakVelo,
      subtitle = "Peak Velocity (m/s)", 
      icon = icon("gauge-high"),
      color = "olive"
    )
  })
  
  ### S2S Plot Data joined -----
  dfS2SplotData <- reactive({
    
    #Session 1
    sess1 <- "Session 1"
    
    # Add session 1 column
    s1df <- dfP2P1() %>%
      mutate("Session" = sess1)
    
    
    #Session 2
    sess2 <- "Session 2"
    
    # Add session 2 column
    s2df <- dfP2P2() %>%
      mutate("Session" = sess2)
    
    
    # Combined Session table
    S2Sjoined <- bind_rows(s1df, s2df)
    
    # Group by Session
    S2Sjoined <- group_by(S2Sjoined, Session)
    
    return(S2Sjoined)
  })
  
  ### S2S Plot Output ----- 
  output$S2Splot <-  renderEcharts4r({
    dfS2SplotData() |> 
      group_by(Session) |>
      e_charts(Load) |> 
      e_scatter(
        MV,
        bind = Load,
        symbol = "diamond",
        symbol_size = 10,
        legend = FALSE,
      ) |>
      e_lm(
        MV ~ Load, name = c("Session 1", "Session 2")) |>
      e_title(
        text = "Load-Velocity Relationship: Session Comparison") |>
      e_tooltip(
        formatter = htmlwidgets::JS(
          "function(params){
        return('<strong>Load: ' + params.name + 
                '</strong><br />%1RM: ' + params.value[0] + 
                '<br />MV: ' + params.value[1]) 
                }"
        )
      )|>
      e_axis_labels(x = "kg", y = "m/s") |>
      e_legend(right = "35%", bottom = 0) |>
      e_x_axis(
        min = 0, 
        max = 200) |>
      e_y_axis(
        min = 0, 
        max = 2, 
        formatter = e_axis_formatter(
          digits = 2)) |>
      e_toolbox_feature(
        feature = "saveAsImage") |>
      e_theme("infographic") |>
      e_show_loading()
  })
  
  #----------#
  ## Power Profile ----- 
  
  ###  Athlete Observe Function -----
  observe({
    df <- appData
    
    # Filter users by data
    userFilter <- df %>%
      select(User)
    
    # List of Athletes filtered from exercise select
    athList <- unique(userFilter)
    
    # Update P1 select input
    updateSelectInput(session,"playerPP",choices=athList)
    
  })
  
  ###  PP Exercise Observe Function -----
  observe({
    user <- input$playerPP
    
    # Filter users by exercise select
    exrcsFilter <- appData %>%
      filter(User == user) %>%
      select(Exercise)
    
    # List of exercise filtered from Athletes select
    exList <- unique(exrcsFilter)
    
    # Update PP Exercise select input
    updateSelectInput(session,"exercisePP",choices=exList)
  })
  
  ###  PP Date Observe Function -----
  observe({
    user <- input$playerPP
    exrcs <- input$exercisePP
    
    # Filter dates from athlete 1 select
    dateFilter <- appData %>%
      filter(Exercise == exrcs, User == user) %>%
      select(Date)
    
    # List of dates for P1
    dateList <- unique(dateFilter)
    
    # Update PP Date select input
    updateSelectInput(session,"datePP",choices=dateList)
  })
  
  ### PP Data -----
  dfPow <- reactive({
    
    ath <- input$playerPP
    exrcs <- input$exercisePP
    date <- input$datePP
    
    PPdf <- appData %>%
      filter(User == ath, Exercise == exrcs, Date == date) %>%
      group_by(Load) %>%
      filter(aVelocity == max(aVelocity)) %>%
      transmute("Load" = Load, 
                "Velocity (m/s)" = round(aVelocity,2),
                "Power (W)" = round(aPower,2))
    
    return(PPdf)
  })
  
  #### PP Summary Table ----- 
  output$PPtable <- renderDT({
    datatable(
      dfPow(), 
      rownames = FALSE,
      filter = "none",
      options = list(
        dom = 't',
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      )
    )
  })
  
  #### PP Plot Data -----
  dfPowPlot <- reactive({
    
    ath <- input$playerPP
    exrcs <- input$exercisePP
    date <- input$datePP
    
    power <- appData %>%
      filter(User == ath, Exercise == exrcs, Date == date) %>%
      group_by(Load) %>%
      summarise("AvgP" = mean(aPower))
    
    ## GG plot for build
    plotGG <- ggplot(power, aes(x = Load, y = AvgP,)) +
      geom_smooth(method = loess) 
    
    ## GG Build
    powBuild <- ggplot_build(plotGG)
    
    # Power curve x and y data frame
    dataPCurve <- as.data.frame(powBuild$data[[1]]) %>% 
      select (x,y) %>%
      transmute("x" = round(x,0), "y" = round(y,0))
    
    return(dataPCurve)
  })
  
  #### PP Rec Data -----
  dfPowRec <- reactive({
    
    # Input filters
    user <- input$playerPP
    exrcs <- input$exercisePP
    date <- input$datePP
    
    # Filter df
    histDF <- appData %>% 
      filter(User == user, Exercise == exrcs, Date == date)
    
    # Sort data
    df <- histDF %>% 
      mutate("MVT" = ifelse(Exercise == "Back Squat", 0.3, ifelse(Exercise == "Bench Press", 0.17))) %>%
      group_by(Load) %>%
      filter(aVelocity == max(aVelocity)) %>%
      select(Load, MVT, aVelocity) %>%
      mutate("relLoad" = Load/100)
    
    lmDF <- lm(data = df, aVelocity ~ relLoad)
    
    slope <- lmDF$coefficients[2]
    
    def <- ifelse(slope > -1, "v", "f")
    
    return(def)
  })
  
  #### PP Outputs -----
  ##### PP Rec -----
  output$PPrec <- renderValueBox({
    rec <- dfPowRec()
    
    text <- if(rec == "f") {
      "Force Deficit"
    } else {
      "Velocity Deficit"
    }
    
    valueBox(
      value = text,
      subtitle = "",
      color = "black"
    )
  })
  
  ##### PP Velocity Zone -----
  output$PPvZone <- renderValueBox({
    val <- dfPowPlot()
    pPeak <- max(val$y)
    ZoneLow <- round(pPeak*0.05,0)
    ZoneHigh <- round(pPeak*0.1,0)
    df <- val %>% 
      filter(y < pPeak-ZoneLow & y > pPeak-ZoneHigh ) %>%
      select(x)
    
    nr <- nrow(df)
    
    df1 <- df %>% slice_head(n = round(nr/2,0)-1)
    
    minL <-  min(df1$x)
    maxL <-  max(df1$x)
    
    # Rec color
    rec <- dfPowRec()
    
    clr <- ifelse(rec == "v", "green", "black")
    
    valueBox(
      value = paste0(minL, "kg", " - ", maxL, "kg"),
      subtitle = "Velocity Training Zone", 
      #icon = icon("gauge-high"),
      color = clr
    )
  })
  
  ##### PP Training Zone -----
  output$PPtZone <- renderValueBox({
    # Rec
    rec <- dfPowRec()
    
    # Data
    val <- dfPowPlot()
    pPeak <- max(val$y)
    ZoneLow <- round(pPeak*0.05,0)
    ZoneHigh <- round(pPeak*0.1,0)
    
    df <- val %>% 
      filter(y < pPeak-ZoneLow & y > pPeak-ZoneHigh) %>%
      select(x)
    
    nr <- nrow(df)
    
    df1 <- if(rec == "f") {
      df %>% slice_tail(n = round(nr/2,0)-1)
    } else {
      df %>% slice_head(n = round(nr/2,0)-1)
    }
    
    minL <-  min(df$x)
    maxL <-  max(df$x)
    
    valueBox(
      value = paste0(minL, "kg", " - ", maxL, "kg"),
      subtitle = "Recommended Training Zone", 
      #icon = icon("gauge-high"),
      color = "green"
    )
  })
  
  #### PP Plot Output -----
  output$PPplot <-  renderEcharts4r({
    dataPCurve <- dfPowPlot()
    
    # chart formatter
    max <- list(
      name = "Max",
      type = "max"
    )
    
    ## echarts4R plot
    PowPlot <- dataPCurve |> 
      e_charts(x) |> 
      e_bar(y, legend = FALSE) |>
      e_mark_line(serie = "y", data = list(type = "max")) |>
      e_title(text = "Power - Load Profile", subtext = "Modeled with 2nd Order Polynomial") |>
      e_tooltip(
        trigger = "item",
        formatter = e_tooltip_pointer_formatter(digits = 2),
        axisPointer = list(
          type = "cross")) |>
      e_x_axis(
        min = 0, 
        max = 200) |>
      e_toolbox_feature(
        feature = "saveAsImage") |>
      e_theme("infographic") |>
      e_show_loading()
    
    return(PowPlot)
  })
}

shinyApp(ui, server)
