# MA HAI infections rates compared across 71 MA hospitals
# Data was scraped from the official mass.gov website multiple PDFs

library(dplyr)
library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(scales)
library(lattice)
library(data.table)
library(shinyBS)

data <- read.csv("data/hosp_graded.csv")
colnames(data)
CAUTIdata <- read.csv("data/CAUTI_ICU_MA_2019.csv")
CLABSIdata <- read.csv("data/CLABSI_ICU_MA_2019.csv")
ICUNames <- read.csv("data/ICUTypes_names.csv")

data$HospCAUTIRate <- round(data$CAUTIRateCrude*1000, 2) # per 1000 catheter days
data$HospCLABSIRate <- round(data$CAUTIRateCrude*1000, 2) # per 1000 central line days
data$zipcode <- data$zipcode
row.names(data) <- data$HospRowID

# data for the hospital level data table popup bubble
data <- data %>% mutate(
  HospCAUTI_per10kAdm = round((data$CAUTICases/data$H9NumAdm)*10000, 2), 
  HospCLABSI_per10kAdm = round((data$CLABSICases/data$H9NumAdm)*10000, 2)
)


CAUTIdata$CAUTI_Rate_1000_CatheterDays <- round(CAUTIdata$CAUTI_ICU_AdjRate*1000, 2)
CLABSIdata$CLABSI_Rate_1000_CL_Days <- round(CLABSIdata$CLABSI_ICU_AdjRate*1000, 2)


CLABSIdata$ICU_Type <- CLABSIdata$H9ICUType_CLABSI
CAUTIdata$ICU_Type <- CAUTIdata$H9ICUType_CAUTI
colnames(CLABSIdata)

# merge two data frames for CLABSI and CAUTI
x <- merge(CLABSIdata, CAUTIdata, by= c("ICU_Type","OrgID"), all.x = T)
nrow(x)
colnames(x)

# remove duplicated ICUs names

x <- x[, c("OrgID", "ICU_Type","H9CLABSINum","H9CLDays" ,"CLABSIDays_by_Hosp","StratumWt.x","CLABSIRawRate","CLABSI_ICU_AdjRate",
           "CLABSI_Rate_1000_CL_Days","H9ICAUTINum","H9CathDays","CAUTIDays_by_Hosp","StratumWt.y", "RawRate","CAUTI_ICU_AdjRate",
           "CAUTI_Rate_1000_CatheterDays")]
colnames(x)

# rename some columns in x data frame

colnames(x)[c(6,13,14)] <- c("WtCLABSI", "WtCAUTI", "CAUTIRawRate")

# merge data and x dataframes to get all information in one place

x1 <- merge(data, x, by = "OrgID")
colnames(x1)

# subset columns needed
x1 <- x1[, c(1,3,5,6,7,8,9,10,11,12,35,40,41,42,46,47,48,49,50,51,63,64,65,66,67,
             38,69,70,71,72,43,74,75,76,77,78,79,80,81,82,83,84)]


x1$H9Name <- x1$H9Name.x

# save data as a new dataframe hospdata
hospdata <-  data[order(data$HospCAUTIRate),]

## UI

ui <- navbarPage("Massachusetts Healthcare Associated Infections Interactive Report 2019:", 
                 id="nav", 
                 tabPanel("ICU Rate Explorer",
                          div(class="outer",
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css")
                              ),
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("map", width="100%", height="100%"),
                              
                              tags$div(id="cite",
                                       strong('Data compiled for ', 
                                              tags$em('Nosocomial Infection Rates in MA, 2019'), 
                                              br(),
                                              ' by Natasha Dukach (MS in Epidemiology in Biostatistics,expected May 2022) 
                                              and Monika M Wahi (MPH, CPH)'))
                          )
                 ),
                 
                 tabPanel("Data collection ",
                          
                          tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css")
                          ),
                          tags$div(
                            br(),h5(tags$li(strong("
                          Nosocomial infections"), "or healthcare-associated infections (HAI),
                          are infection(s) acquired during the process of receiving health care
                          that was not present during the time of admission. High HAI rates are the fault of the hospital, 
                          and indicate low healthcare quality. 
                          This is because HAIs caused in patients by the hospital can severely injure or even kill a patient."),
                                    br(),
                                    tags$ul(tags$li("The most common HAIs are central line-associated bloodstream infections (CLABSI) and catheter-associated 
                                    urinary tract infections (CAUTI). These are possible for patients to get when they are catheterized improperly in the intensive
                                    care unit (ICU) setting.")),
                                    br(),
                                    tags$ul(tags$li("A central line bloodstream infection", strong("(CLABSI)"), "occurs when bacteria or other germs enter the patient's central line
                                  and then enter into their bloodstream."),
                                            br(),
                                  tags$li(strong("Central line-days:"), "The total number of days a central line is in place for each patient in the intensive care unit (ICU). 
                                                    The count is performed each day, and each patient with a central line is counted as a central line-day.")),
                                    br(),
                                    tags$ul(tags$li("Catheter-associated urinary tract Infection, or", strong("CAUTI"), 
                                            "is a urinary tract infection associated with urinary catheter use."),
                                            br(),
                                            tags$li(strong("Indwelling urinary catheter days:"),"the number of patients with an indwelling urinary catheter 
                                                    device multiplied by the number of days on the device. 
                                                    The daily counts are added up for the entire year to give the catheter days for that year.")),
                                    br(),
                                  
                                    tags$li("Massachusetts hospitals are required by regulation to participate in the NHSN,
                                    (the National Healthcare Safety Network which is a federal voluntary program).
                                    However, scientific studies have shown the data in then NHSN represent an undercount.
                                            Massachusetts has open data government (ODG) regulations the require data sharing about healthcare quality, 
                                            so Massachusetts department of public health (DPH) issues ", 
                                            tags$a(href="https://www.mass.gov/lists/healthcare-associated-infections-reports","the dashboard",target="_blank"),
                                            "for each year, and the most recent data available are for 2019."),
                                            br(),br(),
                                            tags$li("We were unable to download the data from the original Massachusetts DPH dashboard. The dashboard provides only individual",
                                                    tags$a(href="https://mdphgis.maps.arcgis.com/apps/MapSeries/index.html?appid=b6376f9d84cb46c996a15e3bc22dcbb3",
                                                           "PDF records", target="_blank"),"with data on them."),
                                            br(),
                                            tags$li("These reports were downloaded and the data from them scraped in RStudio using packages 'pdftools'
                                            and 'pdftables' into a logical data structure. Data were analyzed in R to prioritize rates so we could determine
                                            which HAI would drive the display and the comparison between hospitals. We chose CAUTI because it is prevalent (because urinary catheterization is common) 
                                            and can result in death, and it was highly correlated with CLABSI and other infectionrates"),
                                            br(),
                                            tags$li("In order to use this online application, 
                          an application programming interface (API) key is issued from the PDF Tables web site, 
                          and is used in the RStudio programming to pass the data to the online application."), 
                                            br(),
                                            tags$li("The code resulted in the data being processed in to a series of unstructured *.xlsx files and downloaded locally.
                          Then, in a final data cleaning step, data were transformed into the tables in the usable format for the interactive dashboard."),
                                            br(),
                                            strong("Visualizing healthcare-associated infections data:"),
                                            br(),br(),
                                  tags$li("To calculate the CAUTI rate, we used the numerator of number of CAUTI cases in 2019 in the data per hospital.
                                            We chose to use number of admissions in 2019 as our denominator for the CAUTI rate rather than 
                                            catheter-days, as we wanted to preserve the concept of the individual. We would have preferred 
                                                   to use ICU admissions or number of people catheterized as the denominator, 
                                                   but those data were not available."),
                                  br(),
                                  tags$li("We used crude CAUTI rates divided by quantile cut-offs for color-coding on the map."),
                                  br(),
                                  tags$li("In the 'View rates by ICU Type' pop-up data table on the main page we adjusted CAUTI and CLABSI 
                                  rates for each ICU per hospital. 
                                  Each ICU rate was multiplied by the weight (proportion of catheter and central line days assigned to that setting)."),
                                  br(),
                                  tags$li("Press the button below to see the Types and the Number of Intensive Care Units in Massachusetts, 2019"), 
                                  br(),
                                            # tipify(bsButton("pB2", "Button", style = "inverse", size = "small"),
                                            #        "example of pointless button"),
                                            actionButton("show", "Show ICU Types"),
                                            br(), br(),
                                            bsModal("model", "ICU Types", "show"),
                                            
                                    ))
                          ))

## Server

server <- function(input, output, session) {
  
  # create a reactive value RV that will store the click position
  
  RV<-reactiveValues(Clicks=list())
  
  # create modal event observer 
  modal <- observeEvent(input$button_click, {
    click <- input$map_marker_click
    
    #format the modal table
    output$table <- showModal(modalDialog(
      size = "l",
      h4(strong("OrgID"),click$id),
      footer = modalButton("Cancel"),
      renderDataTable(rownames= FALSE, options = list(autoWidth=T),
                      { names(x1)[c(27,28,34,35,41)] <- c("ICU Type", "CLABSI infections", "CLABSI rate per 1000 Central Line Days", "CAUTI infections", "CAUTI rate per 1000 Catheter Days")
                      df <- x1[x1$OrgID==click$id, c("ICU Type", "CLABSI infections", "CAUTI infections","CLABSI rate per 1000 Central Line Days","CAUTI rate per 1000 Catheter Days")]
                      x <- as.data.frame(rbind(df),col.names = F)
                      x
                      })
    ))
  })
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager) %>% 
      setView(lng = -71.91, lat = 42.34, zoom = 8.5) %>%
      addLegend("bottomright",
                colors = c("green", "orange", "red", "black"), 
                labels= c("Least probable: (0 to 25 percentile)","Somewhat probable: (less or equal to 50 percentile)", "More probable: 
                          (more than 50 - less or equal to 75 percentile)", "Most probable: (above 75th percentile)"),
                title= "Colors are based on percentiles of catheter-associated infection crude rates. (read: 'Data collection')",
                opacity = 0.6)
  })
  
  # A reactive expression that returns the set of hospitals that are
  # in visible map's bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(hospdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(hospdata,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the dependent variable(s) the user has chosen, or that were initially chosen to map to color.
  observe({
    colorBy <- hospdata$HospCAUTIRate
    
    getColor  <- function(hospdata) {
      sapply(hospdata$HospCAUTIRate, function(HospCAUTIRate) {
        if(HospCAUTIRate == 0) {
          "green"
        } else if(HospCAUTIRate >= 0.000 & HospCAUTIRate <= 0.67) {
          "orange"
        } else if(HospCAUTIRate >= 0.67 & HospCAUTIRate <= 1.3)  {
          "red"
        } else {
          "black"
        }
      })
    }
    
    # this quantile function gives the distribution of values in the sample, such that all hospitals' CAUTI rates in MA in 2019
    quantile <- quantile(hospdata$HospCAUTIRate)
    
    # get markers on the map
    leafletProxy("map", data = hospdata) %>%
      clearShapes() %>% 
      fitBounds(lng1 = max(hospdata$longitude),lat1 = max(hospdata$latitude),
                lng2 = min(hospdata$longitude),lat2 = min(hospdata$latitude)) %>%
      addCircleMarkers(~longitude, ~latitude,layerId = ~OrgID,group = "myMarkers",
                       fillColor = getColor(hospdata),  opacity = 0.6,  fillOpacity = 0.5, weight = 1,
                       # icon = icons,
                       options = markerOptions('Shiny.onInputChange(\"map_zoom", this.id, {priority: \"event\")'),
                       popup= ~paste("<b>", hospdata$H9Name, "</b></br>",
                                     "Number of Admissions = ","<b>", hospdata$H9NumAdm, "</b></br>",
                                     "ICU beds = ", "<b>", hospdata$H9ICUBeds, "</b></br>",
                                     "Overall catheter infections rate for all ICUs = ", "<b>", hospdata$HospCAUTI_per10kAdm, "per 10,000 admissions","</b></br>",
                                     "Overall central line infections rate for all ICUs = ", "<b>", hospdata$HospCAUTI_per10kAdm, "per 10,000 admissions","</b></br>",
                                     actionLink(inputId = "modal", label = "View rates by ICU Type", 
                                                onclick = 'Shiny.setInputValue(\"button_click\", this.id, {priority: \"event\"})'))) 
    
  })
  
  
  # When map is clicked, show a popup with hospital info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
  })
  
  observeEvent(input$show,{
    showModal(modalDialog(
      DT::renderDataTable({
        DT::datatable(ICUNames, escape = FALSE)
      })
    ))
  }
  )
  
  
  
}


shinyApp(ui, server)
