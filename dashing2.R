library(shiny)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(shinyWidgets)
require(maps)  
require(mapdata) 
library(ggplot2) 
library(readxl) 
library(ggthemes) 
library(ggrepel) 
library(plotly)
library(stringi)
library(stringr)
library(shinythemes)
library(htmltools)
library(htmlwidgets)
library(dplyr)
library(leaflet)
library(DT)
library(devtools)
library(forecast)
library(jsonlite)
library(geojsonio)
library(shinycssloaders)

#setwd("C:/Users/MSI/Desktop/baru")

ui <- dashboardPage(skin="purple",
                    dashboardHeader(title = "Covidgraph"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Situasi Global Indonesia", tabName = "situasi"),
                        menuItem("Info Covid-19 Provinsi", tabName = "provinsi"),
                        menuItem("Forecast!", tabName = "prediksi"),
                        menuItem("Info Covid-19 Kota Malang", tabName = "malang",badgeLabel = "new!", badgeColor = "green")
                      )
                    ),
                    dashboardBody(    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
                                      tabItems(
                                        # First tab content
                                        tabItem(tabName = "situasi",
                                                fluidRow(tags$style(type='text/css','#predikboost{background-color:#FFFFFF;color:#000000;font-size:12px}'),
                                                         valueBoxOutput("b0",width = 12),
                                                         valueBoxOutput("b1",width = 3),
                                                         valueBoxOutput("b2",width = 3),
                                                         valueBoxOutput("b3",width = 3),
                                                         valueBoxOutput("b4",width = 3),
                                                         box(withSpinner(plotlyOutput("plot1", height = 500))),
                                                         box(withSpinner(plotlyOutput("plot2", height = 500))),
                                                         box(width = 12,
                                                             # title = "Controls",
                                                             chooseSliderSkin("HTML5",color = 'black'),
                                                             uiOutput("sleding")
                                                         )
                                                )
                                        ),
                                        tabItem(tabName = "provinsi",
                                                fluidRow(tags$style(type='text/css','#predikboost{background-color:#FFFFFF;color:#000000;font-size:12px}'),
                                                         box(
                                                           title = "Sebaran Kasus Positif Covid-19 per Provinsi di Indonesia", width = 12, solidHeader = TRUE, status = "primary",
                                                           "(Klik pada Daerah/Provinsi untuk mengetahui #Kasus Positif Covid-19)"
                                                         ),
                                                         box(withSpinner(leafletOutput("m1",height = 500))
                                                             ,width = 12
                                                         ),
                                                         box(withSpinner(DT::dataTableOutput("table")),background = 'aqua')
                                                )
                                        ),
                                        # Second tab content
                                        tabItem(tabName = "prediksi",fluidRow(
                                          box(withSpinner(plotlyOutput("p1", height = 500))),
                                          box(#width = 3,
                                            # title = "Controls",
                                            uiOutput("sleding2")
                                          ),
                                          box(#width = 3,
                                            background = 'red',
                                            # title = "Controls",
                                            uiOutput("ide")
                                          ),
                                          box(
                                            title = "Info Forecast", solidHeader = TRUE, status = "primary",
                                            "Forecast menggunakan metode Exponential Smoothing"
                                            
                                          ),
                                          box(background = 'yellow',title = "Output ETS menggunakan R:",
                                              withSpinner(verbatimTextOutput("predikboost",placeholder = TRUE)), width = 12,height = 100)
                                        )
                                        
                                        ),
                                        tabItem(tabName = "malang",
                                                fluidRow(
                                                  valueBox(paste0("Sebaran Kasus Covid-19 di Kota Malang"),"(ODP: Orang Dalam Pengawasan,PDP: Pasien Dengan Pengawasan)",width=12),
                                                  box(withSpinner(leafletOutput("mos1",height = 500))
                                                      ,width = 12),
                                                  box(withSpinner(DT::dataTableOutput("table2")))
                                                )
                                        )
                                      )
                    )
)

server <- function(input, output) {
  #ini data 1
  datajs1<-fromJSON("https://services5.arcgis.com/VS6HdKS0VfIhv8Ct/arcgis/rest/services/Statistik_Perkembangan_COVID19_Indonesia/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
  datajs1<-datajs1$features
  datajso1<-data.frame(datajs1$attributes)
  data1<-datajso1
  #edit data 1
  cek<-is.na(data1$Jumlah_Kasus_Kumulatif)
  i=1
  repeat{
    if(cek[i]==TRUE){n1=i-1}
    if(cek[i]==TRUE){break}
    i=i+1
  }
  df<-data1[1:n1,]
  Tanggal<-as.Date(seq(as.Date('2020-03-02'),as.Date('2020-12-29'),by = 1))
  df$Tanggal<-Tanggal[1:n1]
  
  
  
  ## ini data 2
  datajs2<-fromJSON("https://services5.arcgis.com/VS6HdKS0VfIhv8Ct/arcgis/rest/services/COVID19_Indonesia_per_Provinsi/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
  datajs2<-as.data.frame(datajs2$features)
  datajso2<-data.frame(datajs2$geometry,datajs2$attributes)
  head2<-c("X","Y","FID","Kode_Provi","Provinsi","Kasus_Posi","Kasus_Semb","Kasus_Meni")
  data2<-datajso2
  colnames(data2)<-head2
  ##################
  ## ini data 3
  states <- geojsonio::geojson_read("https://opendata.arcgis.com/datasets/649e7da8e03540e0b3f496f76d00cdec_0.geojson", what = "sp")
  class(states)
  names(states)
  #########
  
  letuce<-0
  
  #widgets
  output$sleding = renderUI({
    n=length(df[,3])
    sliderInput("sleding", label =h3("Pilih Rentang Hari:"), min = 1,max = n, value = c(1,n),
                round = T,pre = " Hari ke- ",step = 1)
  })
  output$sleding2 = renderUI({
    n=length(df[,3])
    sliderInput("sleding2", label =h3("Forecast selama beberapa hari kedepan:"), min = 1,max = n, value = 10,
                round = T,post = " Hari",step = 1)
  })
  filedata = reactive({
    rentang<-input$sleding
    df<-df[rentang[1]:rentang[2],]
  })
  
  
  output$ide = renderUI({
    as<-c("Jumlah Kasus", "Jumlah Kasus Sembuh","Jumlah Pasien Dalam Perawatan","Jumlah Kasus Meninggal ")
    selectInput("ide",h3("Pilihan Variabel:"),selected ="Jumlah Kasus",as,width = '200%')
    
  })
  
  dataw = reactive({
    dr<-filedata()
    as<-c("Jumlah Kasus", "Jumlah Kasus Sembuh","Jumlah Pasien Dalam Perawatan","Jumlah Kasus Meninggal ")
    dr<-dr[,c("Jumlah_Kasus_Kumulatif","Jumlah_Pasien_Sembuh","Jumlah_pasien_dalam_perawatan","Jumlah_Pasien_Meninggal")]
    colnames(dr)<-as
    dr<-dr[,input$ide]
  })
  
  #_____________________
  output$plot1 <- renderPlotly({
    Sys.sleep(letuce)
    ds<-as.data.frame(filedata())
    
    fig <- plot_ly(ds, x = ~Tanggal, y = ~Jumlah_Kasus_Baru_per_Hari, 
                   name = 'Kasus Baru', type = 'scatter', mode = 'lines',
                   line = list(color = '#d63447', width = 2)) 
    fig <- fig %>% add_trace(y = ~Jumlah_Kasus_Sembuh_per_Hari, 
                             name = 'Kasus Sembuh', line = list(color = '#f57b51', width = 2)) 
    fig <- fig %>% add_trace(y = ~Jumlah_Kasus_Dirawat_per_Hari, 
                             name = 'Kasus Dirawat', line = list(color = '#381460', width = 2,dash='dot')) 
    fig <- fig %>% add_trace(y = ~Jumlah_Kasus_Meninggal_per_Hari, 
                             name = 'Kasus Meninggal', line = list(color = '#1b262c', width = 2,dash='dot'))
    fig <- fig %>% layout(title =list(text="Kasus Covid-19 Indonesia Perhari",y=0.98) ,
                          xaxis = list(title = "Tanggal"),
                          yaxis = list (title = "Frekuensi"), 
                          legend = list(title=list(text='<b> Pilihan (Klik): </b>'),x = 200,y = 1),
                          font=list(family = "arial",size = 14))
    fig
    
  })
  output$plot2 <- renderPlotly({
    Sys.sleep(letuce)
    ds<-as.data.frame(filedata())
    fig <- plot_ly(ds, x = ~Tanggal, y = ~Jumlah_Kasus_Kumulatif, 
                   name = 'Kasus Baru', type = 'scatter', mode = 'lines',
                   line = list(color = '#d63447', width = 2)) 
    fig <- fig %>% add_trace(y = ~Jumlah_Pasien_Sembuh, 
                             name = 'Kasus Sembuh', line = list(color = '#f57b51', width = 2)) 
    fig <- fig %>% add_trace(y = ~Jumlah_pasien_dalam_perawatan, 
                             name = 'Kasus Dirawat', line = list(color = '#381460', width = 2,dash='dot')) 
    fig <- fig %>% add_trace(y = ~Jumlah_Pasien_Meninggal, 
                             name = 'Kasus Meninggal', line = list(color = '#1b262c', width = 2,dash='dot'))
    fig <- fig %>% layout(title =list(text="Kasus Covid-19 Indonesia Kumulatif",y=0.98) ,
                          xaxis = list(title = "Tanggal"),
                          yaxis = list (title = "Frekuensi Kumulatif"), 
                          legend = list(title=list(text='<b> Pilihan (Klik): </b>'),x = 200,y = 1),
                          font=list(family = "arial",size = 14))
    fig
    
  })
  ## valuebox
  output$b1 <- renderValueBox({
    valueBox(
      paste0(sum(df$Jumlah_Kasus_Baru_per_Hari)," (","+",df$Jumlah_Kasus_Baru_per_Hari[n1],")"), "Total Kasus COVID-19 Di Indonesia",
      color = "blue"
    )
  })
  output$b2 <- renderValueBox({
    valueBox(
      paste0(sum(df$Jumlah_Kasus_Sembuh_per_Hari)," (","+",df$Jumlah_Kasus_Sembuh_per_Hari[n1],")"), "Total Pasien Sembuh Di Indonesia",
      color = "yellow"
    )
  })
  output$b3 <- renderValueBox({
    valueBox(
      paste0(sum(df$Jumlah_Kasus_Dirawat_per_Hari)," (","+",df$Jumlah_Kasus_Dirawat_per_Hari[n1],")"), "Total Pasien Dirawat Di Indonesia" ,
      color = "purple"
    )
  })
  output$b4 <- renderValueBox({
    valueBox(
      paste0(sum(df$Jumlah_Kasus_Meninggal_per_Hari)," (","+",df$Jumlah_Kasus_Meninggal_per_Hari[n1],")"), "Total Pasien Meninggal Di Indonesia",
      color = "red"
    )
  })
  output$b0 <- renderValueBox({
    valueBox(
      paste("Data per tanggal",df$Tanggal[length(df$Tanggal)]), "Data yang digunakan pada App ini diperoleh dari https://www.covid19.go.id",
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "aqua"
    )
  })
  ##### tab provinsi
  output$m1<-renderLeaflet({
    Sys.sleep(letuce)
    labelsa <- sprintf(
      "<strong>%s</strong>",
      data2$Provinsi
    ) %>% lapply(htmltools::HTML)
    m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      setView(lng=115.31131, lat=-2.44977914,zoom = 4.5)%>% addProviderTiles(providers$CartoDB.DarkMatter)
    
    pal <- colorNumeric(
      palette = "Reds",
      NULL)
    
    m<-m%>% 
      #leaflet(data2) %>% addTiles() %>%
      addCircles(data=data2,lng = ~X, lat = ~Y, weight = 25,
                 radius = ~Kasus_Posi, popup = ~paste(labelsa,h6(""),"Kasus Positif:",Kasus_Posi,
                                                      h6(""),"Kasus Sembuh:",Kasus_Semb,
                                                      h6(""),"Kasus Meninggal:",Kasus_Meni)
                 
                 ,color = ~pal(Kasus_Posi)
      )%>%addLegend(title = "Kasus Positif",pal = pal,values = data2$Kasus_Posi,position = "bottomright",opacity = 0.7)
    
    # m<-m %>% addLegend(pal = pal, values = ~Kasus_Posi, opacity = 0.7, title = "Kasus Positif",
    #         position = "bottomright")
    m
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    Sys.sleep(letuce)
    datar<-data2[,-c(1:4)]
    colnames(datar)<-c("Provinsi","Kasus Positif","Kasus Sembuh","Kasus Meninggal")
    datar<-as.data.frame(datar[order(datar$`Kasus Positif`,decreasing = T),])
    rownames(datar)<-seq(1:nrow(datar))
    datar
  }))
  
  output$p1 <- renderPlotly({
    Sys.sleep(letuce)
    as<-c("Jumlah Kasus", "Jumlah Kasus Sembuh","Jumlah Pasien Dalam Perawatan","Jumlah Kasus Meninggal ")
    n2<-input$sleding2
    datat<-dataw()
    datat<-ts(datat)
    fore<- forecast(datat, h = n2,level = c(80,95))
    prop<-Tanggal
    prop<-prop[(n1+1):(n1+n2)]
    
    fig <- plot_ly()
    fig <- fig %>% add_lines(x =df$Tanggal, y = datat,
                             color = I("black"), name = "observed")
    fig <- fig %>% add_ribbons(x = prop, ymin = fore$lower[, 2], ymax = fore$upper[, 2],
                               color = I("gray95"), name = "95% confidence")
    fig <- fig %>% add_ribbons(x = prop, ymin = fore$lower[, 1], ymax = fore$upper[, 1],
                               color = I("gray80"), name = "80% confidence")
    fig <- fig %>% add_lines(x = prop, y = fore$mean, color = I("red"), name = "prediction")
    
    fig <- fig %>% layout(title =list(text=paste( "Forecast",input$ide,"Covid-19 Indonesia (Kumulatif)")
                                      ,
                                      y=0.98) ,
                          xaxis = list(title = "Tanggal"),
                          yaxis = list (title = "Frekuensi Kumulatif"), 
                          legend = list(title=list(text='<b> Pilihan (Klik): </b>'),x = 200,y = 1),
                          font=list(family = "arial",size = 14))
    fig
  })
  
  output$predikboost=renderPrint({
    Sys.sleep(letuce)
    n2<-input$sleding2
    datat<-dataw()
    datat<-ts(datat)
    options(digits = 4)
    fore<- forecast(datat, h = n2,level = c(80,95))
    summary(fore)
    #fore
  })
  
  #################### Malang !
  output$mos1<-renderLeaflet({
    Sys.sleep(letuce)
    pal <- colorNumeric("Reds", NULL)
    m <- leaflet(states) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      setView(lng=112.621391, lat= -7.983908,zoom = 12.5)
    
    m<-m %>% addProviderTiles(providers$Stamen.Toner)
    m<-m %>% addPolygons()
    
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Kasus ODP: %g Orang<br/>Kasus PDP: %g Orang<br/>Kasus Positif: %g Orang
      <br/>Kasus Sembuh: %g <br/>Total: %g Orang",
      states$KELURAHAN,states$ODP,states$PDP,states$POSITIF,states$SEMBUH,states$JML_KASUS
    ) %>% lapply(htmltools::HTML)
    
    m <- m %>% addPolygons(
      fillColor = ~pal(JML_KASUS),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))
    m %>% addLegend(pal = pal, values = ~JML_KASUS, opacity = 0.7, title = "Total Kasus",
                    position = "bottomright")
    
    
  })
  output$table2 <- DT::renderDataTable(DT::datatable({
    Sys.sleep(letuce)
    datamala<-data.frame(states$KELURAHAN,states$KEC,states$ODP,states$PDP,states$POSITIF,states$SEMBUH,states$JML_KASUS)
    colnames(datamala)<-c("Kelurahan","Kecamatan","ODP","PDP","Positif","Sembuh","Total Kasus")
    datamala<-as.data.frame(datamala[order(datamala$`Total Kasus`,decreasing = T),])
    rownames(datamala)<-seq(1:nrow(datamala))
    datamala
  }))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}

shinyApp(ui, server)




