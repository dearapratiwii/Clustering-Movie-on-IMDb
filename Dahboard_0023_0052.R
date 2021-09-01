library(tidyverse)
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(factoextra)
library(fpc)
library(NbClust)
library(Rtsne)
library(cluster)

data=read.csv("D:/data hasil preprocess 1.csv",sep=",",dec='.')
str(data)

data1=read.csv("D:/data hasil preprocess 1.csv",sep=",",dec='.')
data1$genre=as.integer(as.factor(data1$genre))
data1$directors=as.integer(as.factor(data1$directors))
data1$actor=as.integer(as.factor(data1$actor))
imdb = read.csv("D:/data hasil preprocess 1.csv")

gower_df <- daisy(imdb,
                  metric = "gower" ,
                  type = list(logratio = 2))

silhouette <- c()
silhouette = c(silhouette, NA)
for(i in 2:10){
  pam_clusters = pam(as.matrix(gower_df),
                     diss = TRUE,
                     k = i)
  silhouette = c(silhouette ,pam_clusters$silinfo$avg.width)
}


tsne_object <- Rtsne(gower_df, is_distance = TRUE)
pam_imdb = pam(gower_df, diss = TRUE, k = 8)
imdb[pam_imdb$mean, ]
pam_imdb
tsne_df <- tsne_object$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_imdb$clustering))
tsne_df

tema <-
  theme(text = element_text(family = "Verdana", color = "#444444")) +
  theme(plot.title = element_text(size = 24)) +
  theme(plot.subtitle = element_text(size = 18)) +
  theme(axis.title = element_text(size = 14)) +
  theme(axis.title.y = element_text(angle = 0, vjust = .5, margin = margin(r = 15))) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title.x = element_text(margin = margin(t = 20))) +
  theme(legend.title = element_blank())

header <- dashboardHeader(title = HTML(paste(icon("film"), "Dashboard Film tahun 2015-2019")))

sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              #First menu
              menuItem("About",tabName = "about",icon = icon("question-circle")),
              #Second menu
              menuItem("Data",tabName = "data",icon = icon("table")),
              #Third menu
              menuItem("Histogram",tabName="chart",icon = icon("bar-chart-o")),
              #Menu 4
              menuItem("Scatter plot",tabName = "scatt",icon = icon("spinner")),
              #Menu 5
              menuItem("Penentuan jumlah cluster",tabName = "clust",icon=icon("users")),
              menuItem("K-means clustering",tabName= "kmeans",icon=icon("users",lib="font-awesome")),
              menuItem("Clustered Data",tabName = "finaldata",icon=icon("table"))
  )
)

info.1 <- valueBox(
  value = mean(data$rating),
  subtitle = "rata-rata rating",
  icon = icon("chart"), width = 3)

body<-dashboardBody(
  tabItems(
    #Tab item for first menu
    tabItem(tabName = "about",h2("this is a dashboard informing about statistics of film")),
    #Tab item for second menu
    tabItem(tabName = "data",h2("Data"),DT::dataTableOutput("Tab1",width = "100%",height="auto")),
    #Tab item for third menu
    tabItem(tabName = "chart", fluidRow(sidebarPanel(width=2, checkboxGroupInput(
      inputId = "keterangan1",
      label = "genre",
      choices = c("Action"="Action","Adventure"="Adventure","Animation"="Animation","Biography"="Biography","Comedy"="Comedy","Crime"="Crime","Drama"="Drama","Family"="Family","Fantasy"="Fantasy","Horror"="Horror"),
      inline = TRUE,selected="Action")),box(
        title = "Run time",
        solidHeader = TRUE,
        plotOutput("grafik1", height = "300"),
        width = "5",
        status = "primary"),box(
          title = "Rating",
          solidHeader = TRUE,
          plotOutput("grafik2", height = "300"),
          width = "5",
          status = "primary"),box(
            title = "Gross",
            solidHeader = TRUE,
            plotOutput("grafik3", height = "300"),
            width = "5",
            status = "primary"),box(
              title = "Metascore",
              solidHeader = TRUE,
              plotOutput("grafik4", height = "300"),
              width = "5",
              status = "primary"),box(
                title = "Rating",
                solidHeader = TRUE,
                plotOutput("grafik5", height = "300"),
                width = "5",
                status = "primary")
      )),
    #tab item menu 4
    tabItem(tabName = "scatt",fluidRow(
      sidebarPanel(width=2,
      selectInput(inputId = "x",
        label = "Variabel X",choices = c("run time"="run_time","gross"="gross","rating"="rating","metascore"="metascore","votes"="votes"),
        selected = "run time"),
      selectInput(inputId = "y",
        label = "Variabel Y",choices = c("run time"="run_time","gross"="gross","rating"="rating","metascore"="metascore","votes"="votes"),
        selected = "gross")),
      mainPanel(position="right",box(
          title = "Scatter plot Variabel X dan Y",
          solidHeader = TRUE,
          plotOutput("grafik6", height = "300"),
          width = "12",
          status = "primary"),valueBoxOutput("box1",width=3))
      )),
    #tab item ke-5
    tabItem(tabName = "clust",h1("Number of Clusters"),fluidRow(
      box(h2("Elbow Method"),plotOutput("method1")),
      box(h2("Average silhouette method"),plotOutput("method2"))),fluidRow(  
        box(h2("Gap Statistic method"),plotOutput("method3")),
        box(h2("NbClust method"),plotOutput("method4"))
      )),
    #tab item ke-6
    tabItem(tabName = "kmeans",h1("K-means clustering"),
            fluidRow(
              box(plotOutput("clusterchart")),
              box(sliderInput("clustnum","Number of clusters",1,10,6))
            ),
            h2("K-means Statistics"),
            fluidRow(
              box(h3("Cluster Statistics"),htmlOutput("stat1"))
            )),
    #tab item ke-7
    tabItem(tabName = "finaldata",h1("Clustered Data"),fluidRow(column(5,tableOutput("clustdata"))))
    )
  )

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
  output$Tab1 <- DT::renderDataTable({
    data
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # GRAFIK INTERAKTIF 1
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$grafik1 <- renderPlot({
    data%>% filter(data$genre==input$keterangan1) %>%
      ggplot(data,mapping=aes(x=run_time,fill=genre)) +
      geom_histogram(binwidth = 0.5)+
      labs(x = "Run time") + theme_classic() +
      tema
  })
  
  output$grafik2 <- renderPlot({
    data%>% filter(data$genre==input$keterangan1) %>%
      ggplot(data,mapping=aes(x=rating,fill=genre)) +
      geom_histogram()+
      labs(x = "Rating") + theme_classic() +
      tema
  })
  
  output$grafik3 <- renderPlot({
    data%>% filter(data$genre==input$keterangan1) %>%
      ggplot(data,mapping=aes(x=gross,fill=genre)) +
      geom_histogram()+
      labs(x = "Gross") + theme_classic() +
      tema
  })
  output$grafik4 <- renderPlot({
    data%>% filter(data$genre==input$keterangan1) %>%
      ggplot(data,mapping=aes(x=metascore,fill=genre)) +
      geom_histogram()+
      labs(x = "Metascore") + theme_classic() +
      tema
  })
  output$grafik5 <- renderPlot({
    data%>% filter(data$genre==input$keterangan1) %>%
      ggplot(data,mapping=aes(x=votes,fill=genre)) +
      geom_histogram()+
      labs(x = "Jumlah Votes") + theme_classic() +
      tema
  })
  output$grafik6 <- renderPlot({
    req(input$x)
    req(input$y)
      ggplot(data,mapping=aes_string(x=paste0("`", input$x, "`"),y=paste0("`", input$y, "`")))+
      geom_jitter()+labs(x=input$x,y=input$y)+
      theme_classic() +
      tema
  })
  
  # Grafik Elbow
  output$method1 <- renderPlot({
    fviz_nbclust(data1[,2:10], pam, method = "wss") +
      geom_vline(xintercept = 2, linetype = 2)+
      labs(subtitle = "Elbow method")
  })
  # Grafik Silhouette
  output$method2 <- renderPlot({
    plot(1:10, silhouette,
         xlab = "Clusters",
         ylab = "Silhouette Width")
    lines(1:10, silhouette)
    silhouette
  })
  output$method3 <- renderPlot({
    set.seed(123)
    fviz_nbclust(data1[,2:10], kmeans, nstart = 25,  method = "gap_stat", nboot = 10, verbose = FALSE)+
      labs(subtitle = "Gap statistic method")
  })
  output$method4 <- renderPlot({
    NbClust(data1[,2:10],distance = "euclidean",
            min.nc = 4, max.nc = 10, method = "kmeans", index = "all")
  })
  output$clusterchart <- renderPlot({
    fviz_cluster((eclust(data1[,2:10], "kmeans", k = input$clustnum, nstart = 25, graph = FALSE)), geom = "point", ellipse.type = "norm",
                 palette = "jco", ggtheme = theme_minimal())
    
  })
  output$stat1 <- renderPrint({
    pam_imdb = pam(gower_df, diss = TRUE, k = input$clustnum)
    imdb[pam_imdb$medoids, ]
    summary(pam_imdb)
  })
  output$stat2 <- renderPrint({
    (cluster.stats(dist(data1[,2:10]),(eclust(data1[,2:10], "kmeans", k = input$clustnum, nstart = 25, graph = FALSE))$cluster))$dunn
  })
  output$clustdata <- renderTable(tsne_df)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MENGGABUNGKAN ELEMEN UI DAN SERVER
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shinyApp(ui = ui, server = server)