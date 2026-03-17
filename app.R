## app.R ##

# libraries ----
library(shiny)
library(shinydashboard)
library(bslib)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

# Prepare dataset ----
Budget_2025 <- read_excel("../mock_budget.xlsx")

Budget_2025$Data <- as.Date(paste0(as.character(Budget_2025$Anno),"/", as.character(Budget_2025$Mese_n),"/1"))
lev <- c("Stipendio", "Buoni pasto", "Altro", "Affitto", "Elettricità", "Internet", "Cellulare", "Spesa", "Pasti fuori", "Trasporti", "Svago", "Varie")
Budget_2025$Categoria <- factor(Budget_2025$Categoria, levels = lev)
Budget_2025$Transazione <- as.factor(Budget_2025$Transazione)
tot_uscite <- sum(Budget_2025$Valore[Budget_2025$Transazione=="Uscite"])
tot_entrate <- sum(Budget_2025$Valore[Budget_2025$Transazione=="Entrate"])

tot <- Budget_2025%>%
  select(Data, Transazione, Valore)%>%
  group_by(Data, Transazione)%>%
  summarise(sum = sum(Valore))

tot_saldo <- tot %>% pivot_wider(names_from = Transazione, values_from = sum) %>%
  mutate(saldo = Entrate - Uscite)

# Compute percentages
tot_saldo$fraction = tot_saldo$Uscite / tot_saldo$Entrate

# header ----
header <- dashboardHeader(title = "My Dashboard")

# sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("By month", tabName = "widgets", icon = icon("calendar"), badgeLabel = "NEW!", badgeColor = "green"),
    menuItem("By category", tabName = "category", icon = icon("chart-column"), badgeLabel = "NEW!", badgeColor = "green")
    
  )
)

# body ----
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
              box(title = "Full time series", plotOutput("full_timeseries"), width = 600)
            ),
            
            fluidRow(
              column(width = 4,
                     valueBox(tot_entrate-tot_uscite, "Cumulative revenues", icon = icon("piggy-bank"), color = "green", width = NULL),
                     valueBox((tot_entrate-tot_uscite)/(length(unique(Budget_2025$Data))), "Avg monthly revenues", icon = icon("money-bill-trend-up"), color = "yellow", width = NULL),
                     box(sliderInput("slider", "Select target:", 0, 2000, 1000), width = NULL, background = "black")
              ),
              
              box(title = "Revenues time series", plotOutput("revenue_barplot"))
              
            )
    ),
    
    tabItem(tabName = "widgets",
            fluidRow(
              column(width = 7,
                     box(selectInput("month", "Select a month to display:", Budget_2025$Data, selected = "none"), width = NULL, background = "black"),
                     infoBoxOutput("monthBox", width = 50)
                     
                     
              )
              ,
              box(title = "% of budget spent for the selected month", plotOutput("donut")),
              box(title = "Transactions by selected month", plotOutput("bymonth"))
              
            )
            
            
    ),
    
    tabItem(tabName = "category",
            fluidRow(
              box(selectInput("cat", "Select a category to display:", Budget_2025$Categoria, selected = "none"), width = NULL, background = "black"),
              infoBoxOutput("catBox"),
              
              box(title = "Expenses by selected category", plotOutput("bycat"))
            )
    )
  )
  
  
)

ui <- dashboardPage(header, sidebar, body)

# server ----
server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$full_timeseries <- renderPlot({
    ggplot(tot, aes(y=sum, x=Data, colour = Transazione, label=sum)) +
      geom_line(size=1) +
      geom_point() + 
      geom_text(nudge_y=50) +
      scale_color_brewer(palette = "Dark2") +
      labs(y="euros", x="date") +
      theme(legend.position = "none") +
      scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") + 
      theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8))
  })
  
  output$revenue_barplot <- renderPlot({
    ggplot(tot_saldo, aes(x=Data, y=saldo, label=saldo)) +
      geom_col(fill="gold2") +
      geom_text(nudge_y=50) +
      geom_hline(yintercept = input$slider, lwd = 1, lty = 3, color = "#1B9E77FF") +
      geom_hline(yintercept = mean(tot_saldo$saldo), lwd = 1, lty = 3) + 
      scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") + 
      theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8)) +
      labs(y="euros", x="date")
  })
  
  output$monthBox <- renderInfoBox({
    infoBox("You have selected ", paste(months(as.Date(input$month)), format(as.Date(input$month), format="%Y")), icon = icon("calendar-check"),
            color = "navy"
    )
  })
  
  output$bymonth <- renderPlot({
    ggplot(Budget_2025%>%filter(Data==input$month)%>%
             arrange(Transazione)
           , aes(y=Valore, x=Categoria, fill = Transazione, label = round(Valore))) +
      geom_col() + 
      theme(legend.position = "none") + 
      scale_fill_brewer(palette = "Dark2") +
      geom_text(nudge_y=60) +
      labs(y="euros", x="category") + 
      theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8))
    
  })
  
  output$avg <- renderTable(Budget_2025%>%filter(Categoria=="input$cat")%>%summarise(mean(Valore)))
  
  output$bycat <- renderPlot({
    ggplot(Budget_2025%>%filter(Categoria==input$cat), aes(y=Valore, x=Data, label = round(Valore))) +
      geom_col(fill = "steelblue") + 
      theme(legend.position = "none") + 
      geom_text(nudge_y=10) +
      labs(y="euros", x="date") +
      scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") + 
      theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8)) +
      geom_hline(yintercept = as.integer(Budget_2025%>%filter(Categoria==input$cat)%>%summarise(mean(Valore))), lwd = 1, lty = 3)
    
  })
  
  output$donut <- renderPlot({
    ggplot(tot_saldo%>%filter(Data==input$month)) +
      # Disegniamo la fetta delle Uscite (da 0 a fraction)
      geom_rect(aes(ymax = fraction, ymin = 0, xmax = 4, xmin = 3), fill = "#D95F02FF") +
      # Disegniamo la fetta del Saldo (da fraction a 1)
      geom_rect(aes(ymax = 1, ymin = fraction, xmax = 4, xmin = 3), fill = "#1B9E77FF") +
      
      coord_polar(theta = "y") +
      xlim(c(1, 4)) +
      theme_void() +
      
      # Aggiungiamo la percentuale nel buco
      annotate("text", x = 1, y = 0, size = 12, fontface = "bold",
               label = paste0(round(tot_saldo%>%filter(Data==input$month)%>%pull(fraction) * 100, 1), "%"), color = "#D95F02FF") +
      
      # Aggiungiamo un'etichetta descrittiva sotto la percentuale
      annotate("text", x = 1, y = 0, size = 4.5, vjust = 3.5,
               label = "Budget spent", color = "grey30") 
  })
  
}

shinyApp(ui, server)


