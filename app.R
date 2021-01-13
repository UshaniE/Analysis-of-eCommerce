# Load R packages
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(plotly)
library(ggplot2) # for creating graphs
library(scales) # for access break/formatting functions
library(gridExtra) # for arranging plots
library(lubridate) # for working with dates
library(shinyWidgets)

# Load data
combine_data <- read.csv("C:\\Users\\DELL\\Documents\\Ushani\\Data Science\\Uni Malaya\\Courses\\WQD 7001 - Principals of DS\\Assignment & Tutorials\\Group project\\Shiny\\App-Directory\\data\\combine_data.csv")

delivered <- combine_data %>%
  filter(order_status == "delivered") # delivered orders

# Define UI
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage(
                  "eCommerce",
                  
                  # tabPanel 1
                  tabPanel("Home",
                           #sidebarPanel(
                           #tags$img(height = 300, width =250,src = "Picture2.png")),
                           #setBackgroundColor("white")
                           sidebarPanel(
                             tags$br(),
                             h3("If you are a customer"),
                             column(12,offset=1,h4("Let us show the best selling products and product reviews")),
                             tags$br(),
                             tags$br(),
                             tags$hr(),
                             tags$br(),
                             h3("If you are a portal admin"),
                             column(12,offset=1,h4("Let us show the key insights on your business")),
                             tags$br(),
                             tags$br(),
                             tags$br(),
                             tags$br(),
                             tags$br(),
                             tags$br(),
                             tags$br(),
                           ),
                           mainPanel(
                             wellPanel(h1("Exploration of Customer Behaviour in eCommerce")),
                             fluidRow(
                               column(12,offset=1.5,tags$img(height = 300, width =600,src = "Picture1.png")),
                             )
                           )
                  ),
                  
                  
                  # tabPanel 2 (Customer) # -------------------------------------
                  tabPanel("Customer Portal",
                           sidebarPanel(
                             dateRangeInput('dateRange',
                                            label = 'Select Date Range', # Select Date range
                                            start = as.Date('2016-10-11'), end = as.Date('2018-09-03')),
                             
                             selectInput(inputId = "Product", label = "Select Product", choices = delivered$product_category_name_english)),
                           
                           mainPanel(
                             fluidRow(
                               column(8,plotOutput("GraphTopProducts",height = 300, width =500 )), ## Top Selling products
                             ),
                             fluidRow(
                               column(12,tableOutput("TableRatePrice")) ## Products ratings and price
                             ),
                           )), # tabpanel 2
                  
                  
                  # tabPanel 3 (Admin) #------------------------------------
                  tabPanel("Admin Portal",
                           sidebarPanel(
                             dateRangeInput('dateRange',
                                            label = 'Select Date Range', # Select Date range
                                            start = as.Date('2016-10-11'), end = as.Date('2018-09-03')),
                             radioButtons(inputId = "GraphType",label =  "Choose a  GMV Graph Type:",
                                          choices = c("Cumulative","Daily"), selected ="Cumulative"),
                           ),
                           
                           mainPanel(
                             fluidRow(
                               valueBoxOutput("GMV_fig"), ## valuebox for GMV
                               valueBoxOutput("AOV_fig"), ## valuebox For AOV
                               valueBoxOutput("A30_user")), ## valuebox For A30 user
                             
                             fluidRow(
                               column(12,plotOutput("GraphGMV",height = 150))), ## GMV Graph
                             
                             fluidRow(
                               column(6,plotOutput("GraphTopSelling",height = 300, width =400 )), ## Sales Performance based on products
                               column(6,plotOutput("GraphTopStates",height = 300, width =400 ))), ## Sales Performance based on state
                           ) 
                  ), # tabPanel3
                  
                  # tabPanel 4 #------------------------------------------------
                  tabPanel("Guide",
                           mainPanel(
                             h1("How to use this App"),
                             tags$br(),
                             h3("Home Tab"),
                             h5("This is the main page of this application."), 
                             
                             tags$br(),
                             h3("Customer Portal Tab"),
                             h5("You can visualize required information by selecting date range and required product."),
                             
                             column(12,offset=0.5,h4("1. Select Date Range")),
                             column(12,offset=0.75,h5("You can select a date range from 11th October 2016 to 03rd September 2018,which you wish to explore.
                                                      The Best selling graph shows sales of top 10 products with in the selected duration.")),
                             
                             column(12,offset=0.5,h4("2. Select Product Category")),
                             column(12,offset=0.75,h5("You can select the product category from dropdown menu.The table will show the Price, 
                                                      Review Score details of all products under the selected category.")),
                             
                             
                             tags$br(),
                             tags$br(),
                             h3("Admin Portal"),
                             column(12,offset=0.5,h4("1.Select Date Range")),
                             column(12,offset=0.75,h5("Following information will be displyed")),
                             
                             column(12,offset=1,h5("1.Gross Merchandise Value (GMV): Indicates the cumulative value of merchandise sold(delivered) less returns, not including shipping cost,during the selected time period")),
                             column(12,offset=1,h5("2.Average Order Value (AOV): Indicates the average value of GMV, during the selected time period")),
                             column(12,offset=1,h5("3.Active 30 (A30) Users: Indicates the average value of GMV, during the selected time period")),                      
                             column(12,offset=1,h5("4.GMV Graph : It shows the GMV over the selected period")),
                             column(12,offset=1,h5("5.Revenue Distribution among Products Graph: This shows the top 10 products, which contributes to revenue over the selected period")),
                             column(12,offset=1,h5("6.Revenue Distribution among States Graph: This shows the top 10 States, which contributes to revenue over the selected period")),
                             
                             column(12,offset=0.5,h4("2. Choose GMV Graph Type")),
                             column(12,offset=1,h5("The GMV graph type in default is set as cummalative. If you want to see daily variation tick 'Daily' ratio button. The GMV graph will change to daily")),
                             
                             
                           )
                  )# Navbar 1, tabPanel5        
                  
                ) # Navbarpage
)# fluidpage



# Define server function  
server <- function(input, output,session) {
  
  output$txtout <- renderText({
    paste( input$txt1,input$txt3, input$txt2, sep = " " )
  })
  
  # Customer Portal # --------------------------
  
  output$GraphTopProducts <- renderPlot({   ## Output for Graph Top Selling Products
    
    TopSellProduct <- delivered %>%
      filter(orderdeliveredDate <= input$dateRange[2] & orderdeliveredDate >= input$dateRange[1]) %>%
      group_by(product_category_name_english) %>%
      summarise(Product_Count = length(product_category_name_english))
    
    TopSellProduct %>%
      arrange(desc(Product_Count))%>% 
      slice(1:10)%>%
      ggplot(.,aes(x=reorder(product_category_name_english,Product_Count), y= Product_Count ))+
      geom_bar(stat = 'identity', fill = "cyan3") + theme(axis.text.x = element_text(size = 10)) + 
      theme(axis.text.y = element_text(size = 10)) +  scale_y_continuous(limits = c(0,14000)) +
      labs(title = "Best Selling Products") + labs(x = "Product Category Name")+
      geom_text(aes(label=comma(Product_Count)),hjust=0,color = "red", size=4)+ coord_flip()+
      theme(plot.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"))+ theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$TableRatePrice <- renderTable({   ## Output for Products rating price table
    
    ProductRate <- delivered %>%
      select(product_category_name_english,product_id,price,review_score,Category_score_review) %>%
      group_by(product_id)
    
    subset(ProductRate,ProductRate$product_category_name_english ==input$Product) 
    
  })
  
  # Admin Portal #------------------------------
  
  output$GMV_fig <- renderValueBox({  ## output for GMV 
    deliveredToDate <- delivered %>%
      filter(orderdeliveredDate <= input$dateRange[2] & orderdeliveredDate >= input$dateRange[1])  ## filter for order delivered during the date range
    valueBox(
      paste0("$", prettyNum(ceiling(sum(deliveredToDate$price)/1000), big.mark = ",")),
      "Total GMV ('000s)", color = "red")
  })
  
  output$AOV_fig <- renderValueBox({  ## output for AOV
    deliveredToDate <- delivered %>%
      filter(orderdeliveredDate <= input$dateRange[2] & orderdeliveredDate >= input$dateRange[1]) 
    valueBox(
      paste0("$", round(sum(deliveredToDate$price)/length(unique(deliveredToDate$order_id)),digits = 2)),
      "Average Order Value",color = "olive")
  })  
  
  output$A30_user <- renderValueBox({  ## output for A30
    UserToDate <- combine_data %>%
      filter(order_purchase_timestamp <= input$dateRange[2] & order_purchase_timestamp >= (input$dateRange[2] - 30)) ## filter for date range
    valueBox(
      prettyNum(length(unique(UserToDate$customer_id)),big.mark = ","),
      "A30 Users",color = "blue")
  })
  
  output$GraphGMV <- renderPlot({   ## Output for GMV_Plot
    
    dailyGMV <- delivered %>%   # Daily delivered order
      filter(orderdeliveredDate <= input$dateRange[2] & orderdeliveredDate >= input$dateRange[1]) %>%
      group_by(orderdeliveredDate) %>%
      summarize(price_sum = sum(price))
    
    if (input$GraphType == "Cumulative") {
      GMV <- cumsum(dailyGMV$price_sum)/1000
    }
    else {
      GMV <- (dailyGMV$price_sum)/1000
    }
    
    ggplot(data= dailyGMV, mapping = aes(x= orderdeliveredDate, y= GMV, group=1,
                                         text = paste('Date : ', orderdeliveredDate,
                                                      "<br>GMV: $ ", prettyNum(GMV * 1000, big.mark = ",")))) + 
      
      geom_line(color = "dodgerblue4") +
      labs(x = "Date", y = "GMV ($'000)" ) +
      theme(plot.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"))
    #ggplot(p,tooltip = c("text"))
  })
  
  # Sales Revenue based on product
  
  output$GraphTopSelling <- renderPlot({   ## Output for Graph Top Selling Products
    
    TopSellRevenue <- delivered %>%
      filter(orderdeliveredDate <= input$dateRange[2] & orderdeliveredDate >= input$dateRange[1]) %>%
      group_by(product_category_name_english) %>%
      summarise(ProductRevenue = sum(price))
    
    TopSellRevenue %>%
      arrange(desc(ProductRevenue))%>% 
      slice(1:10)%>% 
      ggplot(.,aes(x=reorder(product_category_name_english,ProductRevenue), y= ProductRevenue/1000 ))+
      geom_col(fill = "cyan3")+ theme(axis.text.x = element_text(size = 10))  + 
      theme(axis.text.y = element_text(size = 10)) +  scale_y_continuous(limits = c(0,2000))+
      labs(title = "Revenue distribution :Products") + labs(x = "Product Category Name", y = "Revenue ($ 000s)")+
      geom_text(aes(label=comma(ProductRevenue/1000)),hjust=0,color = "red", size=4)+ coord_flip()+
      theme(plot.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"))+ theme(plot.title = element_text(hjust = 0.1))
    
    
    
  })
  
  output$GraphTopStates <- renderPlot({   ## Output for Graph Top Revenue generating States
    
    TopRevenueState <- delivered %>%
      filter(orderdeliveredDate <= input$dateRange[2] & orderdeliveredDate >= input$dateRange[1]) %>%
      group_by(customer_state)%>%
      summarize(StateRevenue = sum(price))
    
    TopRevenueState  %>%
      arrange(desc(StateRevenue)) %>%
      slice(1:10) %>%
      ggplot(.,aes(x=reorder(customer_state,StateRevenue), y= StateRevenue/1000 ))+
      geom_bar(stat = 'identity', fill = "cyan3") + theme(axis.text.x = element_text(size = 10)) +
      theme(axis.text.y = element_text(size = 10)) + scale_y_continuous(limits = c(0,7000))+
      labs(title = "Revenue Distribution : States") + labs(x = "State", y = "Revenue ($ 000s)")+
      geom_text(aes(label=comma(StateRevenue/1000)),hjust=0,color = "red", size= 4)+
      theme(plot.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"))+ theme(plot.title = element_text(hjust = 0.25))+ coord_flip()
    
  })
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
