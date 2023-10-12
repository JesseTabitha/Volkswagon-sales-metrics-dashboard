library(shiny)
library(shinydashboard)
library(ggplot2)
library(readxl)
library(GGally)
library(plotrix)


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)
server <- function(input, output) { }
shinyApp(ui, server)

total_sales<-read.csv("C:/Users/ashwin s/Desktop/Ashwin/R PROJECT/salesrev_csv.csv")
total_sales_df<-data.frame(total_sales)
total_sales_df

sales1<-read.csv("C:/Users/ashwin s/Desktop/Ashwin/R PROJECT/CWSALES.csv")
sales_df<-data.frame(sales1)
sales_df
sum(sales_df$TOTAL)

ratings1<-read.csv("C:/Users/ashwin s/Desktop/Ashwin/R PROJECT/Scrapped_Car_Reviews_Volkswagen.csv")
ratings_df<-data.frame(ratings1)

ratings_Clean<-na.omit(ratings_df)
options(pillar.print_max = Inf)
ratings_df
ratings_Clean

#ratings mean
mean(ratings_Clean$Rating)

#Explorative Data Analysis Data set
df=read_excel("C:/Users/ashwin s/Desktop/Ashwin/R PROJECT/Sales Metrics VW AG.xlsx")
df

#Earnings before and after tax coded data 

regions=c("Earnings  before tax", "Earnings  after tax")
Years=c(2020,2019,2018,2017,2016)
Values <- matrix(c(11667,18356,15643,13673,7292,8824,	14029,	12153,	11463,	5379), nrow = 2, ncol = 5, byrow = TRUE)


#sales revenue and gross profit coded data
regions=c("Sales Revenue", "Gross Profit")
Years=c(2020,2019,2018,2017,2016)
Values <- matrix(c(222884,252632,235849,229550,217267,38947,49142,46350,43549,40997), nrow = 2, ncol = 5, byrow = TRUE)

#YEARLY EMPLOYEE  DATA
empcount<-read.csv("C:/Users/ashwin s/Desktop/Ashwin/R PROJECT/VW YEARLY EMPLOYEES.csv")
empcount_df<-data.frame(empcount)

#Delivery Study For Revenue Study
del=read_excel("C:/Users/ashwin s/Desktop/Ashwin/R PROJECT/vw_sale3.xlsx")
del
attach(del)

#data for shares details
years<-c(2020,2019,2018,2017,2016)
ordinaryshares<-c(295090,295090,295090,295090,295090)
preferredshares<-c(206205,206205,206205,206205,206205)
turnoverOfOrdShares<-c(3.1,3.3,4.3,3.5,3.3)
grossLiquidity<-c(66078,53428,39626,31556,26451)
ratioOfMarketCapitalisationToEquity<-c(0.64,0.72,0.60,0.77)
Priceperearnings<-c(10.2,6.5,5.9,7.5,13.4)

price_equity<-data.frame(years,Priceperearnings)
price_equity

gross_liquid<-data.frame(grossLiquidity,years)
gross_liquid

turnover<-data.frame(turnoverOfOrdShares,years)
turnover

prod=read_excel("C:/Users/ashwin s/Desktop/Ashwin/R PROJECT/vw_sale7.xlsx", sheet = 3)
print(prod, n=20)
attach(prod)



#UI desgin dashboard
header <- dashboardHeader(title = "Automotive Sales Metrics")  
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Performance Study", tabName = "performance", icon = icon("dashboard")),
    menuItem("Revenue study", icon = icon("dollar"), tabName = "Revenue"),
    menuItem("VW Official", icon = icon("send",lib='glyphicon'), 
             href = "https://annualreport2021.volkswagenag.com/group-management-report/volkswagen-ag/annual-result.html")
    
    )
)



frow1 <- fluidRow(
  theme=bslib::bs_theme(),
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)
frow2 <- fluidRow( 
  box(
    theme=bslib::bs_theme(),
    title = "Yearly Units Sold"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("ScatterPlot", height = "300px")
  )
  ,box(
    title = "Maxinmum product rating"
    ,theme=bslib::bs_theme()
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("histplot", height = "300px")
  )
  ,box(
    title = "Volkswagen AG Annual Number of Employees"
    ,theme=bslib::bs_theme()
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("yearlyemp", height = "300px")
  )
  ,box(
    title = "Turn Over Per Share"
    ,theme=bslib::bs_theme()
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("turnover", height = "300px")
  )
  ,box(
    title = "Price Per Earning"
    ,status = "primary"
    ,theme=bslib::bs_theme()
    ,solidHeader = TRUE 
    ,collapsible = TRUE ,
    plotOutput("priceearning")
  ),
  tabBox(
    tabPanel(
      title = "Model Production 2020"
      ,plotOutput("production1")
    ),
    tabPanel(
      title = "Model Production 2019"
      ,plotOutput("production2")
    )
  )
    
)
frow3<-fluidRow(
    box(
      title = "Earning Before And After Tax "
      ,status = "primary"
      ,theme=bslib::bs_theme()
      ,solidHeader = TRUE 
      ,collapsible = TRUE ,
      plotOutput("EarningPlot")
    ),
  box(
      title = "Sales RevenueAnd Gross Profit"
      ,status = "primary"
      ,theme=bslib::bs_theme()
      ,solidHeader = TRUE 
      ,collapsible = TRUE ,
      plotOutput("SalesRevenue")
    ),
  box(
    title = "Vehicle deliveries in different parts of the world"
    ,status = "primary"
    ,theme=bslib::bs_theme()
    ,solidHeader = TRUE 
    ,collapsible = TRUE ,
    plotOutput("delivery")
  )
  , box(
    title = "Yearly Gross Liquidity"
    ,status = "primary"
    ,theme=bslib::bs_theme()
    ,solidHeader = TRUE 
    ,collapsible = TRUE ,
    plotOutput("grossliquid")
  )
)
  

# combine the two fluid rows to make the body
body <- dashboardBody(
  tabItems(
    tabItem(
      h1("Performance Study"),
      tabName="performance",
      frow1,
      frow2),
    tabItem(
      h1("Revenue Study"),
      tabName="Revenue",
      fluidRow(
        valueBoxOutput("value4")
        ,valueBoxOutput("value5")
        ,valueBoxOutput("value6")
      ),
      fluidRow(
        box(
          title = "Explorative Data Analysis"
          ,width = 14
          ,theme=bslib::bs_theme()
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE ,
          plotOutput("edaplot")
        )
      ),
      frow3
  )
)
)


#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Sales Metrics Volkswagen AG ', header, sidebar, body, skin='red')


# create the server functions for the dashboard  
server <- function(input, output) { 
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(sum(sales_df$TOTAL), format="d", big.mark=','),
      paste('Top Unit Sold in a Year:',max(sales_df$TOTAL))
      ,icon = icon("car")
      ,color = "purple")  
  })
  output$value3 <- renderValueBox({ 
    valueBox(
      formatC(max(empcount_df$Employees), format="d", big.mark=',')
      ,paste('Annual Employee:',max(empcount_df$Year))
      ,icon = (icon("user"))
      ,color = "green")  
  })
  output$value2 <- renderValueBox({
    valueBox(
      formatC(mean(ratings_Clean$Rating), format="f", big.mark=',')
      ,paste('Top Model rating:',max(ratings_Clean$Rating))
      ,icon = icon("fa-solid fa-thumbs-up")
      ,color = "yellow")   
  })
  
  output$value4 <- renderValueBox({
    valueBox(
      formatC(sum(df$`Return on investment (ROI)5`), format="f", big.mark=','),
      'Total ROI:'
      ,icon = icon("dollar")
      ,color = "purple")  
  })
  output$value5 <- renderValueBox({ 
    valueBox(
      formatC(max(df$`Equity ratio(Auotomotive group)`), format="d", big.mark=',')
      ,"Equity Ratio"
      ,icon = icon("signal", lib = "glyphicon")
      ,color = "green")  
  })
  output$value6 <- renderValueBox({
    valueBox(
      formatC(max(del$`Delivery count`), format="d", big.mark=',')
      ,"Total Delivery in Region"
      ,icon = icon("car")
      ,color = "yellow")   
  })
  
  #creating the plotOutput content
  output$ScatterPlot <- renderPlot({
    ggplot(total_sales_df,aes(x=Year,y=Sales))+ggtitle("Yearly Product Sales")+geom_point(aes(color = factor(Sales))) +
      stat_smooth(method = "lm",
                  col = "#C42126", se = FALSE, linewidth= 1
      )
  })

  output$histplot <- renderPlot({
    hist(ratings_df$Rating,main="Rating",xlab = "rating",ylab="No. of Rating",col = "yellow",border = "blue",prob = TRUE)
  })
  
  output$edaplot<-renderPlot({
    ggpairs(df, columns=2:8,  mapping=NULL  ,upper = list(continuous = wrap("cor", size = 3)))  
  })
  
  output$EarningPlot<-renderPlot({
    tax1 <- data.frame(Earnings = c(11667,8824,18356,14029,15643,12153,13673,11463,7292,5379), 
                       Years = rep(c("2020", "2019",
                                     "2018","2017","2016"),
                                   each = 2),
                       TAX = LETTERS[1:2])
    
    
    ggplot(tax1,aes(x = Years, y =Earnings, fill = TAX)) +
      geom_bar(stat = "identity", position="dodge")+
      ggtitle("Earning Before And After Tax")
 } )
  
  output$SalesRevenue<-renderPlot({
    barplot(Values,names.arg=Years,xlab="Sales Revenue and Gross Profit over the Years",ylab="Revenue",
            main="Sales Revenue and Gross Profit chart",col = c("red","pink"),border="black")
    legend("topleft", regions, cex = 0.6, fill = c("red","pink"))
    
  })
  
  output$delivery<-renderPlot({
    pie3D(Percent_count,labels=c("Western Europe(31.3%)","Central and Eastern Europe(7.16%)", "Other European Markets(3.05%)","North America(8.60%)","South America(4.83%)","Asia Pacific(45.1%)"),labelcex= 0.9,height=0.15, theta= 1.3,border = "white",edges=200,radius=1, col=hcl.colors(length(Percent_count), "Spectral"), main="Vehicle deliveries in different parts of the world",explode=0.2)
    
  })
  
  output$yearlyemp<-renderPlot({
    ggplot(data=empcount_df, aes(x=Year, y=Employees, group=1)) +
      geom_line(color="red")+
      geom_point()
  })
  
  output$turnover<-renderPlot({
    ggplot(turnover, aes(x=years, y= turnoverOfOrdShares)) +
      geom_bar(stat="identity",fill="steelblue")+
      theme_minimal()+
      ggtitle("Turnover Of Ordered Shares")
  })

  output$grossliquid<-renderPlot({
    ggplot(gross_liquid, aes(x=years, y=grossLiquidity )) +
      geom_bar(stat="identity", fill="steelblue")+
      theme_minimal()+
      ggtitle("Gross Liquidity")
  })
  output$priceearning<-renderPlot({
   
    ggplot(price_equity, aes(x=years, y=Priceperearnings)) +
      geom_segment( aes(x=years, xend=dividenddevlopment, y=0, yend=Priceperearnings)) +
      geom_point(size=4)+
      ggtitle("Price Per Earning")
  })
  
  output$production1<-renderPlot({
    
    ggplot(data= prod, aes(x = year2020,y=Units, fill = Units)) + 
      geom_bar(stat="identity",position=position_dodge(1))+
      ggtitle("Production of units 2020")
  })
  
  output$production2<-renderPlot({
    ggplot(data= prod, aes(x = year2019,y=Units, fill = Units)) + 
      geom_bar(stat="identity",position=position_dodge(1))+
      ggtitle(" Prodcution Of Units 2019")
  })
  
}


shinyApp(ui, server)