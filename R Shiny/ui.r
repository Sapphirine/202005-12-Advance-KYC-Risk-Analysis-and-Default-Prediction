library(shinythemes)
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(tidyverse)
library(gghighlight)
library(RColorBrewer)


f_data <- read.csv("financial_data.csv")

# regroup
A_level <- c("A","A-","A+","AA","AA-","AAA","AA+")
B_level <- c("BBB","BBB-","BBB+","BB+","BB","BB-","B","B-","B+")
C_level <- c("CCC","CCC-","CCC+")
f_data$Rating <- NA
f_data$Rating[which(f_data$S.P.Rating %in% A_level)] <- "A"
f_data$Rating[which(f_data$S.P.Rating %in% B_level)] <- "B"
f_data$Rating[which(f_data$S.P.Rating %in% C_level)] <- "C"
f_data$industry <- as.character(f_data$industry)


industry <- as.matrix(unlist(lapply(strsplit(f_data$industry,"\\s|\\&|\\-|\\/|\\,|\\,"), function(l) l[[1]])),ncol=1)
head<-as.data.frame(table(industry)) %>% arrange(desc(Freq)) %>% head(5) %>% 
  select(industry) %>% unlist() %>% droplevels()
industry[which(!(industry %in% head))] <- "others"
f_data$industry <- industry
all_feature <- c("fye","sales.1","foreign.scr","fcf","ltm.sales.gr","ltm.ni.gr","ebitda.int","debt.t..bv","cash.int",
                 "interest","ret.on.cap","cfo.debt","debt.ebitda","cfo.interest","ebitda.int.1","ebit.sales","debt.t.asts",
                 "cfo.capex","sales.3yr.avg.growth","ebitda.gr","ni.growth","ni.5yr.gr")
all_group <- c("industry","Rating","country")


ui <- dashboardPage(
  dashboardHeader(title = "Risk Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(tags$embed("Introduction"), icon=icon("lightbulb"), tabName = "intro"),
      menuItem(tags$em("Requirement"), icon=icon("search"), tabName = "require"),
      menuItem(tags$em("Exploration"), icon=icon("search"), tabName = "explore"),
      menuItem(tags$em("Sentiment Analysis"), icon=icon("meh-blank"), tabName = "sentiment"),
      menuItem(tags$em("Enter Data"), icon=icon("sign-in"), tabName = "enter"),
      menuItem(tags$em("Upload and Download"), icon=icon("upload"),tabName = "upload")
    )
  ),  dashboardBody(
    
    tabItems(
      
      # intro
      tabItem(tabName = 'intro',
              tags$h1("Welcome to the Risk Analysis and Default Prediction Platform!"),
              tags$h4("Market Capitalization measures if the company is worth in the open market
              and also its prospects in market perception. Companies with a large market 
              capitalization are often well-known and have a good reputation for its services 
              or products. Investing on these companies would be more conservative and therefore, 
              will result in less risks."),
              tags$h4(
              "This platform is desigend to give you insights about
              potential risks when 
              you are making investments by predicting each company's market capitalization.
              When you have information about a company,
              such as country, industry, financial ratios .etc, you can enter the data in the 'Enter'
              tab and get the prediction for its market capitalization. If you have data for multiple
              companies, you can also upload your dataset as a .csv file download the results in the 'Upload and Download' tab."),
              tags$h4("Please make sure to look at the 'Requirement' tab to see what kind of information is needed for the predictions")
              
      ),
      
      # require
      tabItem(tabName = 'require',
              tags$h1("Requirements of Information Needed"),
              tags$h4("When you look at the 'Enter Data' tab, you may get confused on what does each option mean. Here are the descriptions of each selection bar."),
              DT::dataTableOutput("requirement_table"),
              # tags$h3("Note: if the data entered have missing values, the prediction function will NOT work. We are currently working on this function to allow for automatic missing value imputation.",
              #         style="color:blue;font-size:130%")
              
              
      ),
      
      # explore
      tabItem(tabName = 'explore',
                sidebarPanel(
                  selectInput("name",
                              label="Please select a company",
                              choices = f_data$name),
                  selectInput("feature",
                              label="Please select a feature",
                              choices = all_feature),
                  selectInput("group",
                              label="Group on",
                              choices = all_group),
                  selectizeInput("compare",
                                 label="Compare the feature with companies:",
                                 choices = f_data$name, multiple = TRUE)
                ),

                # Show a plot of the generated distribution
                mainPanel(
                  plotOutput("boxPlot"),
                  plotOutput("comparePlot")
                )
      ),
      
      # sentiment
      tabItem(tabName = 'sentiment',
              tags$h1("Sentiment Analysis"),
              DT::dataTableOutput("sentiment_table"),
              titlePanel("Realtime Sentiment Analysis"),
              
              # Sidebar  
              sidebarLayout(
                sidebarPanel(
                  selectInput("name",
                              label="Please select a company",
                              choices = f_data$name)
                ),
                
                # Show texts
                mainPanel(
                  textOutput("balabala"),
                  verbatimTextOutput("sentiment"),
                  textOutput("headline")
                )
              )
      ),
      
      # enter
      tabItem(tabName = 'enter',
              sidebarPanel(style = "overflow-y:scroll; max-height: 600px; position:relative;",
                    numericInput('sic', 'Enter sic:', 0),
                    selectInput("countryba", "Country:",
                                c("US" = "US",
                                  "other" = "other")),
                    numericInput("fye", "Enter fye:", 0),
                    numericInput("sales1", "Enter sales 1:", 0),
                    
                    numericInput("foreign_scr", "Enter foreign scr:", 0),
                    numericInput("fcf", "Enter fcf:", 0),
                    numericInput("ltm_sales_gr", "Enter ltm sales gr:", 0),
                    numericInput("ltm_ni_gr", "Enter ltm ni gr:", 0),
                    numericInput("net_income", "Enter net income:", 0),
                    numericInput("sales_3yr_avg_growth", "Enter sales 3yr avg growth:", 0),
                    numericInput("ebitda_gr", "Enter ebitda gr:", 0),
                    numericInput("ni_growth", "Enter ni growth:", 0),
                    numericInput("ni.5yr.gr", "Enter ni.5yr.gr:", 0),
                    numericInput("ebitda.int", "Enter ebitda.int:", 0),
                    numericInput("debt.t..bv", "Enter debt.t..bv:", 0),
                    numericInput("cash.int", "Enter cash.int:", 0),
                    numericInput("interest", "Enter interest:", 0),
                    numericInput("ret.on.cap", "Enter ret.on.cap:", 0),
                    numericInput("cfo.debt", "Enter cfo.debt:", 0),
                    numericInput("debt.ebitda", "Enter debt.ebitda:", 0),
                    numericInput("ebitda.int.1", "Enter ebitda.int.1:", 0),
                    numericInput("cfo.interest", "Enter cfo.interest:", 0),
                    numericInput("ebit.int", "Enter ebit.int:", 0),
                    numericInput("ebit.sales", "Enter ebit.sales:", 0),
                    numericInput("debt.t.asts", "Enter debt.t.asts:", 0),
                    numericInput("cfo.capex", "Enter cfo.capex:", 0)
              ),
              mainPanel(
              h3("This platform is desigend to give you predictions on the market cap of a company
                                  so that you can get insights on the risks of investment."),
              h3("By simply selecting the features
                                  or financial ratios, you can get the prediction results"),
              h3("Here you can see the results:"),
              uiOutput('table'))

              
      ),
      
      # upload & download
      tabItem(tabName = 'upload',
              tags$h1("Upload the data and Download the Predictions!"),
              # column(width = 4,
              #        fileInput('upload_data', h3('Upload the data here and see the preview of the predictions',
              #                                  style="color:blue;font-size:130%"),
              #                  multiple = FALSE,accept=c('.csv'))),
              div(style="display:inline-block",fileInput('upload_data', 'Upload datasets for predictions'), style="float:right"),
              tags$h3("Click the 'Download Predictions' button to download the predictions in csv format.",
                      style="font-weight: bold; font-size:100%"),
              div(style="display:inline-block",downloadButton('downloadData', 'Download Predictions'), style="float:right"),
              # column(width = 4,
              #        downloadButton("downloadData",
              #                       em('Download Predictions'))),
              tags$h3("See a preview of the predictions:",
                      style="font-weight: bold; font-size:100%"),
              DT::dataTableOutput("pred_table")
  
      )
      
      
      
    )
  )
)