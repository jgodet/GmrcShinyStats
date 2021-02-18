# univarie.r
# written by JuG
# August 05 2019


#' Do something
#' @author JuG
#' @description
#' @param
#' @details
#' @examples
#'
#'
#' @return
#' @export



univarie <- function(){
  if(!require(shiny)){install.packages('shiny')}
  require(shiny)
  a <- fluidPage(

    navbarPage(title = NULL,id="descriptif",

               tabPanel("Informations BDD",
                        fluidPage(
                          titlePanel("Informations sur la base de données"),

                          navlistPanel(
                            "Menu",
                            tabPanel("Informations brutes",

                                     fluidRow(
                                       splitLayout(cellWidths = c("30%","70%"),
                                                   downloadButton('PDFdescriptif1o1',label="AIDE et Détails",class = "butt")
                                       )
                                     ),#finFluidRow
                                     tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
                                     verbatimTextOutput ("tableauBASE"),
                                     plotOutput('plotNAbase1')),
                            tabPanel("Données manquantes cumulées par variable",
                                     fluidRow(
                                       splitLayout(cellWidths = c("30%","70%"),
                                                   downloadButton('PDFdescriptif1o2',label="AIDE et Détails",class = "butt"),
                                                   h4("Faites attention s'il y a un filtre")
                                       )
                                     ),#finFluidRow

                                     tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
                                     h4("Descriptif cumulé des données manquantes par variable",align="center"),
                                     p("On représente ci-dessous les données manquantes en proportions par variable étudiée."),
                                     plotOutput('plotNAbase2'),
                                     tableOutput("tableNAbase2")),
                            tabPanel("Données manquantes cumulées par sujet",
                                     fluidRow(
                                       splitLayout(cellWidths = c("30%","70%"),
                                                   downloadButton('PDFdescriptif1o3',label="AIDE et Détails",class = "butt"),
                                                   h4("Faites attention s'il y a un filtre")
                                       )
                                     ),#finFluidRow

                                     tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
                                     h4("Descriptif cumulé des données manquantes par sujet",align="center"),
                                     p("On représente ci-dessous les données manquantes en proportions par sujet d'étude."),
                                     plotOutput("plotNAbase3"),
                                     tableOutput("tableNAbase3"))
                          )# fin navlistpanel

                        ) # fin fluipage

               ),# fin tabpanel




               ###################################################
               #####   PAGE 3.2     ###############################
               ###################################################



               tabPanel("Descriptif univarié",
                        fluidPage(
                          titlePanel("Analyses descriptives"),
                          sidebarLayout(
                            sidebarPanel(
                              uiOutput("propositions"),
                              radioButtons("qualiquanti", "Nature de la variable",
                                           c(Quantitative="quant", Qualitative="qual"),"quant"
                              )
                            ),
                            # Create a spot for the barplot
                            mainPanel(
                              # fluidRow(
                              #   # splitLayout(cellWidths = c("30%","70%"),
                              #   #             downloadButton('PDFdescriptif2',label="AIDE et Détails",class = "butt"),
                              #   #         h4("Faites attention s'il y a un filtre")
                              #   # )
                              # ),#finFluidRow

                              tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
                              fluidRow(
                                column(6,   textOutput("descriptifUni"),br(),  tableOutput("descvar")),
                                column(6,     plotOutput("plot1") , plotOutput("plot2") )
                              )# fin fluid row du main panel

                            )# fin MainPanel

                          )# fin sidebarlayout
                        )# fin fluidpage
               )# fin tabPanel 2

    )# fin navbarPage

  )

  return(a)
}

