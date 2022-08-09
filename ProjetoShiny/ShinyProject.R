library(shiny)
library(shinythemes)
library(lifecontingencies)

ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage(
                  "Calculo de Anuidade",
                  tabPanel("Anuidades",
                           sidebarPanel(
                             tags$h3("Que tipo de Anuidade?")),
                           sidebarPanel(numericInput("P", h3("De quanto?"), value = 1000, min=0)),
                           sidebarPanel(
                             selectInput("J", h3("Taxa de Juros"), 
                                         choices = list("10% a.a" = 0.1, "11% a.a" = 0.11,
                                                        "12% a.a" = 0.12), selected = 1)),
                           sidebarPanel(
                             selectInput("H", h3("Antecipada ou Postecipada?"),
                                         choices = list("Antecipada" = 1, "Postecipada" = 2, selected = 1))),
                           sidebarPanel(
                             numericInput("A", 
                                          h3("Por quantos anos?"), 
                                          value = 1, min=0))),
                  
                  
                  # sidebarPanel
                  mainPanel(
                    h1("Resultado"),
                    h4("As caracteristicas dessa anuidade são:"),
                    verbatimTextOutput("txtout2"),
                    
                    h4("O custo dessa anuidade é:"),
                    verbatimTextOutput("txtout"),
                    
                    
                  ) # mainPanel
                  
                )
) # navbarPage
# fluidPage

# Define server function  
server <- function(input, output, session) {
  vals <- reactiveValues()
  observe({
    vals$J <- as.numeric(input$J)
    vals$A <- as.numeric(input$A)
    vals$P <- as.numeric(input$P)
  })
  observeEvent(input$H,{
    vals$X <- ifelse(input$H == 1, vals$X <- "immediate", vals$X <- "due")
    vals$Z <- ifelse(input$H == 1, vals$Z <- "Antecipada", vals$Z <- "Postecipada")
    
  })
  output$txtout <- renderText({ 
    paste( vals$P*annuity(i=vals$J, n=vals$A, type = vals$X ))
  })
  output$txtout2 <- renderText({ 
    paste("          Taxa de Juros escolhida:", vals$J*100, "% a.a", "
          Valor da anuidade:", vals$P, "
          Duração da anuidade:", vals$A, "anos", "
          Tipo da anuidade:", vals$Z)
  })
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)