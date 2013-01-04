library(shiny)

# Define UI for VPE application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Verification of Pathogen Eradication in Animal Colonies"),

  sidebarPanel(
     numericInput("k", "Number of waves of testing (k):", 2, min=0),
     numericInput("n", "Number of consecutive negative tests per wave (n):", 25, min=0),
     numericInput("r", "Growth rate (r) (ex. 0.1):", 0.1, min=0, step=0.1),
     numericInput("t", "Time between testing waves (t):", 1, min=0),
    br(),
    a(href="vpe_notes.html","Click here for help")
  ),

  mainPanel(
    h4(textOutput("ci95")),
    h4(textOutput("ci99"))
  )
))
