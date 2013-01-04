library(shiny)

# Define the function that calls the FORTRAN routine.
vpe <- function(k,n,r,t,cl) {

         dyn.load('ci_following_neg_tests.so')
        
         returned_data = .Fortran('ci_following_neg_tests', 
                                 k=as.integer(k),
                                 n=as.integer(n),
                                 r=as.single(r),
                                 t=as.single(t),
                                 cl=as.single(cl),
                                 upperlim=single(1))

        return(returned_data$upperlim)
}

# Define server logic for vpe application.
shinyServer(function(input, output) {

  output$ci95 <- reactiveText(function() {
    #vpe(input$k,input$n,input$r,input$t,.95)
    sprintf("The 95%% confidence interval is [0-%6.3g]\n", vpe(input$k,input$n,input$r,input$t,.95))
  })

  output$ci99 <- reactiveText(function() {
    #vpe(input$k,input$n,input$r,input$t,.99)
    sprintf("The 99%% confidence interval is [0-%6.3g]\n", vpe(input$k,input$n,input$r,input$t,.99))
  })

})
