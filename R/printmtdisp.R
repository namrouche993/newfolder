#' runaplication
#'
#' @export
#' @import shiny
#' @import datasets
runapplication<-function(){
  ui <- fluidPage(

    # Give the page a title
    titlePanel("Telephones by region en regions algerie essai bark"),

    # Generate a row with a sidebar
    sidebarLayout(

      # Define the sidebar with one input
      sidebarPanel(
        selectInput("region", "Region:",
                    choices=colnames(WorldPhones)),
        hr(),
        helpText("Data from AT&T (1961) The World's Telephones.")
      ),

      # Create a spot for the barplot
      mainPanel(
        plotOutput("phonePlot")
      )

    )

  )

  server <- function(input, output, session) {

    # Fill in the spot we created for a plot
    output$phonePlot <- renderPlot({

      # Render a barplot
      barplot(WorldPhones[,input$region]*1000,
              main=input$region,
              ylab="Number of Telephones",
              xlab="Year")
    })

  }

  shinyApp(ui, server)


  }


#' Title
#'
#' @export
#'
printmt0 <- function(){
  print(mt$disp)
}

#' print external data
#'
#' @export
#'
print_external_data<-function(){
  print(newfolder::dat)
}

#' print_rnorm_systime
#'
#' @export
#'
print_rnorm_systime <- function(){
  paste0(rnorm(5,0,5),"- - - - - ",Sys.time())
}




#                                           ##### THIS IS WRONG #####
# #' print external data                    ##### THIS IS WRONG #####
# #'                                        ##### THIS IS WRONG #####
# #' @export                                ##### THIS IS WRONG #####
# #'                                        ##### THIS IS WRONG #####
# print_external_data2<-function(){         ##### THIS IS WRONG #####
#   print(dat[1:2,1:2])                     ##### THIS IS WRONG #####
# }                                         ##### THIS IS WRONG #####
#                                           ##### THIS IS WRONG #####
#                                           ##### THIS IS WRONG #####
