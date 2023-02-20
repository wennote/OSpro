library('shiny')

OSapp <- function() {

    ui <- fluidPage(

      theme = shinythemes::shinytheme('cerulean'),

      navbarPage("OS Projection", id = 'SOMEID',


                 en_UI("OS_en"),
                 OS_UI("OS_pred")

      )

    )

    server <- function(input, output, session) {

      en_Server("OS_en")
      OS_Server("OS_pred")

  }
    shinyApp(ui,server)
}


