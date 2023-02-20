############################## Variables and Functions #############################################

f_en_plot <- function(accrual,x,y) {

  ymax <- 1.08*max(accrual[,y],na.rm=TRUE)

  ggplot(data=accrual,mapping = aes(x = accrual[,x], y = accrual[,y])) +
    geom_col(fill='dodgerblue2')  +
    # geom_text(data=accrual, aes(group = total), vjust = -0.5,
    #           size = 3, position = position_dodge(0.1))  +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Enrolled Subjects") + xlab("") + ylim(c(0,ymax)) +
    scale_x_date(date_breaks = "3 month", date_labels = "%m/%Y") +
    theme(legend.title = element_blank(),
          legend.text = element_text(color = "cornflowerblue", size = 13),
          legend.position='right') +
    theme(text = element_text(size=rel(4)),
          strip.text.x = element_text(size=rel(4)),
          strip.text.y = element_text(size=rel(4))) +
    theme(plot.title = element_text(hjust = 0.5,size=14), # Put title in the middle of plot
          plot.caption = element_text(size=6, hjust=0)) # Make caption size smaller

}

############################## UI #############################################

en_UI <- function(id) {

  tabPanel("Enrollment",

          sidebarLayout(
             sidebarPanel(

               h3("Browse Enrollment:"),hr(),

               selectInput(NS(id,"all"),"All comer",choices=c("S0total","S1total","S2total","S3total"),
                          selected="S0total"),

               selectInput(NS(id,"wt"),"Wide Type",choices=c("S0WT","S1WT","S2WT","S3WT"),
                           selected="S0WT"),

               selectInput(NS(id,"mt"),"Mutant",choices=c("S0MT","S1MT","S2MT","S3MT"),
                           selected="S0MT"),

               # radioButtons(inputId=NS(id,"category"),
               #              label='Category (Total):',
               #              choices=c("Overall (S0)"='S0total',
               #                        "Overall (S1)"="S1total",
               #                        "Overall (S2)" = "S2total",
               #                        "Overall (S3)" = "S3total"
               #              ),
               #              selected="S0total"),



               # selectInput(NS(id,"category"),"Which population?",choices=colnames(accrual0),
               #             selected="S0total"),

               numericInput(NS(id,"n"), "Rows", value = 5, min = 1, step = 1),

               width=3),

             mainPanel(

               h3("Accrual by Month (Total, WildType and Mutant)"),hr(),
               plotOutput(NS(id,"hist_plot")),
               hr(),
               linebreaks(20),
               tableOutput(NS(id,"head"))

             )
  ))

}

############################## server #############################################

en_Server <- function(id) {

  moduleServer(id,function(input,output,session) {

    enr <- reactive({

      accrual <- accrual0 %>%
        mutate(month=1:nrow(.),
               date=as.Date(date,format="%m/%d/%Y"))

    })

    output$hist_plot <- renderPlot(width=1000, height=700,{

       A <- f_en_plot(enr(),"date",input$all)
       B <- f_en_plot(enr(),"date",input$wt)
       C <- f_en_plot(enr(),"date",input$mt)

       title <- ""
       grid.arrange(A, B, C, nrow=3,top=textGrob(title, gp=gpar(fontsize=20,font=3)))

    })


    output$head <- renderTable({

      head(enr(), input$n)

    })

  })
}
