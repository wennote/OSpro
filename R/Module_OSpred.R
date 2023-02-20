library('shiny')
############################## Variables and Functions #############################################


f_ss_sim     <-  function(Nsim = 10,
                          N=500,
                          req.events = 360,
                          snap=27.5,
                          med.C = 25,
                          hr = 0.75,
                          en=en,
                          lossFU.prob = 0.05,
                          lossFU.time = 12)    {


  nC <- nT <- N/2

  hzC <- log(2)/med.C
  hzT <- log(2)/(med.C/hr)

  dsNew <- list()

  cutoff <- rep(0,Nsim)

  OS <- rep(0,Nsim)
  an.date <- matrix(rep(0,3*Nsim), ncol=3)
  accur.dur <- rep(0,Nsim)

  for (j in 1:Nsim) {
    # Treatment Assignment
    group <- rbinom(N, 1, nT /N)

    # simulate survival time - Exponential
    surv.T.T <-  rexp(N, rate = hzT) # group code = 1
    surv.T.C  <- rexp(N, rate = hzC) # group code = 0

    # simulate enrollment time - cumulative sampling from Exponential
    # enroll.T <- cumsum(rexp(nT + nC, enroll.rate))
    # enroll.T1 <- cumsum(rexp(N, en[1]))
    # enroll.T2 <- 8 + cumsum(rexp(N, en[2]))
    # enroll.T3 <- 23 + cumsum(rexp(N, en[3]))
    # enroll.T4 <- 39 + cumsum(rexp(N, en[4]))
    # enroll.T5 <- 40 + cumsum(rexp(N, en[4]))
    #
    # enroll.T <- c(enroll.T1[enroll.T1<9], enroll.T2[enroll.T2<24], enroll.T3[enroll.T3<40],
    #               enroll.T4[enroll.T4<41], enroll.T5)[1:N]

    temp <- function(x,y) {

      vec <- x + cumsum(rexp(N,y))

      vec[vec < x+1]

    }

    len <- map2(en[,1],en[,2], temp)

    enroll.T <- unlist(len)[1:N]

    # simulate loss to FU time - Exponential with rate
    lossFU.T <- rexp(N, rate = -log(1 - lossFU.prob) / lossFU.time)

    # put all together
    data.df <- data.frame(surv.T.T,surv.T.C,enroll.T,lossFU.T,group,
                          surv.T = (  (group == 1) * surv.T.T  +
                                        (group == 0) * surv.T.C))

    data.df <-
      cbind(
        data.df,
        a = enroll.T + data.df$surv.T,
        # study day to event
        b = enroll.T + lossFU.T,
        # study day to loss FU
        c = ((enroll.T + data.df$surv.T) <= (enroll.T + lossFU.T))*(enroll.T + data.df$surv.T) +
          ((enroll.T + data.df$surv.T) > (enroll.T + lossFU.T))*999999,
        # study day of event
        d = (data.df$surv.T <= lossFU.T) * 1
      )


    data.df$cutoffdt <- sort(data.df$c)[req.events]
    num.OS <- length(data.df$c[data.df$c < snap])

    ### interm analysis date

    an.date[j,] <- sort(data.df$c)[c(100,200,250)]

    dsNew[[j]] <- data.df
    cutoff[j] <- data.df$cutoffdt[1]
    OS[j] <- num.OS
    accur.dur[j] <- enroll.T[N]
  }

  med.date <- apply(an.date,2,median)
  accru.dat <- median(accur.dur)
  accru3.dat <- accru.dat + 3

  list(dsNew=dsNew, cutoff=median(cutoff), OS=median(OS),
       dat=c(accru.dat, accru3.dat,med.date))

}

### f_OS_pred ###

f_OS_pred <- function(Nsim=20,N=500,hr=0.694,enroll=enroll,req.events=360,drop=0.05,category='Overall',dur=120){


  OS <- function(x){
    X <-  f_ss_sim(Nsim=Nsim,N=N,en=enroll,lossFU.prob=drop,req.events=req.events,hr=hr,snap=x)
    X$OS
  }

  # first.dt <- as.Date("09/01/2022", format="%m/%d/%Y")

  #  mon <- first.dt + seq(0,dur,1)*30.4375

  mon <- seq(0,dur,1)

  A <- sapply(mon,OS)

  data.frame(cat=category,time=mon, OS=A)

}

first.dt=as.Date("09/01/2022", format="%m/%d/%Y")

mon <- first.dt + seq(0,120,1)*30.4375


###############################################  UI ###############################################

OS_UI <- function(id) {

  tabPanel("OS Projection",

          sidebarLayout(
            sidebarPanel(
             h3("OS Plots:"),hr(),

             sliderInput(NS(id,"hr_total"), "HR (All Comers):",
                         min = 0.2, max = 1.0,
                         value = 0.694, step = 0.001),

            sliderInput(NS(id,"hr_WT"), "HR (Wild Type):",
                                        min = 0.2, max = 1.0,
                                        value = 0.75, step = 0.001),
            sliderInput(NS(id,"hr_MT"), "HR (Mutant Type):",
                        min = 0.2, max = 1.0,
                        value = 0.562, step = 0.001),

            sliderInput(NS(id,"dropout"), "Drop off rate per year:",
                        min = 0.01, max = 0.21,
                        value = 0.05, step = 0.01),

              numericInput(NS(id,"num"), "Simulation Times", value = 5, min = 1, step = 1),

            radioButtons(inputId =NS(id,"axis"),
                         label   = "By Months or Calendar date?",
                         choices=c("Months since FPFV"="month",
                                   "Calendar Date"="cal"

                         ), inline=TRUE,
                         selected=   "cal" ),


           # selectInput(NS(id,"date_choice"),"Which Date?",choices=mon,selected='09/01/2022',multiple = TRUE),

              width=3),

            mainPanel(
             h3("Simulated Trajectory of OS"),hr(),
             plotOutput(NS(id,"OS_plot"),click=NS(id,"plot_click")),
             hr(),

             linebreaks(8),

            # verbatimTextOutput(NS(id,"info")),

             h4("Projected Events by Biomarker with a Click:"),hr(),

             htmlOutput(NS(id,"pred_tab"))

            )
          )
    )
}

###############################################  server ###############################################

  OS_Server <- function(id) {

    moduleServer(id,function(input,output,session) {

    en <- reactive({

          en_1 <- accrual[,c("month","total")]
          en_2 <- accrual[,c("month","WT")]
          en_3 <- accrual[,c("month","MT")]

            Nsim <- as.numeric(input$num)

            overall <- f_OS_pred(Nsim=Nsim,N=500,hr=input$hr_total,enroll=en_1,drop=input$dropout, req.events=360,category='Overall')
            WildType <- f_OS_pred(Nsim=Nsim,N=350,hr=input$hr_WT,enroll=en_2, drop=input$dropout, req.events=200,category='Wild Type')
            Mutant <- f_OS_pred(Nsim=Nsim,N=150,hr=input$hr_MT,enroll=en_3, drop=input$dropout, req.events=160,category='Mutant')

            overall2 <- WildType %>%
              mutate(MTOS=OS) %>%
              select(time,MTOS) %>%
              left_join(Mutant,by="time") %>%
              mutate(OS=OS + MTOS,
                     cat="WT+MT") %>%
              select(cat,time,OS)

            bind_rows(overall,WildType,Mutant,overall2)

          })

          output$OS_plot <- renderPlot(width=1000, height=600,{

            first.dt=as.Date("09/01/2022", format="%m/%d/%Y")

            # if (is.null(input$click)) click0 <- NULL
            #
            # click0 <- input$click$x

            if (input$axis=='cal') {

              comb <- en() %>% mutate(time=first.dt + time*30.4375)

              ggplot(data=comb, aes(time , OS )) +
                geom_line(aes(color = cat)) +
                scale_x_date(date_breaks = "6 month", date_labels = "%m/%Y") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                geom_hline(yintercept=360, linetype="dashed",color='red') +
                #annotate('text', x = 20800, y = 380, label = "Required Overall Events", color="cornflowerblue", size=5,fontface = 'bold')
                geom_hline(yintercept=100, linetype="dashed") +
                #geom_hline(xintercept=click0, linetype="dashed") +
                #   annotate('text', x = 15, y = 120, label = "Required MT Events", color="cornflowerblue", size=5,fontface = 'bold') +
                geom_hline(yintercept=234, linetype="dashed",color='blue') +
                #   annotate('text', x = 15, y = 250, label = "Required WT Events", color="cornflowerblue", size=5,fontface = 'bold')+
                scale_color_manual(values=c('cornflowerblue','aquamarine3','blue4','red')) +
                scale_y_continuous(expand = c(0, 0),limits=c(0, 400), breaks = seq(0, 400, by = 50)) +
                labs(x="",
                     y="# OS events", size=2, title="",caption="") +
                theme(axis.title.x = element_text(size = 16),
                      axis.text.x = element_text(size = 14),
                      axis.text.y = element_text(size = 14),
                      axis.title.y = element_text(size = 16)) +
                theme(legend.title = element_blank(),
                      legend.text = element_text(face="bold", size = 18),
                      legend.position="bottom")
            }

            else {

              comb <- en()

            ggplot(data=comb, aes(time , OS )) +
              geom_line(aes(color = cat)) +
              scale_x_continuous(expand = c(0, 0),limits=c(0, 130), breaks = seq(0, 120, by = 12)) +
             # scale_x_date(date_breaks = "6 month", date_labels = "%m/%Y") +
             # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            geom_hline(yintercept=360, linetype="dashed",color='red') +
            #annotate('text', x = 20800, y = 380, label = "Required Overall Events", color="cornflowerblue", size=5,fontface = 'bold')
              geom_hline(yintercept=100, linetype="dashed") +
            #   annotate('text', x = 15, y = 120, label = "Required MT Events", color="cornflowerblue", size=5,fontface = 'bold') +
             geom_hline(yintercept=234, linetype="dashed",color='blue') +
             # geom_hline(xintercept=click0, linetype="dashed") +
            #   annotate('text', x = 15, y = 250, label = "Required WT Events", color="cornflowerblue", size=5,fontface = 'bold')+
              scale_color_manual(values=c('cornflowerblue','aquamarine3','blue4','red')) +
             scale_y_continuous(expand = c(0, 0),limits=c(0, 400), breaks = seq(0, 400, by = 50)) +
              labs(x="",
                   y="# OS events", size=2, title="",caption="") +
              theme(axis.title.x = element_text(size = 16),
                    axis.text.x = element_text(size = 14),
                    axis.text.y = element_text(size = 14),
                    axis.title.y = element_text(size = 16)) +
              theme(legend.title = element_blank(),
                    legend.text = element_text(face="bold", size = 18),
                    legend.position="bottom")
            }


          })


          output$pred_tab <- renderUI({

            first.dt=as.Date("09/01/2022", format="%m/%d/%Y")

            df <- en() %>%
              mutate(Calendar=first.dt + time*30.4375,
                     Time=time,
                     OSr=round(OS)) %>%
              select(Calendar,Time,cat,OSr) %>%
              spread(cat,OSr) %>%
              select(Calendar,Time,everything())

            if (is.null(input$plot_click)) return()

            if (input$axis !='cal')

            df <- df %>%
              filter( Time==round(input$plot_click$x))

            # col_label=c("Prior Infection","Vaccine","Strain",
            #             "Visit","N","GMT","GMFR","GMR (95% CI)")
            # tableLabel <- setNames(names(df),col_label)

            getClinDT(
              filter = "none",
              data = df,
              #rowGroupVar="TRT01P",
              #columnsWidth = c(2,2,1,1,1,1,2),
              width = "600px" ,# change dimension table,
              pageLength = Inf,
              #colnames=tableLabel,
              buttons = getClinDTButtons(type = c("csv", "excel", "pdf"))
            )


          })

          # output$info <- renderPrint({
          #
          #   req(input$plot_click)
          #   x <- round(input$plot_click$x)
          #
          #   if (input$axis=='cal') x <- as.character(as_date(x))
          #   y <- round(input$plot_click$y)
          #
          #   cat("[", x, ", ", y, "]", sep = "")
          #
          # })

    })

  }

#f_num_date(20343)


