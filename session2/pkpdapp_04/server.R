# server.r (Reactive Server Objects) ------------------------------------------
server <- function(input, output) {

# Define reactive containing placeholder inputs
  Rinput <- reactive({
  # Define reactive object
    list(
      NID = 100,
      DOSEAMT = input$DOSEAMT,
      DOSEFRQ = as.integer(input$DOSEFRQ),  # convert from character to integer
      DOSEDUR = input$DOSEDUR,
      COVBWT = 70,
      COVSEX = 0
    )
  })
  
# Define reactive model object
  Rmod <- reactive({
  # Read inputs
    NID <- Rinput()$NID
  # Define reactive object
    if (NID > 1) {
      mod %>%
        omat(diag(c(0.1, 0.1, 0.2, 0.2))) %>%
        smat(matrix(0.1))
    } else {
      mod
    }
  })
  
# Define reactive simulation code
  Rsimdf <- reactive({
  # Read inputs  
    NID <- Rinput()$NID
    DOSEAMT <- Rinput()$DOSEAMT
    DOSEFRQ <- Rinput()$DOSEFRQ
    DOSEDUR <- Rinput()$DOSEDUR
    COVBWT <- Rinput()$COVBWT
    COVSEX <- Rinput()$COVSEX
    simmod <- Rmod()
  # Define reactive object
    dosetimes <- seq(0, 24*(DOSEDUR - 1), by = DOSEFRQ)
    simdf <- simmod %>%
      param(WT = COVBWT, SEX = COVSEX) %>%
      ev(amt = rep(DOSEAMT, length(dosetimes)), time = dosetimes) %>%
      carry_out(amt, evid) %>%
      mrgsim_df(nid = NID, start = 0, end = 24*(DOSEDUR + 1), delta = 0.5) %>%
      as_tibble()
  })
  
# Define PK plot output
  output$pkplot <- renderPlot({
  # Read inputs
    NID <- Rinput()$NID
    DOSEDUR <- Rinput()$DOSEDUR
    simdf <- Rsimdf()
  # Define output
    p <- NULL
    p <- ggplot(aes(x = time, y = CP), data = simdf)
    if (NID == 1) {
      p <- p + geom_line(colour = "blue", size = 1)
    } else {
      p <- p + stat_summary(geom = "line", fun.y = median, colour = "blue", size = 1)
      p <- p + stat_summary(geom = "ribbon", fun.ymin = ci90lo, fun.ymax = ci90hi,
        alpha = 0.4, fill = "blue")
    }
    p <- p + labs(x = "Time (hours)", y = "Concentration (ng/mL)")
    p <- p + scale_x_continuous(breaks = 0:(DOSEDUR + 1)*24)
    p
  })
}
