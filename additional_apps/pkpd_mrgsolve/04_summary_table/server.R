# server.r (Reactive Server Objects) ------------------------------------------
# All reactive components of your application are specified here. This is any
#   code from your original script that depends on the inputs that were
#   identified.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
app_server <- function(input, output) {
  
# Define simulation code in a reactive function. Like renderPlot this runs the
#   code inside { } but instead assigns it to a "reactive object" (i.e. Rsimdf).
#   This object can be called like a function with no arguments, Rsimdf(), and
#   will return the result of the code within the { }. In the case of Rsimdf(),
#   this will return the simdf object.
  Rsimdf <- reactive({
  # Application inputs
    NID <- 1
    DOSEAMT <- input$DOSEAMT
    DOSEFRQ <- as.double(input$DOSEFRQ)  # convert from character
    DOSEDUR <- input$DOSEDUR
    COVBWT <- input$COVBWT
    COVSEX <- as.double(input$COVSEX)
  # Modify Simulation Conditions (reactive) 
    if (NID > 1) {
      simmod <- mod %>%
        omat(diag(c(0.1, 0.1, 0.2, 0.2))) %>%
        smat(matrix(0.1))
    } else {
      simmod <- mod
    }
  # Simulation Code (reactive)
    dosetimes <- seq(0, 24*(DOSEDUR), by = DOSEFRQ)
    simdf <- simmod %>%
      param(WT = COVBWT, SEX = COVSEX) %>%
      ev(amt = rep(DOSEAMT, length(dosetimes)), time = dosetimes) %>%
      carry_out(amt, evid) %>%
      mrgsim_df(nid = NID, start = 0, end = 24*(DOSEDUR + 1), delta = 0.5) %>%
      as_tibble()
  })
  
# The renderPlot code remains mostly the same, with the simulation code removed
#   instead being called in using Rsimdf(). By separating the simulation code
#   from each output that needs it, it will only be run once instead of three
#   times (once each for output$pkplot, output$pksdtable and output$pksstable).
#   Running the simulation code three times wouldn't actually be an issue for
#   the application in it's current state, but once the ability to simulate
#   multiple individuals is implemented these types of optimisations become very
#   important.
  output$pkplot <- renderPlot({
  # Application inputs
    NID <- 1
    DOSEDUR <- input$DOSEDUR
  # Server objects
    simdf <- Rsimdf()
  # Script Outputs 
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
  
# The renderTable function is used for sending data.frame data to the UI. It
#   is similar to renderPlot, except that the final object, pksum024, should be
#   a data.frame or tibble containing the values and text to be displayed in 
#   the desired table for the UI.
  output$pksdtable <- renderTable({
  # Application inputs
    DOSEFRQ <- as.double(input$DOSEFRQ)
    DOSEDUR <- input$DOSEDUR
  # Server objects
    simdf <- Rsimdf()
  # Script Outputs 
    pksum024 <- simdf %>%
      filter(evid == 0 & time <= DOSEFRQ) %>%
      group_by(ID) %>%
      summarise(
        AUCtau = pk.calc.auc.last(CP, time, interval = c(0, DOSEFRQ)),
        Cmax = pk.calc.cmax(CP),
        Ctrough = pk.calc.ctrough(CP, time, end = DOSEFRQ)) %>%
      mutate(Cave = pk.calc.cav(AUCtau, start = 0, end = DOSEFRQ)) %>%
      summarise_at(vars(-ID), list(Median = median, ci90lo = ci90lo, ci90hi = ci90hi)) %>%
      summarise_all(signif, digits = 3) %>%
      pivot_longer(everything()) %>%
      separate(name, c("Metric", "stat"), sep = "_") %>%
      pivot_wider(id_cols = Metric, names_from = "stat", values_from = "value") %>%
      unite("90% PI", ci90lo, ci90hi, sep = " - ")
  })
  
  output$pksstable <- renderTable({
  # Application inputs
    DOSEFRQ <- as.double(input$DOSEFRQ)
    DOSEDUR <- input$DOSEDUR
  # Server objects
    simdf <- Rsimdf()
  # Script Outputs 
    pksumSS <- simdf %>%
      filter(evid == 0 & time >= (24*DOSEDUR - DOSEFRQ) & time <= 24*DOSEDUR) %>%
      group_by(ID) %>%
      summarise(
        AUCtau = pk.calc.auc.last(CP, time, interval = c(24*DOSEDUR - DOSEFRQ, 24*DOSEDUR)),
        Cmax = pk.calc.cmax(CP),
        Ctrough = pk.calc.ctrough(CP, time, end = 24*DOSEDUR)) %>%
      mutate(Cave = pk.calc.cav(AUCtau, start = 24*DOSEDUR - DOSEFRQ, end = 24*DOSEDUR)) %>%
      summarise_at(vars(-ID), list(Median = median, ci90lo = ci90lo, ci90hi = ci90hi)) %>%
      summarise_all(signif, digits = 3) %>%
      pivot_longer(everything()) %>%
      separate(name, c("Metric", "stat"), sep = "_") %>%
      pivot_wider(id_cols = Metric, names_from = "stat", values_from = "value") %>%
      unite("90% PI", ci90lo, ci90hi, sep = " - ")
  })
  
}  # app_server

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 