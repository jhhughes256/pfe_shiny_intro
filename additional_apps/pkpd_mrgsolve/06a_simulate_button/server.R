# server.r (Reactive Server Objects) ------------------------------------------
# All reactive components of your application are specified here. This is any
#   code from your original script that depends on the inputs that were
#   identified.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
app_server <- function(input, output) {
  
  Rsimdf <- reactive({
  # Application inputs
    NID <- input$NID
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
  
  output$pkplot <- renderPlot({
  # Application inputs
    NID <- input$NID
    DOSEDUR <- input$DOSEDUR
    logscale <- input$logscale
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
    if (logscale) {
      p <- p + scale_y_log10()
    }
    p
  })
  
  output$pksdtable <- renderTable({
  # Application inputs
    NID <- input$NID
    DOSEFRQ <- as.double(input$DOSEFRQ)
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
      unite("90% PI", ci90lo, ci90hi, sep = " - ") %>%
      purrr::when(
        NID == 1 ~ mutate(., `90% PI` = "N/A"),
        NID != 1 ~ .
      )
  })
  
  output$pksstable <- renderTable({
  # Application inputs
    NID <- input$NID
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
      unite("90% PI", ci90lo, ci90hi, sep = " - ") %>%
      purrr::when(
        NID == 1 ~ mutate(., `90% PI` = "N/A"),
        NID != 1 ~ .
      )
  })
  
}  # app_server

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 