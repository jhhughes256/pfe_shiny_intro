# server.r (Reactive Server Objects) ------------------------------------------
# All reactive components of your application are specified here. This is any
#   code from your original script that depends on the inputs that were
#   identified.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
app_server <- function(input, output) {
  
# The reactive function is replaced with an eventReactive function. Instead of
#   updating whenever inputs update, this function only updates when a specific
#   event occurs. The event being used here is the user pressing the button.
#   The first argument is the event expression (the simulation button), while
#   the second argument is the code that should be run when the event occurs
#   contained in { }. An optional argument is ignoreNULL (set to TRUE by 
#   default), which determines whether the reactive should wait for the event
#   before running the code in { }. By setting this to FALSE, the code runs
#   once when the application starts, before waiting for the event. Try setting
#   ignoreNULL to TRUE to see the difference!
# The code being run collects the inputs that should only update when the button
#   is pressed and returns them in a list (much like how input returns them!). 
#   Each input in this object is accessed by calling the reactive object, 
#   Rinput(), followed by asking for an object from that list, Rinput()$NID.
#   Note that input$logscale is not included, as we don't need to resimulate to
#   change the scale of a plot!
  Rinput <- eventReactive(input$sim, ignoreNULL = FALSE, {
    list(
      NID = input$NID,
      DOSEAMT = input$DOSEAMT,
      DOSEFRQ = as.double(input$DOSEFRQ),  # convert from character
      DOSEDUR = input$DOSEDUR,
      COVBWT = input$COVBWT,
      COVSEX = as.double(input$COVSEX)
    )
  })
  
# Notice that instead of referring to input$NID, Rinput()$NID has been put in 
#   its place. Because Rinput() is only updated when the actionButton is pressed,
#   this also applies to all downstream reactive objects that rely on its value.
# You *could* avoid having the Rinput reactive object and replace the reactive
#   below with an eventReactive, but this would not stop inputs such as NID and
#   DOSEFRQ updating when generating outputs, leading to potentially disastrous
#   reactivity. This could be solved by wrapping the inputs that the simulation 
#   depends on in an isolate function, isolate(input$NID), however (in my 
#   opinion) this is more likely to result in human error and frustrating bugs 
#   in larger apps. Additionally, using eventReactive as a replacement causes
#   plots linked to this object to "flicker" as you change inputs.
  Rsimdf <- reactive({
  # Button-dependent inputs
    NID <- Rinput()$NID
    DOSEAMT <- Rinput()$DOSEAMT
    DOSEFRQ <- Rinput()$DOSEFRQ  # convert from character
    DOSEDUR <- Rinput()$DOSEDUR
    COVBWT <- Rinput()$COVBWT
    COVSEX <- Rinput()$COVSEX
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
  
# Note that input$logscale is called straight from input and is not sent through
#   the button-dependent Rinput(), while NID and DOSEDUR are not. If these were
#   called directly, the plot would change to accomodate different NID and DOSEDUR
#   values, even though the simulation hadn't changed! 
  output$pkplot <- renderPlot({
  # Application inputs
    logscale <- input$logscale
  # Button-dependent inputs
    NID <- Rinput()$NID
    DOSEDUR <- Rinput()$DOSEDUR
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
  # Button-dependent inputs
    NID <- Rinput()$NID
    DOSEFRQ <- Rinput()$DOSEFRQ
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
  # Button-dependent inputs
    NID <- Rinput()$NID
    DOSEFRQ <- Rinput()$DOSEFRQ
    DOSEDUR <- Rinput()$DOSEDUR
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