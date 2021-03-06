require(rkwarddev)
local({
  
  # Author names and contact information
  about.info <- rk.XML.about(
    name = "qPCR amplification curve analysis",
    author = c(
      person(given = "Stefan", family = "Roediger",
             email = "Stefan.Roediger@b-tu.de", 
             role = c("aut","cre"))),
    about = list(desc = "GUI interface to perform qPCR amplification curve analysis",
                 version = "0.0.1-3", url = "https://github.com/devSJR/qpcR_rk")
  )
  
  ## help page
  plugin.summary <- rk.rkh.summary(
    "Analysis of qPCR amplification curve data. The plugin is primarily targeted at the analysis of amplification curve data from nucleic acid experiments."
  )
  plugin.usage <- rk.rkh.usage(
    "Chose a data set and method to analyze amplification curve data."
  )
  
  # Define dependencies
  dependencies.info <- rk.XML.dependencies(dependencies = list(rkward.min = "0.6.3"), 
                                           package = list(c(name = "chipPCR", min = "0.0.8.10"),
                                                          c(name = "DT", min = "0.1"),
                                                          c(name = "MBmca", min = "0.0.3-5"),
                                                          c(name = "rpivotTable", min = "0.1.5.7"),
                                                          c(name = "qpcR", min = "1.4.0")))
  
  # General settings
  
  # Definition of plot labels and appearance
  generic.plot.options <- rk.plotOptions()
  plot.main <- rk.XML.input(label = "Main title", initial = "Amplification curve analysis")
  plot.xlab <- rk.XML.input(label = "Abscissa label", initial = "Cycle")
  plot.ylab <- rk.XML.input(label = "Ordinate label", initial = "RFU")
  
  var.select <- rk.XML.varselector(label = "Select data")
  selected.x <- rk.XML.varslot(label = "Cycles", source = var.select, multi = FALSE, types = "number", required = TRUE)
  var.data <- rk.XML.varslot(label = "RFU data", source = var.select, multi = TRUE, classes = "numeric", types = "number", required = TRUE)
  
  # Definition of smoothing parameters
  smoother.chk <- rk.XML.cbox("Use smoother", value = "TRUE", un.value = "FALSE")
  
  # Background alteration
  trans.chk <- rk.XML.cbox("Rotate baseline region", value = "TRUE", un.value = "FALSE")
  bg.outliers.chk <- rk.XML.cbox("bg.outliers", value = "TRUE", un.value = "FALSE")
  median.chk <- rk.XML.cbox("Use median", value = "TRUE", un.value = "FALSE")
  
  background.start.spin <- rk.XML.spinbox(label = "Background start", min = "0", initial = "1", real = FALSE)
  background.stop.spin <- rk.XML.spinbox(label = "Background cycle stop", min = "1", initial = "10", real = FALSE)
  background.frame <- rk.XML.frame(rk.XML.row(background.start.spin, background.stop.spin), label = "Background")
  
  method.reg.drop <- rk.XML.dropdown(label = "Background regression method",
                                     options = list("Least squares" = c(val = "least"), 
                                                    "Robust" = c(val = "lmrob", chk = TRUE),
                                                    "Rank-based linear model" = c(val = "rfit")))
  
  # Smoothing
  method.smooth.drop <- rk.XML.dropdown(label = "Smoothing method",
                                        options = list("Savitzky-Golay smoothing filter" = c(val = "savgol", chk = TRUE), 
                                                       "Cubic spline smooth" = c(val = "smooth"),
                                                       "moving average" = c(val = "mova")))
  
  # Deal with hook effect
  hook.chk <- rk.XML.cbox(label = "Ignore hook effect", value = "TRUE", un.value = "FALSE")
  
  # Normalization
  method.norm.drop <- rk.XML.dropdown(label = "Normalization",
                                      options = list("No normalization" = c(val = "none", chk = TRUE), 
                                                     "Minimum-Maximum" = c(val = "minm"),
                                                     "Maximum" = c(val = "max"),
                                                     "z-score" = c(val = "zscore")
                                      ))
  
  # Model selection and Cq calculation
  fit.model.drop <- rk.XML.dropdown(label = "Fitting model",
                                    options = list("Nonlinear sigmoidal model - l4" = c(val = "l4"), 
                                                   "Nonlinear sigmoidal model - l5" = c(val = "l5", chk = TRUE),
                                                   "Nonlinear sigmoidal model - l6" = c(val = "l6"),
                                                   "Nonlinear sigmoidal model - l7" = c(val = "l7"),
                                                   "Spline" = c(val = "spl3")
                                    ))
  
  Cq.efficiency.drop <- rk.XML.dropdown(label = "Method of efficiency estimation",
                                        options = list("Maximum of the first derivative curve" = c(val = "cpD1"), 
                                                       "Maximum of the first derivative curve (default)" = c(val = "cpD2", chk = TRUE),
                                                       "Corbett Research method" = c(val = "CQ"),
                                                       "Guescini method (Cy0)" = c(val = "Cy0"),
                                                       "Exponential region" = c(val = "expR"),
                                                       "Maximum of the efficiency curve" = c(val = "maxE")
                                        ))
  
  
  # Plot appearance
  legend.pos.drop <- rk.XML.dropdown(label = "Position of legend",
                                     options = list("Bottomright" = c(val = "bottomright"), 
                                                    "Bottom" = c(val = "bottom"),
                                                    "Bottomleft" = c(val = "bottomleft"),
                                                    "Left" = c(val = "left"),
                                                    "Topleft" = c(val = "topleft", chk = TRUE),
                                                    "Top" = c(val = "top"),
                                                    "Topright" = c(val = "topright"),
                                                    "Right" = c(val = "right"),
                                                    "Center" = c(val = "center")))
  
  ncol.legend.spin <- rk.XML.spinbox(label = "Number of columns in legend", min = "1", initial = "1", real = FALSE)
  
  ## Add the Cq value and the amplification efficiency to the legend
  Cq.Eff.chk <- rk.XML.cbox(label = "Add Cq and Efficiency to legend", value = "1", un.value = "0", chk = FALSE)
  
  # Add horizontal line in baseline region
  abline.h.chk <- rk.XML.cbox(label = "Horizontal baseline", value = "TRUE", un.value = "FALSE", chk = FALSE)
  
  legend.frame <- rk.XML.frame(legend.pos.drop, ncol.legend.spin, Cq.Eff.chk, 
                               abline.h.chk, rk.XML.stretch(), label = "Legend")
  
  # Plot preview
  preview.chk <- rk.XML.preview(label = "Preview")
  generic.plot.options <- rk.plotOptions()
  plot.text.color <- rk.plotOptions(embed = "rkward::color_chooser", button = FALSE)
  
  basic.settings <- rk.XML.row(
    var.select,
    rk.XML.col(
      selected.x,
      var.data,
      preview.chk,
      rk.XML.stretch()
    ),
    rk.XML.col(
      plot.main,
      plot.xlab,
      plot.ylab
    )
  )
  
  # Definition of setting for the analysis
  # Definition for the smoother function
  
  complex..analysis.chk  <- rk.XML.cbox("Complex analysis", value = "1", un.value = "0", chk = FALSE)
  
  warn.chk  <- rk.XML.cbox("Show warnings", value = "0", un.value = "-1")
  
  # Definition of output options
  
  digits.table <- rk.XML.spinbox(label = "Number of digits in table output", min = "1", max = "9", initial = "3", real = FALSE)
  
  ## Table outputs
  interactive.table.chk <- rk.XML.cbox(label = "Use interactive table", value = "1", un.value = "0", chk = FALSE)
  interactive.pivot.table.chk <- rk.XML.cbox(label = "Use interactive Pivot table", value = "1", un.value = "0", chk = FALSE)
  
  ## Results and data output
  output.smooth.data.chk <- rk.XML.cbox(label = "Save smoothed data", value = "1", un.value = "0", chk = FALSE)
  
  # Definition of the complete GUI
  
  full.dialog <- rk.XML.dialog(
    label = "qPCR analysis",
    rk.XML.tabbook(tabs = list("Basic settings" = list(basic.settings,warn.chk), 
                               "Smoothing and Pre-processing" = list(smoother.chk, 
                                                                     method.smooth.drop, 
                                                                     trans.chk, median.chk, 
                                                                     bg.outliers.chk, 
                                                                     method.reg.drop, 
                                                                     method.norm.drop,
                                                                     background.frame,
                                                                     hook.chk),
                               "Analysis options" = list(Cq.efficiency.drop, complex..analysis.chk, fit.model.drop),
                               "Plot options" = list(generic.plot.options, legend.frame),
                               "Output options" = list(digits.table, interactive.pivot.table.chk, interactive.table.chk, output.smooth.data.chk)
    )
    )
  )
  
  JS.calc <- rk.paste.JS(
    echo("options(warn = ", warn.chk,")\n"),
    js.var.data <- rk.JS.vars(var.data, join = ", "), # get selected vars
    echo("raw.data <- as.matrix(data.frame(rk.list(", selected.x,", ", js.var.data,")))\n\n"),
    
    echo("
      smooth.data <- as.data.frame(sapply(2L:ncol(raw.data), function(i) {
      CPP(raw.data[, 1], raw.data[, i], smoother = ", smoother.chk,", method = \"", method.smooth.drop,"\", 
      trans = ", trans.chk,", bg.outliers = \"", bg.outliers.chk,"\", median = \"", median.chk,"\", 
      method.reg = \"", method.reg.drop,"\",
      method.norm = \"", method.norm.drop,"\", bg.range = c(", background.start.spin,", ", background.stop.spin,"))[\"y.norm\"]}))
      smooth.data <- cbind(as.numeric(raw.data[, 1]), smooth.data)
      colnames(smooth.data) <- colnames(raw.data)
    \n"),
    
    echo("Cq.data <- lapply(2L:ncol(smooth.data), function(i) {\n"),
    ite(id(hook.chk), echo("smooth.data[which(smooth.data[, i] == max(smooth.data[, i])):length(smooth.data[, i]), i] <- max(smooth.data[, i])\n")),
    echo("\t\tres.fit <- pcrfit(data = smooth.data, cyc = 1, fluo = i, model = ", fit.model.drop,")\n"),
    echo("\t\tres.efficiency <- efficiency(res.fit, type = \"", Cq.efficiency.drop,"\", plot = FALSE)\n"),
    echo("\t\t})\n"),
    
    echo("res.out <- t(rbind(sapply(1L:length(Cq.data), function(i) {\n"),
    echo("\t\tas.data.frame(Cq.data[[i]])\n"),
    echo("}\n"),
    echo(")))\n"),
    echo("res.out <- cbind(Sample = colnames(smooth.data[, -1]), res.out)\n"),
    js(if(complex..analysis.chk) {
      # The output of the plugin can provide all information about the cruve fit and the Cq calculation
      # or only a limited set of information (default). 
      echo("res.out <- as.data.frame(res.out[, c(\"Sample\", \"cpD2\", \"eff\", \"fluo\", \"resVar\", \"AICc\", \"Rsq.ad\", \"cpD1\", \"cpE\", \"cpR\", \"cpT\", \"Cy0\", \"cpCQ\", \"cpMR\", \"init1\", \"init2\", \"cf\")])\n")
    } else {
      echo("res.out <- as.data.frame(res.out[, c(\"Sample\", \"cpD2\", \"eff\", \"fluo\")])\n")
    }
    )
  )
  
  
  JS.print <- rk.paste.JS(
    rk.paste.JS.graph(
      echo("n.colors <- ncol(smooth.data[, -1])\n"),
      echo("colors <- rainbow(n.colors, s = 1, v = 1, start = 0, end = max(1, n.colors - 1)/n.colors, alpha = 1)\n"),
      echo("plot(NA, NA, xlim = range(smooth.data[, 1]), ylim = range(smooth.data[, -1]), main = \"", plot.main,"\", xlab = \"", plot.xlab,"\", ylab = \"", plot.ylab,"\")\n"),
      echo("lapply(1L:ncol(smooth.data[, -1]), function(y) {try(lines(smooth.data[, 1], smooth.data[, y + 1], col = colors[y], lwd = 1.5))})\n"),
      js(if(abline.h.chk) {
        echo("abline(h = 0, col = \"grey\")\n")
      }),
      js(if(Cq.Eff.chk){
        echo("legend(\"", legend.pos.drop,"\", paste(colnames(smooth.data[, -1]), formatC(unlist(res.out[, 2]), digits = 3), formatC(unlist(res.out[, 3]), digits = 3)), ncol = ", ncol.legend.spin,", pch = 15, col = colors)")
      } else {
        echo("legend(\"", legend.pos.drop,"\", colnames(smooth.data[, -1]), ncol = ", ncol.legend.spin,", pch = 15, col = colors)")
      }
      )
    ),
    ite("full", rk.paste.JS(
      echo("rk.print.literal (\"qPCR analysis results:\")"),
      echo("\nrk.print(res.out, digits = ", digits.table,")\n"),
      js(if(interactive.table.chk) {echo("\nrk.print(datatable(res.out))\n")}),
      js(if(interactive.pivot.table.chk) {echo("\nrk.print(rpivotTable(res.out))\n")}),
      level = 3)
    )
  )
  
  qPCRAanalysis <<-  rk.plugin.skeleton(
    about = about.info,
    dependencies = dependencies.info,
    xml = list(dialog = full.dialog),
    js = list(require = c("chipPCR", "DT", "MBmca", "qpcR", "rpivotTable"),
              calculate = JS.calc,
              doPrintout = JS.print),
    rkh = list(plugin.summary, plugin.usage),
    pluginmap = list(
      name = "qPCR analysis",
      hierarchy = list("analysis", "qPCR")),
    load = TRUE,
    overwrite = TRUE,
    show = TRUE
  )
  
})

rk.build.plugin(qPCRAanalysis)