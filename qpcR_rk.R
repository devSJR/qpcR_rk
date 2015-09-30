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
                 version = "0.0.1-1", url = "")
  )
  
  ## help page
  plugin.summary <- rk.rkh.summary(
    "Analysis of qPCR amplification curve data. The plugin is primarily targetet at the analysis of amplification curve data from nucleic acid experiments."
  )
  plugin.usage <- rk.rkh.usage(
    "Chose a data set and method to analyse amplification curve data."
  )
  
  # Define dependencies
  dependencies.info <- rk.XML.dependencies(dependencies = list(rkward.min = "0.6.3"), 
                                           package = list(c(name = "chipPCR", min = "0.0.8.10"),
							  c(name = "MBmca", min = "0.0.3-5"),
							  c(name = "qpcR", min = "1.4.0")))
  # General settings
  
  # Definition of plot labels and appearance
  generic.plot.options <- rk.plotOptions()
  plot.main <- rk.XML.input(label = "Main title", initial = "Amplification curve analysis")
  plot.xlab <- rk.XML.input(label = "Abscissa label", initial = "Cycle")
  plot.ylab <- rk.XML.input(label = "Ordinate label", initial = "RFU")
  
  var.select <- rk.XML.varselector(label = "Select data")
  selected.x <- rk.XML.varslot(label = "Cycles", source = var.select, types = "number", required = TRUE)
  var.data <- rk.XML.varslot(label = "RFU data", source = var.select, multi = TRUE, classes = "numeric", types = "number", required = TRUE)
  
  # Definition of smoothing parameters
  smoother.chk <- rk.XML.cbox("Use smoother", value = "TRUE", un.value = "FALSE")
  
  # Backround alteration
  trans.chk <- rk.XML.cbox("Rotate baseline region", value = "TRUE", un.value = "FALSE")
  bg.outliers.chk <- rk.XML.cbox("bg.outliers", value = "TRUE", un.value = "FALSE")
  median.chk <- rk.XML.cbox("Use median", value = "TRUE", un.value = "FALSE")

  method.reg.drop <- rk.XML.dropdown(label = "Backround regression method",
				  options = list("Least squares" = c(val = "least"), 
						"Robust" = c(val = "lmrob", chk = TRUE),
						"Rank-based linear model" = c(val = "rfit")))
  
  # Smoothing
  method.smooth.drop <- rk.XML.dropdown(label = "Smoothing method",
				    options = list("Savitzky-Golay smoothing filter" = c(val = "savgol", chk = TRUE), 
						  "Cubic spline smooth" = c(val = "smooth"),
						  "moving average" = c(val = "mova")))

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
						  "Nonlinear sigmoidal model - l6" = c(val = "l6")
						  ))
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
    ))
  
  # Defintion of setting for the analysis
  # Definion for the smoother function
  
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
  legend.frame <- rk.XML.frame(legend.pos.drop, ncol.legend.spin, rk.XML.stretch(), label="Legend")

  
  
  warn.chk  <- rk.XML.cbox("Surpress warnings", value = "1", un.value = "0")
  
  abline.chk  <- rk.XML.cbox("Show line", value = "1", un.value = "0")
  
  full.dialog <- rk.XML.dialog(
    label = "qPCR analysis",
    rk.XML.tabbook(tabs = list("Basic settings" = list(basic.settings), 
                               "Options qPCR analysis" = list(warn.chk),
                               "Smoothing and Pre-processing" = list(smoother.chk, 
								     method.smooth.drop, 
								     trans.chk, median.chk, 
								     bg.outliers.chk, 
								     method.reg.drop, 
								     method.norm.drop,
								     fit.model.drop),
                               "Plot options" = list(generic.plot.options, abline.chk, 
                                                     legend.frame)))
  )
  
  JS.calc <- rk.paste.JS(
    js.var.data <- rk.JS.vars(var.data, join = ", "), # get selected vars
    echo("raw.data <- as.matrix(data.frame(rk.list(", selected.x,", ", js.var.data,")))\n\n"),
    
    echo("smooth.data <- as.data.frame(sapply(2L:ncol(raw.data), function(i) {\n"),
    echo("\t\tCPP(raw.data[, 1], raw.data[, i], smoother = ", smoother.chk,", method = \"", method.smooth.drop,"\", \n"),
    echo("\t\ttrans = ", trans.chk,", bg.outliers = \"", bg.outliers.chk,"\", median = \"", median.chk,"\", \n"),
    echo("\t\tmethod.reg = \"", method.reg.drop,"\",\n"),
    echo("\t\tmethod.norm = \"", method.norm.drop,"\")[\"y.norm\"]}))\n"),
    echo("smooth.data <- cbind(as.numeric(raw.data[, 1]), smooth.data)\n"),
    echo("colnames(smooth.data) <- colnames(raw.data)\n"),
    
    echo("Cq.data <- lapply(2L:ncol(smooth.data), function(i) {\n"),
    echo("\t\tres.fit <- pcrfit(data = smooth.data, cyc = 1, fluo = i, model = ", fit.model.drop,")\n"),
    echo("\t\tres.efficiency <- efficiency(res.fit, type = \"cpD2\", plot = FALSE)\n"),
    echo("\t\t})\n")
  )
  
  JS.print <- rk.paste.JS(
    rk.paste.JS.graph(
      
      echo("plot(NA, NA, xlim = range(raw.data[, 1]), ylim = range(smooth.data[, -1]))\n"),
      echo("lapply(2L:ncol(smooth.data), function(y) {try(lines(smooth.data[, 1], smooth.data[, y], col = 2))})\n")
    ),
    ite("full", rk.paste.JS(
      echo("\nrk.print(summary(raw.data))\n"),
      level = 3)
    )
  )
  
  rk.plugin.skeleton(
    about = about.info,
    dependencies = dependencies.info,
    xml = list(dialog = full.dialog),
    js = list(require = c("chipPCR", "MBmca", "qpcR"),
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
