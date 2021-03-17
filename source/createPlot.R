# ## ggplot & cowplot theme
library("ggplot2")
library("cowplot")
# ## Statistics
# library("ggpubr")
# ## 95% CI interval for the mean
# library("Hmisc")
# ## Beeswarm dots
# library("ggbeeswarm")
# ## Already loaded from ui.R
# ## Colour themes
# # library("ggsci")
library("RColorBrewer")


createPlot <- function(input) {

  p <- 'ggplot(plotData, aes(x = {input$m1Column}, y = {input$m2Column}, \\
  group = factor({input$subColumn}), color = factor({input$subColumn}))) + \\
  geom_point(aes(colour = factor({input$subColumn}))) + \\
  geom_line(aes(y = my.rmc$model$fitted.values), linetype = 1) + '
  
  p <- paste0(p, 'ggtitle("{input$plotTitle}") + \\
ylab("{input$yAxisTitle}") + \\
xlab("{input$xAxisTitle}") + \\
{input$plotTheme} + \\
scale_shape_identity() + ')

  if (input$plotLegend == FALSE) {
    p <- paste0(p, 'theme(legend.position = "none",
             plot.title = element_text(size = {input$titleFontSize}, hjust = 0.5),
             axis.title = element_text(size = {input$axisLabelFontSize}),
             axis.text = element_text(size = {input$scaleFontSize}),
             axis.text.x = element_text(angle = {input$xAxisAngle},
                hjust = {input$xAxishjust},
                vjust = {input$xAxisvjust})) + ')
  } else {
    p <- paste0(p, 'theme(plot.title = element_text(size = {input$titleFontSize}, hjust = 0.5),
             axis.title = element_text(size = {input$axisLabelFontSize}),
             axis.text = element_text(size = {input$scaleFontSize}),
             axis.text.x = element_text(angle = {input$xAxisAngle},
                hjust = {input$xAxishjust},
                vjust = {input$xAxisvjust})) + ')
  }

  if (input$plotMajorGrid == TRUE) {
    if (input$plotMinorGrid == TRUE) {
      p <- paste0(p, 'background_grid(major = "xy", minor = "xy") + ')
    } else {
      p <- paste0(p, 'background_grid(major = "xy", minor = "none") + ')
    }
  }

  if (input$plotPalette != "default") {
    if (input$plotPalette == "NPG") {
      p <- paste0(p, 'scale_color_npg() + scale_fill_npg() + ')
    } else if (input$plotPalette == "AAAS") {
      p <- paste0(p, 'scale_color_aaas() + scale_fill_aaas() + ')
    } else if (input$plotPalette == "NEJM") {
      p <- paste0(p, 'scale_color_nejm() + scale_fill_nejm() + ')
    } else if (input$plotPalette == "Lancet") {
      p <- paste0(p, 'scale_color_lancet() + scale_fill_lancet() + ')
    } else if (input$plotPalette == "JAMA") {
      p <- paste0(p, 'scale_color_jama() + scale_fill_jama() + ')
    } else if (input$plotPalette == "JCO") {
      p <- paste0(p, 'scale_color_jco() + scale_fill_jco() + ')
    } else if (input$plotPalette == "UCSCGB") {
      p <- paste0(p, 'scale_color_ucscgb() + scale_fill_ucscgb() + ')
    } else if (input$plotPalette == "LocusZoom") {
      p <- paste0(p, 'scale_color_locuszoom() + scale_fill_locuszoom() + ')
    } else if (input$plotPalette == "IGV") {
      p <- paste0(p, 'scale_color_igv() + scale_fill_igv() + ')
    } else if (input$plotPalette == "UChicago") {
      p <- paste0(p, 'scale_color_uchicago() + scale_fill_uchicago() + ')
    } else if (input$plotPalette == "UChicago Light") {
      p <- paste0(p, 'scale_color_uchicago("light") + scale_fill_uchicago("light") + ')
    } else if (input$plotPalette == "UChicago Dark") {
      p <- paste0(p, 'scale_color_uchicago("dark") + scale_fill_uchicago("dark") + ')
    } else if (input$plotPalette == "Star Trek") {
      p <- paste0(p, 'scale_color_startrek() + scale_fill_startrek() + ')
    } else if (input$plotPalette == "Tron Legacy") {
      p <- paste0(p, 'scale_color_tron() + scale_fill_tron() + ')
    } else if (input$plotPalette == "Futurama") {
      p <- paste0(p, 'scale_color_futurama() + scale_fill_futurama() + ')
    } else if (input$plotPalette == "Rick and Morty") {
      p <- paste0(p, 'scale_color_rickandmorty() + scale_fill_rickandmorty() + ')
    } else if (input$plotPalette == "The Simpsons") {
      p <- paste0(p, 'scale_color_simpsons() + scale_fill_simpsons() + ')
    } else {
      ## Color Brewer
      p <- paste0(p, 'scale_colour_manual(values = colorRampPalette(brewer.pal({brewer.pal.info[input$plotPalette,]$maxcolors}, "{input$plotPalette}"))(n)) + ')
    }
  }

  if (input$autoScale == FALSE) {
    p <- paste0(p, 'ylim({input$minScale}, {input$maxScale}) + ')
  }


  ## Remove the 3 last characters of p, as we don't know where is the end
  p <- substr(p,1,nchar(p)-3)

  return(p)
}

