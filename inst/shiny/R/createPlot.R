library("ggplot2")
library("cowplot")
library("RColorBrewer")
library("pals")
library("dplyr")

createPlot <- function(input) {
  return(list(
    staticplot = makePlotCode(input,"static"),
    interactive = makePlotCode(input,"interactive")
  ))
}

makePlotCode <- function(input, plottype) {

  p <- 'plotData <- na.omit(select(plotData,all_of(c("{input$subColumn}", "{input$m1Column}", "{input$m2Column}")))) \n'

  if (plottype == "static"){
    p <- paste0(p, 'ggplot(plotData, aes(x = {input$m1Column}, y = {input$m2Column}, \\
  color = factor({input$subColumn}))) + \\
geom_rmc(my.rmc) + ')
  } else if (plottype == "interactive"){
    p <- paste0(p, 'ggplot(plotData, aes(x = {input$m1Column}, y = {input$m2Column}, \\
  color = factor({input$subColumn}))) + \\
geom_point_interactive(aes(colour = factor({input$subColumn}), data_id = factor({input$subColumn}), tooltip = paste0("{input$subColumn}: ", factor({input$subColumn})))) + \\
geom_line_interactive(aes(y = my.rmc$model$fitted.values, data_id = factor({input$subColumn}), tooltip = paste0("{input$subColumn}: ", factor({input$subColumn}))), linetype = 1) + ')
  }


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
             axis.text.x = element_text(angle = {input$xAxisAngle}, hjust = {input$xAxishjust}),
             axis.text.y = element_text(vjust = {input$yAxisvjust})) + ')
  } else {
    p <- paste0(p, 'theme(plot.title = element_text(size = {input$titleFontSize}, hjust = 0.5),
             axis.title = element_text(size = {input$axisLabelFontSize}),
             axis.text = element_text(size = {input$scaleFontSize}),
             axis.text.x = element_text(angle = {input$xAxisAngle}, hjust = {input$xAxishjust}),
             axis.text.y = element_text(vjust = {input$yAxisvjust})) + ')

  p <- paste0(p, 'guides(colour = guide_legend(title= "{input$legendTitle}")) + ')

  }

  if (input$plotMajorGrid == TRUE) {
    if (input$plotMinorGrid == TRUE) {
      p <- paste0(p, 'background_grid(major = "xy", minor = "xy") + ')
    } else {
      p <- paste0(p, 'background_grid(major = "xy", minor = "none") + ')
    }
  }

  if (input$plotPalette != "default") {

    #New palettes from pals
    if (input$plotPalette == "coolwarm") {
      p <- paste0(p, 'scale_colour_manual(values = coolwarm(n)) + ')
    } else if (input$plotPalette == "parula") {
      p <- paste0(p, 'scale_colour_manual(values = parula(n)) + ')
    } else if (input$plotPalette == "ocean.haline") {
      p <- paste0(p, 'scale_colour_manual(values = ocean.haline(n)) + ')
    } else if (input$plotPalette == "cubicl") {
      p <- paste0(p, 'scale_colour_manual(values = cubicl(n)) + ')
    } else if (input$plotPalette == "kovesi.rainbow") {
      p <- paste0(p, 'scale_colour_manual(values = kovesi.rainbow(n)) + ')
    } else if (input$plotPalette == "ocean.phase") {
      p <- paste0(p, 'scale_colour_manual(values = ocean.phase(n)) + ')
    } else if (input$plotPalette == "viridis") {
      p <- paste0(p, 'scale_colour_manual(values = viridis(n)) + ')

    } else {
      ## Color Brewer
      p <- paste0(p, 'scale_colour_manual(values = colorRampPalette(brewer.pal({brewer.pal.info[input$plotPalette,]$maxcolors}, "{input$plotPalette}"))(n)) + ')
    }
  }

  if (input$autoScale == FALSE) {
    p <- paste0(p, 'xlim({input$xminScale}, {input$xmaxScale}) +
  ylim({input$yminScale}, {input$ymaxScale}) + ')
  }

  if (input$addText == TRUE){

    p <- paste0(p, 'annotate("text",
          x = {ifelse(input$textLocation == "topleft" || input$textLocation == "bottomleft",-Inf,Inf)},
          y = {ifelse(input$textLocation == "topleft" || input$textLocation == "topright",Inf,-Inf)},
          size = 5,
          label = deparse(bquote(atop(~~italic(r[rm])~"="~ .(sprintf("%.2f", round(my.rmc$r, 2))),
            ~italic(p)~.(ifelse(my.rmc$p < 0.001, "< 0.001",
                          ifelse(my.rmc$p < 0.01, "< 0.01",
                            ifelse(my.rmc$p < 0.05 & my.rmc$p > 0.045, "< 0.05",
                              paste0("= ",round(my.rmc$p, digits = 2))))))))),
          hjust = {ifelse(input$textLocation == "topleft" || input$textLocation == "bottomleft",-0.5, 1.5)},
          vjust = {ifelse(input$textLocation == "topleft" || input$textLocation == "topright", 1.5, -0.5)},
          parse = TRUE) + ')
  }

  ## Remove the 3 last characters of p, as we don't know where is the end
  p <- substr(p,1,nchar(p)-3)

  return(p)
}
