<<<<<<< HEAD
# ## ggplot & cowplot theme
library("ggplot2")
library("cowplot")
library("RColorBrewer")
library("pals")


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
             axis.text.x = element_text(angle = {input$xAxisAngle}, \\
hjust = {input$xAxishjust}, \\
vjust = {input$xAxisvjust})) + ')
  } else {
    p <- paste0(p, 'theme(plot.title = element_text(size = {input$titleFontSize}, hjust = 0.5),
             axis.title = element_text(size = {input$axisLabelFontSize}),
             axis.text = element_text(size = {input$scaleFontSize}),
             axis.text.x = element_text(angle = {input$xAxisAngle}, \\
hjust = {input$xAxishjust}, \\
vjust = {input$xAxisvjust})) +
  guides(colour = guide_legend(title= "{input$legendTitle}")) + ')
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
    p <- paste0(p, 'ylim({input$minScale}, {input$maxScale}) + ')
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
=======
# ## ggplot & cowplot theme
library("ggplot2")
library("cowplot")
library("RColorBrewer")
library("pals")


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
             axis.text.x = element_text(angle = {input$xAxisAngle}, \\
hjust = {input$xAxishjust}, \\
vjust = {input$xAxisvjust})) + ')
  } else {
    p <- paste0(p, 'theme(plot.title = element_text(size = {input$titleFontSize}, hjust = 0.5),
             axis.title = element_text(size = {input$axisLabelFontSize}),
             axis.text = element_text(size = {input$scaleFontSize}),
             axis.text.x = element_text(angle = {input$xAxisAngle},
hjust = {input$xAxishjust}, \\
vjust = {input$xAxisvjust})) + ')
  }

  if (input$legendTitle != "Legend Title") {
    p <- paste0(p, 'labs(colour = ("{input$legendTitle}")) + ')
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
    p <- paste0(p, 'ylim({input$minScale}, {input$maxScale}) + ')
  }
  
  if (input$addText == TRUE){
    
    p <- paste0(p, 'annotate("text",
          x = {ifelse(input$textLocation == "topleft" || input$textLocation == "bottomleft",-Inf,Inf)},
          y = {ifelse(input$textLocation == "topleft" || input$textLocation == "topright",Inf,-Inf)}, 
          label = bquote(atop(~~italic(r[rm])~"="~ .(sprintf("%.2f", round(my.rmc$r, 2))),
            ~italic(p)~.(ifelse(my.rmc$p < 0.001, "< 0.001", 
                          ifelse(my.rmc$p < 0.01, "< 0.01",
                            ifelse(my.rmc$p < 0.05 & my.rmc$p >= 0.045, "< 0.05",
                              paste0("= ",round(my.rmc$p, digits = 2)))))))), 
          hjust = {ifelse(input$textLocation == "topleft" || input$textLocation == "bottomleft",-0.5, 1.5)}, 
          vjust = {ifelse(input$textLocation == "topleft" || input$textLocation == "topright", 1.5, -0.5)}) + ')
  }
  
  ## Remove the 3 last characters of p, as we don't know where is the end
  p <- substr(p,1,nchar(p)-3)
  
  return(p)
}
>>>>>>> 2f50255d471ce9727c5ba07035ffb5958324037f
