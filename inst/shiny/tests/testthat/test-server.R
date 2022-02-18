#test the module
testServer(dataUpload, expr = {

  session$setInputs(sampleData = TRUE, whichsampleData = "gilden2010", header = TRUE,
                    decimalPoint = ".", quote = "", sep = ",")
  expect_equal(inputData()$rt, rmcorr::gilden2010$rt)

})

testServer(expr = {
  # set inputs
  expect_warning(session$setInputs(`rmcorr-sampleData` = TRUE, `rmcorr-whichsampleData` = "gilden2010",
                    `rmcorr-header` = TRUE, `rmcorr-sep` = ",", `rmcorr-quote` = "",
                    `rmcorr-decimalPoint` = ".", subColumn = "sub",
                    m1Column = "rt", m2Column = "acc", CIlevel = 0.95, bootstrap = FALSE,
                    bootstrapnreps = 10, bootstrapout = FALSE, bootseed = 5), "coerced into a factor")

  # test the rmcorr calculation?
  expect_equal(processedData()$rmc()$r, -0.4061, tolerance = 0.001)

  # with bootstrap set to false, should be no resamples
  expect_null(processedData()$rmc()$resamples)

  # change bootstrap and bootstrapout to true, should have resamples with length of bootstrapnreps
  expect_warning(session$setInputs(bootstrap = TRUE, bootstrapout = TRUE),"coerced into a factor")
  expect_length(processedData()$rmc()$resamples, input$bootstrapnreps)

  # make sure plot with custom palette is created without error
  session$setInputs(plotLegend = TRUE,
                    legendTitle = "Legend Title",
                    plotMajorGrid = F,
                    plotMinorGrid = F,
                    plotPalette = "coolwarm",
                    plotTheme = "theme_cowplot()",
                    plotTitle = "Main Title",
                    titleFontSize = 15,
                    axisLabelFontSize = 15,
                    scaleFontSize = 15,
                    yAxisTitle = "y",
                    xAxisTitle = "x",
                    xAxishjust = .5,
                    xAxisvjust = .5,
                    xAxisAngle = 0,
                    autoScale = T,
                    addText = F,
                    width = 600,
                    height = 600)
  output$rmcorrPlot
})
