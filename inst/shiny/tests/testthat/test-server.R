#test the module
testServer(dataUpload, expr = {

  session$setInputs(sampleData = TRUE, whichsampleData = "gilden2010", header = TRUE,
                    decimalPoint = ".", quote = "", sep = ",")
  expect_equal(inputData()$rt, rmcorr::gilden2010$rt,)

})

testServer(expr = {
  # set inputs
  expect_warning(session$setInputs(`rmcorr-sampleData` = TRUE, `rmcorr-whichsampleData` = "gilden2010",
                    `rmcorr-header` = TRUE, `rmcorr-sep` = ",", subColumn = "sub",
                    m1Column = "rt", m2Column = "acc", CIlevel = 0.95, bootstrap = FALSE,
                    bootstrapnreps = 10, bootstrapout = FALSE, bootseed = 5), "coerced into a factor")

  # test the rmcorr calculation?
  expect_equal(processedData()$rmc()$r, -0.4061, tolerance = 0.001)

  # with bootstrap set to false, should be no resamples
  expect_null(processedData()$rmc()$resamples)

  # change bootstrap and boostrapout to true, should have resamples with length of bootstrapnreps
  expect_warning(session$setInputs(bootstrap = TRUE, bootstrapout = TRUE),"coerced into a factor")
  expect_length(processedData()$rmc()$resamples, input$bootstrapnreps)

  # make sure plot with custom palette is created without error
  session$setInputs(plotLegend = TRUE, plotMajorGrid = F,
                                   plotMinorGrid = F, plotPalette = "coolwarm",
                                   autoScale = T, addText = T, width = 600, height = 600)
  output$rmcorrPlot
})
