testServer("dataUpload", expr = {
  #test the rmcorr calculation?
  # session$setInputs(`rmcorr-sampleData` = TRUE, `rmcorr-whichsampleData` = "gilden2010")
  session$setInputs(sampleData = TRUE, whichsampleData = "gilden2010")
  # print(inputData())
  # expect_equal(processedData()$rmc()$r, -0.4061, tolerance = 0.001)



})
