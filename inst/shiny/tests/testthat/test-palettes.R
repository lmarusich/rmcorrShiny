# Test the get_brewer_name function from source/paletteColours.R

test_that("Getting palette colors works", {
  expect_equal(get_brewer_name("PiYG"), c("#8E0152", "#C51B7D", "#DE77AE", "#F1B6DA",
                                          "#FDE0EF", "#F7F7F7", "#E6F5D0", "#B8E186",
                                          "#7FBC41", "#4D9221", "#276419"))
  expect_equal(get_brewer_name("cubicl"), NULL)
})
