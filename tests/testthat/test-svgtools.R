test_that("linesSymbols with circles sets order circles correctly (vertical alignment)", {
  svg <- svgtools::read_svg("_testdata/linesSymbols_ex_circle.svg")

  testValues <- data.frame("V1" = c(10, 20, 30, 40, 50, 60))
  svg <- linesSymbols(
    svg = svg, 
    frame_name = "Rahmen2", 
    group_name = "GR2", 
    scale_real = 0:100, 
    values = testValues$V1, 
    alignment = "vertical", 
    has_lines = FALSE, 
    symbol_type = "circle"
  )
  GR2Circles <- xml2::xml_find_all(svg, "/svg/g[@id=\"GR2\"]/circle")
  
  # expect circles with lower x coordinate to also have a higher y value (and vice versa)
  for (cc1 in 2:length(GR2Circles)) {
    for (cc2 in 1:(cc1 - 1)) {
      cc1x <- as.numeric(xml2::xml_attr(GR2Circles[[cc1]], "cx"))
      cc2x <- as.numeric(xml2::xml_attr(GR2Circles[[cc2]], "cx"))
      
      if (cc1x < cc2x) {
        expect_gt(
          as.numeric(xml2::xml_attr(GR2Circles[[cc1]], "cy")),
          as.numeric(xml2::xml_attr(GR2Circles[[cc2]], "cy"))
        )
      } else {
        expect_lt(
          as.numeric(xml2::xml_attr(GR2Circles[[cc1]], "cy")),
          as.numeric(xml2::xml_attr(GR2Circles[[cc2]], "cy"))
        )
      }
    }
  }
})
