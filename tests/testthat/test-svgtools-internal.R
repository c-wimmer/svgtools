test_that("rounding options for data value labels work", {
  xmlStub <- xml2::as_xml_document(list(
    xml = list(
      g = structure(list(
        g = structure(list(
          rect = structure(list(), x=0, y=80, width=20, height=80),
          text = structure(list(0), x=10, y=40)
        ), id="bar1")
      ), id="stackedBar")
    )
  ))
  barRects <- xml2::xml_find_all(xmlStub, "g[@id=\"stackedBar\"]/g[@id=\"bar1\"]/rect")
  barLabels <- xml2::xml_find_all(xmlStub, "g[@id=\"stackedBar\"]/g[@id=\"bar1\"]/text")
  
  options("svgtools.roundAwayFromZero" = NULL)
  svgtools:::stackedBar_edit_text(
    barLabels = barLabels,
    order_labels = 1,
    value_set = 6.5,
    rects = barRects,
    order_rects = 1,
    decimals = 0,
    displayLimits = 100,
    labelPosition = "center",
    alignment = "vertical"
  )
  expect_equal(as.numeric(xml2::xml_text(barLabels[[1]])), 6)
  
  options("svgtools.roundAwayFromZero" = TRUE)
  svgtools:::stackedBar_edit_text(
    barLabels = barLabels,
    order_labels = 1,
    value_set = 6.5,
    rects = barRects,
    order_rects = 1,
    decimals = 0,
    displayLimits = 100,
    labelPosition = "center",
    alignment = "vertical"
  )
  expect_equal(as.numeric(xml2::xml_text(barLabels[[1]])), 7)
  
  options("svgtools.roundAwayFromZero" = FALSE)
  svgtools:::stackedBar_edit_text(
    barLabels = barLabels,
    order_labels = 1,
    value_set = 6.5,
    rects = barRects,
    order_rects = 1,
    decimals = 0,
    displayLimits = 100,
    labelPosition = "center",
    alignment = "vertical"
  )
  expect_equal(as.numeric(xml2::xml_text(barLabels[[1]])), 6)
  
  options("svgtools.roundAwayFromZero" = NULL)
})
