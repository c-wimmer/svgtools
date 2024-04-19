expect_strict_monotony <- function(svg, xpathForSymbol, attribute1, attribute2, positive) {
  symbols <- xml2::xml_find_all(svg, xpathForSymbol)
  
  for (elem1 in 2:length(symbols)) {
    for (elem2 in 1:(elem1 - 1)) {
      if (attribute1 == "points") {
        elem1attr1 <- xml2::xml_attr(symbols[[elem1]], attribute1)
        elem1attr1 <- strsplit(elem1attr1, " ", fixed = TRUE)[[1]][1]
        elem1attr1 <- as.numeric(strsplit(elem1attr1, ",", fixed = TRUE)[[1]][1])
        elem2attr1 <- xml2::xml_attr(symbols[[elem2]], attribute1)
        elem2attr1 <- strsplit(elem2attr1, " ", fixed = TRUE)[[1]][1]
        elem2attr1 <- as.numeric(strsplit(elem2attr1, ",", fixed = TRUE)[[1]][1])
      } else {
        elem1attr1 <- as.numeric(xml2::xml_attr(symbols[[elem1]], attribute1))
        elem2attr1 <- as.numeric(xml2::xml_attr(symbols[[elem2]], attribute1))
      }
      
      if (attribute2 == "points") {
        elem1attr2 <- xml2::xml_attr(symbols[[elem1]], attribute2)
        elem1attr2 <- strsplit(elem1attr2, " ", fixed = TRUE)[[1]][1]
        elem1attr2 <- as.numeric(strsplit(elem1attr2, ",", fixed = TRUE)[[1]][2])
        elem2attr2 <- xml2::xml_attr(symbols[[elem2]], attribute2)
        elem2attr2 <- strsplit(elem2attr2, " ", fixed = TRUE)[[1]][1]
        elem2attr2 <- as.numeric(strsplit(elem2attr2, ",", fixed = TRUE)[[1]][2])
      } else {
        elem1attr2 <- as.numeric(xml2::xml_attr(symbols[[elem1]], attribute2))
        elem2attr2 <- as.numeric(xml2::xml_attr(symbols[[elem2]], attribute2))
      }
      
      if ((positive && elem1attr1 > elem2attr1) || (!positive && elem1attr1 < elem2attr1))  {
        expect_gt(elem1attr2, elem2attr2)
      } else {
        expect_lt(elem1attr2, elem2attr2)
      }
    }
  }
}

test_that("linesSymbols sets order of elements correctly", {
  testFiles <- list.files(path = "./_testdata/", pattern = "^svgtools_check_linesSymbols.*\\.svg$", full.names = TRUE)
  
  for (testFile in testFiles) {
    svg <- svgtools::read_svg(testFile)
    
    filenameSplit <- strsplit(testFile, "(_|\\.)")[[1]]
    symbolType <- filenameSplit[length(filenameSplit) - 2]
    alignment <- filenameSplit[length(filenameSplit) - 1]
    
    testValues <- (1:10) * 10
    
    for (gg in 1:8) {
      svg <- linesSymbols(
        svg = svg, 
        frame_name = paste0("myFrame", gg), 
        group_name = paste0("GR", gg), 
        scale_real = 0:100, 
        values = testValues, 
        alignment = alignment, 
        has_lines = gg >= 5, 
        symbol_type = symbolType
      )

      switch (symbolType,
        circle = { expect_strict_monotony(svg, paste0("/svg/g[@id=\"GR", gg, "\"]/circle"), "cx", "cy", alignment == "horizontal") },
        rect = { expect_strict_monotony(svg, paste0("/svg/g[@id=\"GR", gg, "\"]/rect"), "x", "y", alignment == "horizontal") },
        polygon = { expect_strict_monotony(svg, paste0("/svg/g[@id=\"GR", gg, "\"]/polygon"), "points", "points", alignment == "horizontal") },
        { warning("not implemented!") }
      )
      
      if (gg >= 5) {
        expect_strict_monotony(svg, paste0("/svg/g[@id=\"GR", gg, "\"]/line"), "x1", "y1", alignment == "horizontal")
        expect_strict_monotony(svg, paste0("/svg/g[@id=\"GR", gg, "\"]/line"), "x2", "y2", alignment == "horizontal")
      }
    }
  }
})
