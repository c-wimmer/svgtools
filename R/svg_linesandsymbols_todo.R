## svg: symbole und linien


# Linien
linesSymbols_in <- function (svg_in, group_name) {
  
  named_groups <- xml2::xml_find_all(svg_in, "/svg/g")
  index_group <- base::which(xml2::xml_attr(named_groups, "id") == group_name)
  if (base::length(index_group) != 1) {stop("Fehler: Gruppenname nicht gefunden bzw. nicht eindeutig.")}
  lineGroup <- named_groups[index_group]
  
}

linesSymbols_order_lines <- function (lines_inGroup, alignment) {
  
  lines_x1_values <- lines_y1_values <- lines_x2_values <- lines_y2_values <- NULL
  
  for (n_lines in 1:base::length(lines_inGroup)) {
    
    lines_x1_values <- c(lines_x1_values, base::as.numeric(xml2::xml_attr(lines_inGroup[n_lines], "x1")))
    lines_y1_values <- c(lines_y1_values, base::as.numeric(xml2::xml_attr(lines_inGroup[n_lines], "y1")))
    lines_x2_values <- c(lines_x2_values, base::as.numeric(xml2::xml_attr(lines_inGroup[n_lines], "x2")))
    lines_y2_values <- c(lines_y2_values, base::as.numeric(xml2::xml_attr(lines_inGroup[n_lines], "y2")))
    
  }
  
  order_lines_x <- base::rank(lines_x1_values)  ## TODO: Schauen ob x1 und 2 notwendig sind fuer Reihenfolge-Check
  order_lines_y <- base::rank(-lines_y1_values)
  
  if (alignment == "horizontal") {order_lines <- order_lines_x}
  if (alignment == "vertical") {order_lines <- order_lines_y}
  
  
  return(order_lines)
  
}

## TODO: alignment
linesSymbols_edit_lines <- function (lines_inGroup, order_lines, frame_info, value_set, alignment) {
  
  if (alignment=="horizontal")
  {
    for (n_lines in 1:base::length(lines_inGroup)) {
      
      line_toChange <- lines_inGroup[order_lines[n_lines]]
      
      xml2::xml_set_attr(line_toChange, "x1", frame_info$min_x + value_set[order_lines[n_lines]] * frame_info$scaling_x)
      xml2::xml_set_attr(line_toChange, "x2", frame_info$min_x + value_set[order_lines[n_lines] + 1] * frame_info$scaling_x)
      
    }
  }
  if (alignment=="vertical")
  {
    for (n_lines in 1:base::length(lines_inGroup)) {
      
      line_toChange <- lines_inGroup[order_lines[n_lines]]
      
      xml2::xml_set_attr(line_toChange, "y1", frame_info$max_y - value_set[order_lines[n_lines]] * frame_info$scaling_y)
      xml2::xml_set_attr(line_toChange, "y2", frame_info$max_y - value_set[order_lines[n_lines] + 1] * frame_info$scaling_y)
      
    }
  }
  
}



# pull information about triangles in group (x, y, order, ...)
linesSymbols_info_triangles <- function (triangles_inGroup) {

  dat_triangles <- base::data.frame("Index" = 1:base::length(triangles_inGroup))
  coln_x <- c("p1x", "p2x", "p3x")
  coln_y <- c("p1y", "p2y", "p3y")
  
  for (n_triang in 1:base::length(triangles_inGroup)) {
    
    points <- base::unlist(base::strsplit(xml2::xml_attr(triangles_inGroup[n_triang], "points"), split = " "))
    points <- base::as.numeric(base::unlist(base::strsplit(points, split = ",")))
    dat_triangles$Index[n_triang] <- n_triang
    dat_triangles$p1x[n_triang] <- points[1]
    dat_triangles$p1y[n_triang] <- points[2]
    dat_triangles$p2x[n_triang] <- points[3]
    dat_triangles$p2y[n_triang] <- points[4]
    dat_triangles$p3x[n_triang] <- points[5]
    dat_triangles$p3y[n_triang] <- points[6]
    dat_triangles$max_y[n_triang] <- base::max(dat_triangles[n_triang, coln_y])
    dat_triangles$min_y[n_triang] <- base::min(dat_triangles[n_triang, coln_y])
    dat_triangles$max_x[n_triang] <- base::max(dat_triangles[n_triang, coln_x])
    dat_triangles$min_x[n_triang] <- base::min(dat_triangles[n_triang, coln_x])
    dat_triangles$height[n_triang] <- dat_triangles$max_y[n_triang] - dat_triangles$min_y[n_triang]
    dat_triangles$halfheight[n_triang] <- dat_triangles$height[n_triang] / 2
    dat_triangles$point_upp[n_triang] <- base::which(dat_triangles[n_triang, coln_y] == dat_triangles$min_y[n_triang])
    dat_triangles$point_ldown[n_triang] <-  base::which(dat_triangles[n_triang, coln_y] == 
                                                          dat_triangles$max_y[n_triang] &
                                                          dat_triangles[n_triang, coln_x] == 
                                                          dat_triangles$min_x[n_triang])
    dat_triangles$point_rdown[n_triang] <- base::which(dat_triangles[n_triang, coln_y] == 
                                                         dat_triangles$max_y[n_triang] &
                                                         dat_triangles[n_triang, coln_x] == 
                                                         dat_triangles$max_x[n_triang])
    # plausibility check
    if (!base::all(c(1,2,3) %in% dat_triangles[n_triang, c("point_upp", "point_ldown", "point_rdown")])) {
      
      check_is <- dat_triangles[n_triang, c("point_upp", "point_ldown", "point_rdown")]
      stop (base::paste0("Dreiecke: Bei einem Dreieck ist die Punktangabe in points nicht eindeutig einer Position zuordenbar.
                         Soll: 1,2,3. Ist: ", check_is))
    }
    
    }
  
  # center of triangles
  dat_triangles$mean_x <- base::rowMeans(dat_triangles[ ,c("p1x", "p2x", "p3x")])
  dat_triangles$mean_y <- base::rowMeans(dat_triangles[ ,c("p1y", "p2y", "p3y")])
  
  # order of triangles
  dat_triangles$order_x <- base::rank(dat_triangles$min_x)
  dat_triangles$order_y <- base::rank(dat_triangles$min_y)
  
  # return
  return(dat_triangles)
  
}

linesSymbols_info_xs <- function (symbols_inGroup, frame_info, value_set) {
  
  # symbols info
  dat_symbols <- base::data.frame("Index" = 1:base::length(symbols_inGroup))
  dat_symbols$x1 <- base::as.numeric(xml2::xml_attr(symbols_inGroup, "x1"))
  dat_symbols$x2 <- base::as.numeric(xml2::xml_attr(symbols_inGroup, "x2"))
  dat_symbols$y1 <- base::as.numeric(xml2::xml_attr(symbols_inGroup, "y1"))
  dat_symbols$y2 <- base::as.numeric(xml2::xml_attr(symbols_inGroup, "y2"))
  dat_symbols$min_x <- base::apply(dat_symbols[ , c("x1", "x2")], 1, FUN=base::min)
  dat_symbols$min_y <- base::apply(dat_symbols[ , c("y1", "y2")], 1, FUN=base::min)
  dat_symbols$max_x <- base::apply(dat_symbols[ , c("x1", "x2")], 1, FUN=base::max)
  dat_symbols$max_y <- base::apply(dat_symbols[ , c("y1", "y2")], 1, FUN=base::max)
  dat_symbols$halfheight <- (dat_symbols$max_y - dat_symbols$min_y) / 2
  dat_symbols <- dplyr::arrange(dat_symbols, min_x)
  dat_symbols$Index_arrange <- 1:base::nrow(dat_symbols)
  dat_symbols$value <- base::rep(value_set, each = 2)
  dat_symbols$value_scale_y <- frame_info$max_y - (dat_symbols$value * frame_info$scaling_y)
  dat_symbols$value_scale_x <- frame_info$min_x + (dat_symbols$value * frame_info$scaling_x)
  dat_symbols$y1_new <- base::ifelse (dat_symbols$y1 == dat_symbols$min_y, 
                                      dat_symbols$value_scale_y - dat_symbols$halfheight,
                                      dat_symbols$value_scale_y + dat_symbols$halfheight)
  dat_symbols$y2_new <- base::ifelse (dat_symbols$y2 == dat_symbols$max_y, 
                                      dat_symbols$value_scale_y + dat_symbols$halfheight,
                                      dat_symbols$value_scale_y - dat_symbols$halfheight)
  dat_symbols$x1_new <- base::ifelse (dat_symbols$x1 == dat_symbols$min_x, 
                                      dat_symbols$value_scale_x - dat_symbols$halfheight,
                                      dat_symbols$value_scale_x + dat_symbols$halfheight)
  dat_symbols$x2_new <- base::ifelse (dat_symbols$x2 == dat_symbols$max_x, 
                                      dat_symbols$value_scale_x + dat_symbols$halfheight,
                                      dat_symbols$value_scale_x - dat_symbols$halfheight)
  
  return(dat_symbols)
  
}

linesSymbols_edit_triangles <- function (svg_in, group_name, frame_info, value_set, alignment) {
  
  # available triangles in group
  lineGroup <- linesSymbols_in(svg_in, group_name)
  triangles_inGroup <- xml2::xml_find_all(lineGroup, "./polygon")
  
  # information about triangles
  dat_triangles <- linesSymbols_info_triangles(triangles_inGroup)
  
  switch(alignment,
         
         horizontal = {
           
           for (n_triangles in 1:base::length(triangles_inGroup)) {
             
             # edit points
             triangle_toEdit <- triangles_inGroup[dat_triangles$order_x[n_triangles]]
             
             # calulate new points
             index_upp <- dat_triangles$point_upp[dat_triangles$order_x[n_triangles]]
             index_ldown <- dat_triangles$point_ldown[dat_triangles$order_x[n_triangles]]
             index_rdown <- dat_triangles$point_rdown[dat_triangles$order_x[n_triangles]]
             
             pmidy <- frame_info$max_y - ((value_set[dat_triangles$order_x[n_triangles]] * frame_info$scaling_y) +
                                            dat_triangles$halfheight[dat_triangles$order_x[n_triangles]])
             pminy <- frame_info$max_y - ((value_set[dat_triangles$order_x[n_triangles]] * frame_info$scaling_y) - 
                                            dat_triangles$halfheight[dat_triangles$order_x[n_triangles]])
             pmaxy <- frame_info$max_y - ((value_set[dat_triangles$order_x[n_triangles]] * frame_info$scaling_y) - 
                                            dat_triangles$halfheight[dat_triangles$order_x[n_triangles]])
   
             pmidx <- dat_triangles[dat_triangles$order_x[n_triangles], base::paste0("p", index_upp, "x")]
             pminx <- dat_triangles[dat_triangles$order_x[n_triangles], base::paste0("p", index_ldown, "x")]
             pmaxx <- dat_triangles[dat_triangles$order_x[n_triangles], base::paste0("p", index_rdown, "x")]

             p_mid <- base::paste0(pmidx, ",", pmidy)
             p_l <- base::paste0(pminx, ",", pminy)
             p_r <- base::paste0(pmaxx, ",", pmaxy)
             points_combined <- c(p_mid, p_l, p_r)
             order_points <- c(index_upp, index_ldown, index_rdown)
             points_new <- points_combined[base::sort(base::order(points_combined[order_points]))]
             points_new <- base::paste(points_new, collapse = " ")
             
             xml2::xml_set_attr(triangle_toEdit, "points", points_new)
             
           }
           
         },
         vertical = {
           
           # TODO
           
         })
  

  
  
  
}

linesSymbols_edit_circles <- function (svg_in, group_name, frame_info, value_set, alignment) {
  
  # available circles in group
  lineGroup <- linesSymbols_in(svg_in, group_name)
  symbols_inGroup <- xml2::xml_find_all(lineGroup, "./circle")  ## umstellen bei triangles auf symbols
  
  # order of circles
  symbols_order_x <- base::rank(base::as.numeric(xml2::xml_attr(symbols_inGroup, "cx")))
  symbols_order_y <- base::rank(-base::as.numeric(xml2::xml_attr(symbols_inGroup, "cy")))
  
  base::switch (alignment,
                
                horizontal = {
                  
                  for (n_symbols in 1:base::length(symbols_inGroup)) {
                    
                    symbol_toEdit <- symbols_inGroup[symbols_order_x[n_symbols]]
                    cy_new <- frame_info$max_y - value_set[symbols_order_x[n_symbols]] * frame_info$scaling_y
                    xml2::xml_set_attr(symbol_toEdit, "cy", cy_new)
                    
                  }
                  
                },
                
                vertical = { # testen, ob so funktioniert, default einbauen
                  
                  for (n_symbols in 1:base::length(symbols_inGroup)) {
                    
                    symbol_toEdit <- symbols_inGroup[symbols_order_y[n_symbols]]
                    cx_new <- frame_info$min_x + value_set[symbols_order_y[n_symbols]] * frame_info$scaling_x
                    xml2::xml_set_attr(symbol_toEdit, "cx", cx_new)
                    
                  }
                  
                })
  
  
}

linesSymbols_edit_rects <- function (svg_in, group_name, frame_info, value_set, alignment) {
  
  lineGroup <- linesSymbols_in(svg_in, group_name)
  symbols_inGroup <- xml2::xml_find_all(lineGroup, "./rect")  ## umstellen bei triangles auf symbols
  
  # order of rects
  symbols_order_x <- base::rank(base::as.numeric(xml2::xml_attr(symbols_inGroup, "x")))
  symbols_order_y <- base::rank(-base::as.numeric(xml2::xml_attr(symbols_inGroup, "y")))
  
  base::switch (alignment,
                
                horizontal = {
                  
                  for (n_symbols in 1:base::length(symbols_inGroup)) {
                    
                    symbol_toEdit <- symbols_inGroup[symbols_order_x[n_symbols]]
                    height_half <- base::as.numeric(xml2::xml_attr(symbol_toEdit, "height")) / 2
                    y_new <- frame_info$max_y - (value_set[symbols_order_x[n_symbols]] * frame_info$scaling_y + height_half)
                    xml2::xml_set_attr(symbol_toEdit, "y", y_new)
                    
                  }
                  
                },
                
                vertical = {
                  
                  for (n_symbols in 1:base::length(symbols_inGroup)) {
                    
                    symbol_toEdit <- symbols_inGroup[symbols_order_y[n_symbols]]
                    width_half <- base::as.numeric(xml2::xml_attr(symbol_toEdit, "width")) / 2
                    x_new <- frame_info$min_x + value_set[symbols_order_y[n_symbols]] * frame_info$scaling_x + width_half
                    xml2::xml_set_attr(symbol_toEdit, "x", x_new)
                    
                  }
                  
                })
  
}

linesSymbols_edit_xs <- function (svg_in, group_name, frame_info, value_set, alignment) {
  
  lineGroup <- linesSymbols_in(svg_in, group_name)
  symbols_inGroup <- xml2::xml_find_all(lineGroup, "./line")  ## umstellen bei triangles auf symbols
  # filter lines and symbols
  symbols_x1_values <- base::as.numeric(xml2::xml_attr(symbols_inGroup, "x1"))
  index_symbols <- base::which(symbols_x1_values %in% symbols_x1_values[base::which(base::duplicated(symbols_x1_values))])
  symbols_inGroup <- symbols_inGroup[index_symbols]
  # symbols info
  dat_symbols <- linesSymbols_info_xs(symbols_inGroup, frame_info, value_set)
  
  # edit symbols
  base::switch(alignment,
               
               horizontal = {
                 
                 for (n_symbols in 1:base::length(symbols_inGroup)) {
                   
                   symbol_toEdit <- symbols_inGroup[dat_symbols$Index[n_symbols]]
                   xml2::xml_set_attr(symbol_toEdit, "y1", dat_symbols$y1_new[dat_symbols$Index[n_symbols]])
                   xml2::xml_set_attr(symbol_toEdit, "y2", dat_symbols$y2_new[dat_symbols$Index[n_symbols]])
                   
                 }
                 
               },
               
               vertical = {
                 
                 for (n_symbols in 1:base::length(symbols_inGroup)) {
                   
                   symbol_toEdit <- symbols_inGroup[dat_symbols$Index[n_symbols]]
                   xml2::xml_set_attr(symbol_toEdit, "x1", dat_symbols$x1_new[dat_symbols$Index[n_symbols]])
                   xml2::xml_set_attr(symbol_toEdit, "x2", dat_symbols$x2_new[dat_symbols$Index[n_symbols]])
                   
                 }
                 
               })
  
}

## TODO: Check ob Anzahl Werte == Anzahl Elemente
linesSymbols <- function (svg_in, values, group_name, frame_name, scale_real, alignment, hasSymbols = TRUE, symbolType) {
  
  # input check
  if (hasSymbols) {
    if (!symbolType %in% c("triangle", "circle", "rect", "x")) {stop ("Ungueltiger Symboltyp.")}
  }
  
  # get frame info and scaling
  frame_info <- frame_and_scaling(svg_in, frame_name, scale_real)
  
  # if input-values == vector: transform to data.frame to get 1 row
  if (base::is.null(base::nrow(values))) {values <- base::t(base::data.frame(values))}
  
  # get called group
  linesSymbolsGroup <- stackedBar_in(svg_in, group_name)
  value_set <- values  ## TODO!!!!
  
  # 1 - Lines
  lineGroup <- linesSymbols_in(svg_in, group_name)
  lines_inGroup <- xml2::xml_find_all(lineGroup, "./line")
  
  if (symbolType == "x") {
    
    # filter lines and symbols (muss hier extra sein, da alles linien sind)
    x1_values <- base::as.numeric(xml2::xml_attr(lines_inGroup, "x1"))
    index_symbols <- base::which(x1_values %in% x1_values[base::which(base::duplicated(x1_values))])
    # filter lines
    lines_inGroup <- lines_inGroup[-index_symbols]

  }
  
  
  if (base::length(lines_inGroup) == 0) {
    warning ("Keine Linienelemente in der Gruppe vorhanden.")
  } else {
    order_lines <- linesSymbols_order_lines(lines_inGroup, alignment)
    linesSymbols_edit_lines (lines_inGroup, order_lines, frame_info, value_set)
  }
  
  # 2 - Symbols
  if (hasSymbols) {
    
    base::switch (symbolType,
                  
                  triangle = {
                    
                    linesSymbols_edit_triangles(svg_in, group_name, frame_info, value_set, alignment)
                    
                  },
                  
                  circle = {
                    
                    linesSymbols_edit_cirles(svg_in, group_name, frame_info, value_set, alignment)
                    
                  },
                  
                  rect = {
                    
                    linesSymbols_edit_rects(svg_in, group_name, frame_info, value_set, alignment)
                    
                  },
                  
                  x = {
                    
                    linesSymbols_edit_xs(svg_in, group_name, frame_info, value_set, alignment)
                    
                  })
    
  }  ## TODO: weitere Symbole
  
}




#svg_in <- svg_lines  ## TODO: Noch einbauen
#group_name <- "Kreuze"

#lineGroup <- stackedBar_in(svg_in, group_name)
#symbols_inGroup <- xml2::xml_find_all(lineGroup, "./line")  ## umstellen bei triangles auf symbols

# if (symbolType == "cross") {
#   
#   # filter lines and symbols
#   x1_values <- base::as.numeric(xml2::xml_attr(symbols_inGroup, "x1"))
#   x2_values <- base::as.numeric(xml2::xml_attr(symbols_inGroup, "x2"))
#   y1_values <- base::as.numeric(xml2::xml_attr(symbols_inGroup, "y1"))
#   y2_values <- base::as.numeric(xml2::xml_attr(symbols_inGroup, "y2"))
#   
#   ind_x_identical <- base::which(x1_values == x2_values)
#   ind_y_identical <- base::which(y1_values == y2_values)
#   
#   if (base::length(ind_y_identical) > base::length(ind_x_identical)) {
#     
#     dist_y <- y2_values - y1_values
#     dist_x <- base::round(x2_values - x1_values)[ind_y_identical]
#     ind_y_symb <- base::which(dist_x == base::min(base::unique(dist_x)))
#     ind_y_identical <- ind_y_identical[ind_y_symb]
#     
#   }
#   
#   index_symbols <- base::sort(c(ind_x_identical, ind_y_identical))
#   lines_inGroup <- symbols_inGroup[-index_symbols]
#   symbols_inGroup <- symbols_inGroup[index_symbols]
#   
# }






# 
# 
# scores_x <- value_set * frame_info$scaling_x
# scores_y <- value_set * frame_info$scaling_y
# 
# dat_symbols <- base::data.frame("Index" = 1:base::length(symbols_inGroup))
# dat_symbols$x1 <- base::as.numeric(xml2::xml_attr(symbols_inGroup, "x1"))
# dat_symbols$x2 <- base::as.numeric(xml2::xml_attr(symbols_inGroup, "x2"))
# dat_symbols$y1 <- base::as.numeric(xml2::xml_attr(symbols_inGroup, "y1"))
# dat_symbols$y2 <- base::as.numeric(xml2::xml_attr(symbols_inGroup, "y2"))
# dat_symbols$min_x <- base::apply(dat_symbols[ , c("x1", "x2")], 1, FUN=base::min)
# dat_symbols$min_y <- base::apply(dat_symbols[ , c("y1", "y2")], 1, FUN=base::min)
# dat_symbols$max_x <- base::apply(dat_symbols[ , c("x1", "x2")], 1, FUN=base::max)
# dat_symbols$max_y <- base::apply(dat_symbols[ , c("y1", "y2")], 1, FUN=base::max)
# dat_symbols$half_height <- (dat_symbols$max_x - dat_symbols$min_x) / 2
# dat_symbols$half_width <- (dat_symbols$max_y - dat_symbols$min_y) / 2
# dat_symbols <- dplyr::arrange(dat_symbols, min_x)
# dat_symbols$Index_sorted <- 1:base::nrow(dat_symbols)
# dat_symbols$value <- base::rep(value_set, each = 2)
# 
# cond_x <- dat_symbols$x1 == dat_symbols_x2
# cond_y1_min <- dat_symbols$y1 == dat_symbols$min_y
# cond_x1_min <- dat_symbols$x1 == dat_symbols$min_x
# 
# dat_symbols$y1_new ## TODO
# 
# 
# 
# 
# 
# 
# dat_symbols$height <- base::as.numeric(xml2::xml_attr(symbols_inGroup, "height"))
# dat_symbols$width <- base::as.numeric(xml2::xml_attr(symbols_inGroup, "width"))
# dat_symbols$order_x <- base::rank(base::as.numeric(xml2::xml_attr(symbols_inGroup, "x")))
# dat_symbols$order_y <- base::rank(-base::as.numeric(xml2::xml_attr(symbols_inGroup, "y")))
# dat_symbols$diagonal_half <- (dat_symbols$width * sqrt(2)) / 2
# dat_symbols$mid_x <- dat_symbols$x + dat_symbols$width / 2
# dat_symbols$mid_y <- dat_symbols$y + dat_symbols$height / 2
# dat_symbols$scores_x <- scores_x[dat_symbols$order_x]
# dat_symbols$scores_y <- scores_y[dat_symbols$order_y]
# 
# dat_symbols$p1r <- base::paste0(dat_symbols$scores_y + dat_symbols$diagonal_half, " ", dat_symbols$mid_y)
# dat_symbols$p2u <- base::paste0(dat_symbols$scores_y, " ", dat_symbols$mid_y + dat_symbols$diagonal_half)
# dat_symbols$p3l <- base::paste0(dat_symbols$scores_y - dat_symbols$diagonal_half, " ", dat_symbols$mid_y)
# dat_symbols$p4o <- base::paste0(dat_symbols$scores_y, " ", dat_symbols$mid_y - dat_symbols$diagonal_half)
# 
# 
# for (n_symbols in 1:base::lenght(symbols_inGroup)) {
#   
#   index_symbol <- dat_symbols$order_x[n_symbols]
#   symbol_toEdit <- symbols_inGroup[index_symbol]
#   symbol_points <- base::paste0(dat_symbols[index_symbol, c("p1r", "p2u", "p3l", "p4o")], collapse = ",")
#   
#   
# }



## TODO: rect zu polygon, Punkte einfuegen
## TODO: Check ob n symbole == n linien - 1
## TODO: Wenn Werte Skala > 100 sind, stimmt scaling nicht mehr.















