## svgtools
## Paket zum Anpassen von SVG-Vorlagen

## -- HILFSFUNKTIONEN



## -- frame_and_scaling: Hilfsfunktion: Liest Infos des genannten Rahmens aus und berechnet Skalierung
# svg_in: svg objekt
# frame_name: gesuchter Rahmenname; Zusatzcheck: Error, wenn Rahmenname nicht oder >1 vorhanden ist
# scale_minToMax: Skala min bis max, z.B. 0:100
# dep: xml2
# TODO: Check, ob sinnvolle Skala eingegeben wurde (?)
frame_and_scaling <- function(svg_in, frame_name, scale_minToMax) {
  
  # frame information
  frame_current <- base::data.frame("name" = frame_name,
                                    "min_x" = NA,
                                    "max_x" = NA,
                                    "min_y" = NA,
                                    "max_y" = NA,
                                    "diff_x" = NA,
                                    "diff_y" = NA,
                                    "scale_min" = base::min(scale_minToMax),
                                    "scale_max" = base::max(scale_minToMax),
                                    "scale_diff" = NA,
                                    "scaling_x" = NA,
                                    "scaling_y" = NA)
  
  frame_index <- base::which(xml2::xml_attr(xml2::xml_find_all(svg_in, "/svg/rect"), "id") == frame_name)
  fr <- xml2::xml_find_all(svg_in, "/svg/rect")[frame_index]
  
  # Check if frame is available and unique
  if (base::length(fr) != 1) {
    
    rects <- xml2::xml_find_all(svg_in, "/svg/rect")
    frames_av <- NULL
    
    for (rect in rects)
    {
      if (xml2::xml_has_attr(rect, "id"))
      {
        frames_av <- c(frames_av, xml2::xml_attr(rect, "id"))
      }
    }
    stop(base::paste0("Rahmen konnte nicht gefunden werden oder es gibt mehr Rahmen mit dem gleichen Namen.
                      Verfuegbare Rahmen:", frames_av))
    #stop(base::paste0("Frame not in document or more than one frames with the same name. Available frames:", frames_av))
    
  }
  
  # calculate scaling
  frame_current$min_x <- base::as.numeric(xml2::xml_attr(fr, "x"))
  frame_current$max_x <- frame_current$min_x + base::as.numeric(xml2::xml_attr(fr, "width"))
  frame_current$min_y <- base::as.numeric(xml2::xml_attr(fr, "y"))
  frame_current$max_y <- frame_current$min_y + base::as.numeric(xml2::xml_attr(fr, "height"))
  frame_current$diff_x <- frame_current$max_x - frame_current$min_x
  frame_current$diff_y <- frame_current$max_y - frame_current$min_y
  frame_current$scale_diff <- frame_current$scale_max - frame_current$scale_min
  frame_current$scaling_x <- base::abs(frame_current$diff_x / frame_current$scale_diff)
  frame_current$scaling_y <- base::abs(frame_current$diff_y / frame_current$scale_diff)
  
  # return
  return(frame_current)
  
}

# stackedBar_order_groups: Hilfsfunktion
# returns the order of the subgroups depending on the x and y values so that the input values can be mapped correctly to the
# right stacked bar groups. (order can be mixed up in the svg textfile such that it doesn't corresponds to the displayed svg!!)
stackedBar_order_groups <- function(stackedBarGroup, n_subgroups) {
  
  rects_value_y <- rects_value_x <- NULL
  
  if (!n_subgroups == 1) {
    
    for (n_children in 1:base::length(xml2::xml_children(stackedBarGroup))) {
      
      # get y and x value of rects (min)
      barSet <- xml2::xml_children(stackedBarGroup)[n_children]
      rects <- xml2::xml_find_all(barSet, "./rect")
      rects_value_y <- c(rects_value_y, base::min(base::as.numeric(xml2::xml_attr(rects, "y"))))
      rects_value_x <- c(rects_value_x, base::min(base::as.numeric(xml2::xml_attr(rects, "x"))))
      
    }
    
    stackedBars_order_y <- base::order(rects_value_y)
    stackedBars_order_x <- base::order(rects_value_x)
    
  } else {
    
    stackedBars_order_y <- 1
    stackedBars_order_x <- 1
    
  }
  
  return(base::data.frame(stackedBars_order_x, stackedBars_order_y))
  
}  

# stackedBar_in: Hilfsfunktion
# liest angefuehrte Gruppe ein, Check ob eindeutig vorhanden ist. gibt stackedBar Group aus
stackedBar_in <- function(svg_in, group_name) {
  
  # get called group
  named_groups <- xml2::xml_find_all(svg_in, "/svg/g")
  index_group <- base::which(xml2::xml_attr(named_groups, "id") == group_name)
  if (base::length(index_group) != 1) {stop("Fehler: Gruppenname nicht gefunden bzw. nicht eindeutig.")}  ## Diese Schritte ev. vorziehen
  stackedBarGroup <- named_groups[index_group]
  return(stackedBarGroup)
  
}

# stackedBar_checkSub: Hilfsfunktion
# liest n subgruppen aus, checkt ob n subgruppen == n Werte (rows), gibt n subgruppen aus
stackedBar_checkSub <- function(stackedBarGroup, values) {
  
  n_subgroups <- base::length(xml2::xml_find_all(stackedBarGroup, "g"))
  n_subgroups <- base::ifelse (n_subgroups == 0, 1, n_subgroups)
  if (n_subgroups != base::nrow(values)) {
    stop ("Fehler: Anzahl Subgruppen der gewaehlten Gruppe entspricht nicht der Anzahl der Zeilen der Werte.")
    #stop ("Error: Number of (sub)groups not identical to number of rows of values.")
  }
  return(n_subgroups)
  
}

# Hilfsfunktion: Rechtecke eines stackedBar bearbeiten, startpos und Breite anpassen
stackedBar_edit_rects <- function(rects, frame_info, value_set, order_rects, alignment) {
  
  # check: n values == n bars
  if (base::length(value_set) != base::length(rects)) {
    stop ("Fehler: Anzahl Werte entspricht nicht Anzahl Rechtecken.")
  }
  
  switch(alignment,
         
         horizontal = {
           
           # first rect
           rect_start <- base::which(order_rects == 1)
           rect_remain <- order_rects[-rect_start]
           xml2::xml_set_attr(rects[rect_start], "x", frame_info$min_x)
           xml2::xml_set_attr(rects[rect_start], "width", value_set[1] * frame_info$scaling_x)
           pos_next <- base::as.numeric(xml2::xml_attr(rects[rect_start], "x")) + (value_set[1] * frame_info$scaling_x)
           
           # remaining rects
           for (rect_nr in 1:base::length (order_rects_x)) {
             
             if (order_rects_x[rect_nr] == 1) {
               next
             }
             
             index_rect <- which(order_rects == order_rects[rect_nr])
             xml2::xml_set_attr(rects[index_rect], "x", pos_next)
             xml2::xml_set_attr(rects[index_rect], "width", value_set[order_rects[rect_nr]] * frame_info$scaling_x)
             pos_next <- pos_next + value_set[order_rects_x[rect_nr]] * frame_info$scaling_x
             
           }
           
         },
         
         vertical = {
           
           # first rect
           rect_start <- base::which(order_rects == 1)
           rect_remain <- order_rects[-rect_start]
           xml2::xml_set_attr(rects[rect_start], "y", frame_info$max_y - value_set[1] * frame_info$scaling_y)
           xml2::xml_set_attr(rects[rect_start], "height", value_set[1] * frame_info$scaling_y)
           pos_next <- base::as.numeric(xml2::xml_attr(rects[rect_start], "y"))
           
           # remaining rects
           for (rect_nr in 1:base::length (order_rects)) {
             
             if (order_rects[rect_nr] == 1) {
               next
             }
             
             index_rect <- which(order_rects == order_rects[rect_nr])
             xml2::xml_set_attr(rects[index_rect], "y", pos_next - value_set[rect_nr] * frame_info$scaling_y)
             xml2::xml_set_attr(rects[index_rect], "height", value_set[order_rects[rect_nr]] * frame_info$scaling_y)
             pos_next <- base::as.numeric(xml2::xml_attr(rects[rect_nr], "y"))
             
           }
           
         }) ## TODO: default
  
  #return(barSet)
  
}

# Hilfsfunktion: Textelemente eines stackedBar: richtige Reihenfolge herausfinden
stackedBar_order_text <- function(barLabels, value_set) {
  
  labels_value_y <- labels_value_x <- NULL
  
  # check: n values == n texts
  if (base::length(value_set) != base::length(barLabels)) {
    stop ("Fehler: Anzahl Werte entspricht nicht Anzahl Labels.")
  }
  
  for (lb in 1:base::length(barLabels)) {
    
    # get y value of rects (min)
    text_label <- barLabels[lb]
    text_matrix <- xml2::xml_attr(text_label, "transform")
    matrix_values_start <- stringr::str_locate(text_matrix, "matrix\\(")
    matrix_values <- stringr::str_sub(text_matrix, (matrix_values_start[2] + 1), (base::nchar(text_matrix) - 1))
    matrix_values <- base::as.numeric(base::unlist(base::strsplit(matrix_values, split = " ")))
    labels_value_y <- c(labels_value_y, matrix_values[length(matrix_values)])
    labels_value_x <- c(labels_value_x, matrix_values[length(matrix_values) - 1])
    
  }
  
  order_labels_x <- base::order(labels_value_x)
  order_labels_y <- base::order(-labels_value_y)
  
  return(base::data.frame(order_labels_x, order_labels_y))
  
}

# Hilfsfunktion: Text eines stackedBars bearbeiten: Text tauschen, Position anpassen
stackedBar_edit_text <- function(barLabels, order_labels, value_set, rects, order_rects, displayLimit, labelPosition, alignment) {
  
  switch (alignment,
          
          horizontal = {
            
            for (n_text in 1:base::length(barLabels)) {
              
              # change value
              text_toChange <- barLabels[order_labels[n_text]]
              xml2::xml_text(text_toChange) <- base::as.character(value_set[order_labels[n_text]])
              
              # comply with displayLimit
              if ((value_set[order_labels[n_text]] < displayLimit) | (value_set[order_labels[n_text]] == 0)) {
                xml2::xml_set_attr(text_toChange, "display", "none")
              }
              
              # change position
              rectinfo_pos_x <- base::as.numeric(xml2::xml_attr(rects[which(order_rects == order_labels[n_text])], "x"))
              rectinfo_pos_width <- base::as.numeric(xml2::xml_attr(rects[which(order_rects == order_labels[n_text])], "width"))
              
              # label position
              text_pos_center <- rectinfo_pos_x + (rectinfo_pos_width/2)
              text_pos_in <- base::ifelse (values[order_labels[n_text]] >= 0, rectinfo_pos_x + 10, 
                                           rectinfo_pos_x + rectinfo_pos_width - 10)
              text_pos_out <- base::ifelse (values[order_labels[n_text]] >= 0, rectinfo_pos_x + rectinfo_pos_width - 10,
                                            rectinfo_pos_x + 10)
              
              if (labelPosition == "center") {text_pos <- text_pos_center}
              if (labelPosition == "start") {text_pos <- text_pos_in}
              if (labelPosition == "end") {text_pos <- text_pos_out}

              
              text_matrix <- xml2::xml_attr(text_toChange, "transform")
              matrix_values_start <- stringr::str_locate(text_matrix, "matrix\\(")
              matrix_values <- stringr::str_sub(text_matrix, (matrix_values_start[2] + 1), (base::nchar(text_matrix) - 1))
              matrix_values <- base::as.numeric(base::unlist(base::strsplit(matrix_values, split = " ")))
              
              text_pos_matrix <- base::paste0("matrix(", matrix_values[1], " ", matrix_values[2], " ", matrix_values[3], " ", 
                                              matrix_values[4], " ", text_pos, " ", matrix_values[6], ")")
              
              xml2::xml_set_attr(text_toChange, "transform", text_pos_matrix)
              xml2::xml_set_attr(text_toChange, "text-anchor", "middle")
              
            }
            
          },
          
          vertical = {
            
            for (n_text in 1:base::length(barLabels)) {
              
              # change value
              text_toChange <- barLabels[order_labels[n_text]]
              xml2::xml_text(text_toChange) <- base::as.character(value_set[order_labels[n_text]])
              
              # comply with displayLimit
              if ((value_set[order_labels[n_text]] < displayLimit) | (value_set[order_labels[n_text]] == 0)) {
                xml2::xml_set_attr(text_toChange, "display", "none")
              }
              
              # change position
              rectinfo_pos_y <- base::as.numeric(xml2::xml_attr(rects[which(order_rects == order_labels[n_text])], "y"))
              rectinfo_pos_height <- base::as.numeric(xml2::xml_attr(rects[which(order_rects == order_labels[n_text])], "height"))
              
              # label position
              text_pos_center <- rectinfo_pos_y + (rectinfo_pos_height/2)
              text_pos_in <- base::ifelse (values[order_labels[n_text]] >= 0, rectinfo_pos_y + rectinfo_pos_height - 10, 
                                           rectinfo_pos_y + 10)
              text_pos_out <- base::ifelse (values[order_labels[n_text]] >= 0, rectinfo_pos_y + 10,
                                            rectinfo_pos_y + rectinfo_pos_height - 10)
              
              
              if (labelPosition == "center") {text_pos <- text_pos_center}
              if (labelPosition == "start") {text_pos <- text_pos_in}
              if (labelPosition == "end") {text_pos <- text_pos_out}
              
              text_matrix <- xml2::xml_attr(text_toChange, "transform")
              matrix_values_start <- stringr::str_locate(text_matrix, "matrix\\(")
              matrix_values <- stringr::str_sub(text_matrix, (matrix_values_start[2] + 1), (base::nchar(text_matrix) - 1))
              matrix_values <- base::as.numeric(base::unlist(base::strsplit(matrix_values, split = " ")))
              
              text_pos_matrix <- base::paste0("matrix(", matrix_values[1], " ", matrix_values[2], " ", matrix_values[3], " ", 
                                              matrix_values[4], " ", matrix_values[5], " ", text_pos, ")")
              
              xml2::xml_set_attr(text_toChange, "transform", text_pos_matrix)
              xml2::xml_set_attr(text_toChange, "text-anchor", "start")
              xml2::xml_set_attr(text_toChange, "dominant-baseline", "mathematical")
              
            }
            
          })
  
}







## -- HAUPTFUNKTIONEN

#' Liest eine SVG-Datei ein
#' @param file Dateiname (inkl. Pfad)
#' @param enc Encoding (default: "UTF-8")
#' @param summary Soll Zusammenfassung ueber SVG-File (Anzahl Gruppen, verwendete Farben...) angezeigt werden? (default: FALSE)
#' @param display Soll eingelesenes SVG im R-Studio Viewer mithilfe des packages "magick" angezeigt werden? (default: FALSE)
#' @return SVG als XML document
#' @export
#dep: xml2, summary_svg, display_svg
read_svg <- function(file, enc = "UTF-8", summary = FALSE, display = FALSE) {
  
  # read xml
  svg_in <- xml2::read_xml(x = file, encoding = enc, options = c("PEDANTIC","NOBLANKS","NSCLEAN"))
  svg_in <- xml2::xml_ns_strip(svg_in)
  
  # print summary
  if (summary) {summary_svg(svg_in)}
  
  # print svg
  if (display) {display_svg(svg_in)}
  
  # return
  return(svg_in)
  
}

#' Gibt eine Summary von eingelesenem SVG auf der Konsole aus
#' @param svg SVG als XML document
#' @export
#dep: xml2
summary_svg <- function(svg) {
  
  print ("************************")
  print ("** -- SVG SUMMARY: -- **")
  print ("************************")
  
  # Named Groups
  named_groups <- xml2::xml_find_all(svg, "/svg/g")
  base::print("-- NAMED GROUPS:")
  for (group in named_groups)
  {
    if (xml2::xml_has_attr(group, "id"))
    {
      group_name <- xml2::xml_attr(group, "id")
      num_children <- base::length(xml2::xml_children(group))
      base::print(base::paste0(group_name ," with ", num_children, " children"))
    }
  }
  
  # Available Frames
  rects <- xml2::xml_find_all(svg, "/svg/rect")
  base::print("-- AVAILABLE FRAMES:")
  for (rect in rects) {
    if (xml2::xml_has_attr(rect, "id"))
    {
      base::print(xml2::xml_attr(rect, "id"))
    }
  }
  
  # Used Fonts
  base::print("-- USED FONTS:")
  text_elements <- xml2::xml_find_all(svg, "/svg/text")
  used_fonts <- NULL
  for (font in text_elements) {
    used_fonts <- c(used_fonts, xml2::xml_attr(xml2::xml_children(font), "font-family"))
  }
  base::print(base::unique(used_fonts))
  
  # Used Font Sizes
  base::print("-- USED FONT SIZES:")
  used_sizes <- NULL
  for (size in text_elements) {
    used_sizes <- c(used_sizes, xml2::xml_attr(font, "font-size"))
  }
  base::print(base::unique(used_sizes))
  
  # Colors
  base::print("-- USED COLORS:")
  line_elements <- xml2::xml_find_all(svg, "/svg/line")
  used_colors <- NULL
  for (col in text_elements) {
    used_colors <- c(used_colors, xml2::xml_attr(xml2::xml_children(col), "fill"))
  }
  for (col in rects) {
    used_colors <- c(used_colors, xml2::xml_attr(col, "fill"))
  }
  for (col in line_elements) {
    used_colors <- c(used_colors, xml2::xml_attr(col, "stroke"))
  }
  for (col in line_elements) {
    used_colors <- c(used_colors, xml2::xml_attr(col, "fill"))
  }
  used_colors <- base::as.character(stats::na.omit(base::unique(used_colors)))
  base::print(used_colors)
  
  
  
}

#' Zeigt eingelesenes SVG an
#' 
#' @description Die Funktionalitaet haengt von der Entwicklungsumgebung ab. In RStudio wird das SVG im Viewer angezeigt.
#' @param svg SVG als XML document
#' @param width gewuenschte Breite (in Pixel) der Ausgabe (default: NULL)
#' @param height gewuenschte Hoehe (in Pixel) der Ausgabe (default: NULL)
#' @details Wenn weder Breite noch Hoehe angegeben werden, wird die Originalgroesse (gegeben DPI) ausgegeben. Wenn nur einer dieser Parameter angegeben wird, wird der andere entsprechend des Originalverhaeltnisses automatisch skaliert.
#' @export
#dep: magick
display_svg <- function(svg, width = NULL, height = NULL) {
  rsvg <- rsvg::rsvg(charToRaw(toString(svg)),width = width,height = height) #wandelt das XML-Objekt zunaechst in einen String und dann in Bytes um; wird von von rsvg zu bitmap gerendert
  base::print(magick::image_read(rsvg))
}

#' Schreibt eingelesenes SVG in Datei
#' @param svg SVG als XML document
#' @param file Dateiname (inkl. Pfad)
#' @export
#dep: xml2, magick
write_svg <- function(svg, file) {

  # add default namespace
  xml2::xml_set_attr(xml2::xml_find_all(svg, "/svg"), "xmlns", "http://www.w3.org/2000/svg")
  
  # write svg
  xml2::write_xml(x = svg, file = file)
  #cat("svg gespeichert als: ", file, "\n")

}

# stackedBar: passt stacked bar an, rects und text
stackedBar <- function(svg_in, values, group_name, frame_name, scale_real, alignment, hasLabels = TRUE, labelPosition = "center", displayLimit = 0) {
  
  # get frame info and scaling
  frame_info <- frame_and_scaling(svg_in, frame_name, scale_real)
  
  # if input-values == vector: transform to data.frame to get 1 row
  if (base::is.null(base::nrow(values))) {values <- base::t(base::data.frame(values))}
  
  # get called group
  stackedBarGroup <- stackedBar_in(svg_in, group_name)
  
  # get n subgroups
  n_subgroups <- stackedBar_checkSub(stackedBarGroup, values)

  # get order of (sub)groups in xml depending on x-value and depending on y-value
  order_groups <- stackedBar_order_groups(stackedBarGroup, n_subgroups)
  stackedBars_order_x <- order_groups$stackedBars_order_x
  stackedBars_order_y <- order_groups$stackedBars_order_y

  
  # adjust all rect-elements and text-elements of all groups
  for (bar_nr in 1:n_subgroups) {
    
    # values for barSet
    value_set <- values[bar_nr, ]
    
    ## - RECTS
    # get: barSet, rects of barSet, right ordering
    if (!n_subgroups == 1) {
      barSet <- xml2::xml_find_all(stackedBarGroup, "./g")[stackedBars_order_y[bar_nr]]
    } else {
      barSet <- stackedBarGroup
    }
    rects <- xml2::xml_find_all(barSet, "./rect")
    order_rects_x <- base::rank(base::as.numeric(xml2::xml_attr(rects, "x")))
    order_rects_y <- base::rank(-base::as.numeric(xml2::xml_attr(rects, "y")))
    if (alignment == "horizontal") {order_rects <- order_rects_x}
    if (alignment == "vertical") {order_rects <- order_rects_y}
    
    # edit rects
    stackedBar_edit_rects(rects, frame_info, value_set, order_rects, alignment)

    
    ## -- TEXT/LABELS
    # get text elements of group and right ordering
    if (hasLabels) {
      
      barLabels <- xml2::xml_find_all(barSet, "./text")
      order_textOut <- stackedBar_order_text(barLabels, value_set)
      order_labels_x <- order_textOut$order_labels_x
      order_labels_y <- order_textOut$order_labels_y
      if (alignment == "horizontal") {order_labels <- order_labels_x}
      if (alignment == "vertical") {order_labels <- order_labels_y}
      
      # adjust values and position of text elements
      stackedBar_edit_text(barLabels, order_labels, value_set, rects, order_rects, displayLimit, labelPosition, alignment)
    
    }

  }
  
  # return
  return(svg_in)
  
}




## -- Differenzbalken
# Differenzbalken edit rects
diffBar_edit_rects <- function (rects, values, frame_info, frame0_name, order_rects, alignment) {
  
  frame0_x <- xml2::xml_find_all(svg_in, "./line")
  frame0_x <- base::as.numeric(xml2::xml_attr(frame0_x[base::which(xml2::xml_attr(frame0_x, "id")
                                                                   == frame0_name)], "x1"))
  frame0_y <- xml2::xml_find_all(svg_in, "./line")
  frame0_y <- base::as.numeric(xml2::xml_attr(frame0_y[base::which(xml2::xml_attr(frame0_y, "id")
                                                                   == frame0_name)], "y1"))
  
  
  base::switch (alignment,
                
                "horizontal" = {
                  
                  for (rect_nr in 1:base::length(rects)) {
                    
                    if (values[order_rects[rect_nr]] > 0) {
                      
                      xml2::xml_set_attr(rects[order_rects[rect_nr]], "x", frame0_x)
                      xml2::xml_set_attr(rects[order_rects[rect_nr]], 
                                         "width", values[order_rects[rect_nr]] * frame_info$scaling_x)
                      
                    }
                    
                    if (values[order_rects[rect_nr]] < 0) {
                      
                      xml2::xml_set_attr(rects[order_rects[rect_nr]], "x", frame0_x + values[order_rects[rect_nr]] *
                                           frame_info$scaling_x)
                      xml2::xml_set_attr(rects[order_rects[rect_nr]], "width", 
                                         base::abs(values[order_rects[rect_nr]]) * frame_info$scaling_x)
                      
                    }
                    
                    if (values[order_rects[rect_nr]] == 0) {
                      
                      xml2::xml_set_attr(rects[order_rects[rect_nr]], "display", "none")
                      
                    }
                    
                  }
                  
                  
                },
                
                "vertical" = {
                  
                  for (rect_nr in 1:base::length(rects)) {
                    
                    if (values[order_rects[rect_nr]] > 0) {
                      
                      xml2::xml_set_attr(rects[order_rects[rect_nr]], "y", frame0_y)
                      xml2::xml_set_attr(rects[order_rects[rect_nr]], 
                                         "height", values[order_rects[rect_nr]] * frame_info$scaling_y)
                      
                    }
                    
                    if (values[order_rects[rect_nr]] < 0) {
                      
                      xml2::xml_set_attr(rects[order_rects[rect_nr]], "y", frame0_y + values[order_rects[rect_nr]] * 
                                           frame_info$scaling_y)
                      xml2::xml_set_attr(rects[order_rects[rect_nr]], "height", 
                                         base::abs(values[order_rects[rect_nr]]) * frame_info$scaling_y)
                      
                    }
                    
                    if (values[order_rects[rect_nr]] == 0) {
                      
                      xml2::xml_set_attr(rects[order_rects[rect_nr]], "display", "none")
                      
                    }
                    
                  }
                  
                })
  
}

# Differenzbalken edit text
diffBar_edit_texts <- function (barLabels, order_labels, values, rects, order_rects, displayLimit, labelPosition, alignment) {
  
  for (n_text in 1:base::length(barLabels)) {
    
    # change value
    text_toChange <- barLabels[n_text]
    xml2::xml_text(text_toChange) <- base::as.character(values[order_labels[n_text]])
    
    # comply with displayLimit
    if ((base::abs(values[order_labels[n_text]]) < displayLimit) | (values[order_labels[n_text]] == 0)) {
      xml2::xml_set_attr(text_toChange, "display", "none")
    }
    
    # change position
    rectinfo_pos_x <- base::as.numeric(xml2::xml_attr(rects[which(order_rects == order_labels[n_text])], "x"))
    rectinfo_pos_width <- base::as.numeric(xml2::xml_attr(rects[which(order_rects == order_labels[n_text])], "width"))
    rectinfo_pos_y <- base::as.numeric(xml2::xml_attr(rects[which(order_rects == order_labels[n_text])], "y"))
    rectinfo_pos_height <- base::as.numeric(xml2::xml_attr(rects[which(order_rects == order_labels[n_text])], "height"))
    
    if (alignment == "horizontal") {
      
      text_pos_center <- rectinfo_pos_x + (rectinfo_pos_width/2)
      text_pos_in <- base::ifelse (values[order_labels[n_text]] >= 0, rectinfo_pos_x + 10, 
                                   rectinfo_pos_x + rectinfo_pos_width - 10)
      text_pos_out <- base::ifelse (values[order_labels[n_text]] >= 0, rectinfo_pos_x + rectinfo_pos_width - 10,
                                    rectinfo_pos_x + 10)
      
    } else {
      
      text_pos_center <- rectinfo_pos_y + (rectinfo_pos_height/2)
      text_pos_in <- base::ifelse (values[order_labels[n_text]] >= 0, rectinfo_pos_y + rectinfo_pos_height - 10, 
                                   rectinfo_pos_y + 10)
      text_pos_out <- base::ifelse (values[order_labels[n_text]] >= 0, rectinfo_pos_y + 10,
                                    rectinfo_pos_y + rectinfo_pos_height - 10)
      
    }
    
    if (labelPosition == "center") {text_pos <- text_pos_center}
    if (labelPosition == "start") {text_pos <- text_pos_in}
    if (labelPosition == "end") {text_pos <- text_pos_out}
    
    text_matrix <- xml2::xml_attr(text_toChange, "transform")
    matrix_values_start <- stringr::str_locate(text_matrix, "matrix\\(")
    matrix_values <- stringr::str_sub(text_matrix, (matrix_values_start[2] + 1), (base::nchar(text_matrix) - 1))
    matrix_values <- base::as.numeric(base::unlist(base::strsplit(matrix_values, split = " ")))
    
    base::ifelse (alignment == "horizontal", matrix_values[5] <- text_pos, matrix_values[6] <- text_pos)
    
    text_pos_matrix <- base::paste0("matrix(", matrix_values[1], " ", matrix_values[2], " ", matrix_values[3], " ", 
                                    matrix_values[4], " ", matrix_values[5], " ", matrix_values[6], ")")
    
    xml2::xml_set_attr(text_toChange, "transform", text_pos_matrix)
    xml2::xml_set_attr(text_toChange, "text-anchor", "middle")
    
  }
  
}

# Differenzbalken Hauptfunktion
diffBar <- function(svg_in, values, group_name, frame_name, frame0_name, scale_real, alignment, hasLabels = TRUE, labelPosition = "center", displayLimit = 0) {
  
  # get frame info and scaling
  frame_info <- frame_and_scaling(svg_in, frame_name, scale_real)
  
  # if input-values == vector: transform to data.frame to get 1 row
  if (base::is.null(base::nrow(values))) {values <- base::t(base::data.frame(values))}
  
  # get called group
  symbolGroup <- stackedBar_in(svg_in, group_name)
  
  # get rects
  rects <- xml2::xml_find_all(symbolGroup, "./rect")
  if (base::length(rects) != base::length(values)) {stop("Anzahl Rechtecke != Anzahl Werte.")}
  
  # get order of rects
  order_rects_x <- base::order(base::as.numeric(xml2::xml_attr(rects, "y")))
  order_rects_y <- base::order(base::as.numeric(xml2::xml_attr(rects, "x")))
  if (alignment == "horizontal") {order_rects <- order_rects_x}
  if (alignment == "vertical") {order_rects <- order_rects_y}
  
  # edit all rects
  diffBar_edit_rects(rects, values, frame_info, frame0_name, order_rects, alignment)
  
  # edit text
  if (hasLabels) {
    
    texts <- xml2::xml_find_all(symbolGroup, "./text")
    if (base::length(texts) != base::length(values)) {stop("Anzahl Labels != Anzahl Werte.")}
    order_textOut <- stackedBar_order_text(texts, values)
    order_labels_x <- order_textOut$order_labels_x
    order_labels_y <- base::rev(order_textOut$order_labels_y)
    if (alignment == "horizontal") {order_labels <- order_labels_y}
    if (alignment == "vertical") {order_labels <- order_labels_x}
    diffBar_edit_texts(texts, order_labels, values, rects, order_rects, displayLimit, labelPosition, alignment)
    
  }
  
}






## -- Hilfsfunktionen/Tools
# wandelt strings in svg-style Strings um
svg_TextToSvgFormat <- function(input_text) {
  
  input_text <- base::gsub("_", "_x5F_", input_text)
  input_text <- base::gsub("\\(", "_x28_", input_text)
  input_text <- base::gsub("\\)", "_x29_", input_text)
  input_text <- base::gsub("\\,", "C", input_text)
  input_text <- base::gsub("\\;", "_x3B_", input_text)
  input_text <- base::gsub(" ", "__", input_text)
  input_text <- base::gsub("--", "_x2013_", input_text)
  input_text <- base::gsub("\\*", "_x2A_", input_text)
  return(input_text)
  
}

# elemente ausblenden
svg_hideElement <- function(svg_in, element_name, element_type) {
  
  if (element_type == "text") {
    
    elements <- xml2::xml_find_all(svg_in, "./text")
    
    for (element_nr in 1:base::length(element_name)) {
      
      element_search <- svg_TextToSvgFormat(element_name[element_nr])
      check1 <- base::length(base::which(xml2::xml_text(elements) == element_search)) == 0
      check2 <- base::length(base::which(xml2::xml_attr(elements, "id") == element_search)) == 0
      
      
      if (check1 & check2) {stop (base::paste0("Kein Textelement mit der Bezeichnung ", element_name[element_nr], " gefunden."))}
      
      if (base::length(base::which(xml2::xml_text(elements) == element_search)) == 0) {
        xml2::xml_set_attr(elements[base::which(xml2::xml_attr(elements, "id") == 
                                                  element_search)], "display", "none")
      } else {
        xml2::xml_set_attr(elements[base::which(xml2::xml_text(elements) == 
                                                  element_search)], "display", "none")
      }
      
    }
    
  } else {
    
    elements <- xml2::xml_find_all(svg_in, base::paste0("./", element_type))
    
    for (element_nr in 1:base::length(element_name)) {
      
      element_search <- svg_TextToSvgFormat(element_name[element_nr])
      xml2::xml_set_attr(elements[base::which(xml2::xml_attr(elements, "id") == 
                                                element_search)], "display", "none")
      
    }
    
  }
  
}

# Passt Balkenelemente und Textelement hinsichtlich Ausrichtung an
stackedBar_adjust <- function(svg_in, group_name, alignment, ref_rect = NULL, ref_text = NULL) {
  
  # check
  if (!(alignment %in% c("horizontal", "vertical"))) {stop("alignment paramenter muss 'horizontal' oder 'vertical' sein.")}
  
  # check if subgroup has to be adjusted
  subgroup <- ifelse (base::grepl("/", group_name) == FALSE, FALSE, TRUE)
  # check if all subgroups should be adjusted
  if (subgroup) {
    subgroup_all <- ifelse (base::substr(group_name, base::nchar(group_name), base::nchar(group_name)) == "/", TRUE, FALSE)
  }
  
  
  subgroup_levels <- base::length(base::unlist(base::strsplit(group_name, split = "/")))  ## TODO
  
  group_name_main <- base::unlist(base::strsplit(group_name, split = "/"))[1]
  group_name_sub <- base::unlist(base::strsplit(group_name, split = "/"))[2]
  
  symbolGroup <- stackedBar_in(svg_in, group_name_main)
  symbolGroup_subs <- xml2::xml_find_all(symbolGroup, "g")
  
  if (subgroup_all) {
    symbolGroup_edit <- symbolGroup
  } else if (subgroup) {
    symbolGroup_edit <- symbolGroup_subs[base::which(xml2::xml_attr(symbolGroup_subs, "id") == group_name_sub)]
  } else {
    symbolGroup_edit <- symbolGroup
  }
  
  
  n_subgroups <- base::length(xml2::xml_find_all(symbolGroup_edit, "g"))
  if (n_subgroups == 0) {
    n_subgroups <- 1
  }
  
  
  # edit rect position
  if (!is.null(ref_rect)) {
    
    for (n_groups in 1:n_subgroups) {
      
      if (n_subgroups == 1) {
        group_toEdit <- symbolGroup_edit
      } else {
        group_toEdit <- xml2::xml_find_all(symbolGroup_edit, "./g")[n_groups]
      }
      rects <- xml2::xml_find_all(group_toEdit, "./rect")
      order_rects_x <- base::rank(base::as.numeric(xml2::xml_attr(rects, "x")))
      order_rects_y <- base::rank(-base::as.numeric(xml2::xml_attr(rects, "y")))
      if (alignment == "horizontal") {order_rects <- order_rects_x}
      if (alignment == "vertical") {order_rects <- order_rects_y}
      x_adjust <- xml2::xml_attr(rects[base::which(order_rects == ref_rect)], "x")
      y_adjust <- xml2::xml_attr(rects[base::which(order_rects == ref_rect)], "y")
      value_adjust <- base::ifelse (alignment == "horizontal", y_adjust, x_adjust)
      variable_adjust <- base::ifelse (alignment == "horizontal", "y", "x")
      
      for (rect_nr in 1:base::length(rects)) {
        
        xml2::xml_set_attr(rects[order_rects[rect_nr]], variable_adjust, value_adjust)
        
      }
      
    }
    
  }
  
  ## edit text position
  if (!is.null(ref_text)) {
    
    for (n_groups in 1:n_subgroups) {
      
      if (n_subgroups == 1) {
        group_toEdit <- symbolGroup_edit
      } else {
        group_toEdit <- xml2::xml_find_all(symbolGroup_edit, "./g")[n_groups]
      }
      texts <- xml2::xml_find_all(group_toEdit, "./text")
      
      order_textOut <- stackedBar_order_text(texts, 1:base::length(texts))
      order_labels_x <- order_textOut$order_labels_x
      order_labels_y <- order_textOut$order_labels_y
      if (alignment == "horizontal") {
        order_labels <- order_labels_x
      } else {
        order_labels <- order_labels_y
      }
      
      text_matrix <- xml2::xml_attr(texts[base::which(order_labels == ref_text)], "transform")
      matrix_values_start <- stringr::str_locate(text_matrix, "matrix\\(")
      matrix_values <- stringr::str_sub(text_matrix, (matrix_values_start[2] + 1), (base::nchar(text_matrix) - 1))
      matrix_values <- base::as.numeric(base::unlist(base::strsplit(matrix_values, split = " ")))
      
      value_adjust <- base::ifelse(alignment == "horizontal", matrix_values[6], matrix_values[5])
      variable_adjust <- base::ifelse(alignment == "horizontal", 6, 5)
      
      for (n_text in 1:base::length(texts)) {
        
        text_matrix <- xml2::xml_attr(texts[base::which(order_labels == n_text)], "transform")
        matrix_values_start <- stringr::str_locate(text_matrix, "matrix\\(")
        matrix_values <- stringr::str_sub(text_matrix, (matrix_values_start[2] + 1), (base::nchar(text_matrix) - 1))
        matrix_values <- base::as.numeric(base::unlist(base::strsplit(matrix_values, split = " ")))
        matrix_values[variable_adjust] <- value_adjust
        text_pos_matrix <- base::paste0("matrix(", matrix_values[1], " ", matrix_values[2], " ", matrix_values[3], " ", 
                                        matrix_values[4], " ", matrix_values[5], " ", matrix_values[6], ")")
        xml2::xml_set_attr(texts[base::which(order_labels == n_text)], "transform", text_pos_matrix)
        
      }
      
    }
    
  }
  
}

# Passt Farben von Balkenelementen und Textelementen eines stackedBars an
stackedBar_setColor <- function(svg_in, group_name, alignment, color_rects = NULL, color_texts = NULL) {
  
  # check if subgroup has to be adjusted
  subgroup <- ifelse (base::grepl("/", group_name) == FALSE, FALSE, TRUE)
  # check if all subgroups should be adjusted
  if (subgroup) {
    subgroup_all <- ifelse (base::substr(group_name, base::nchar(group_name), base::nchar(group_name)) == "/", TRUE, FALSE)
  }
  
  subgroup_levels <- base::length(base::unlist(base::strsplit(group_name, split = "/")))  ## TODO
  
  group_name_main <- base::unlist(base::strsplit(group_name, split = "/"))[1]
  group_name_sub <- base::unlist(base::strsplit(group_name, split = "/"))[2]
  
  symbolGroup <- stackedBar_in(svg_in, group_name_main)
  symbolGroup_subs <- xml2::xml_find_all(symbolGroup, "g")
  
  if (subgroup_all) {
    symbolGroup_edit <- symbolGroup
  } else if (subgroup) {
    symbolGroup_edit <- symbolGroup_subs[base::which(xml2::xml_attr(symbolGroup_subs, "id") == group_name_sub)]
  } else {
    symbolGroup_edit <- symbolGroup
  }
  
  
  n_subgroups <- base::length(xml2::xml_find_all(symbolGroup_edit, "g"))
  if (n_subgroups == 0) {
    n_subgroups <- 1
  }
  
  
  # rects
  if (!base::is.null(color_rects)) {
    
    for (n_groups in 1:n_subgroups) {
      
      if (n_subgroups == 1) {
        group_toEdit <- symbolGroup_edit
      } else {
        group_toEdit <- xml2::xml_find_all(symbolGroup_edit, "./g")[n_groups]
      }
      rects <- xml2::xml_find_all(group_toEdit, "./rect")
      
      # check
      if (base::length(rects) != base::length(color_rects)) {
        stop ("Anzahl Rechtecke entspricht nicht Anzahl Farben.")
      }
      
      order_rects_x <- base::rank(base::as.numeric(xml2::xml_attr(rects, "x")))
      order_rects_y <- base::rank(-base::as.numeric(xml2::xml_attr(rects, "y")))
      if (alignment == "horizontal") {order_rects <- order_rects_x}
      if (alignment == "vertical") {order_rects <- order_rects_y}
      
      for (n_rects in 1:base::length(rects)) {
        
        xml2::xml_set_attr(rects[order_rects[n_rects]], "fill", color_rects[n_rects])
        ## TODO: if is.null
        
      }
      
    }
    
  }
  
  
  # texts
  if (!base::is.null(color_texts)) {
    
    for (n_groups in 1:n_subgroups) {
      
      if (n_subgroups == 1) {
        group_toEdit <- symbolGroup_edit
      } else {
        group_toEdit <- xml2::xml_find_all(symbolGroup_edit, "./g")[n_groups]
      }
      
      texts <- xml2::xml_find_all(group_toEdit, "./text")
      
      # check
      if (base::length(texts) != base::length(color_texts)) {
        stop ("Anzahl Lables entspricht nicht Anzahl Farben.")
      }
      
      order_labels <- stackedBar_order_text(texts, 1:base::length(texts))
      if (alignment == "horizontal") {order_labels <- order_labels$order_labels_x}
      if (alignment == "vertical") {order_labels <- order_labels$order_labels_y}
      
      for (n_texts in 1:base::length(texts)) {
        
        xml2::xml_set_attr(texts[order_labels[n_texts]], "fill", color_texts[n_texts])
        ## TODO: if is.null
        
      }
      
    }
    
  }
  
}

# Liest Farbcode von einem Element aus
svg_getElementColor <- function(svg_in, element_name, element_type) {
  
  elements <- xml2::xml_find_all(svg_in, base::paste0("./", element_type))
  color_out <- NULL
  
  for (element_nr in 1:base::length(element_name)) {
    
    
    color_out <- c(color_out, xml2::xml_attr(elements[base::which(xml2::xml_attr(elements, "id") == 
                                                                    element_name[element_nr])], "fill"))
    
  }
  
  return(color_out)
  
}

# Ersetzt Farbcode eines Elements
svg_setElementColor <- function(svg_in, element_name, element_type, color_new) {
  
  elements <- xml2::xml_find_all(svg_in, base::paste0("./", element_type))
  
  for (element_nr in 1:base::length(element_name)) {
    
    xml2::xml_set_attr(elements[base::which(xml2::xml_attr(elements, "id") == 
                                              element_name[element_nr])], "fill", color_new[element_nr])
    
  }
  
}

# Ersetzt Text in Textelement
svg_setElementText <- function(svg_in, element_name, text_new, inGroup = NULL) {
  
  if (!is.null(inGroup)) {
    availableGroups <- xml2::xml_find_all(svg_in, "g")
    searchGroup <- availableGroups[base::which(xml2::xml_attr(availableGroups, "id") == inGroup)]
    elements <- xml2::xml_find_all(searchGroup, "./text")
  } else {
    elements <- xml2::xml_find_all(svg_in, "./text")
  }
  
  for (element_nr in 1:base::length(element_name)) {
    
    check1 <- base::length(base::which(xml2::xml_text(elements) == element_name[element_nr])) == 0
    check2 <- base::length(base::which(xml2::xml_attr(elements, "id") == element_name[element_nr])) == 0
    
    
    if (check1 & check2) {stop (base::paste0("Kein Textelement mit der Bezeichnung ", element_name[element_nr], " gefunden."))}
    
    if (base::length(base::which(xml2::xml_text(elements) == element_name[element_nr])) == 0) {
      xml2::xml_set_text(elements[base::which(xml2::xml_attr(elements, "id") == 
                                                element_name[element_nr])], text_new[element_nr])
    } else {
      xml2::xml_set_text(elements[base::which(xml2::xml_text(elements) == 
                                                element_name[element_nr])], text_new[element_nr])
    }
    
  }
  
}








