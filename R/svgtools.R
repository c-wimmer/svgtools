### svgtools
### Paket zum Anpassen von SVG-Vorlagen

### DATEIMANAGEMENT ----

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
  print("-- NAMED GROUPS:")
  for (group in named_groups)
  {
    if (xml2::xml_has_attr(group, "id"))
    {
      group_name <- xml2::xml_attr(group, "id")
      num_children <- length(xml2::xml_children(group))
      print(paste0(group_name ," with ", num_children, " children"))
    }
  }
  
  # Available Frames
  rects <- xml2::xml_find_all(svg, "/svg/rect")
  print("-- AVAILABLE FRAMES:")
  for (rect in rects) {
    if (xml2::xml_has_attr(rect, "id"))
    {
      print(xml2::xml_attr(rect, "id"))
    }
  }
  
  # Used Fonts
  print("-- USED FONTS:")
  svg_all_elements <- xml2::xml_contents(svg)
  text_elements <- xml2::xml_find_all(svg_all_elements, "text")
  used_fonts <- NULL
  for (font in text_elements) {
    used_fonts <- c(used_fonts, xml2::xml_attr(font, "font-family"))
  }
  print(unique(used_fonts))
  
  # Used Font Sizes
  print("-- USED FONT SIZES:")
  used_sizes <- NULL
  for (size in text_elements) {
    used_sizes <- c(used_sizes, xml2::xml_attr(font, "font-size"))
  }
  print(unique(used_sizes))
  
  # Colors
  print("-- USED COLORS:")
  line_elements <- xml2::xml_find_all(svg_all_elements, "line")
  rect_elements <- xml2::xml_find_all(svg_all_elements, "rect")
  used_colors <- NULL
  for (col in text_elements) {
    used_colors <- c(used_colors, xml2::xml_attr(xml2::xml_children(col), "fill"))
  }
  for (col in rect_elements) {
    used_colors <- c(used_colors, xml2::xml_attr(col, "fill"))
  }
  for (col in line_elements) {
    used_colors <- c(used_colors, xml2::xml_attr(col, "stroke"))
  }
  for (col in line_elements) {
    used_colors <- c(used_colors, xml2::xml_attr(col, "fill"))
  }
  used_colors <- as.character(stats::na.omit(unique(used_colors)))
  print(used_colors)
  
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
  print(magick::image_read(rsvg))
}

#' Schreibt SVG in Datei
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

### ALLGEMEINE HILFSFUNKTIONEN ----

## -- frame_and_scaling: Hilfsfunktion: Liest Infos des genannten Rahmens aus und berechnet Skalierung
# svg_in: svg objekt
# frame_name: gesuchter Rahmenname; Zusatzcheck: Error, wenn Rahmenname nicht oder >1 vorhanden ist
# scale_minToMax: Skala min bis max, z.B. 0:100
# dep: xml2
# TODO: Check, ob sinnvolle Skala eingegeben wurde (?)
frame_and_scaling <- function(svg_in, frame_name, scale_minToMax) {
  
  # frame information
  frame_current <- data.frame("name" = frame_name,
                                    "min_x" = NA,
                                    "max_x" = NA,
                                    "min_y" = NA,
                                    "max_y" = NA,
                                    "diff_x" = NA,
                                    "diff_y" = NA,
                                    "scale_min" = min(scale_minToMax),
                                    "scale_max" = max(scale_minToMax),
                                    "scale_diff" = NA,
                                    "scaling_x" = NA,
                                    "scaling_y" = NA)
  
  frame_index <- which(xml2::xml_attr(xml2::xml_find_all(svg_in, "/svg/rect"), "id") == frame_name)
  fr <- xml2::xml_find_all(svg_in, "/svg/rect")[frame_index]
  
  # Check if frame is available and unique
  if (length(fr) != 1) {
    
    rects <- xml2::xml_find_all(svg_in, "/svg/rect")
    frames_av <- NULL
    
    for (rect in rects)
    {
      if (xml2::xml_has_attr(rect, "id"))
      {
        frames_av <- c(frames_av, xml2::xml_attr(rect, "id"))
      }
    }
    stop(paste0("Rahmen konnte nicht gefunden werden oder es gibt mehr Rahmen mit dem gleichen Namen.
                      Verfuegbare Rahmen:", frames_av))
    #stop(paste0("Frame not in document or more than one frames with the same name. Available frames:", frames_av))
    
  }
  
  # calculate scaling
  frame_current$min_x <- as.numeric(xml2::xml_attr(fr, "x"))
  frame_current$max_x <- frame_current$min_x + as.numeric(xml2::xml_attr(fr, "width"))
  frame_current$min_y <- as.numeric(xml2::xml_attr(fr, "y"))
  frame_current$max_y <- frame_current$min_y + as.numeric(xml2::xml_attr(fr, "height"))
  frame_current$diff_x <- frame_current$max_x - frame_current$min_x
  frame_current$diff_y <- frame_current$max_y - frame_current$min_y
  frame_current$scale_diff <- frame_current$scale_max - frame_current$scale_min
  frame_current$scaling_x <- abs(frame_current$diff_x / frame_current$scale_diff)
  frame_current$scaling_y <- abs(frame_current$diff_y / frame_current$scale_diff)
  
  # return
  return(frame_current)
  
}


# elemente ausblenden
svg_hideElement <- function(svg_in, element_name, element_type) {
  
  if (element_type == "text") {
    
    elements <- xml2::xml_find_all(svg_in, "./text")
    
    for (element_nr in 1:length(element_name)) {
      
      element_search <- svg_TextToSvgFormat(element_name[element_nr])
      check1 <- length(which(xml2::xml_text(elements) == element_search)) == 0
      check2 <- length(which(xml2::xml_attr(elements, "id") == element_search)) == 0
      
      
      if (check1 & check2) {stop (paste0("Kein Textelement mit der Bezeichnung ", element_name[element_nr], " gefunden."))}
      
      if (length(which(xml2::xml_text(elements) == element_search)) == 0) {
        xml2::xml_set_attr(elements[which(xml2::xml_attr(elements, "id") == 
                                                  element_search)], "display", "none")
      } else {
        xml2::xml_set_attr(elements[which(xml2::xml_text(elements) == 
                                                  element_search)], "display", "none")
      }
      
    }
    
  } else {
    
    elements <- xml2::xml_find_all(svg_in, paste0("./", element_type))
    
    for (element_nr in 1:length(element_name)) {
      
      element_search <- svg_TextToSvgFormat(element_name[element_nr])
      xml2::xml_set_attr(elements[which(xml2::xml_attr(elements, "id") == 
                                                element_search)], "display", "none")
      
    }
    
  }
  
}

# wandelt strings in svg-style Strings um
svg_TextToSvgFormat <- function(input_text) {
  
  input_text <- gsub("_", "_x5F_", input_text)
  input_text <- gsub("\\(", "_x28_", input_text)
  input_text <- gsub("\\)", "_x29_", input_text)
  input_text <- gsub("\\,", "C", input_text)
  input_text <- gsub("\\;", "_x3B_", input_text)
  input_text <- gsub(" ", "__", input_text)
  input_text <- gsub("--", "_x2013_", input_text)
  input_text <- gsub("\\*", "_x2A_", input_text)
  return(input_text)
  
}

# Liest Farbcode von einem Element aus
svg_getElementColor <- function(svg_in, element_name, element_type) {
  
  elements <- xml2::xml_find_all(svg_in, paste0("./", element_type))
  color_out <- NULL
  
  for (element_nr in 1:length(element_name)) {
    
    
    color_out <- c(color_out, xml2::xml_attr(elements[which(xml2::xml_attr(elements, "id") == 
                                                                    element_name[element_nr])], "fill"))
    
  }
  
  return(color_out)
  
}

# Ersetzt Farbcode eines Elements
svg_setElementColor <- function(svg_in, element_name, element_type, color_new) {
  
  elements <- xml2::xml_find_all(svg_in, paste0("./", element_type))
  
  for (element_nr in 1:length(element_name)) {
    
    xml2::xml_set_attr(elements[which(xml2::xml_attr(elements, "id") == 
                                              element_name[element_nr])], "fill", color_new[element_nr])
    
  }
  
}

### BALKENDIAGRAMME ----

# stackedBar_order_groups: Hilfsfunktion
# returns the order of the subgroups depending on the x and y values so that the input values can be mapped correctly to the
# right stacked bar groups. (order can be mixed up in the svg textfile such that it doesn't corresponds to the displayed svg!!)
stackedBar_order_groups <- function(stackedBarGroup, n_subgroups) {
  
  rects_value_y <- rects_value_x <- NULL
  
  if (!n_subgroups == 1) {
    
    for (n_children in 1:length(xml2::xml_find_all(stackedBarGroup,"./g"))) {
      
      # get y and x value of rects (min)
      barSet <- xml2::xml_find_all(stackedBarGroup,"./g")[n_children]
      rects <- xml2::xml_find_all(barSet, "./rect")
      rects_value_y <- c(rects_value_y, min(as.numeric(xml2::xml_attr(rects, "y"))))
      rects_value_x <- c(rects_value_x, min(as.numeric(xml2::xml_attr(rects, "x"))))
      
    }
    
    stackedBars_order_y <- order(rects_value_y)
    stackedBars_order_x <- order(rects_value_x)
    
  } else {
    
    stackedBars_order_y <- 1
    stackedBars_order_x <- 1
    
  }
  
  return(data.frame(stackedBars_order_x, stackedBars_order_y))
  
}  

# stackedBar_in: Hilfsfunktion
# liest angefuehrte Gruppe ein, Check ob eindeutig vorhanden ist. gibt stackedBar Group aus
stackedBar_in <- function(svg_in, group_name) {
  
  # get called group
  named_groups <- xml2::xml_find_all(svg_in, "/svg/g")
  index_group <- which(xml2::xml_attr(named_groups, "id") == group_name)
  if (length(index_group) != 1) {stop("Fehler: Gruppenname nicht gefunden bzw. nicht eindeutig.")}  ## Diese Schritte ev. vorziehen
  stackedBarGroup <- named_groups[index_group]
  return(stackedBarGroup)
  
}

# stackedBar_checkSub: Hilfsfunktion
# liest n subgruppen aus, checkt ob n subgruppen == n Werte (rows), gibt n subgruppen aus
stackedBar_checkSub <- function(stackedBarGroup, values) {
  
  n_subgroups <- length(xml2::xml_find_all(stackedBarGroup, "g"))
  n_subgroups <- ifelse (n_subgroups == 0, 1, n_subgroups)
  if (n_subgroups != nrow(values)) {
    stop ("Fehler: Anzahl Subgruppen der gewaehlten Gruppe entspricht nicht der Anzahl der Zeilen der Werte.")
    #stop ("Error: Number of (sub)groups not identical to number of rows of values.")
  }
  return(n_subgroups)
  
}

# Hilfsfunktion: Rechtecke eines stackedBar bearbeiten, startpos und Breite anpassen
stackedBar_edit_rects <- function(rects, frame_info, value_set, order_rects, alignment) {
  
  # check: n values == n bars
  if (length(value_set) != length(rects)) {
    stop ("Fehler: Anzahl Werte entspricht nicht Anzahl Rechtecken.")
  }
  
  switch(alignment,
         
         horizontal = {
           
           # first rect
           rect_start <- which(order_rects == 1)
           rect_remain <- order_rects[-rect_start]
           xml2::xml_set_attr(rects[rect_start], "x", frame_info$min_x)
           xml2::xml_set_attr(rects[rect_start], "width", value_set[1] * frame_info$scaling_x)
           pos_next <- as.numeric(xml2::xml_attr(rects[rect_start], "x")) + (value_set[1] * frame_info$scaling_x)
           
           # remaining rects
           for (rect_nr in 1:length (order_rects)) {
             
             if (order_rects[rect_nr] == 1) {
               next
             }
             
             index_rect <- which(order_rects == order_rects[rect_nr])
             xml2::xml_set_attr(rects[index_rect], "x", pos_next)
             xml2::xml_set_attr(rects[index_rect], "width", value_set[order_rects[rect_nr]] * frame_info$scaling_x)
             pos_next <- pos_next + value_set[order_rects[rect_nr]] * frame_info$scaling_x
             
           }
           
         },
         
         vertical = {
           
           # first rect
           rect_start <- which(order_rects == 1)
           rect_remain <- order_rects[-rect_start]
           xml2::xml_set_attr(rects[rect_start], "y", frame_info$max_y - value_set[1] * frame_info$scaling_y)
           xml2::xml_set_attr(rects[rect_start], "height", value_set[1] * frame_info$scaling_y)
           pos_next <- as.numeric(xml2::xml_attr(rects[rect_start], "y"))
           
           # remaining rects
           for (rect_nr in 1:length (order_rects)) {
             
             if (order_rects[rect_nr] == 1) {
               next
             }
             
             index_rect <- which(order_rects == order_rects[rect_nr])
             xml2::xml_set_attr(rects[index_rect], "y", pos_next - value_set[rect_nr] * frame_info$scaling_y)
             xml2::xml_set_attr(rects[index_rect], "height", value_set[order_rects[rect_nr]] * frame_info$scaling_y)
             pos_next <- as.numeric(xml2::xml_attr(rects[rect_nr], "y"))
             
           }
           
         }) ## TODO: default
  
  #return(barSet)
  
}

# Hilfsfunktion: Textelemente eines stackedBar: richtige Reihenfolge herausfinden
stackedBar_order_text <- function(barLabels, value_set) {
  
  labels_value_y <- labels_value_x <- NULL
  
  # check: n values == n texts
  if (length(value_set) != length(barLabels)) {
    stop ("Fehler: Anzahl Werte entspricht nicht Anzahl Labels.")
  }
  
  for (lb in 1:length(barLabels)) {
    
    # get y value of rects (min)
    text_label <- barLabels[lb]
    text_matrix <- xml2::xml_attr(text_label, "transform")
    matrix_values_start <- stringr::str_locate(text_matrix, "matrix\\(")
    matrix_values <- stringr::str_sub(text_matrix, (matrix_values_start[2] + 1), (nchar(text_matrix) - 1))
    matrix_values <- as.numeric(unlist(strsplit(matrix_values, split = " ")))
    labels_value_y <- c(labels_value_y, matrix_values[length(matrix_values)])
    labels_value_x <- c(labels_value_x, matrix_values[length(matrix_values) - 1])
    
  }
  
  order_labels_x <- order(labels_value_x)
  order_labels_y <- order(-labels_value_y)
  
  return(data.frame(order_labels_x, order_labels_y))
  
}

# Hilfsfunktion: Text eines stackedBars bearbeiten: Text tauschen, Position anpassen
stackedBar_edit_text <- function(barLabels, order_labels, value_set, rects, order_rects, decimals, displayLimit, labelPosition, alignment) {
  
  switch (alignment,
          
          horizontal = {
            
            for (n_text in 1:length(barLabels)) {
              
              # change value
              text_toChange <- barLabels[order_labels[n_text]]
              #xml2::xml_text(text_toChange) <- as.character(value_set[order_labels[n_text]])
              xml2::xml_text(text_toChange) <- format(round(value_set[order_labels[n_text]],decimals),nsmall=decimals,decimal.mark=",",big.mark="",small.mark="")
              
              # comply with displayLimit
              if ((value_set[order_labels[n_text]] < displayLimit) | (value_set[order_labels[n_text]] == 0)) {
                xml2::xml_set_attr(text_toChange, "display", "none")
              }
              
              # change position
              rectinfo_pos_x <- as.numeric(xml2::xml_attr(rects[which(order_rects == order_labels[n_text])], "x"))
              rectinfo_pos_width <- as.numeric(xml2::xml_attr(rects[which(order_rects == order_labels[n_text])], "width"))
              
              # label position
              text_pos_center <- rectinfo_pos_x + (rectinfo_pos_width/2)
              text_pos_in <- ifelse (value_set[order_labels[n_text]] >= 0, rectinfo_pos_x + 10, 
                                           rectinfo_pos_x + rectinfo_pos_width - 10)
              text_pos_out <- ifelse (value_set[order_labels[n_text]] >= 0, rectinfo_pos_x + rectinfo_pos_width - 10,
                                            rectinfo_pos_x + 10)
              
              if (labelPosition == "center") {text_pos <- text_pos_center}
              if (labelPosition == "start") {text_pos <- text_pos_in}
              if (labelPosition == "end") {text_pos <- text_pos_out}

              
              text_matrix <- xml2::xml_attr(text_toChange, "transform")
              matrix_values_start <- stringr::str_locate(text_matrix, "matrix\\(")
              matrix_values <- stringr::str_sub(text_matrix, (matrix_values_start[2] + 1), (nchar(text_matrix) - 1))
              matrix_values <- as.numeric(unlist(strsplit(matrix_values, split = " ")))
              
              text_pos_matrix <- paste0("matrix(", matrix_values[1], " ", matrix_values[2], " ", matrix_values[3], " ", 
                                              matrix_values[4], " ", text_pos, " ", matrix_values[6], ")")
              
              xml2::xml_set_attr(text_toChange, "transform", text_pos_matrix)
              xml2::xml_set_attr(text_toChange, "text-anchor", "middle")
              
            }
            
          },
          
          vertical = {
            
            for (n_text in 1:length(barLabels)) {
              
              # change value
              text_toChange <- barLabels[order_labels[n_text]]
              #xml2::xml_text(text_toChange) <- as.character(value_set[order_labels[n_text]])
              xml2::xml_text(text_toChange) <- format(round(value_set[order_labels[n_text]],decimals),nsmall=decimals,decimal.mark=",",big.mark="",small.mark="")
              
              # comply with displayLimit
              if ((value_set[order_labels[n_text]] < displayLimit) | (value_set[order_labels[n_text]] == 0)) {
                xml2::xml_set_attr(text_toChange, "display", "none")
              }
              
              # change position
              rectinfo_pos_y <- as.numeric(xml2::xml_attr(rects[which(order_rects == order_labels[n_text])], "y"))
              rectinfo_pos_height <- as.numeric(xml2::xml_attr(rects[which(order_rects == order_labels[n_text])], "height"))
              
              # label position
              text_pos_center <- rectinfo_pos_y + (rectinfo_pos_height/2)
              text_pos_in <- ifelse (value_set[order_labels[n_text]] >= 0, rectinfo_pos_y + rectinfo_pos_height - 10, 
                                           rectinfo_pos_y + 10)
              text_pos_out <- ifelse (value_set[order_labels[n_text]] >= 0, rectinfo_pos_y + 10,
                                            rectinfo_pos_y + rectinfo_pos_height - 10)
              
              
              if (labelPosition == "center") {text_pos <- text_pos_center}
              if (labelPosition == "start") {text_pos <- text_pos_in}
              if (labelPosition == "end") {text_pos <- text_pos_out}
              
              text_matrix <- xml2::xml_attr(text_toChange, "transform")
              matrix_values_start <- stringr::str_locate(text_matrix, "matrix\\(")
              matrix_values <- stringr::str_sub(text_matrix, (matrix_values_start[2] + 1), (nchar(text_matrix) - 1))
              matrix_values <- as.numeric(unlist(strsplit(matrix_values, split = " ")))
              
              text_pos_matrix <- paste0("matrix(", matrix_values[1], " ", matrix_values[2], " ", matrix_values[3], " ", 
                                              matrix_values[4], " ", matrix_values[5], " ", text_pos, ")")
              
              xml2::xml_set_attr(text_toChange, "transform", text_pos_matrix)
              xml2::xml_set_attr(text_toChange, "text-anchor", "start")
              xml2::xml_set_attr(text_toChange, "dominant-baseline", "mathematical")
              
            }
            
          })
  
}

# Passt Balkenelemente und Textelement hinsichtlich Ausrichtung an
stackedBar_adjust <- function(svg_in, group_name, alignment, ref_rect = NULL, ref_text = NULL) {
  
  # check
  if (!(alignment %in% c("horizontal", "vertical"))) {stop("alignment paramenter muss 'horizontal' oder 'vertical' sein.")}
  
  # check if subgroup has to be adjusted
  subgroup <- ifelse (grepl("/", group_name) == FALSE, FALSE, TRUE)
  # check if all subgroups should be adjusted
  if (subgroup) {
    subgroup_all <- ifelse (substr(group_name, nchar(group_name), nchar(group_name)) == "/", TRUE, FALSE)
  }
  
  
  subgroup_levels <- length(unlist(strsplit(group_name, split = "/")))  ## TODO
  
  group_name_main <- unlist(strsplit(group_name, split = "/"))[1]
  group_name_sub <- unlist(strsplit(group_name, split = "/"))[2]
  
  symbolGroup <- stackedBar_in(svg_in, group_name_main)
  symbolGroup_subs <- xml2::xml_find_all(symbolGroup, "g")
  
  if (subgroup_all) {
    symbolGroup_edit <- symbolGroup
  } else if (subgroup) {
    symbolGroup_edit <- symbolGroup_subs[which(xml2::xml_attr(symbolGroup_subs, "id") == group_name_sub)]
  } else {
    symbolGroup_edit <- symbolGroup
  }
  
  
  n_subgroups <- length(xml2::xml_find_all(symbolGroup_edit, "g"))
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
      order_rects_x <- order(as.numeric(xml2::xml_attr(rects, "x")))
      order_rects_y <- order(-as.numeric(xml2::xml_attr(rects, "y")))
      if (alignment == "horizontal") {order_rects <- order_rects_x}
      if (alignment == "vertical") {order_rects <- order_rects_y}
      x_adjust <- xml2::xml_attr(rects[which(order_rects == ref_rect)], "x")
      y_adjust <- xml2::xml_attr(rects[which(order_rects == ref_rect)], "y")
      value_adjust <- ifelse (alignment == "horizontal", y_adjust, x_adjust)
      variable_adjust <- ifelse (alignment == "horizontal", "y", "x")
      
      for (rect_nr in 1:length(rects)) {
        
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
      
      order_textOut <- stackedBar_order_text(texts, 1:length(texts))
      order_labels_x <- order_textOut$order_labels_x
      order_labels_y <- order_textOut$order_labels_y
      if (alignment == "horizontal") {
        order_labels <- order_labels_x
      } else {
        order_labels <- order_labels_y
      }
      
      text_matrix <- xml2::xml_attr(texts[which(order_labels == ref_text)], "transform")
      matrix_values_start <- stringr::str_locate(text_matrix, "matrix\\(")
      matrix_values <- stringr::str_sub(text_matrix, (matrix_values_start[2] + 1), (nchar(text_matrix) - 1))
      matrix_values <- as.numeric(unlist(strsplit(matrix_values, split = " ")))
      
      value_adjust <- ifelse(alignment == "horizontal", matrix_values[6], matrix_values[5])
      variable_adjust <- ifelse(alignment == "horizontal", 6, 5)
      
      for (n_text in 1:length(texts)) {
        
        text_matrix <- xml2::xml_attr(texts[which(order_labels == n_text)], "transform")
        matrix_values_start <- stringr::str_locate(text_matrix, "matrix\\(")
        matrix_values <- stringr::str_sub(text_matrix, (matrix_values_start[2] + 1), (nchar(text_matrix) - 1))
        matrix_values <- as.numeric(unlist(strsplit(matrix_values, split = " ")))
        matrix_values[variable_adjust] <- value_adjust
        text_pos_matrix <- paste0("matrix(", matrix_values[1], " ", matrix_values[2], " ", matrix_values[3], " ", 
                                        matrix_values[4], " ", matrix_values[5], " ", matrix_values[6], ")")
        xml2::xml_set_attr(texts[which(order_labels == n_text)], "transform", text_pos_matrix)
        
      }
      
    }
    
  }
  
}

# Passt Farben von Balkenelementen und Textelementen eines stackedBars an
stackedBar_setColor <- function(svg_in, group_name, alignment, color_rects = NULL, color_texts = NULL) {
  
  # check if subgroup has to be adjusted
  subgroup <- ifelse (grepl("/", group_name) == FALSE, FALSE, TRUE)
  # check if all subgroups should be adjusted
  if (subgroup) {
    subgroup_all <- ifelse (substr(group_name, nchar(group_name), nchar(group_name)) == "/", TRUE, FALSE)
  }
  
  subgroup_levels <- length(unlist(strsplit(group_name, split = "/")))  ## TODO
  
  group_name_main <- unlist(strsplit(group_name, split = "/"))[1]
  group_name_sub <- unlist(strsplit(group_name, split = "/"))[2]
  
  symbolGroup <- stackedBar_in(svg_in, group_name_main)
  symbolGroup_subs <- xml2::xml_find_all(symbolGroup, "g")
  
  if (subgroup_all) {
    symbolGroup_edit <- symbolGroup
  } else if (subgroup) {
    symbolGroup_edit <- symbolGroup_subs[which(xml2::xml_attr(symbolGroup_subs, "id") == group_name_sub)]
  } else {
    symbolGroup_edit <- symbolGroup
  }
  
  
  n_subgroups <- length(xml2::xml_find_all(symbolGroup_edit, "g"))
  if (n_subgroups == 0) {
    n_subgroups <- 1
  }
  
  
  # rects
  if (!is.null(color_rects)) {
    
    for (n_groups in 1:n_subgroups) {
      
      if (n_subgroups == 1) {
        group_toEdit <- symbolGroup_edit
      } else {
        group_toEdit <- xml2::xml_find_all(symbolGroup_edit, "./g")[n_groups]
      }
      rects <- xml2::xml_find_all(group_toEdit, "./rect")
      
      # check
      if (length(rects) != length(color_rects)) {
        stop ("Anzahl Rechtecke entspricht nicht Anzahl Farben.")
      }
      
      order_rects_x <- order(as.numeric(xml2::xml_attr(rects, "x")))
      order_rects_y <- order(-as.numeric(xml2::xml_attr(rects, "y")))
      if (alignment == "horizontal") {order_rects <- order_rects_x}
      if (alignment == "vertical") {order_rects <- order_rects_y}
      
      for (n_rects in 1:length(rects)) {
        
        xml2::xml_set_attr(rects[order_rects[n_rects]], "fill", color_rects[n_rects])
        ## TODO: if is.null
        
      }
      
    }
    
  }
  
  
  # texts
  if (!is.null(color_texts)) {
    
    for (n_groups in 1:n_subgroups) {
      
      if (n_subgroups == 1) {
        group_toEdit <- symbolGroup_edit
      } else {
        group_toEdit <- xml2::xml_find_all(symbolGroup_edit, "./g")[n_groups]
      }
      
      texts <- xml2::xml_find_all(group_toEdit, "./text")
      
      # check
      if (length(texts) != length(color_texts)) {
        stop ("Anzahl Lables entspricht nicht Anzahl Farben.")
      }
      
      order_labels <- stackedBar_order_text(texts, 1:length(texts))
      if (alignment == "horizontal") {order_labels <- order_labels$order_labels_x}
      if (alignment == "vertical") {order_labels <- order_labels$order_labels_y}
      
      for (n_texts in 1:length(texts)) {
        
        xml2::xml_set_attr(texts[order_labels[n_texts]], "fill", color_texts[n_texts])
        ## TODO: if is.null
        
      }
      
    }
    
  }
  
}

#' Passt Balkendiagramm an
#' 
#' @description Passt ein in der SVG-Datei vorgefertigtes (gestapeltes) Balkendiagramm horizontal oder vertikal an. Vorbereitung: Balkensegmente und Wertelabels sind im SVG zu gruppieren. Mehrere solche Gruppen (Balken) können wiederum gruppiert werden. Die äußerste Gruppe ist zu benennen.
#' @param svg SVG als XML document
#' @param frame_name Name des Rechtseck-Elements mit dem Rahmen des Diagrammbereichs
#' @param group_name Name der Gruppe mit den Balkenelementen bzw. mehreren Balken
#' @param scale_real Unter- und Obergrenze des dargestellten Wertebereichs (bspw. c(0,100))
#' @param values Dataframe mit den Werten, eine Zeile pro Balken
#' @param alignment Ausrichtung der Balken. Entweder "horizontal" (default) oder "vertical"
#' @param has_labels Sollen Wertelabels angezeigt werden? (Default TRUE)
#' @param label_position Position der Wertelabels (Default "center")
#' @param decimals Anzahl der Dezimalstellen der Wertelabels (Default 0)
#' @param display_limit Unter welchem Wert sollen Wertelabels unterdrückt werden? (Default 0) 
#' @return adaptiertes SVG als XML document
#' @export
stackedBar <- function(svg, frame_name, group_name, scale_real, values, alignment = "horizontal", has_labels = TRUE, label_position = "center", decimals = 0, display_limit = 0) {
  
  # check alignment string
  if (!(alignment %in% c("horizontal","vertikal"))) stop("FEHLER: alignment muss 'horizontal' oder 'vertikal' sein!")
  
  # get frame info and scaling
  frame_info <- frame_and_scaling(svg, frame_name, scale_real)
  
  # if input-values == vector: transform to data.frame to get 1 row
  if (is.null(nrow(values))) {values <- t(data.frame(values))}
  
  # get called group
  stackedBarGroup <- stackedBar_in(svg, group_name)
  
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
    if (length(xml2::xml_find_all(stackedBarGroup, "./g"))!=0) barSet <- xml2::xml_find_all(stackedBarGroup, "./g")[stackedBars_order_y[bar_nr]]
    if (length(xml2::xml_find_all(stackedBarGroup, "./g"))==0) barSet <- stackedBarGroup #wenn es keine Untergruppen gibt, nimm direkt die übergebene Gruppe
    
    rects <- xml2::xml_find_all(barSet, "./rect")
    order_rects_x <- order(as.numeric(xml2::xml_attr(rects, "x")))
    order_rects_y <- order(-as.numeric(xml2::xml_attr(rects, "y")))
    if (alignment == "horizontal") {order_rects <- order_rects_x}
    if (alignment == "vertical") {order_rects <- order_rects_y}
    
    # edit rects
    stackedBar_edit_rects(rects, frame_info, value_set, order_rects, alignment)

    
    ## -- TEXT/LABELS
    # get text elements of group and right ordering
    if (has_labels) {
      
      barLabels <- xml2::xml_find_all(barSet, "./text")
      order_textOut <- stackedBar_order_text(barLabels, value_set)
      order_labels_x <- order_textOut$order_labels_x
      order_labels_y <- order_textOut$order_labels_y
      if (alignment == "horizontal") {order_labels <- order_labels_x}
      if (alignment == "vertical") {order_labels <- order_labels_y}
      
      # adjust values and position of text elements
      stackedBar_edit_text(barLabels, order_labels, value_set, rects, order_rects, decimals, display_limit, label_position, alignment)
    
    }

  }
  
  # return
  return(svg)
  
}

### DIFFERENZBALKENDIAGRAMME ----

## -- Differenzbalken
# Differenzbalken edit rects
diffBar_edit_rects <- function (svg, rects, values, frame_info, frame0_name, order_rects, alignment) {
  
  frame0_x <- xml2::xml_find_all(svg, "./line")
  frame0_x <- as.numeric(xml2::xml_attr(frame0_x[which(xml2::xml_attr(frame0_x, "id")
                                                                   == frame0_name)], "x1"))
  frame0_y <- xml2::xml_find_all(svg, "./line")
  frame0_y <- as.numeric(xml2::xml_attr(frame0_y[which(xml2::xml_attr(frame0_y, "id")
                                                                   == frame0_name)], "y1"))
  
  
  switch (alignment,
                
                "horizontal" = {
                  
                  for (rect_nr in 1:length(rects)) {
                    
                    if (values[order_rects[rect_nr]] > 0) {
                      
                      xml2::xml_set_attr(rects[order_rects[rect_nr]], "x", frame0_x)
                      xml2::xml_set_attr(rects[order_rects[rect_nr]], 
                                         "width", values[order_rects[rect_nr]] * frame_info$scaling_x)
                      
                    }
                    
                    if (values[order_rects[rect_nr]] < 0) {
                      
                      xml2::xml_set_attr(rects[order_rects[rect_nr]], "x", frame0_x + values[order_rects[rect_nr]] *
                                           frame_info$scaling_x)
                      xml2::xml_set_attr(rects[order_rects[rect_nr]], "width", 
                                         abs(values[order_rects[rect_nr]]) * frame_info$scaling_x)
                      
                    }
                    
                    if (values[order_rects[rect_nr]] == 0) {
                      
                      xml2::xml_set_attr(rects[order_rects[rect_nr]], "display", "none")
                      
                    }
                    
                  }
                  
                  
                },
                
                "vertical" = {
                  
                  for (rect_nr in 1:length(rects)) {
                    
                    if (values[order_rects[rect_nr]] > 0) {
                      
                      xml2::xml_set_attr(rects[order_rects[rect_nr]], "y", frame0_y)
                      xml2::xml_set_attr(rects[order_rects[rect_nr]], 
                                         "height", values[order_rects[rect_nr]] * frame_info$scaling_y)
                      
                    }
                    
                    if (values[order_rects[rect_nr]] < 0) {
                      
                      xml2::xml_set_attr(rects[order_rects[rect_nr]], "y", frame0_y + values[order_rects[rect_nr]] * 
                                           frame_info$scaling_y)
                      xml2::xml_set_attr(rects[order_rects[rect_nr]], "height", 
                                         abs(values[order_rects[rect_nr]]) * frame_info$scaling_y)
                      
                    }
                    
                    if (values[order_rects[rect_nr]] == 0) {
                      
                      xml2::xml_set_attr(rects[order_rects[rect_nr]], "display", "none")
                      
                    }
                    
                  }
                  
                })
  
}

# Differenzbalken edit text
diffBar_edit_texts <- function (barLabels, order_labels, values, rects, order_rects, displayLimits, decimals, labelPosition, alignment, label_edge, ignore_rotation=TRUE) {
  
  if (length(displayLimits)>1) displayLimitRange <- c(min(displayLimits),max(displayLimits))
  if (length(displayLimits)==1) displayLimitRange <- c(-abs(displayLimits),abs(displayLimits))
  
  for (n_text in 1:length(barLabels)) {
    
    # change value
    text_toChange <- barLabels[n_text]
    numvalue <- values[order_labels[n_text]]
    value <- format(round(numvalue,decimals),nsmall=decimals,decimal.mark=",",big.mark="",small.mark="")
    xml2::xml_text(text_toChange) <- value
    
    # comply with displayLimit
    if ((values[order_labels[n_text]] > displayLimitRange[1] & values[order_labels[n_text]] < displayLimitRange[2]) | (values[order_labels[n_text]] == 0)) {
      xml2::xml_set_attr(text_toChange, "display", "none")
    }
    
    # change position
    rectinfo_pos_x <- as.numeric(xml2::xml_attr(rects[which(order_rects == order_labels[n_text])], "x"))
    rectinfo_pos_width <- as.numeric(xml2::xml_attr(rects[which(order_rects == order_labels[n_text])], "width"))
    rectinfo_pos_y <- as.numeric(xml2::xml_attr(rects[which(order_rects == order_labels[n_text])], "y"))
    rectinfo_pos_height <- as.numeric(xml2::xml_attr(rects[which(order_rects == order_labels[n_text])], "height"))
    
    if (alignment == "horizontal") {
      
      text_pos_center <- rectinfo_pos_x + (rectinfo_pos_width/2)
      text_pos_in <- ifelse (values[order_labels[n_text]] >= 0, rectinfo_pos_x + label_edge, 
                                   rectinfo_pos_x + rectinfo_pos_width - label_edge)
      text_pos_out <- ifelse (values[order_labels[n_text]] >= 0, rectinfo_pos_x + rectinfo_pos_width - label_edge,
                                    rectinfo_pos_x + label_edge)
      
    } else {
      
      text_pos_center <- rectinfo_pos_y + (rectinfo_pos_height/2)
      text_pos_in <- ifelse (values[order_labels[n_text]] >= 0, rectinfo_pos_y + rectinfo_pos_height - label_edge, 
                                   rectinfo_pos_y + label_edge)
      text_pos_out <- ifelse (values[order_labels[n_text]] >= 0, rectinfo_pos_y + label_edge,
                                    rectinfo_pos_y + rectinfo_pos_height - label_edge)
      
    }
    
    if (labelPosition == "center") {text_pos <- text_pos_center}
    if (labelPosition == "start") {text_pos <- text_pos_in}
    if (labelPosition == "end") {text_pos <- text_pos_out}
    
    if (!is.na(xml2::xml_attr(text_toChange,"transform")))
    {
      text_matrix <- xml2::xml_attr(text_toChange, "transform")
      matrix_values_start <- stringr::str_locate(text_matrix, "matrix\\(")
      matrix_values <- stringr::str_sub(text_matrix, (matrix_values_start[2] + 1), (nchar(text_matrix) - 1))
      matrix_values <- as.numeric(unlist(strsplit(matrix_values, split = " ")))
      pos_x <- matrix_values[5]
      pos_y <- matrix_values[6]
      hasTransformAttr <- TRUE
    }
    if (is.na(xml2::xml_attr(text_toChange,"transform")))
    {
      pos_x <- xml2::xml_attr(text_toChange,"x")
      pos_y <- xml2::xml_attr(text_toChange,"y")
      hasTransformAttr <- FALSE
    }
    
    if (!ignore_rotation && hasTransformAttr)
    {
      ifelse (alignment == "horizontal", matrix_values[5] <- text_pos, matrix_values[6] <- text_pos)
      
      text_pos_matrix <- paste0("matrix(", matrix_values[1], " ", matrix_values[2], " ", matrix_values[3], " ", 
                                      matrix_values[4], " ", matrix_values[5], " ", matrix_values[6], ")")
      
      xml2::xml_set_attr(text_toChange, "transform", text_pos_matrix)
      xml2::xml_set_attr(text_toChange, "text-anchor", "middle")
    }
    
    if (ignore_rotation || !hasTransformAttr)
    {
      if (alignment=="horizontal") pos_x <- text_pos
      if (alignment=="vertikal") pos_y <- text_pos
      xml2::xml_set_attr(text_toChange, "x", pos_x)
      xml2::xml_set_attr(text_toChange, "y", pos_y)
      xml2::xml_set_attr(text_toChange, "transform", NULL)
      if (labelPosition == "start") xml2::xml_set_attr(text_toChange, "text-anchor", ifelse(numvalue>0,"start","end"))
      if (labelPosition == "center") xml2::xml_set_attr(text_toChange, "text-anchor", "middle")
      if (labelPosition == "end") xml2::xml_set_attr(text_toChange, "text-anchor", ifelse(numvalue>0,"end","start"))
    }
    
  }
  
}


#' Passt Differenzbalkendiagramm an
#' 
#' @description Passt ein in der SVG-Datei vorgefertigtes Differenzbalkendiagramm horizontal oder vertikal an. Vorbereitung: Balkensegmente und Wertelabels sind im SVG zu gruppieren. Mehrere solche Gruppen (Balken) können wiederum gruppiert werden. Die äußerste Gruppe ist zu benennen.
#' @param svg SVG als XML document
#' @param frame_name Name des Rechtseck-Elements mit dem Rahmen des Diagrammbereichs
#' @param frame0_name Name der 0er-Linie
#' @param group_name Name der Gruppe mit den Balkenelementen bzw. mehreren Balken
#' @param scale_real Unter- und Obergrenze des dargestellten Wertebereichs (bspw. c(0,100))
#' @param values Dataframe mit den Werten, eine Zeile pro Balken
#' @param alignment Ausrichtung der Balken. Entweder "horizontal" (default) oder "vertical"
#' @param has_labels Sollen Wertelabels angezeigt werden? (Default TRUE)
#' @param label_position Position der Wertelabels (Default "center")
#' @param decimals Anzahl der Dezimalstellen der Wertelabels (Default 0)
#' @param display_limits Zwischen welchen Werten (Minimum und Maximum) rund um Null sollen Wertelabels unterdrückt werden? (Default c(0,0) = keine unterdrückten Werte)
#' @param label_edge Abstand der Labels zum Balkenmaximum bei label_position start/end. (Default 10)
#' @param ignore_rotation Soll ggf. Rotation der vorbereiteten Textelemente ignoriert werden? (Default TRUE)
#' @return adaptiertes SVG als XML document
#' @export
diffBar <- function(svg, frame_name, frame0_name, group_name, scale_real, values, alignment = "horizontal", has_labels = TRUE, label_position = "center", decimals = 0, display_limits = c(0,0), label_edge = 10, ignore_rotation = TRUE) {
  
  # check alignment string
  if (!(alignment %in% c("horizontal","vertikal"))) stop("FEHLER: alignment muss 'horizontal' oder 'vertikal' sein!")
  
  # get frame info and scaling
  frame_info <- frame_and_scaling(svg, frame_name, scale_real)
  
  # if input-values == vector: transform to data.frame to get n rows
  if (is.null(nrow(values))) {values <- data.frame(values)}
  
  # get called group
  symbolGroup <- stackedBar_in(svg, group_name)
  
  # get n subgroups
  n_subgroups <- stackedBar_checkSub(symbolGroup, values)
  
  # get order of (sub)groups in xml depending on x-value and depending on y-value
  order_groups <- stackedBar_order_groups(symbolGroup, n_subgroups)
  diffBars_order_x <- order_groups$stackedBars_order_x
  diffBars_order_y <- order_groups$stackedBars_order_y
  
  
  # adjust all rect-elements and text-elements of all groups
  for (bar_nr in 1:n_subgroups) {
    
    # values for barSet
    value_set <- values[bar_nr, ]
    
    ## - RECTS
    # get: barSet, rects of barSet, right ordering
    if (!n_subgroups == 1) {
      barSet <- xml2::xml_find_all(symbolGroup, "./g")[diffBars_order_y[bar_nr]]
    } else {
      barSet <- symbolGroup
    }
    rects <- xml2::xml_find_all(barSet, "./rect")
    order_rects_x <- order(as.numeric(xml2::xml_attr(rects, "x")))
    order_rects_y <- order(-as.numeric(xml2::xml_attr(rects, "y")))
    if (alignment == "horizontal") {order_rects <- order_rects_x}
    if (alignment == "vertical") {order_rects <- order_rects_y}
    
    # edit rects
    diffBar_edit_rects(svg, rects, value_set, frame_info, frame0_name, order_rects, alignment)
    
    
    ## -- TEXT/LABELS
    # get text elements of group and right ordering
    if (has_labels) {
      
      barLabels <- xml2::xml_find_all(barSet, "./text")
      order_textOut <- stackedBar_order_text(barLabels, value_set)
      order_labels_x <- order_textOut$order_labels_x
      order_labels_y <- order_textOut$order_labels_y
      if (alignment == "horizontal") {order_labels <- order_labels_x}
      if (alignment == "vertical") {order_labels <- order_labels_y}
      
      # adjust values and position of text elements
      diffBar_edit_texts(barLabels, order_labels, value_set, rects, order_rects, display_limits, decimals, label_position, alignment, label_edge, ignore_rotation)
      
    }
    
  }
  
  return(svg)
  
}

### LINIEN- UND SYMBOLDIAGRAMME ----

linesSymbols_in <- function (svg_in, group_name) {
  
  named_groups <- xml2::xml_find_all(svg_in, "/svg/g")
  index_group <- which(xml2::xml_attr(named_groups, "id") == group_name)
  if (length(index_group) != 1) {stop("Fehler: Gruppenname nicht gefunden bzw. nicht eindeutig.")}
  lineGroup <- named_groups[index_group]
  
}

linesSymbols_duplicate_lines <- function (lineGroup, alignment, number) {
  
  newline <- xml2::xml_find_first(lineGroup[[1]], "./line")
  if (alignment=="vertical") width <- abs(as.numeric(xml2::xml_attr(newline, "x2"))-as.numeric(xml2::xml_attr(newline, "x1")))
  if (alignment=="horizontal") width <- abs(as.numeric(xml2::xml_attr(newline, "y2"))-as.numeric(xml2::xml_attr(newline, "y1")))
  for (ll in 2:number)
  {
    newline <- xml2::xml_add_sibling(.x = newline,.value=newline)
    if (alignment=="vertical")
    {
      xml2::xml_set_attr(newline,"x1",as.numeric(xml2::xml_attr(newline, "x1"))+width)
      xml2::xml_set_attr(newline,"x2",as.numeric(xml2::xml_attr(newline, "x2"))+width)
    }
    if (alignment=="horizontal")
    {
      xml2::xml_set_attr(newline,"y1",as.numeric(xml2::xml_attr(newline, "y1"))+width)
      xml2::xml_set_attr(newline,"y2",as.numeric(xml2::xml_attr(newline, "y2"))+width)
    }
  }
  lines_inGroup <- xml2::xml_find_all(lineGroup, "./line")
  return(lines_inGroup)
  
}

linesSymbols_order_lines <- function (lines_inGroup, alignment) {
  
  lines_x1_values <- lines_y1_values <- lines_x2_values <- lines_y2_values <- numeric()
  
  #Korrektur falsch gedrehter Linien (x2<x1 oder y2<y1)
  for (n_lines in 1:length(lines_inGroup)) {
    x1 <- as.numeric(xml2::xml_attr(lines_inGroup[n_lines], "x1"))
    y1 <- as.numeric(xml2::xml_attr(lines_inGroup[n_lines], "y1"))
    x2 <- as.numeric(xml2::xml_attr(lines_inGroup[n_lines], "x2"))
    y2 <- as.numeric(xml2::xml_attr(lines_inGroup[n_lines], "y2"))
    if (alignment=="vertical" && x2<x1)
    {
      xml2::xml_set_attr(x = lines_inGroup[n_lines],attr = "x1",value=x2)
      xml2::xml_set_attr(x = lines_inGroup[n_lines],attr = "x2",value=x1)
    }
    if (alignment=="horizontal" && y2<y1)
    {
      xml2::xml_set_attr(x = lines_inGroup[n_lines],attr = "y1",value=y2)
      xml2::xml_set_attr(x = lines_inGroup[n_lines],attr = "y2",value=y1)
    }
  }
  
  for (n_lines in 1:length(lines_inGroup)) {
    lines_x1_values <- c(lines_x1_values, as.numeric(xml2::xml_attr(lines_inGroup[n_lines], "x1")))
    lines_y1_values <- c(lines_y1_values, as.numeric(xml2::xml_attr(lines_inGroup[n_lines], "y1")))
    lines_x2_values <- c(lines_x2_values, as.numeric(xml2::xml_attr(lines_inGroup[n_lines], "x2")))
    lines_y2_values <- c(lines_y2_values, as.numeric(xml2::xml_attr(lines_inGroup[n_lines], "y2")))
  }
  
  order_lines_x <- order(lines_x1_values)
  order_lines_y <- order(-lines_y1_values)
  
  if (alignment == "vertical") {order_lines <- order_lines_x}
  if (alignment == "horizontal") {order_lines <- order_lines_y}
  
  return(order_lines)
  
}

linesSymbols_edit_lines <- function (lines_inGroup, order_lines, frame_info, value_set, alignment) {
  
  if (alignment=="vertical")
  {
    for (n_lines in 1:length(lines_inGroup)) {
      
      line_toChange <- lines_inGroup[order_lines[n_lines]]
      
      xml2::xml_set_attr(line_toChange, "y1", frame_info$max_y - (value_set[order_lines[n_lines]] - frame_info$scale_min) * frame_info$scaling_y)
      xml2::xml_set_attr(line_toChange, "y2", frame_info$max_y - (value_set[order_lines[n_lines] + 1] - frame_info$scale_min) * frame_info$scaling_y)
      
    }
  }
  if (alignment=="horizontal")
  {
    for (n_lines in 1:length(lines_inGroup)) {
      
      line_toChange <- lines_inGroup[order_lines[n_lines]]
      
      xml2::xml_set_attr(line_toChange, "x1", frame_info$min_x + (value_set[order_lines[n_lines]] - frame_info$scale_min) * frame_info$scaling_x)
      xml2::xml_set_attr(line_toChange, "x2", frame_info$min_x + (value_set[order_lines[n_lines] + 1] - frame_info$scale_min) * frame_info$scaling_x)
      
    }
  }
  
}

# pull information about polygons in group (x, y, order, ...)
linesSymbols_info_polygons <- function (polygons_inGroup) {
  
  dat_polygons <- data.frame(Index = numeric(),num_points = numeric(),min_x = numeric(),max_x = numeric(),min_y = numeric(),max_y = numeric())
  
  for (pp in 1:length(polygons_inGroup))
  {
    dat_polygons <- plyr::rbind.fill(dat_polygons,data.frame(Index = pp))
    min_x <- min_y <- Inf
    max_x <- max_y <- -Inf
    points_str <- str_squish(xml2::xml_attr(polygons_inGroup[pp], "points"))
    points <- strsplit(points_str,split=" ")[[1]]
    dat_polygons[pp,]$num_points <- length(points)
    for (qq in 1:length(points))
    {
      if (!(paste0("p",qq,"_x") %in% colnames(dat_polygons)))
      {
        dat_polygons[,paste0("p",qq,"_x")] <- as.numeric(NA)
        dat_polygons[,paste0("p",qq,"_y")] <- as.numeric(NA)
      }
      x <- as.numeric(strsplit(points[qq],",")[[1]][1])
      y <- as.numeric(strsplit(points[qq],",")[[1]][2])
      dat_polygons[pp,paste0("p",qq,"_x")] <- x
      dat_polygons[pp,paste0("p",qq,"_y")] <- y
      if (min_x > x) min_x <- x
      if (min_y > y) min_y <- y
      if (max_x < x) max_x <- x
      if (max_y < y) max_y <- y
    }
    dat_polygons[pp,]$min_x <- min_x
    dat_polygons[pp,]$max_x <- max_x
    dat_polygons[pp,]$min_y <- min_y
    dat_polygons[pp,]$max_y <- max_y
  }
  
  # center, gravity, width and height of polygons
  dat_polygons$center_x <- (dat_polygons$min_x + dat_polygons$max_x)/2
  dat_polygons$center_y <- (dat_polygons$min_y + dat_polygons$max_y)/2
  dat_polygons$gravity_x <- rowMeans(dat_polygons[,grep("^p[0-9]+\\_x$",names(dat_polygons))],na.rm=TRUE)
  dat_polygons$gravity_y <- rowMeans(dat_polygons[,grep("^p[0-9]+\\_y$",names(dat_polygons))],na.rm=TRUE)
  dat_polygons$width <- (dat_polygons$max_x - dat_polygons$min_x)
  dat_polygons$height <- (dat_polygons$max_y - dat_polygons$min_y)
  
  # order of polygons
  dat_polygons$order_x <- order(dat_polygons$min_x)
  dat_polygons$order_y <- order(dat_polygons$min_y)
  
  # return
  return(dat_polygons)

}

linesSymbols_edit_polygons <- function (svg_in, group, frame_info, value_set, alignment, scatter = FALSE) {
  
  # available polygons in group
  polygons_inGroup <- xml2::xml_find_all(group, "./polygon")
  if (length(polygons_inGroup)!=length(value_set)) stop(paste0("Falsche Anzahl Polygonelemente in der Gruppe (",length(polygons_inGroup)," vorhanden, ",length(value_set)," erwartet)."))
  
  # information about polygons
  dat_polygons <- linesSymbols_info_polygons(polygons_inGroup)
  
  switch(alignment,
         
         vertical = {
           
           for (pp in 1:length(polygons_inGroup)) {
             
             dat_polygon <- dat_polygons[pp,,drop=FALSE]
             location_y <- (frame_info$max_y - (value_set[dat_polygon$order_x] - frame_info$scale_min) * frame_info$scaling_y)
             diff_y <- (dat_polygon$gravity_y - location_y)
             points_new <- character()
             for (qq in 1:dat_polygon$num_points)
             {
               old_x <- dat_polygon[,paste0("p",qq,"_x")]
               new_y <- (dat_polygon[,paste0("p",qq,"_y")] - diff_y)
               points_new <- c(points_new,paste0(old_x,",",new_y))
             }
             points_new <- paste(points_new, collapse = " ")
             xml2::xml_set_attr(polygons_inGroup[pp], "points", points_new)

           }
           
         },
         
         horizontal = {
           
           for (pp in 1:length(polygons_inGroup)) {
             
             dat_polygon <- dat_polygons[pp,,drop=FALSE]
             location_x <- (frame_info$min_x + (value_set[dat_polygon$order_y] - frame_info$scale_min) * frame_info$scaling_x)
             diff_x <- (dat_polygon$gravity_x - location_x)
             points_new <- character()
             for (qq in 1:dat_polygon$num_points)
             {
               old_y <- dat_polygon[,paste0("p",qq,"_y")]
               new_x <- (dat_polygon[,paste0("p",qq,"_x")] - diff_x)
               points_new <- c(points_new,paste0(new_x,",",old_y))
             }
             points_new <- paste(points_new, collapse = " ")
             xml2::xml_set_attr(polygons_inGroup[pp], "points", points_new)
             
           }
           
         })
  
}

linesSymbols_info_linegroups <- function (linegroups_inGroup) {
  
  dat_linegroups <- data.frame(Index = numeric(),num_lines = numeric(),min_x = numeric(),max_x = numeric(),min_y = numeric(),max_y = numeric())
  
  for (pp in 1:length(linegroups_inGroup))
  {
    dat_linegroups <- plyr::rbind.fill(dat_linegroups,data.frame(Index = pp))
    min_x <- min_y <- Inf
    max_x <- max_y <- -Inf
    lines <- xml2::xml_find_all(linegroups_inGroup[pp], "./line")
    if (length(lines)==0) stop("Liniengruppe ohne Linienelemente gefunden.")
    dat_linegroups[pp,]$num_lines <- length(lines)
    for (qq in 1:length(lines))
    {
      if (!(paste0("l",qq,"_x1") %in% colnames(dat_linegroups)))
      {
        dat_linegroups[,paste0("l",qq,"_x1")] <- as.numeric(NA)
        dat_linegroups[,paste0("l",qq,"_x2")] <- as.numeric(NA)
        dat_linegroups[,paste0("l",qq,"_y1")] <- as.numeric(NA)
        dat_linegroups[,paste0("l",qq,"_y2")] <- as.numeric(NA)
      }
      x1 <- as.numeric(xml2::xml_attr(lines[qq],"x1"))
      x2 <- as.numeric(xml2::xml_attr(lines[qq],"x2"))
      y1 <- as.numeric(xml2::xml_attr(lines[qq],"y1"))
      y2 <- as.numeric(xml2::xml_attr(lines[qq],"y2"))
      dat_linegroups[pp,paste0("l",qq,"_x1")] <- x1
      dat_linegroups[pp,paste0("l",qq,"_x2")] <- x2
      dat_linegroups[pp,paste0("l",qq,"_y1")] <- y1
      dat_linegroups[pp,paste0("l",qq,"_y2")] <- y2
      if (min_x > x1) min_x <- x1
      if (min_x > x2) min_x <- x2
      if (min_y > y1) min_y <- y1
      if (min_y > y2) min_y <- y2
      if (max_x < x1) max_x <- x1
      if (max_x < x2) max_x <- x2
      if (max_y < y1) max_y <- y1
      if (max_y < y2) max_y <- y2
    }
    dat_linegroups[pp,]$min_x <- min_x
    dat_linegroups[pp,]$max_x <- max_x
    dat_linegroups[pp,]$min_y <- min_y
    dat_linegroups[pp,]$max_y <- max_y
  }
  
  # center, gravity, width and height of polygons
  dat_linegroups$center_x <- (dat_linegroups$min_x + dat_linegroups$max_x)/2
  dat_linegroups$center_y <- (dat_linegroups$min_y + dat_linegroups$max_y)/2
  dat_linegroups$gravity_x <- rowMeans(dat_linegroups[,grep("^l[0-9]+\\_x(1|2)$",names(dat_linegroups))],na.rm=TRUE)
  dat_linegroups$gravity_y <- rowMeans(dat_linegroups[,grep("^l[0-9]+\\_y(1|2)$",names(dat_linegroups))],na.rm=TRUE)
  dat_linegroups$width <- (dat_linegroups$max_x - dat_linegroups$min_x)
  dat_linegroups$height <- (dat_linegroups$max_y - dat_linegroups$min_y)
  
  # order of polygons
  dat_linegroups$order_x <- order(dat_linegroups$min_x)
  dat_linegroups$order_y <- order(dat_linegroups$min_y)
  
  # return
  return(dat_linegroups)
  
}

linesSymbols_edit_linegroups <- function (svg_in, group, frame_info, value_set, alignment, scatter = FALSE) {
  
  #available groups
  linegroups_inGroup <- xml2::xml_find_all(group, "./g")
  if (length(linegroups_inGroup)!=length(value_set)) stop(paste0("Falsche Anzahl Gruppen mit Linienelementen in der Gruppe (",length(linegroups_inGroup)," vorhanden, ",length(value_set)," erwartet)."))
  
  # symbols info
  dat_linegroups <- linesSymbols_info_linegroups(linegroups_inGroup)
  
  # edit symbols
  switch(alignment,
               
               vertical = {
                 
                 for (pp in 1:length(linegroups_inGroup)) {
                   dat_linegroup <- dat_linegroups[pp,,drop=FALSE]
                   location_y <- (frame_info$max_y - (value_set[dat_linegroup$order_x] - frame_info$scale_min) * frame_info$scaling_y)
                   diff_y <- (dat_linegroup$gravity_y - location_y)
                   lines <- xml2::xml_find_all(linegroups_inGroup[pp], "./line")
                   for (qq in 1:dat_linegroup$num_lines)
                   {
                     new_y1 <- (dat_linegroup[,paste0("l",qq,"_y1")] - diff_y)
                     new_y2 <- (dat_linegroup[,paste0("l",qq,"_y2")] - diff_y)
                     xml2::xml_set_attr(lines[qq], "y1", new_y1)
                     xml2::xml_set_attr(lines[qq], "y2", new_y2)
                   }
                 }
                 
               },
               
               horizontal = {
                 
                 for (pp in 1:length(linegroups_inGroup)) {
                   dat_linegroup <- dat_linegroups[pp,,drop=FALSE]
                   location_x <- (frame_info$min_x + (value_set[dat_linegroup$order_y] - frame_info$scale_min) * frame_info$scaling_x)
                   diff_x <- (dat_linegroup$gravity_x - location_x)
                   lines <- xml2::xml_find_all(linegroups_inGroup[pp], "./line")
                   for (qq in 1:dat_linegroup$num_lines)
                   {
                     new_x1 <- (dat_linegroup[,paste0("l",qq,"_x1")] - diff_x)
                     new_x2 <- (dat_linegroup[,paste0("l",qq,"_x2")] - diff_x)
                     xml2::xml_set_attr(lines[qq], "x1", new_x1)
                     xml2::xml_set_attr(lines[qq], "x2", new_x2)
                   }
                 }
                 
               })
  
}

linesSymbols_edit_circles <- function (svg, group, frame_info, value_set, alignment, scatter = FALSE) {
  
  # available circles in group
  symbols_inGroup <- xml2::xml_find_all(group, "./circle")
  if (length(symbols_inGroup)!=length(value_set)) stop(paste0("Falsche Anzahl Kreiselemente in der Gruppe (",length(symbols_inGroup)," vorhanden, ",length(value_set)," erwartet)."))
  
  # order of circles
  symbols_order_x <- order(as.numeric(xml2::xml_attr(symbols_inGroup, "cx")))
  symbols_order_y <- order(as.numeric(xml2::xml_attr(symbols_inGroup, "cy")))
  
  switch (alignment,
                
                vertical = {
                  
                  for (n_symbols in 1:length(symbols_inGroup)) {
                    symbol_toEdit <- symbols_inGroup[symbols_order_x[n_symbols]]
                    cy_new <- frame_info$max_y - (value_set[n_symbols] - frame_info$scale_min) * frame_info$scaling_y 
                    xml2::xml_set_attr(symbol_toEdit, "cy", cy_new)
                  }
                  
                },
                
                horizontal = {
                  
                  for (n_symbols in 1:length(symbols_inGroup)) {
                    symbol_toEdit <- symbols_inGroup[symbols_order_y[n_symbols]]
                    cx_new <- frame_info$min_x + (value_set[n_symbols] - frame_info$scale_min) * frame_info$scaling_x 
                    xml2::xml_set_attr(symbol_toEdit, "cx", cx_new)
                  }
                  
                })
  
  return(svg)
}

linesSymbols_edit_rects <- function (svg, group, frame_info, value_set, alignment, scatter = FALSE) {
  
  # available rects in group
  symbols_inGroup <- xml2::xml_find_all(group, "./rect")
  if (length(symbols_inGroup)!=length(value_set)) stop(paste0("Falsche Anzahl Rechteckelemente in der Gruppe (",length(symbols_inGroup)," vorhanden, ",length(value_set)," erwartet)."))
  
  # order of rects
  symbols_order_x <- order(as.numeric(xml2::xml_attr(symbols_inGroup, "x")))
  symbols_order_y <- order(as.numeric(xml2::xml_attr(symbols_inGroup, "y")))

  switch (alignment,
                
                vertical = {
                  
                  for (n_symbols in 1:length(symbols_inGroup)) {
                    symbol_toEdit <- symbols_inGroup[symbols_order_x[n_symbols]]
                    height_half <- as.numeric(xml2::xml_attr(symbol_toEdit, "height")) / 2
                    y_new <- frame_info$max_y - ((value_set[n_symbols] - frame_info$scale_min) * frame_info$scaling_y + height_half)
                    xml2::xml_set_attr(symbol_toEdit, "y", y_new)
                  }
                  
                },
                
                horizontal = {
                  
                  for (n_symbols in 1:length(symbols_inGroup)) {
                    symbol_toEdit <- symbols_inGroup[symbols_order_y[n_symbols]]
                    width_half <- as.numeric(xml2::xml_attr(symbol_toEdit, "width")) / 2
                    x_new <- frame_info$min_x + (value_set[n_symbols] - frame_info$scale_min) * frame_info$scaling_x - width_half
                    xml2::xml_set_attr(symbol_toEdit, "x", x_new)
                  }
                  
                })
  
  return(svg)
}


#' Passt Linien- und Symboldiagramme an
#' 
#' @description Passt die Symbole (und Lininen) in der SVG-Datei vorgefertigtes Symboldiagramm horizontal oder vertikal an. Vorbereitung: Linien und Symbolelemente (siehe Details) sind im SVG zu gruppieren. Die Gruppe ist zu benennen.
#' @param svg SVG als XML document
#' @param frame_name Name des Grafikrahmens.
#' @param group_name Name der Gruppe mit den zu bearbeitenden Symbolen (und optional Verbindungslinien).
#' @param scale_real Unter- und Obergrenze des dargestellten Wertebereichs (bspw. c(0,100))
#' @param values Dataframe bzw. Vektor mit den Werten
#' @param alignment Ausrichtungsytyp der Symbole/Linien. Entweder "horizontal" (x-Werte anpassen) oder "vertical" (y-Werte anpassen) (Default: vertical).
#' @param has_lines Sind Linien vorhanden? (Default = TRUE)
#' @param symbol_type Typ der Symbolgruppe, die angepasst werden soll: c("circle","rect","polygon","linegroup"). (Default: NULL = nur Linie).
#' @param ... Argumente fuer Subfunktionen
#' @return adaptiertes SVG als XML document
#' @details Die Linien- und Symbolelemente werden anhand ihrer x/y-Koordinaten von links nach rechts (aufsteigende x-Koordinate) bzw. oben nach unten (aufsteigende y-Koordinate) vorab sortiert und die Datenwerte entsprechend zugewiesen.\cr\cr
#' Symboltyp "circle": Kreiselemente vom Typ "circle" im SVG. Passt die Attribute cx und cy an.\cr
#' Symboltyp "rect": Rechteckelemente vom Typ "recht" im SVG. Passt die Attribute 
#' @export
linesSymbols <- function (svg, frame_name, group_name, scale_real, values, alignment = "vertical", has_lines = TRUE, symbol_type = NULL, ...) {
  
  # input check
  if (!is.null(symbol_type)) {
    if (!symbol_type %in% c("circle","rect","polygon","linegroup")) { stop ("Ungueltiger Symboltyp (symbol_type).") }
  }
  if (!alignment %in% c("horizontal","vertical")) { stop ("Ungueltige Ausrichtungsart (alignment): 'horizontal' und 'vertical' erlaubt.") }
  # if input-values == data.frame: transform first row to vector
  if (is.data.frame(values))
  {
    if (nrow(values)>1) warning("Dataframe mit mehr als einer Zeile uebergeben: Verwende nur erste Zeile.")
    values <- as.numeric(values[1,])
  }
  if (!is.numeric(values)) { stop ("Ungueltige Datenwerte (values): Nur numerische Werte erlaubt.") }
  
  # get frame info, scaling and group
  frame_info <- frame_and_scaling(svg, frame_name, scale_real)
  group <- linesSymbols_in(svg, group_name)
  
  # 1 - Lines
  if (has_lines)
  {
    lines_inGroup <- xml2::xml_find_all(group, "./line")
    if (length(lines_inGroup) == 0) { stop ("Keine Linienelemente in der Gruppe vorhanden.") }
    if (length(lines_inGroup) == 1 && length(values)!=2)
    {
      warning("Nur ein Linienelement in der Gruppe vorhanden. Dupliziere unter Annahme gleichbleibender Breite/Hoehe.")
      lines_inGroup <- linesSymbols_duplicate_lines(group, alignment, length(values)-1)
    }
    if (length(lines_inGroup) != length(values)-1) { stop(paste0("Falsche Anzahl Linienelemente in der Gruppe (",length(lines_inGroup)," vorhanden, ",(length(values)-1)," erwartet).")) }
    order_lines <- linesSymbols_order_lines(lines_inGroup, alignment)
    linesSymbols_edit_lines(lines_inGroup, order_lines, frame_info, values, alignment)
  }
  
  # 2 - Symbols
  if (!is.null(symbol_type))
  {
    args <- list(...)
    if (is.null(args[['scatter']])) scatter <- FALSE
    if (symbol_type == "circle")    linesSymbols_edit_circles   (svg, group, frame_info, values, alignment, scatter = scatter)
    if (symbol_type == "rect")      linesSymbols_edit_rects     (svg, group, frame_info, values, alignment, scatter = scatter)
    if (symbol_type == "polygon")   linesSymbols_edit_polygons  (svg, group, frame_info, values, alignment, scatter = scatter)
    if (symbol_type == "linegroup") linesSymbols_edit_linegroups(svg, group, frame_info, values, alignment, scatter = scatter)
  }
  
  return(svg)
}

### STREUDIAGRAMM ----

#' Passt Streudiagramme an
#' 
#' @description Vervielfaeltigt und passt ein in der SVG-Datei vorgefertigtes Symbol horizontal und vertikal an. Vorbereitung: Einzelnes Symbol ist in SVG zu gruppieren. Die Gruppe ist zu benennen.
#' @param svg SVG als XML document
#' @param frame_name Name des Grafikrahmens.
#' @param group_name Name der Gruppe mit der Symbolvorlage
#' @param scale_real_x Unter- und Obergrenze des dargestellten Wertebereichs (bspw. c(0,100)) auf der x-Achse
#' @param scale_real_y Unter- und Obergrenze des dargestellten Wertebereichs (bspw. c(0,100)) auf der y-Achse
#' @param values Dataframe bzw. Matrix mit den Werten, erste Spalte fuer X und zweite Spalte fuer Y
#' @param symbol_type Typ der Symbolgruppe, die angepasst werden soll: c("circle","rect","polygon","linegroup")
#' @return adaptiertes SVG als XML document
#' @export
scatterSymbols <- function(svg, frame_name, group_name, scale_real_x, scale_real_y, values, symbol_type)
{
  # input check
  if (!is.null(symbol_type)) {
    if (!symbol_type %in% c("circle","rect","polygon","linegroup")) { stop ("Ungueltiger Symboltyp (symbol_type).") }
  }
  if (length(dim(values!=2))) stop("Falsches Eingabeformat der Datenwerte (values). Erwarte 2-dimensionales Objekt (dataframe, matrix) mit x-Werten in erster und y-Werten in zweiter Spalte")
  if (!is.numeric(values[,1]) || !is.numeric(values[,2])) { stop ("Ungueltige Datenwerte (values): Nur numerische Werte erlaubt.") }
  # Anpassung mittels linesSymbols
  svg <- linesSymbols(svg = svg,frame_name = frame_name,group_name = group_name,scale_real = scale_real_x,values = values[,1],alignment = "horizontal",has_lines = FALSE,symbol_type = symbol_type,zero_line = NULL,scatter = TRUE)
  svg <- linesSymbols(svg = svg,frame_name = frame_name,group_name = group_name,scale_real = scale_real_y,values = values[,2],alignment = "vertical",has_lines = FALSE,symbol_type = symbol_type,zero_line = NULL,scatter = TRUE)
}

### TEXT ----

svg_setElementText <- function(svg, element_name, text_new, alignment = NULL, inGroup = NULL, hide_blank = FALSE) {
  
  if (!is.null(inGroup)) {
    availableGroups <- xml2::xml_find_all(svg, "g")
    searchGroup <- availableGroups[which(xml2::xml_attr(availableGroups, "id") == inGroup)]
    elements <- xml2::xml_find_all(searchGroup, "./text")
  } else {
    elements <- xml2::xml_find_all(svg, "./text")
  }
  
  for (element_nr in 1:length(element_name)) {
    
    check1 <- length(which(xml2::xml_text(elements) == element_name[element_nr])) == 0
    check2 <- length(which(xml2::xml_attr(elements, "id") == element_name[element_nr])) == 0
    
    
    if (check1 & check2) {stop (paste0("Kein Textelement mit der Bezeichnung ", element_name[element_nr], " gefunden."))}
    
    if (length(which(xml2::xml_text(elements) == element_name[element_nr])) == 0) {
      # edit text
      xml2::xml_set_text(elements[which(xml2::xml_attr(elements, "id") == 
                                                element_name[element_nr])], text_new[element_nr])
      # edit alignment
      if (!is.null(alignment)) {
        xml2::xml_set_attr(elements[which(xml2::xml_attr(elements, "id") == 
                                                  element_name[element_nr])], "text-anchor", alignment)
      }
      # hide if blank
      if (nchar(text_new)==0 && hide_blank) xml2::xml_set_attr(elements[which(xml2::xml_attr(elements, "id") == element_name[element_nr])], "display", "none")
      
    } else {
      # edit text
      xml2::xml_set_text(elements[which(xml2::xml_text(elements) == 
                                                element_name[element_nr])], text_new[element_nr])
      
      # edit alignment
      if (!is.null(alignment)) {
        xml2::xml_set_attr(elements[which(xml2::xml_text(elements) == 
                                                  element_name[element_nr])], "text-anchor", alignment)
      }
      
      # hide if blank
      if (nchar(text_new)==0 && hide_blank) xml2::xml_set_attr(elements[which(xml2::xml_text(elements) == element_name[element_nr])], "display", "none")
    }
    
  }
  
  return(svg)
}

#' Ersetzt Text in Textelement
#' 
#' @description Passt ein in der SVG-Datei vorhandenes Textelement an. Kann entweder via Textinhalt selbst oder via Benennung des Textelements angewendet werden.
#' @param svg SVG als XML document
#' @param element_name Name des Textelements bzw. Textinhalt des Textelements, wenn dieses nicht gesondert beschriftet ist.
#' @param text Text der eingetragen werden soll.
#' @param alignment Textausrichtung. Moegliche Parameter: start, middle, end. (Default NULL)
#' @param in_group In welcher Gruppe befindet sich das Textelement (Default NULL).
#' @param hide_blank Soll Textelement versteckt werden, wenn es leer ist? (Default FALSE)
#' @return adaptiertes SVG als XML document
#' @export
changeText <- function(svg, element_name, text, alignment = NULL, in_group = NULL, hide_blank = FALSE) {
  return(svg_setElementText(svg = svg,element_name = element_name,text_new = text,alignment = alignment,inGroup = in_group, hide_blank = hide_blank))
}
