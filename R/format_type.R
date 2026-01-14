# ---- Functions for formatting the type material ----
# Author: Lara Serpa Jaegge Deccache, Leandro Lacerda Giacomin and Claudio Nicoletti de Fraga
# Following the International Code of Nomenclature for Algae, Fungi, and Plants (Madrid Code; Turland et al. 2025).

#' Format Type Material
#'
#' @description
#' Format type specimens and illustrations.
#'
#'
#' @param df A data frame containing taxonomic columns (e.g., \code{acceptedScientificName}, \code{scientificName}, \code{basionym}, \code{namePublishedIn}, etc.).
#' @param journal_format Character string specifying the journal style: \code{"Taxon"}, \code{"Systematic Botany"}, or \code{"Willdenowia"}. Default is \code{"Taxon"}.
#'
#' @returns A formatted character string containing the type material block.
#'

format_type <- function(df = NULL,
                        journal_format = c("Taxon", "Systematic Botany", "Willdenowia")) {

  # ---- Initial settings and treatment ----
  journal_format <- match.arg(journal_format)
  rules <- get_style_rules(journal_format)

  df <- df[!is.na(df$category) &
             df$category != "" & df$category != "NA", ]
  if (nrow(df) == 0)
    return("")

  # Define hierarchy for type citations
  priority_map <- c(
    "holotype" = 1,
    "lectotype" = 2,
    "neotype" = 3,
    "epitype" = 4,
    "syntype" = 5,
    "isotype" = 6,
    "isolectotype" = 7,
    "isoneotype" = 8,
    "isosyntype" = 9
  )

  df$cat_lower <- trimws(tolower(as.character(df$category)))
  df$weight <- priority_map[df$cat_lower]
  df$weight[is.na(df$weight)] <- 99

  # Prioritize type category and then by designation status
  df <- df[order(df$weight, !grepl("here|by", tolower(as.character(
    df$typeDesignated
  )))), ]

  # ---- Standardize Title (e.g., "Lectotype (designated here)") ----
  main_row <- df[1, ]
  main_cat <- main_row$cat_lower

  # Capitalize first letter
  title_txt <- paste0(toupper(substr(main_cat, 1, 1)), substr(main_cat, 2, nchar(main_cat)))

  design <- tolower(as.character(main_row$typeDesignated))
  if (grepl("by", design)) {
    pub <- format_type_publication(main_row)
    if (pub != "")
      title_txt <- paste0(title_txt, " (designated by ", pub, ")")
  } else if (grepl("here", design)) {
    title_txt <- paste0("**", title_txt, " (designated here)**")
  }

  # ---- Standardize Geography and RecordedBy data ----
  loc_source <- df[order(nchar(as.character(df$locality)), decreasing = TRUE), ][1, ]

  geo <- format_locality(
    get_inferred(loc_source$country, loc_source$inferredCountry),
    get_inferred(
      loc_source$stateProvince,
      loc_source$inferredStateProvince
    ),
    get_inferred(loc_source$municipality, loc_source$inferredMunicipality),
    get_inferred(loc_source$locality, loc_source$inferredLocality)
  )

  # Date formatting
  day_v <- as.character(loc_source$day[1])
  month_v <- as.character(loc_source$month[1])
  year_v <- as.character(loc_source$year[1])

  day_v[is.na(day_v) | day_v == "NA" | day_v == ""] <- ""
  month_v[is.na(month_v) | month_v == "NA" | month_v == ""] <- ""
  year_v[is.na(year_v) | year_v == "NA" | year_v == ""] <- ""

  if (day_v == "" && month_v == "" && year_v == "") {
    dt <- "s.dat."
  } else {
    dt <- safe_paste(day_v, month_v, year_v)
  }

  # RecordedBy formatting
  loc_source$recordedBy <- format_initials(loc_source$recordedBy, journal_format = journal_format)
  cl <- safe_paste(loc_source$recordedBy, loc_source$recordNumber)
  if (cl != "")
    cl <- paste0("_", cl, "_")

  # Combine Geo + Date + Collector
  if (grepl(":$", geo)) {
    remainder <- paste(c(dt, cl)[c(dt, cl) != ""], collapse = ", ")
    full_geo <- paste0(geo, " ", remainder)
  } else {
    full_geo <- paste(c(geo, dt, cl)[c(geo, dt, cl) != ""], collapse = ", ")
  }

  full_geo <- trimws(gsub("\\s+", " ", full_geo))

  # ---- Standardize Illustrations or Specimens as Type material ----
  is_illustration <- grepl("illustration",
                           tolower(main_row$basisOfRecord),
                           ignore.case = TRUE) ||
    grepl("illustration", tolower(main_row$category), ignore.case = TRUE)

  if (is_illustration) {
    ## ---- Illustration header logic construction ----
    # Source (Description/Title of the plate)
    source_txt <- if (!is.na(main_row$illustrationSource) &&
                      main_row$illustrationSource != "") {
      main_row$illustrationSource
    } else {
      "[Illustration]"
    }

    # Digital Object (Check if column exists)
    dig_obj_txt <- ""
    if ("digitalObject" %in% names(main_row)) {
      do_val <- as.character(main_row$digitalObject)
      if (!is.na(do_val) && do_val != "" && do_val != "NA") {
        dig_obj_txt <- paste0(" [digital object ", do_val, "]")
      }
    }

    # "Later published in" (Common for names of Vellozo)
    pub_icon_txt <- ""
    if ("illustrationPublishedIn" %in% names(main_row)) {
      pub_val <- as.character(main_row$illustrationPublishedIn)
      if (!is.na(pub_val) && pub_val != "" && pub_val != "NA") {
        pub_icon_txt <- paste0("; later published in ", pub_val)
      }
    }

    # Assemble Illustration String
    final_content <- paste0(
      if (geo != "")
        paste0(geo, ": ")
      else
        "",
      "[illustration] ",
      source_txt,
      dig_obj_txt,
      pub_icon_txt
    )

  } else {

    # ---- Type specimen header logic construction ----

    unique_cats <- unique(df$cat_lower)
    unique_cats <- unique_cats[unique_cats != "paratype"]
    unique_cats <- unique_cats[order(priority_map[unique_cats])]

    herbs_parts <- sapply(unique_cats, function(cat_name) {
      sub_d <- df[df$cat_lower == cat_name, ]

      # Group barcodes by collectionCode
      h_grouped <- sapply(unique(sub_d$collectionCode), function(h_code) {
        records <- sub_d[sub_d$collectionCode == h_code, ]

        barcodes <- records$barcode[!is.na(records$barcode) &
                                      records$barcode != "" & records$barcode != "NA"]
        b_txt <- if (length(barcodes) > 0)
          paste0(" [", paste(barcodes, collapse = ", "), "]")
        else
          ""

        # Check for "seen" markers (! or photo!)
        view_txt <- ""
        if (any(records$viewPhoto == "!", na.rm = TRUE)) {
          view_txt <- " photo!"
        } else if (any(records$view == "!", na.rm = TRUE)) {
          view_txt <- "!"
        }

        # Handle cases where collectionCode might be missing
        if (is.na(h_code) ||
            h_code == "")
          h_code <- "[Unknown Herbarium]"

        paste0(h_code, b_txt, view_txt)
      })

      if (cat_name == main_cat) {
        return(paste(h_grouped, collapse = ", "))
      } else {
        lbl <- if (length(h_grouped) > 1 ||
                   nrow(sub_d) > 1)
          paste0(cat_name, "s")
        else
          cat_name
        return(paste0(lbl, ": ", paste(h_grouped, collapse = ", ")))
      }
    })

    final_content <- paste0(full_geo, " (", paste(herbs_parts, collapse = "; "), ")")
  }

  # Final Return
  return(paste0(title_txt, ": ", final_content))
}

#' Format Type Publication
#'
#' @description Format authorship and publications for typification.
#'
#' @param df A data frame containing taxonomic columns (e.g., \code{acceptedScientificName}, \code{scientificName}, \code{basionym}, \code{namePublishedIn}, etc.).
#'
#' @keywords internal
#' @noRd

format_type_publication <- function(df) {

  clean <- function(x)
    if (is.na(x) || x == "NA" || x == "")
      ""
  else
    as.character(x)

  auth <- clean(df$typeAuthorship[1])
  year <- clean(df$typePublishedInYear[1])
  page <- clean(df$typePage[1])

  ref <- trimws(paste(auth, year))
  if (ref != "" && page != "") {
    return(paste0(ref, ": ", page))
  }
  return(ref)
}

#' Format Type Locality
#'
#' @description Concatenates country, state, municipality, and specific locality details into a standardized botanical string.
#'
#' @param country Character string.
#' @param stateProvince Character string.
#' @param municipality Character string.
#' @param locality Character string (specific location details).
#'
#' @keywords internal
#' @noRd

format_locality <- function(country,
                            stateProvince,
                            municipality,
                            locality) {

  # ---- Initial settings and treatment ----
  country <- trimws(as.character(country))
  stateProvince <- trimws(as.character(stateProvince))
  municipality <- trimws(as.character(municipality))
  locality <- trimws(as.character(locality))

  admin_parts <- c()

  # ---- country ----
  if (!is.na(country) && country != "" && country != "NA") {
    admin_parts <- country
  }

  base_admin <- paste(admin_parts, collapse = "")

  # ---- stateProvince ----
  if (!is.na(stateProvince) &&
      stateProvince != "" && stateProvince != "NA") {
    base_admin <- if (base_admin != "")
      paste0(base_admin, ". ", stateProvince)
    else
      stateProvince
  }

  # ---- municipality ----
  if (!is.na(municipality) &&
      municipality != "" && municipality != "NA") {
    if (base_admin != "") {
      base_admin <- paste0(base_admin, ", ", municipality)
    } else {
      base_admin <- municipality
    }
  }

  # ---- locality ----
  if (!is.na(locality) && locality != "" && locality != "NA") {
    if (base_admin != "") {
      return(paste0(base_admin, ": ", locality))
    } else {
      return(locality)
    }
  } else {

    if (base_admin != "") {
      return(base_admin)
    }
    return("")
  }
}

#' Format Paratypes and Syntypes
#'
#' @description It groups duplicates, orders them by geography, and formats collection data and barcodes.
#'
#' @param par_data A data frame containing collection and locality columns.
#' @param label Character string for the block title (e.g., "Paratypes", "Syntypes").
#' @param journal_format Character string specifying the journal style: \code{"Taxon"}, \code{"Systematic Botany"}, or \code{"Willdenowia"}. Default is \code{"Taxon"}.
#'
#' @keywords internal

format_other_types <- function(par_data,
                               label = "Paratypes",
                               journal_format = c("Taxon", "Systematic Botany", "Willdenowia")) {
  journal_format <- match.arg(journal_format)

  # ---- Initial settings and treatment ----
  if (nrow(par_data) == 0) {
    return("")
  }

  par_data <- par_data[order(
    par_data$country,
    par_data$stateProvince,
    par_data$municipality,
    par_data$collectionCode
  ), ]

  # ---- Grouping specimens (Collector + Number + Year) ----
  par_data$coll_id <- paste(par_data$recordedBy, par_data$recordNumber, par_data$year)
  unique_colls <- unique(par_data$coll_id)

  p_items <- sapply(unique_colls, function(id) {
    d <- par_data[par_data$coll_id == id, ]

    c_cty <- get_inferred(d$country[1], d$inferredCountry[1])
    c_st <- get_inferred(d$stateProvince[1], d$inferredStateProvince[1])
    c_mun <- get_inferred(d$municipality[1], d$inferredMunicipality[1])
    c_loc <- get_inferred(d$locality[1], d$inferredLocality[1])

    # ---- Type specimen header logic construction ----
    geo_parts <- format_locality(c_cty, c_st, c_mun, c_loc)

    dt <- safe_paste(d$day[1], d$month[1], d$year[1])

    # Format collector initials according to journal style
    d_recorded <- format_initials(d$recordedBy[1], journal_format = journal_format)
    cl <- safe_paste(d_recorded, d$recordNumber[1])
    if (cl != "")
      cl <- paste0("_", cl, "_")

    # Format herbaria and barcodes: Code [Barcode]
    herbs <- mapply(function(h, b) {
      b_clean <- if (!is.na(b) &&
                     b != "" && b != "NA")
        paste0(" [", b, "]")
      else
        ""
      paste0(h, b_clean)
    }, d$collectionCode, d$barcode)

    geo_parts <- sub("\\s+$", "", geo_parts)

    geo_parts <- sub("([\\.,;])+$", "", geo_parts)


    if (grepl(":$", geo_parts)) {
      remainder <- paste(c(dt, cl)[c(dt, cl) != ""], collapse = ", ")
      main_line <- paste0(geo_parts, " ", remainder)
    } else {
      main_line <- paste(c(geo_parts, dt, cl)[c(geo_parts, dt, cl) != ""], collapse = ", ")
    }

    return(paste0(main_line, " (", paste(herbs, collapse = ", "), ")"))
  })

  return(paste0("\n\n", label, ": ", paste(p_items, collapse = "; "), "."))
}
