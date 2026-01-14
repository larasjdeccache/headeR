# ---- Functions for formatting publications, synonyms and homonyms ----
# Author: Lara Serpa Jaegge Deccache, Leandro Lacerda Giacomin and Claudio Nicoletti de Fraga
# Following the International Code of Nomenclature for Algae, Fungi, and Plants (Madrid Code; Turland et al. 2025).

#' Format Protologue Citations
#'
#' @description
#' Format the original publication (protologue) citation according to the standards of Taxon, Systematic Botany, or Willdenowia.
#'
#' @param data A data frame (or single row) containing columns: \code{namePublishedIn}, \code{volume}, \code{page}, \code{suppl}, \code{issue}, and \code{namePublishedInYear}.
#' @param journal_format Character string specifying the journal style: \code{"Taxon"}, \code{"Systematic Botany"}, or \code{"Willdenowia"}.
#'
#' @return A character string containing the standardized protologue citation.
#'
#' @importFrom utils write.csv
#'

format_publication <- function(data,
                               journal_format = c("Taxon", "Systematic Botany", "Willdenowia")) {
  # ---- Initial settings and treatment ----
  # Ensure the journal format matches the allowed options
  journal_format <- as.character(journal_format)[1]
  df <- as.data.frame(data)

  cols_to_fix <- c(
    "namePublishedIn",
    "series",
    "volume",
    "page",
    "suppl",
    "issue",
    "namePublishedInYear"
  )

  for (col in cols_to_fix) {
    if (col %in% names(df)) {
      df[[col]] <- as.character(df[[col]])
      df[[col]][is.na(df[[col]]) | df[[col]] == "NA"] <- ""
    } else {
      df[[col]] <- ""
    }
  }

  # ---- Standardize Year (Art. 31 of Madrid Code) ----
  df$year_display <- df$namePublishedInYear
  complex_idx <- grepl("publ\\.", df$namePublishedInYear)
  if (any(complex_idx)) {
    years_list <- regmatches(df$namePublishedInYear[complex_idx],
                             gregexpr("\\b\\d{4}\\b", df$namePublishedInYear[complex_idx]))
    df$year_display[complex_idx] <- sapply(years_list, function(y) {
      if (length(y) >= 2)
        paste0(y[1], " [", y[2], "]")
      else
        y[1]
    })
  }

  # ---- Standardize Publication's components ----
  series_part <- ifelse(df$series != "", paste0(", s\u00e9r. ", df$series, ", "), "")

  vol_content <- mapply(function(s, i) {
    if (s != "" && i != "") {
      return(paste0("(", s, ", ", i, ")"))
    }
    if (i != "") {
      return(paste0("(", i, ")"))
    }
    if (s != "") {
      return(paste0("(", s, ")"))
    }
    return("")
  }, df$suppl, df$issue)

  page_vol <- mapply(function(p, v, vc) {
    if (p == "") {
      return("")
    }
    if (v != "" || vc != "") {
      return(paste0(": ", p))
    }
    return(paste0(" ", p))
  }, df$page, df$volume, vol_content)

  # ---- Publication header logic construction ----
  is_full <- df$volume == "" & df$page == ""

  df$pub_citation <- ifelse(
    is_full,
    df$namePublishedIn,
    paste0(
      df$namePublishedIn,
      series_part,
      " ",
      df$volume,
      vol_content,
      page_vol
    )
  )

  prefix <- if (journal_format %in% c("Taxon", "Willdenowia"))
    " in "
  else
    ", "

  df$formatted_publication <- mapply(function(cit, yr) {
    if (cit == "") {
      return("")
    }

    res <- if (yr != "")
      paste0(prefix, trimws(cit), ". ", yr, ".")
    else
      paste0(prefix, trimws(cit), ".")

    res <- gsub("\\.\\.", ".", res)
    res <- gsub("\\s+", " ", res)
    return(trimws(res))
  }, df$pub_citation, df$year_display)

  return(df)
}

#' Format Synonyms
#'
#' @description
#' Organize and format the list of homotypic and heterotypic synonyms for an correct name.
#'
#' @param df A data frame containing taxonomic columns (e.g., \code{acceptedScientificName}, \code{scientificName}, \code{basionym}, \code{namePublishedIn}, etc.).
#' @param group_data Data frame with raw data for names linked to an accepted name.
#' @param accepted_name String containing the accepted scientific name.
#' @param journal_format Character string specifying the journal style: \code{"Taxon"}, \code{"Systematic Botany"}, or \code{"Willdenowia"}.
#' @param include_types Logical. If \code{"TRUE"}, looks for and formats type specimens associated with the synonym.
#'
#' @return A formatted character string containing the synonymy block.
#'

format_synonym <- function(df = NULL,
                           group_data,
                           accepted_name,
                           journal_format = c("Taxon", "Systematic Botany", "Willdenowia"),
                           include_types = TRUE) {
  # ---- Initial settings and treatment ----
  # Ensure the journal format matches the allowed options
  journal_format <- match.arg(journal_format)
  # Loads the header formatting style rules
  rules <- get_style_rules(journal_format)

  # Remove duplicates for the list of names
  syns <- group_data[group_data$scientificName_raw != accepted_name, ]
  syns <- syns[!duplicated(syns[, c("scientificName_raw", "namePublishedInYear", "status")]), ]
  syns <- syns[!is.na(syns$scientificName_raw) &
                 syns$scientificName_raw != "NA", ]

  if (nrow(syns) == 0) {
    return(list(homo = "", hete = ""))
  }

  # Identify basionym of the accepted name
  acc_row <- group_data[group_data$scientificName_raw == accepted_name, ][1, ]
  acc_bas <- if (!is.na(acc_row$basionym))
    as.character(acc_row$basionym)
  else
    ""

  norm <- function(x)
    tolower(trimws(gsub("\\s+", " ", as.character(x))))

  acc_bas_norm <- norm(acc_bas)

  homo_lines <- c()
  hete_lines <- c()

  for (i in seq_len(nrow(syns))) {
    row <- syns[i, ]
    row_bas_norm <- norm(row$basionym)
    row_name_norm <- norm(row$scientificName_raw)

    pub_str <- clean_str(row$formatted_publication)

    # ---- Standardize syn. nov., nom. inval., nom. illeg. ----
    status_tag <- ""
    st_val <- tolower(clean_str(row$status))
    if (grepl("syn\\. nov\\.", st_val))
      status_tag <- ", *syn. nov.*"
    else if (grepl("nom\\. inval\\.", st_val))
      status_tag <- ", *nom. inval.*"
    else if (grepl("nom\\. illeg\\.", st_val))
      status_tag <- ", *nom. illeg.*"

    # ---- Homonym treatment "non ..." ----
    # Searches for an older homonym in the entire dataframe
    non_citation <- ""
    if (!is.null(df)) {
      non_citation <- find_homonym(row, df)
    }

    # Build the base string of the name
    full_syn_str <- paste0(row$scientificName, " ", pub_str, status_tag, non_citation)

    # ---- Classification of Synonyms ----
    is_homotypic <- (row_name_norm == acc_bas_norm) ||
      (row_bas_norm != "" && row_bas_norm == acc_bas_norm)

    # ---- Heterotipic synonyms type material (logic for header_complete) ----
    if (isTRUE(include_types) && !is_homotypic) {
      type_rows <- group_data[group_data$scientificName_raw == row$scientificName_raw &
                                !is.na(group_data$category) &
                                grepl("type", group_data$category, ignore.case = TRUE) &
                                !grepl("paratype|syntype", group_data$category, ignore.case = TRUE), ]

      if (nrow(type_rows) > 0) {
        type_txt <- format_type(type_rows, journal_format = journal_format)
        if (type_txt != "") {
          full_syn_str <- paste0(full_syn_str, rules$type_title_prefix, type_txt)
        }
      }
    }

    # ---- Synonyms header logic construction ----

    if (is_homotypic) {
      homo_lines <- c(homo_lines, full_syn_str)
    } else {
      hete_lines <- c(hete_lines, full_syn_str)
    }
  }

  homo_block <- if (length(homo_lines) > 0) {
    paste0(rules$homo_prefix,
           paste(homo_lines, collapse = paste0(".", rules$homo_prefix)),
           ".")
  } else
    ""

  # Format assembly based on journal style
  hete_block <- ""
  if (length(hete_lines) > 0) {
    hete_lines <- sapply(hete_lines, function(x)
      if (!grepl("\\.$", x))
        paste0(x, ".")
      else
        x)
    hete_block <- paste0(rules$hete_first_prefix, hete_lines[1])
    if (length(hete_lines) > 1) {
      hete_block <- paste0(hete_block,
                           paste0(rules$hete_sub_prefix, hete_lines[-1], collapse = ""))
    }
  }

  return(list(homo = homo_block, hete = hete_block))
}

#' Find and Format Homonym Reference
#'
#' @description Checks the complete dataset if there is an older homonym. If found, it returns a formatted citation string following the standard.
#'
#' @param current_row A single-row data frame containing the taxon to be checked.
#' @param df A data frame containing taxonomic columns (e.g., \code{acceptedScientificName}, \code{scientificName}, \code{basionym}, \code{namePublishedIn}, etc.).
#'
#' @return A character string with the "non" citation or an empty string if no older homonym is found.
#'
#' @importFrom utils write.csv
#'
#' @keywords internal
#' @noRd
find_homonym <- function(current_row, df = NULL) {
  # ---- Initial settings and treatment ----
  curr_year <- as.numeric(gsub("[^0-9]", "", current_row$namePublishedInYear))
  if (is.na(curr_year)) {
    return("")
  }

  # Filter the entire dataframe for:
  # - Same genus and specific epithet
  # - Same infraspecific epithet (if any)
  # - Different author
  # - Previous year of publication

  homonyms <- df[df$genus == current_row$genus &
                   df$specificEpithet == current_row$specificEpithet &
                   df$infraspecificEpithet == current_row$infraspecificEpithet &
                   df$scientificNameAuthorship != current_row$scientificNameAuthorship &
                   !is.na(df$namePublishedInYear), ]

  if (nrow(homonyms) == 0) {
    return("")
  }

  # Convert years for numerical comparison
  homonyms$year_num <- as.numeric(gsub("[^0-9]", "", homonyms$namePublishedInYear))
  older <- homonyms[which(homonyms$year_num < curr_year), ]

  if (nrow(older) > 0) {
    # Search for the oldest homonym
    best_match <- older[order(older$year_num), ][1, ]

    # ---- Homonyms header logic construction ----
    name_cit <- gsub("\\*\\*", "", best_match$scientificName)
    pub_cit <- clean_str(best_match$formatted_publication)
    pub_cit <- sub("\\.+\\s*$", "", pub_cit)

    if (grepl("^\\s*in\\b", pub_cit, ignore.case = TRUE)) {
      return(paste0(" [non ", name_cit, " ", pub_cit, "]"))
    } else {
      return(paste0(" [non ", name_cit, " in ", pub_cit, "]"))
    }
  }

  return("")
}
