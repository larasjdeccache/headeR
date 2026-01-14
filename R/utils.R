# ---- Auxiliary functions for cleaning or formatting data ----
# Author: Lara Serpa Jaegge Deccache, Leando Lacerda Giacomin and Claudio Nicoletti de Fraga
# Following the International Code of Nomenclature for Algae, Fungi, and Plants (Madrid Code; Turland et al. 2025).

#' Standardize and clean strings
#'
#' @description
#' Helper to ensure a value is a character and not a literal "NA" or empty list.
#'
#' @param x A character string or vector.
#' @return A cleaned character string
#'
#' @keywords internal
#' @noRd

clean_str <- function(x) {
  if (is.null(x)) return("")
  x <- as.character(x)
  x[is.na(x) | x == "NA" | x == "NULL"] <- ""
  trimws(gsub("\\s+", " ", x))
}

#' Safe Concatenation of Strings
#'
#' @description
#' An auxiliary function that concatenates strings while automatically removing empty spaces (e.g., NA, NULL, "!", or "NA").
#'
#' @param ... Character strings or objects to be concatenated.
#' @param sep Character string to separate the terms. Default is a single space.
#'
#' @return A single concatenated string
#'
#' @keywords internal
#' @noRd

safe_paste <- function(..., sep = " ") {
  args <- list(...)
  args <- lapply(args, function(x) {
    x <- as.character(x)
    x[is.na(x) | x == "NA" | x == "NULL" | x == "!"] <- ""
    return(x)
  })
  res <- do.call(paste, c(args, list(sep = sep)))
  return(trimws(gsub("\\s+", " ", res)))
}

#' ---- Format inferred data ----
#' Prioritizes original data and wraps inferred data in brackets.
#'
#' @param original The primary data value.
#' @param inferred The inferred or corrected value.
#' @return A formatted string, with inferred data in brackets if original is missing.
#'
#' @keywords internal
#' @noRd

get_inferred <- function(original, inferred) {
  n <- as.character(original)
  i <- as.character(inferred)

  # Check if original exists
  if (!is.na(n) && n != "" && n != "NA" && n != "NULL") {
    return(trimws(n))
  }

  # If the original fails, try the inferred one
  if (length(i) > 0 && !is.na(i) && i != "" && i != "NA" && i != "NULL") {
    return(paste0("[", trimws(i), "]"))
  }

  return("")
}

#' Style rules for different journals
#'
#' @description
#' Centralizes the formatting parameters for synonyms and type prefixes.
#'
#' @param journal_format Character string specifying the journal style: \code{"Taxon"}, \code{"Systematic Botany"}, or \code{"Willdenowia"}. Default is \code{"Taxon"}.
#'
#' @return A list of formatting rules.
#'
#' @keywords internal
#' @noRd

get_style_rules <- function(journal_format = c("Taxon", "Systematic Botany", "Willdenowia")) {

  # ---- Initial settings and treatment ----
  # Ensure journal_format is a single string
  journal_format <- as.character(journal_format)[1]

  # ---- Taxon ----
  if (journal_format == "Taxon") {
    return(list(
      sep_syn = "", # separator between homo and hete in header_simple
      homo_prefix = " \u2261 ", # prefix for homotypic synonyms
      hete_first_prefix = "\n\n= ", # first heterotypic in block (newline)
      hete_sub_prefix = "\n\n= ", # subsequent heterotypic entries
      type_title_prefix = " \u2013 " # prefix before type citation in header lines
    ))
  }
  # ---- Wildenowia ----
  if (journal_format == "Willdenowia") {
    return(list(
      sep_syn = "",
      homo_prefix = " \u2261 ",
      hete_first_prefix = "\n\n= ",
      hete_sub_prefix = "\n\n= ",
      type_title_prefix = " \u2013 "
    ))
  } else {
    # Default
    list(
      sep_syn = "\n",
      homo_prefix = " \u2261 ",
      hete_first_prefix = "\n\n= ",
      hete_sub_prefix = "\n\n= ",
      type_title_prefix = " \u2013 "
    )
  }
}
