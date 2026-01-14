# ---- Functions for formatting the name of the taxa ----
# Author: Lara Serpa Jaegge Deccache, Leandro Lacerda Giacomin and Claudio Nicoletti de Fraga
# Following the International Code of Nomenclature for Algae, Fungi, and Plants (Madrid Code; Turland et al. 2025).

#' Formatting scientific names
#'
#' @description
#' Auxiliary function to formats genus, specific epithets, and infraspecific epithets names in italics, using Markdown syntax (*).
#'
#' @param df A data frame containing a \code{scientificName} column or the individual components (\code{genus}, \code{specificEpithet}, etc.).
#' @param journal_format Character string specifying the journal style: \code{"Taxon"}, \code{"Systematic Botany"}, or \code{"Willdenowia"}.
#'
#' @return A data frame with the \code{scientificName} column standardized with italic markers.
#'

format_name <- function(df,
                        journal_format = c("Taxon", "Systematic Botany", "Willdenowia")) {
  # ---- Initial settings and treatment ----
  # Ensure the journal format matches the allowed options
  journal_format <- match.arg(journal_format)

  # Create a backup of the original name before cleaning
  df <- as.data.frame(df)
  df$scientificName_raw <- as.character(df$scientificName)

  # Check if authorship is in a separate column and merge if missing from scientificName
  if ("scientificNameAuthorship" %in% names(df)) {
    for (i in 1:nrow(df)) {
      auth <- trimws(as.character(df$scientificNameAuthorship[i]))
      s_name <- trimws(as.character(df$scientificName[i]))
      if (!is.na(auth) &&
          auth != "" && !grepl(auth, s_name, fixed = TRUE)) {
        df$scientificName[i] <- paste(s_name, auth)
      }
    }
  }

  # Clean whitespace and parse the name components
  df$scientificName <- trimws(gsub("\\s+", " ", as.character(df$scientificName)))
  df <- parse_name(df)

  # Ensure infraspecific columns exist
  if (!"infraspecificBlock" %in% names(df))
    df$infraspecificBlock <- ""
  df$infraspecificBlock[is.na(df$infraspecificBlock)] <- ""

  # ---- Standardize the scientific names ----
  df$genus <- gsub("^(\\w)", "\\U\\1", tolower(trimws(df$genus)), perl = TRUE)
  df$specificEpithet <- tolower(trimws(df$specificEpithet))
  if ("infraspecificEpithet" %in% names(df)) {
    df$infraspecificEpithet <- tolower(trimws(df$infraspecificEpithet))
  }

  results <- character(nrow(df))
  hybrid_pattern <- "(^x\\s|^\u00d7\\s|\\bx\\b|\\b\u00d7\\b)"
  # ---- Standardize names ----
  for (i in 1:nrow(df)) {
    ## ---- Binomial and hybrid treatment ----
    is_hybrid <- grepl(hybrid_pattern, df$specificEpithet[i])

    if (is_hybrid) {
      # Remove the 'x' and extra spaces to format only the name in italics
      clean_epithet <- trimws(gsub(hybrid_pattern, "", df$specificEpithet[i]))
      species <- paste0("*", df$genus[i], "* \u00d7 *", clean_epithet, "*")
    } else {
      # Genus + specific epithet
      species <- paste0("*", df$genus[i], " ", df$specificEpithet[i], "*")
    }

    # ---- Standardize Authorship ----
    authorship <- if (!is.na(df$scientificNameAuthorship[i]) &&
                      df$scientificNameAuthorship[i] != "") {
      paste0(" ", df$scientificNameAuthorship[i])
    } else {
      ""
    }

    # ---- Standardize infraspecific names ----
    infra_raw <- df$infraspecificBlock[i]
    # Add italics to the epithet following rank (subsp., var., etc.)
    formatted_infra <- gsub("(subsp\\.|var\\.|f\\.|subf\\.|ser\\.)\\s+(\\w+)",
                            "\\1 *\\2*",
                            infra_raw)

    # Autonym detection and assembly
    is_autonym <- FALSE
    if ("infraspecificEpithet" %in% names(df)) {
      is_autonym <- tolower(df$specificEpithet[i]) == tolower(df$infraspecificEpithet[i])
    }

    if (is_autonym && formatted_infra != "") {
      # Autonym: *Genus species* Author rank *epithet*
      results[i] <- paste0(species, authorship, " ", formatted_infra)
    } else if (formatted_infra != "") {
      # Heterotypic: *Genus species* rank *epithet* Author
      results[i] <- paste0(species, " ", formatted_infra, authorship)
    } else {
      # Simple binomial: *Genus species* Author
      results[i] <- paste0(species, authorship)
    }
  }

  # ---- Final cleanup ----
  df$scientificName <- trimws(results)
  df$scientificName <- gsub("\\*\\*", "", df$scientificName)
  df <- df[!is.na(df$scientificName) &
             df$scientificName != "", , drop = FALSE]
  if (nrow(df) == 0) {
    warning("The provided data frame is empty or contains no valid scientific names.")
    return(df)
  }

  # ---- Initials formatting according to journal ----
  df$scientificName <- sapply(df$scientificName, function(x)
    format_initials(x, journal_format = journal_format), USE.NAMES = FALSE)

  df$scientificName_raw <- trimws(gsub("\\s+", " ", as.character(df$scientificName_raw)))

  return(df)
}

#' Internal parser for scientific names
#'
#' @description Deconstructs a full scientific name string into taxonomic components.
#'
#' @param df A data frame containing taxonomic columns (e.g., \code{acceptedScientificName}, \code{scientificName}, \code{basionym}, \code{namePublishedIn}, etc.).
#'
#' @keywords internal
#' @noRd

parse_name <- function(df) {
  if (!"scientificName" %in% names(df))
    stop("Column 'scientificName' not found.")

  # Split 'genus', 'specificEpithet', etc. into columns
  cleaned_names <- trimws(gsub("\\s+", " ", as.character(df$scientificName)))

  # Creating columns for treatment
  df$genus <- ""
  df$specificEpithet <- ""
  df$taxonRank <- ""
  df$infraspecificEpithet <- ""
  df$infraspecificBlock <- ""
  df$scientificNameAuthorship <- ""

  # Infraspecific rank delimitation
  taxonRank_list <- c("var.", "subsp.", "f.", "subf.", "ser.")
  hybrid_syms <- c("x", "\u00d7")

  for (i in 1:nrow(df)) {
    separate_name <- strsplit(cleaned_names[i], " ")[[1]]

    if (length(separate_name) < 2)
      next
    # Genus and specific epithet treatment
    df$genus[i] <- separate_name[1]
    # Hybrid treatment
    if (length(separate_name) > 2 &&
        separate_name[2] %in% hybrid_syms) {
      df$specificEpithet[i] <- paste0("\u00d7 ", separate_name[3])
      remainder <- 4
    } else {
      df$specificEpithet[i] <- separate_name[2]
      remainder <- 3
    }

    if (length(separate_name) >= remainder) {
      infra_name <- separate_name[remainder:length(separate_name)]

      # Determine split between infraspecific epithets and authorship
      split_point <- length(infra_name) + 1
      for (j in 1:length(infra_name)) {
        if (grepl("^[A-Z\\(]", infra_name[j]) &&
            !(infra_name[j] %in% taxonRank_list)) {
          split_point <- j
          break
        }
      }

      # Infraspecific names treatment
      if (split_point > 1) {
        infraspecificBlock <- infra_name[1:(split_point - 1)]

        df$infraspecificBlock[i] <- paste(infraspecificBlock, collapse = " ")

        first_epithet <- which(infraspecificBlock %in% taxonRank_list)[1]
        if (!is.na(first_epithet)) {
          df$taxonRank[i] <- infraspecificBlock[first_epithet]

          if ((first_epithet + 1) <= length(infraspecificBlock)) {
            df$infraspecificEpithet[i] <- infraspecificBlock[first_epithet + 1]
          }
        }
      }

      # Authorship treatment
      if (split_point <= length(infra_name)) {
        df$scientificNameAuthorship[i] <- paste(infra_name[split_point:length(infra_name)], collapse = " ")
      }
    }
  }
  return(df)
}

#' Format initials for authors/collectors according to journal style
#'
#' @description
#' Adds or removes spaces between initials according to the journal style.
#'
#' For example:
#' - Taxon: "J. A. Ratter" -> "J.A. Ratter"
#' - Willdenowia: "J.A. Ratter" -> "J. A. Ratter"
#'
#' @param name Character string with person names (may contain multiple people separated by commas)
#' @param journal_format Character string specifying the journal style: "Taxon" or "Willdenowia".
#'
#' @keywords internal
#' @noRd

format_initials <- function(name,
                            journal_format = c("Taxon", "Systematic Botany", "Willdenowia")) {
  journal_format <- match.arg(journal_format)
  if (is.null(name) || is.na(name) || name == "")
    return("")
  s <- as.character(name)

  if (journal_format == "Willdenowia") {
    # Ensure a space after period when followed by another initial (J.A. -> J. A.)
    s <- gsub("(\\b[A-Za-z])\\.(?=[A-Za-z]\\.)", "\\1. ", s, perl = TRUE)
    # Also make sure there is a single space after an initial before a surname if missing
    s <- gsub("(\\b[A-Za-z])\\.([A-Z][a-z])", "\\1. \\2", s, perl = TRUE)
  } else {
    # Taxon/Systematic Botany: collapse spaces between consecutive initials (J. A. -> J.A.)
    s <- gsub("(\\b[A-Za-z])\\.\\s+([A-Za-z])\\.", "\\1.\\2.", s, perl = TRUE)
  }

  # Cleanup multiple spaces if created
  s <- trimws(gsub("\\s+", " ", s))
  return(s)
}
