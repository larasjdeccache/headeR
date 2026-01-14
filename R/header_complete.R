#' Generate standardized complete botanical taxonomic headings with type material
#'
#' @description
#' `header_complete()` is the advanced function of the \code{headeR} package.
#' It produces full taxonomic treatments suitable for monographs and revisions.
#'
#' Unlike \code{\link{header_simple}}, this function processes and formats detailed type information (Holotypes, Lectotypes, Neotypes, Epitypes, Syntypes, and Paratypes)
#'
#' It processes nomenclature data according to the **International Code of Nomenclature for Algae, Fungi, and Plants (Madrid Code; Turland et al. 2025)**.
#'
#' @details
#' This function groups synonyms into:
#' * **Homotypic synonym (nomenclatural synonym):** "A name based on the same type as that of another name (Art. 14.4)" (Glossary - Turland et al. 2025).
#' * **Heterotypic (taxonomic synonym):** "A name based on a type different from that of another name referring to the same taxon (Art. 14.4)" (Glossary - Turland et al. 2025).
#'
#' The function also performs complex operations such as:
#' \itemize{
#'   \item **Type placement:** Attaches type citations to the end of the synonyms block (accepted name + homotypic synonyms + type material | heterotypic synonyms + type material).
#'   \item **Lectotype handling:** If a lectotype is designated, other original material is automatically relabeled (e.g., to "Remaining syntypes").
#'   \item **Alerts:** Automatically flags taxa with syntypes that lack a lectotype, aiding the taxonomist in identifying necessary lectotypifications.
#' }
#'
#' @param df A data frame containing taxonomic columns (e.g., \code{acceptedScientificName}, \code{scientificName}, \code{basionym}, \code{namePublishedIn}, etc.).
#' @param journal_format Character string specifying the journal style: \code{"Taxon"}, \code{"Systematic Botany"}, or \code{"Willdenowia"}. Default is \code{"Taxon"}.
#' @param label Character string specifying the label of the remaining syntypes' contents (e.g., \code{"Remaining syntypes"}, \code{"Other original material"}). Default is \code{"Remaining syntypes"}.
#' @param include_notes Logical. Include user notes. Default is \code{TRUE}.
#' @param notes_label Character string for the notes section label. Default is \code{"Notes"}.
#' @param include_homonyms Logical. If \code{TRUE}, includes homonyms in the output list. Default is \code{"FALSE"}.
#' @param dir Character string defining the directory where the file will be saved. Default is \code{"results"}.
#' @param file_name Character string defining the name of the resulting Markdown file. Default is \code{"header_complete.md"}.
#' @param export_docx Logical. If \code{TRUE}, converts the markdown to a Word document.
#'
#' @return A formatted Markdown file (and optionally DOCX) containing the full taxonomic treatment.
#'
#' @importFrom rmarkdown pandoc_convert
#' @export
#'
#' @author Lara Serpa Jaegge Deccache (<larasjdeccache@jbrj.gov.br>), Leandro Lacerda Giacomin and Claudio Nicoletti de Fraga
#'
#' @seealso \code{\link{header_simple}} for treatments without type specimens.
#'
#' @section Reference:
#' Turland NJ, Wiersema JH, Barrie FR, Gandhi KN, Gravendyck J, Greuter W, Hawksworth DL, Herendeen PS, Klopper RR, Knapp S, Kusber W-H, Li D-Z, May TW, Monro AM, Prado J, Price MJ, Smith GF, Zamora Señoret JC. 2025. International Code of Nomenclature for algae, fungi, and plants (Madrid Code). Regnum Vegetabile 162. Chicago: University of Chicago Press. <https://doi.org/10.7208/chicago/9780226839479.001.0001>
#'
#' @examples
#' # Loading example data
#' data(heading_data)
#'
#' # Generate a full monograph-style treatment
#' header_complete(
#'   df = heading_data,
#'   journal_format = "Taxon",
#'   label = "Other original material",
#'   dir = tempdir(),
#'   file_name = "header_complete_test.md",
#'   include_homonyms = FALSE
#' )
header_complete <- function(df = NULL,
                            journal_format = c("Taxon", "Systematic Botany", "Willdenowia"),
                            label = "Remaining syntypes",
                            include_notes = TRUE,
                            notes_label = "Notes",
                            include_homonyms = FALSE,
                            dir = "results",
                            file_name = "header_complete.md",
                            export_docx = FALSE) {
  message("headeR: standardizing your complete taxonomic header")
  # ---- Initial settings and treatment ----
  # Ensure the journal format matches the allowed options
  journal_format <- match.arg(journal_format)
  # Construct the full output path from dir and file_name
  output_path <- file.path(dir, file_name)

  # Loads the header formatting style rules
  rules <- get_style_rules(journal_format)

  # Ensure status column exists
  if (!"status" %in% names(df))
    df$status <- NA

  # If 'status' is empty: Check if scientificName == acceptedScientificName.
  df$status <- mapply(function(st, sci, acc) {
    st_clean <- clean_str(st)
    if (st_clean == "") {
      if (clean_str(sci) == clean_str(acc)) {
        return("correct name")
      }
      return("sin.")
    }
    return(st)
  },
  df$status,
  df$scientificName,
  df$acceptedScientificName)

  # Internal definition of the norm to ensure that x is always text
  norm <- function(x) {
    x <- as.character(x)
    tolower(trimws(gsub("\\s+", " ", x)))
  }

  # Format scientific names and publications
  data_fmt <- format_name(df, journal_format = journal_format)
  data_fmt <- format_publication(data_fmt, journal_format = journal_format)

  # Extract list of unique accepted names (correct name)
  accepted_names <- unique(data_fmt$acceptedScientificName)
  accepted_names <- accepted_names[!is.na(accepted_names) &
                                     accepted_names != ""]

  # ---- Construction of headers for each taxon ----
  result_list <- lapply(seq_along(accepted_names), function(i) {
    acc_name <- accepted_names[i]

    message(paste0(" [", i, "/", length(accepted_names), "] Processing: "), acc_name)

    # Filter the dataset for the current accepted name and remove empty rows
    group <- data_fmt[which(as.character(data_fmt$acceptedScientificName) == acc_name), ]
    group <- group[!is.na(group$scientificName) &
                     group$scientificName != "", ]

    # ---- Homonym treatment ----
    if (!isTRUE(include_homonyms)) {
      if (all(grepl("homonym|exclude", tolower(group$status)))) {
        return(NULL)
      }
    }

    # ---- Standardize the accepted names ----
    # Main header construction (format correct name line)
    acc_idx <- which(norm(group$scientificName_raw) == norm(acc_name))

    if (length(acc_idx) > 0) {
      # Take the first occurrence
      acc_row <- group[acc_idx[1], ]
      name_fmt <- clean_str(acc_row$scientificName)
      pub_acc <- clean_str(acc_row$formatted_publication)

      if (pub_acc == "") {
        valid_pubs <- group$formatted_publication[group$formatted_publication != "" &
                                                    !is.na(group$formatted_publication)]
        if (length(valid_pubs) > 0)
          pub_acc <- clean_str(valid_pubs[1])
      }

      acc_bas <- clean_str(acc_row$basionym)
      acc_status <- tolower(clean_str(acc_row$status))
    } else {
      # If the correct name is not in the rows, create a temporary dataframe to format the name correctly
      temp_df <- data.frame(scientificName = acc_name,
                            stringsAsFactors = FALSE)
      temp_fmt <- format_name(temp_df, journal_format = journal_format)
      name_fmt <- clean_str(temp_fmt$scientificName)
      pub_acc <- ""
      acc_bas <- ""
      acc_status <- ""
    }

    # ---- Standardize Synonyms ----
    syn_info <- format_synonym(
      df = data_fmt,
      group_data = group,
      accepted_name = acc_name,
      journal_format = journal_format,
      include_types = TRUE
    )

    # Apply bold formatting to the accepted name italicized part (binomial)
    name_bold <- gsub("(\\*.*?\\*)", "**\\1**", name_fmt)

    # ---- Header structuring ----
    header_main <- paste0(name_bold, " ", pub_acc)

    if (syn_info$homo != "") {
      # Use a single dot then append the synonym block (syn_info$homo contains its own leading space)
      header_main <- paste0(gsub("\\.+$", "", header_main), ".", syn_info$homo)
    }

    # ---- Standardize Holotypes, Lectotypes, Neotypes and Epitypes ----
    # Identify the rows that belong to the same basionym (homotypic synonyms)
    target_names <- norm(c(acc_name, acc_bas))
    is_target_taxon <- (norm(group$scientificName_raw) %in% target_names) |
      (norm(group$basionym) %in% target_names)

    # Excludes paratypes and syntypes for treatment
    t_data_acc <- group[is_target_taxon & !is.na(group$category) &
                          !grepl("paratype|syntype", group$category, ignore.case = TRUE), ]

    # Format type block
    if (nrow(t_data_acc) > 0) {
      res_type <- format_type(t_data_acc, journal_format = journal_format)
      if (res_type != "") {
        header_main <- paste0(gsub("\\.+$", "", header_main),
                              rules$type_title_prefix,
                              res_type)
      }
    }

    header_main <- paste0(gsub("\\.+$", "", header_main), ".")

    # ---- Standardize Paratypes, Syntypes and Remaining Syntypes ----

    # If there is a lectotype, the syntypes become remnants.
    has_lectotype <- any(grepl("lectotype", t_data_acc$category, ignore.case = TRUE),
                         na.rm = TRUE)

    # Paratype filtering
    is_paratype <- grepl("paratype", group$category, ignore.case = TRUE)
    par_data <- group[is_paratype, ]
    # Paratypes header contruction
    paratype_block <- format_other_types(par_data, label = "Paratypes", journal_format = journal_format)

    # Remaining syntypes treatment
    all_orig_mat <- group[is_target_taxon & !is.na(group$category) &
                            grepl("original material|isosyntype|syntype",
                                  group$category,
                                  ignore.case = TRUE), ]

    remaining_synt_data <- all_orig_mat[!grepl("lectotype", all_orig_mat$category, ignore.case = TRUE), ]

    # Syntypes and remaining syntypes header contruction
    if (has_lectotype) {
      remaining_synt_block <- format_other_types(remaining_synt_data,
                                                 label = label,
                                                 journal_format = journal_format)
      syntype_block <- ""
    } else {
      remaining_synt_block <- ""
      synt_data <- remaining_synt_data[grepl("syntype", remaining_synt_data$category, ignore.case = TRUE), ]
      syntype_block <- format_other_types(synt_data, label = "Syntypes", journal_format = journal_format)
    }

    # ---- User notes and alerts ----
    # Determine if any syntypes exist to trigger the mandatory alert
    has_syntypes <- any(grepl("syntype", group$category, ignore.case = TRUE),
                        na.rm = TRUE)
    alert_txt <- if (has_syntypes &&
                     !has_lectotype)
      "\n\n**Alert!** Syntypes detected; lectotypification required."
    else
      ""

    notes_block <- ""
    # Process the 'notes' column if it exists in the dataframe
    if (isTRUE(include_notes) && "notes" %in% names(group)) {
      # Filter only rows with actual content in the notes field
      notes_data <- group[!is.na(group$notes) & group$notes != "", ]
      if (nrow(notes_data) > 0) {
        notes_data <- notes_data[!duplicated(notes_data[, c("notes", "scientificName")]), ]
        formatted_notes <- sapply(seq_len(nrow(notes_data)), function(j) {
          paste0(
            notes_data$notes[j],
            " (",
            notes_data$scientificName[j],
            " - ",
            notes_data$barcode[j],
            ")"
          )
        })
        notes_block <- paste0("\n\n**",
                              notes_label,
                              ":** ",
                              paste(formatted_notes, collapse = "\n\n"))
      }
    }

    # ---- Combine name, publication, synonyms, types, and paratypes, syntypes or remaining syntypes ----
    return(
      paste0(
        header_main,
        syn_info$hete,
        remaining_synt_block,
        syntype_block,
        paratype_block,
        alert_txt,
        notes_block
      )
    )
  })

  # ---- Export and Final Numbering ----
  final_text <- unlist(result_list)
  final_text <- final_text[final_text != "" & !is.na(final_text)]

  if (length(final_text) > 0) {
    indices <- seq_along(final_text)
    # Adds "**1**\." at the beginning of the string
    final_text <- paste0("**", indices, "**\\. ", final_text)
  }

  # ---- Save as Markdown and optionally convert to DOCX ----
  out_dir <- dirname(output_path)
  if (!dir.exists(out_dir))
    dir.create(out_dir, recursive = TRUE)
  writeLines(final_text,
             con = output_path,
             sep = "\n\n---\n\n",
             useBytes = TRUE)

  full_output_path <- normalizePath(output_path, mustWork = TRUE)

  message("\n\u2714 Markdown generated at '", full_output_path, "'")

  if (isTRUE(export_docx) &&
      requireNamespace("rmarkdown", quietly = TRUE)) {
    docx_path <- gsub("\\.md$", ".docx", full_output_path)
    template_path <- system.file("template.docx", package = "headeR")
    attempts <- 0
    while (!file.exists(full_output_path) && attempts < 10) {
      Sys.sleep(0.2)
      attempts <- attempts + 1
    }
    tryCatch({
      rmarkdown::pandoc_convert(
        input = full_output_path,
        to = "docx",
        output = docx_path,
        options = if (file.exists(template_path))
          c(paste0("--reference-doc=", template_path))
        else
          NULL,
        verbose = FALSE
      )
      message("\n\u2714 DOCX generated at '", docx_path, "'")
    }, error = function(e) {
      stop(
        "\nPandoc conversion failed. This often happens if the .md file is locked by another process or if the path is invalid. Error: ",
        e$message
      )
    })
  }
  # ---- Final message ----
  message(
    "\n",
    paste(rep("-", 30), collapse = ""),
    "\n",
    "\U0001f4a1 IMPORTANT REMINDER: \n",
    "\n\u26a0 Ortographical variants (Art. 60) and hybrid notations (Art. H.3) should be manually verified \n",
    "Automated formatting is a support tool; database inconsistencies or complex cases may require manual adjustment. Always verify your results against the Madrid Code (Turland et al. 2025).\n",
    paste(rep("-", 30), collapse = ""),
    "\n"
  )
}
