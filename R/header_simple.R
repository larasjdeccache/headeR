#' Generate standardized simple botanical taxonomic headings
#'
#' @description
#' The primary function of the \code{headeR} package. The `header_simple()` function automates the creation of basic taxonomic headings.
#' It processes nomenclature data according to the **International Code of Nomenclature for Algae, Fungi, and Plants (Madrid Code; Turland et al. 2025)**.
#' @details
#' This function groups synonyms into:
#' * **Homotypic synonym (nomenclatural synonym):** "A name based on the same type as that of another name (Art. 14.4)" (Glossary - Turland et al. 2025).
#' * **Heterotypic (taxonomic synonym):** "A name based on a type different from that of another name referring to the same taxon (Art. 14.4)" (Glossary - Turland et al. 2025).
#'
#' It is ideal for checklists, simple floras, or preliminary taxonomic treatments.
#'
#' @seealso \code{\link{header_complete}} for treatments including type specimens.
#'
#' @param df A data frame containing taxonomic columns. Required columns often include: `acceptedScientificName`, `scientificName`, `basionym`, `namePublishedIn`, `status`, and `category`.
#' @param journal_format Character string specifying the journal style: \code{"Taxon"}, \code{"Systematic Botany"}, or \code{"Willdenowia"}. Default is \code{"Taxon"}.
#' @param include_notes Logical. If \code{TRUE}, appends content from the `notes` column to the end of the block. Default is \code{TRUE}.
#' @param notes_label Character string for the label preceding notes (e.g., "Taxonomic Notes", "Nomenclatural notes"). Default is \code{"Notes"}.
#' @param include_homonyms Logical. If \code{FALSE} (default), names flagged as "homonym" or "exclude" in the `status` column are skipped. If \code{TRUE}, they are formatted and numbered.
#' @param dir Character string defining the directory where the file will be saved. Default is \code{"results"}.
#' @param file_name Character string defining the name of the resulting Markdown file. Default is \code{"header_complete.md"}.
#' @param export_docx Logical. If \code{TRUE}, converts the markdown to a Word document.
#'
#' @return A Markdown file (and optionally a DOCX file) saved in `dir`.
#'
#' @importFrom rmarkdown pandoc_convert
#' @export
#'
#' @author Lara Serpa Jaegge Deccache (<larasjdeccache@jbrj.gov.br>), Leandro Lacerda Giacomin and Claudio Nicoletti de Fraga
#'
#' @section Reference:
#' Turland NJ, Wiersema JH, Barrie FR, Gandhi KN, Gravendyck J, Greuter W, Hawksworth DL, Herendeen PS, Klopper RR, Knapp S, Kusber W-H, Li D-Z, May TW, Monro AM, Prado J, Price MJ, Smith GF, Zamora Señoret JC. 2025. International Code of Nomenclature for algae, fungi, and plants (Madrid Code). Regnum Vegetabile 162. Chicago: University of Chicago Press. <https://doi.org/10.7208/chicago/9780226839479.001.0001>
#'
#' @examples
#' # Loading example data
#' data(heading_data)
#'
#' # Generate a simple checklist in Taxon format
#' header_simple(
#'   df = heading_data,
#'   journal_format = "Taxon",
#'   dir = tempdir(), # Saving to temporary directory
#'   file_name = "header_simple_test.md",
#'   export_docx = FALSE
#' )
header_simple <- function(df = NULL,
                          journal_format = c("Taxon", "Systematic Botany", "Willdenowia"),
                          include_notes = TRUE,
                          notes_label = "Notes",
                          include_homonyms = FALSE,
                          dir = "results",
                          file_name = "header_simple.md",
                          export_docx = FALSE) {
  message("headeR: standardizing your simple taxonomic header")
  # ---- Initial settings and treatment ----
  # Ensure the journal format matches the allowed options
  journal_format <- match.arg(journal_format)
  # Construct the full output path from dir and file_name
  output_path <- file.path(dir, file_name)
  # Loads the header formatting style rules
  rules <- get_style_rules(journal_format)

  # Standardize scientific names and publications
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

    # Avoid duplicate synonyms in the list
    group <- group[!duplicated(group[, c("scientificName", "formatted_publication")]), ]

    # ---- Standardize the synonyms ----
    syn_info <- format_synonym(
      df = data_fmt,
      group_data = group,
      accepted_name = acc_name,
      journal_format = journal_format,
      include_types = FALSE
    )

    homo_part <- if (syn_info$homo != "")
      paste0(rules$sep_syn, syn_info$homo)
    else
      ""
    hete_part <- syn_info$hete
    synonym_block <- paste0(homo_part, hete_part)

    # ---- Homonym treatment ----
    if (!isTRUE(include_homonyms)) {
      if (all(grepl("homonym|exclude", tolower(group$status)))) {
        return(NULL)
      }
    }

    # ---- Standardize the accepted names ----
    # Main header construction (the accepted name text line)
    acc_idx <- which(group$scientificName_raw == acc_name)

    if (length(acc_idx) > 0) {
      # Take the first occurrence
      acc_row <- group[acc_idx[1], ]
      name_fmt <- clean_str(acc_row$scientificName)
      pub_acc <- clean_str(acc_row$formatted_publication)
      acc_bas <- clean_str(acc_row$basionym)
    } else {
      # If the accepted name is not in the CSV lines, create a temporary dataframe to format the name correctly
      temp_df <- data.frame(scientificName = acc_name,
                            stringsAsFactors = FALSE)
      temp_fmt <- format_name(temp_df, journal_format = journal_format)
      name_fmt <- clean_str(temp_fmt$scientificName)
      pub_acc <- ""
      acc_bas <- ""
    }

    # Apply bold formatting to the accepted name italicized part (binomial)
    name_bold <- gsub("(\\*.*?\\*)", "**\\1**", name_fmt)
    header_main <- paste0(name_bold, " ", pub_acc)

    # --- BLOCO DE NOTAS ---
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
    return(paste0(header_main, synonym_block, notes_block))
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
