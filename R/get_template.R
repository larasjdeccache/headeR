#' Download a template CSV for headeR
#' @param dir Character string defining the directory where the file will be saved. Default is \code{"results"}.
#' @param file_name Character string defining the name of the resulting Markdown file. Default is \code{"header_complete.md"}.
#'
#' @importFrom utils write.csv
#'
#' @export
get_template <- function(dir = "data_template",
                         file_name = "template_headeR.csv") {

  output_path <- file.path(dir, file_name)
  template <- headeR::heading_data[0:7, ]
  utils::write.csv(template, output_path, row.names = FALSE, na = "")

  message(paste("\u2714 Template created at:", getwd(), "/", output_path))
}
