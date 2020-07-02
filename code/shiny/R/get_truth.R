get_truth <- function(file, inc_cum, source) {
  data.table::fread(file,
                    colClasses  = c(
                      "date"          = "Date",
                      "location"      = "character",
                      # location_name = readr::col_character(),
                      "value"         = "double"
                    ),nThread = 1) %>%
    dplyr::mutate(inc_cum = inc_cum, source = source) %>%
    na.omit()
}
