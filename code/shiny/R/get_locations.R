get_locations = function(file) {
  readr::read_csv(file,
                    col_types = readr::cols(
                      abbreviation  = readr::col_character(),
                      location      = readr::col_character(),
                      location_name = readr::col_character()))
}
