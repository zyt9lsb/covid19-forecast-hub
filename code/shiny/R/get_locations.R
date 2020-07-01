get_locations = function(file) {
  data.table::fread(file,
                    colClasses=c(
                      "abbreviation"  = "character",
                      "location"      = "character",
                      "location_name" = "character"))
}
