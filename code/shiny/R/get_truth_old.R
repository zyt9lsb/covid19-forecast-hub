get_truth_old <- function(locations){
  truth_cols = c(
    "date"          = "Date",
    "location"      = "character",
    # location_name = readr::col_character(),
    "value"         = "double"
  )
  
  # JHU
  inc_jhu = data.table::fread(file_in("data-truth/truth-Incident Deaths.csv"),
                              colClasses  = truth_cols) %>%
    dplyr::mutate(inc_cum = "inc", source = "JHU-CSSE") %>%
    na.omit()
  
  cum_jhu = data.table::fread(file_in("data-truth/truth-Cumulative Deaths.csv"),
                              colClasses = truth_cols) %>%
    dplyr::mutate(inc_cum = "cum", source = "JHU-CSSE")
  
  
  # USAFacts
  inc_usa = data.table::fread(file_in("data-truth/usafacts/truth_usafacts-Incident Deaths.csv"),
                              colClasses=truth_cols) %>%
    dplyr::mutate(inc_cum = "inc", source = "USAFacts") %>%
    na.omit()
  
  cum_usa = data.table::fread(file_in("data-truth/usafacts/truth_usafacts-Cumulative Deaths.csv"),
                              colClasses=truth_cols) %>%
    dplyr::mutate(inc_cum = "cum", source = "USAFacts")
  
  
  # NYTimes
  inc_nyt = data.table::fread(file_in("data-truth/nytimes/truth_nytimes-Incident Deaths.csv"),
                              colClasses =truth_cols) %>%
    dplyr::mutate(inc_cum = "inc", source = "NYTimes") %>%
    na.omit()
  
  cum_nyt = data.table::fread(file_in("data-truth/nytimes/truth_nytimes-Cumulative Deaths.csv"),
                              colClasses = truth_cols) %>%
    dplyr::mutate(inc_cum = "cum", source = "NYTimes")
  
  
  bind_rows(
    bind_rows(inc_jhu, inc_usa, inc_nyt) %>% dplyr::mutate(unit = "day"),
    
    bind_rows(inc_jhu, inc_usa, inc_nyt) %>%
      dplyr::mutate(week = MMWRweek::MMWRweek(date)$MMWRweek) %>%
      dplyr::group_by(location, week, inc_cum, source) %>%
      dplyr::summarize(date = max(date),
                       value = sum(value, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(weekdays(date) == "Saturday") %>%
      dplyr::mutate(unit = "wk") %>%
      dplyr::select(-week),
    
    bind_rows(cum_jhu, cum_usa, cum_nyt) %>%
      dplyr::mutate(unit = "wk") %>%
      dplyr::filter(weekdays(date) == "Saturday"),
    
    bind_rows(cum_jhu, cum_usa, cum_nyt) %>% dplyr::mutate(unit = "day")
  ) %>%
    dplyr::left_join(locations, by = c("location")) %>%
    dplyr::mutate(death_cases = "death",
                  simple_target = paste(unit, "ahead", inc_cum, death_cases))
}  
