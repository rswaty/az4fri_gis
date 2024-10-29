


library(tidyverse)

pas <- unique(final_df$ID)


# try to outputs
purrr::map(
  .x = pas,  # vector of param values
  .f = ~rmarkdown::render(
    input = "pa_report.Rmd",  # RMarkdown filepath
    params = list(ID = .x),  # iterated parameter value
    output_file = stringr::str_glue("pa_reports/", .x, "-report.html")  # iterated output path
  )
)
