


library(tidyverse)

firesheds <- unique(final_df$ID)


# try to outputs
purrr::map(
  .x = firesheds,  # vector of param values
  .f = ~rmarkdown::render(
    input = "fireshed_report.Rmd",  # RMarkdown filepath
    params = list(ID = .x),  # iterated parameter value
    output_file = stringr::str_glue("reports/", .x, "-report.html")  # iterated output path
  )
)
