library(tidyverse)
calc_delay = function(data){
  data |>
    summarize(delay = mean(`arr_delay` + `dep_delay`))
}