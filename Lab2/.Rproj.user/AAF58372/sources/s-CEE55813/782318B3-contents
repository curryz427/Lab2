# create a new variable
drg.data <- read.csv("DRG_data.csv") %>%
  group_by(DRG.Definition) %>%
  mutate(DRG = strsplit(as.character(DRG.Definition),
                        split = ' ')[[1]][1]) %>%
  ungroup()

# calculation function
library(dplyr)
#' Title
#'
#' @param type
#'
#' @return
#' @export
#'
#' @examples claculate_DRG(mean)
calculate_DRG <- function(type) {
  DRG_spread <- DRG %>%
    select(DRG, `Provider Id`, `Provider State`, `Average Medicare Payments`) %>%
    spread(DRG, `Average Medicare Payments`)
  switch(type,
         mean = apply(DRG_spread[,3:102], MARGIN = 2, mean, na.rm = T),
         median = apply(DRG_spread[,3:102], MARGIN = 2, median, na.rm = T),
         sd = apply(DRG_spread[,3:102], MARGIN = 2, sd, na.rm = T))
}
