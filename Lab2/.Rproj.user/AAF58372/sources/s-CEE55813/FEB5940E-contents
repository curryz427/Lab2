
library(tidyverse)
library(dplyr)
# create a new variable called DRG
drg.data <- read.csv("DRG_data.csv") %>%
  group_by(DRG.Definition) %>%
  mutate(DRG = strsplit(as.character(DRG.Definition),
                        split = ' ')[[1]][1]) %>%
  ungroup()

# function of boxplot
#' boxplot
#'
#' @param k
#'
#' @return boxplot
#' @export
#'
#' @import tidyverse
#'
#' @examples boxplot("Average.Medicare.Payments")
boxplot = function(k){
  if (k == "Average.Medicare.Payments"){

  g <-ggplot(drg.data,aes(x = DRG,y = Average.Medicare.Payments)) +
    scale_y_continuous(trans='log10') +
    geom_boxplot(outlier.shape = NA) +
    theme(axis.text.x =element_text(angle = 90,size = 6,hjust = 1)) +
    ylab('log Average Medicare Payments') +
    xlab('DRG Code') +
    ggtitle('Average Medicare Payments for Hospitals by DRG Code')
  }
  if (k == 'Average.Total.Payments'){
    g <-ggplot(drg.data,aes(x = DRG,y = Average.Total.Payments))+
      scale_y_continuous(trans='log10')+
      geom_boxplot(outlier.shape = NA)+
      theme(axis.text.x =element_text(angle = 90,size = 6,hjust = 1))+
      ylab('Log Average Medicare Payments')+
      xlab('DRG Code')+
      ggtitle('Average Total Payments for Hospitals by DRG Code')
    g
  }
}
