library(ggplot2)
library(tidyr)


plot_chi_squared_pearson_residuals = function(contingency_table) {
  chisq = chisq.test(contingency_table)
  
  p = data.frame(chisq$residuals)
  
  p$race = row.names(p)
  
  p = pivot_longer(data=p, cols = colnames(contingency_table))
  
  p <- mutate(p, race = str_wrap(race, width = 6))
  
  ggplot(p, aes(name, value, fill=race)) + 
    theme_bw() +
    geom_bar(stat='identity') + 
    facet_grid(rows=vars(race)) + 
    coord_flip() +
    guides(fill = "none") +
    labs(x = element_blank(), y = 'Pearson Residuals')
}


plot_chi_squared_contribution_percent = function(contingency_table, y_lab) {
  chisq = chisq.test(contingency_table)
  
  contrib <- chisq$residuals^2/chisq$statistic
  
  p = data.frame(contrib)
  
  p$race = row.names(p)
  
  p = pivot_longer(data=p, cols = colnames(contingency_table))
  
  # p %>% rename(c('Proportion of '='value'))
  
  p <- mutate(p, race = str_wrap(race, width = 6))
  
  ggplot(p, aes(name, value, fill=race)) + 
    geom_bar(stat='identity') + 
    theme_bw() +
    facet_grid(rows=vars(race)) + 
    coord_flip() +
    guides(fill = "none")+
    labs(x = element_blank(), y = y_lab)
}