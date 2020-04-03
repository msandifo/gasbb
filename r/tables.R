#tables

t.y.av <- cbind( 
  
  tibble(value=c("supply - TJ/day", "supply less CI demand"), "max", "min"),
               round(  rbind(y.av   %>%  subset( reservation=="0%") %>% select(-reservation, -adj.supply.force.maj, -m.max, -m.min) %>%
                  pivot_wider(names_from = year, values_from = c(  supply)),
                
  y.av   %>%  subset( reservation=="0%") %>% select(-reservation, -supply, -m.max, -m.min) %>%
  pivot_wider(names_from = year, values_from = c(  adj.supply.force.maj)),
 
y.av   %>%  subset( reservation=="0%") %>% select(-reservation, -supply, -adj.supply.force.maj, -m.min) %>%
  pivot_wider(names_from = year, values_from = c(  m.max)),
y.av   %>%  subset( reservation=="0%") %>% select(-reservation, -supply, -adj.supply.force.maj, -m.max) %>%
  pivot_wider(names_from = year, values_from = c(  m.min))
               ), 
0)
) 
  
knitr::kable( (t.y.av ))
