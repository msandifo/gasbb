fit <- lm(data=merge.df %>% subset(vwp<300),  vwp~benchmark_price)
summary(fit)
coef(summary(fit))
broom::tidy(fit)
 broom::glance(fit)
broom::augment(fit)
weekly.fit <- lm(data=merge.byweek.df %>% subset(vwp<100),  (vwp)~benchmark_price)
 
summary(weekly.fit)
coef(summary(weekly.fit))[,1:2] %>% round(2)
broom::tidy(weekly.fit)
broom::glance(weekly.fit)
broom::augment(weekly.fit)

monthly.fit <- lm(data=m.df %>% subset(vwp<150),  vwp~benchmark_price)
broom::tidy(monthly.fit)
broom::glance(monthly.fit)
broom::augment(monthly.fit)

get_fits <- function(df) {
  fit <-lm(data=df %>% subset(vwp<100),  (vwp)~benchmark_price)
paste0 ("intercept = ",broom::tidy( fit)[1,2:3] %>% round(2) %>% glue::glue_collapse( sep = c(" "), last="±"), "$/MWhr\n",
 "slope     = ",broom::tidy( fit)[2,2:3] %>% round(2) %>% glue::glue_collapse( sep = c(" "), last="±"), "$/GJ")
}
