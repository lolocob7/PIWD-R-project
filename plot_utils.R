library(ggplot2)

plot_unemployment_trend <- function(df, country_name) {
  # Filter data for the specific country
  country_data <- df[df$geo == country_name, ]
  
  if (nrow(country_data) == 0) {
    return(NULL)
  }
  
  # Create the plot
  p <- ggplot(country_data, aes(x = TIME_PERIOD, y = OBS_VALUE, color = sex)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = paste("Unemployment Rate Trend in", country_name),
         x = "Year",
         y = "Unemployment Rate (%)",
         color = "Sex") +
    theme_minimal()
    
  return(p)
}
