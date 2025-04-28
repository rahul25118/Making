# Load required libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
install.packages("plotly")
library(plotly)
library(rmarkdown)

# Load merged dataset
merged_data <- read.csv("merged_btc_nifty_eth.csv", stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date)
merged_data <- merged_btc_nifty_eth
sum(is.na(merged_data))

merged_data$Date <- as.Date(merged_data$Date, format = "%Y-%m-%d") # adjust format if needed


# Function to format numbers for plots
format_number <- function(x) {
  ifelse(is.na(x), "",
         ifelse(x >= 1e6, paste0(round(x / 1e6, 1), "M"),
                ifelse(x >= 1e3, paste0(round(x / 1e3, 1), "K"), round(x, 0))))
}

# Create price trend plot
price_plot <- ggplot(merged_data) +
  geom_line(aes(x = Date, y = BTC_Close, color = "BTC Close"), na.rm = TRUE) +
  geom_line(aes(x = Date, y = ETH_Close, color = "ETH Close"), na.rm = TRUE) +
  geom_line(aes(x = Date, y = NIFTY_Close, color = "NIFTY Close"), na.rm = TRUE) +
  scale_y_continuous(
    name = "BTC/ETH Price (USD)",
    labels = format_number,
    sec.axis = sec_axis(~ ., name = "NIFTY Price (INR)", labels = format_number)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") +
  scale_color_manual(values = c("BTC Close" = "#3B82F6", "ETH Close" = "#10B981", "NIFTY Close" = "#F59E0B")) +
  labs(title = "Price Trends (2017–2025)", x = "Date", color = "Asset") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# Convert to interactive plot
price_plotly <- ggplotly(price_plot, tooltip = c("x", "y", "color")) %>%
  layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))

# Create volatility plot
volatility_plot <- ggplot(merged_data) +
  geom_area(aes(x = Date, y = BTC_PriceRange, fill = "BTC Price Range"), alpha = 0.3, na.rm = TRUE) +
  geom_area(aes(x = Date, y = ETH_PriceRange, fill = "ETH Price Range"), alpha = 0.3, na.rm = TRUE) +
  geom_area(aes(x = Date, y = NIFTY_PriceRange, fill = "NIFTY Price Range"), alpha = 0.3, na.rm = TRUE) +
  scale_y_continuous(
    name = "BTC/ETH Price Range (USD)",
    labels = format_number,
    sec.axis = sec_axis(~ ., name = "NIFTY Price Range (INR)", labels = format_number)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") +
  scale_fill_manual(values = c("BTC Price Range" = "#3B82F6", "ETH Price Range" = "#10B981", "NIFTY Price Range" = "#F59E0B")) +
  labs(title = "Volatility (Daily Price Range)", x = "Date", fill = "Asset") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# Convert to interactive plot
volatility_plotly <- ggplotly(volatility_plot, tooltip = c("x", "y", "fill")) %>%
  layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))

# Create volume plot
volume_plot <- ggplot(merged_data) +
  geom_bar(aes(x = Date, y = BTC_Volume, fill = "BTC Volume"), stat = "identity", alpha = 0.3, na.rm = TRUE) +
  geom_bar(aes(x = Date, y = ETH_Volume, fill = "ETH Volume"), stat = "identity", alpha = 0.3, na.rm = TRUE) +
  geom_bar(aes(x = Date, y = NIFTY_Volume, fill = "NIFTY Volume"), stat = "identity", alpha = 0.3, na.rm = TRUE) +
  scale_y_continuous(
    name = "BTC/ETH Volume",
    labels = format_number,
    sec.axis = sec_axis(~ ., name = "NIFTY Volume", labels = format_number)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") +
  scale_fill_manual(values = c("BTC Volume" = "#3B82F6", "ETH Volume" = "#10B981", "NIFTY Volume" = "#F59E0B")) +
  labs(title = "Trading Volume", x = "Date", fill = "Asset") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# Convert to interactive plot
volume_plotly <- ggplotly(volume_plot, tooltip = c("x", "y", "fill")) %>%
  layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))

#Add this to display the plots
price_plotly
volatility_plotly
volume_plotly
