# plot4.R
# Creates plot4.png (480x480) using base R

read_power_subset <- function(data_file) {
  con <- file(data_file, open = "r")
  on.exit(close(con), add = TRUE)
  
  header <- readLines(con, n = 1)
  col_names <- strsplit(header, ";")[[1]]
  
  lines <- readLines(con)
  keep <- grepl("^(1/2/2007|2/2/2007);", lines)
  
  dat <- read.table(text = lines[keep],
                    sep = ";",
                    header = FALSE,
                    col.names = col_names,
                    na.strings = "?",
                    stringsAsFactors = FALSE)
  
  dat$DateTime <- strptime(paste(dat$Date, dat$Time), format = "%d/%m/%Y %H:%M:%S")
  
  num_cols <- c("Global_active_power", "Global_reactive_power", "Voltage",
                "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
  for (nm in num_cols) dat[[nm]] <- as.numeric(dat[[nm]])
  
  dat
}

data_file <- "household_power_consumption.txt"
df <- read_power_subset(data_file)

png("plot4.png", width = 480, height = 480)

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

# Top-left
plot(df$DateTime, df$Global_active_power, type = "l",
     xlab = "", ylab = "Global Active Power")

# Top-right
plot(df$DateTime, df$Voltage, type = "l",
     xlab = "datetime", ylab = "Voltage")

# Bottom-left
plot(df$DateTime, df$Sub_metering_1, type = "l",
     xlab = "", ylab = "Energy sub metering")
lines(df$DateTime, df$Sub_metering_2, col = "red")
lines(df$DateTime, df$Sub_metering_3, col = "blue")
legend("topright",
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       col = c("black", "red", "blue"),
       lty = 1, bty = "n")

# Bottom-right
plot(df$DateTime, df$Global_reactive_power, type = "l",
     xlab = "datetime", ylab = "Global Reactive Power")

dev.off()
