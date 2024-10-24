setwd("/Users/clementtwumasi/Desktop/AIRehab_cluster project")

library("tsoutliers") #for automatic detection and correction of time series outliers
# Importing external scripts
#for plotting series with outliers
source("plot-tsoutliers-script.r")

# Setting working directory
setwd("/Users/clementtwumasi/Desktop/AIRehab_cluster project/64 test")

options(repr.plot.width=8, repr.plot.height=8,repr.plot.res=300) #Setting plot size

# data_directory_path<- setwd("/Users/clementtwumasi/Desktop/AIRehab_cluster project/54 participants")

data_directory_path<- setwd("/Users/clementtwumasi/Desktop/AIRehab_cluster project/64 test")

# Loading relevant R packages
library(readxl)
library(dplyr)
library(tidyr)
library(cluster)
library(flexclust)
library(ggplot2)
library(gridExtra)
library(officer)

# Importing the various data structure

# Step 3: List and filter the files
all_files <- list.files(path = data_directory_path)
excel_files <- all_files[grepl("\\.xls$|\\.xlsx$", all_files, ignore.case = TRUE)]


# printing all the names of the excel data
print(excel_files)

# gtools::mixedsort(print(excel_files))

length(excel_files)

# extract only the Left knee data files
l_files <- all_files[grepl("L_CT_LC_m_DT_Tr.*\\.xls$|L_CT_LC_m_DT_Tr.*\\.xlsx$", all_files, ignore.case = F)]

# Step 4: Import the datasets
left_datasets <- lapply(l_files, read_excel)


cat("Total number of datasets (for Left Knee):")
length(left_datasets)

# view first data (1st 10 rows of the data)
head(left_datasets[[1]], n=10)

left_combined_data<- do.call("rbind",left_datasets)

dim(left_combined_data)

names(left_combined_data)[c(1,2)]<- c("Patient.ID", "Time_secs")

names(left_combined_data)

length(unique(left_combined_data$"Patient.ID"))

left_combined_data$Patient.ID<- as.factor(left_combined_data$Patient.ID)

levels(left_combined_data$"Patient.ID")

# Extract only the Left knee data files
R_files <- all_files[grepl("R_CT_LC_m_DT_Tr.*\\.xls$|R_CT_LC_m_DT_Tr.*\\.xlsx$", all_files, ignore.case = TRUE)]

# Step 4: Import the datasets
right_datasets <- lapply(R_files, read_excel)


cat("Total number of datasets (for Right Knee):")
length(right_datasets)

# view first data (1st 10 rows of the data)
head(right_datasets[[1]], n=10)

right_combined_data<- do.call("rbind",right_datasets)

dim(right_combined_data)

names(right_combined_data)

names(right_combined_data)[c(1,2)]<- c("Patient.ID", "Time_secs")

right_combined_data$Patient.ID<- as.factor(right_combined_data$Patient.ID)

levels(right_combined_data$Patient.ID)

head(right_combined_data)

names(right_combined_data)

main_outcome_data_right<- right_combined_data[, c("Patient.ID","Time_secs","Force (N)", 
                                                  "z axis displacement (m)","x axis displacement (m)",
                                                  "y axis displacement (m)", 'wt','ht','age','gender','Pain')]



names(main_outcome_data_right)[c(3:6)]<- c("Force_N","z_axis_disp_m", "x_axis_disp_m","y_axis_disp_m")

head(main_outcome_data_right, n=10)

main_outcome_data_left<- left_combined_data[, c("Patient.ID","Time_secs","Force (N)", 
                                                  "z axis displacement (m)","x axis displacement (m)",
                                                  "y axis displacement (m)", 'wt','ht','age','gender','Pain')]

names(main_outcome_data_left)[c(3:6)]<- c("Force_N","z_axis_disp_m", "x_axis_disp_m","y_axis_disp_m")

# as.vector(main_outcome_data_left[, c(5)])$Y_scale==as.vector(main_outcome_data_right[, c(5)])$Y_scale

levels(main_outcome_data_right$"Patient.ID")

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

names(main_outcome_data_right)

table(is.na(main_outcome_data_right$Force_N))

table(is.na(main_outcome_data_left$Force_N))

# Complete cases (Right knee):
main_outcome_data_right<- main_outcome_data_right[complete.cases(main_outcome_data_right$Force_N), ]
table(is.na(main_outcome_data_right$Force_N))

# Complete cases (Left knee):
main_outcome_data_left<- main_outcome_data_left[complete.cases(main_outcome_data_left$Force_N), ]
table(is.na(main_outcome_data_left$Force_N))

# Plotting using ggplot2
ggplot(main_outcome_data_right, aes(x = Time_secs, y = Force_N)) +
  geom_line(color = "blue") + 
  # geom_point(color = "red") +
 facet_wrap(~ Patient.ID) +  # Create a facet for each patient ID
  labs(title = "Force over Time (Right knee)",
       x = "Time (seconds)",
       y = "Force") +
  theme_minimal()


# plot(scaled_combined_data$Time_secs, scaled_combined_data$Force_mm, type="p")

# Plotting using ggplot2
ggplot(main_outcome_data_left, aes(x = Time_secs, y = Force_N)) +
  geom_line(color = "blue") + 
  # geom_point(color = "red") +
 facet_wrap(~ Patient.ID) +  # Create a facet for each patient ID
  labs(title = "Force over Time (Left knee)",
       x = "Time (seconds)",
       y = "Force") +
  theme_minimal()


# Plotting using ggplot2
ggplot(main_outcome_data_right, aes(x = Time_secs, y = x_axis_disp_m)) +
  geom_line(color = "blue") + 
  # geom_point(color = "red") +
 facet_wrap(~ Patient.ID) +  # Create a facet for each patient ID
  labs(title = "X displacement over Time (Right knee)",
       x = "Time (seconds)",
       y = "X displacement") +
  theme_minimal()

# Plotting using ggplot2
ggplot(main_outcome_data_left, aes(x = Time_secs, y = x_axis_disp_m)) +
  geom_line(color = "blue") + 
  # geom_point(color = "red") +
 facet_wrap(~ Patient.ID) +  # Create a facet for each patient ID
  labs(title = "X displacement over Time (Left knee)",
       x = "Time (seconds)",
       y = "X displacement") +
  theme_minimal()

# Plotting using ggplot2
ggplot(main_outcome_data_right, aes(x = Time_secs, y = y_axis_disp_m)) +
  geom_line(color = "blue") + 
  # geom_point(color = "red") +
 facet_wrap(~ Patient.ID) +  # Create a facet for each patient ID
  labs(title = "Y displacement over Time (Right knee)",
       x = "Time (seconds)",
       y = "Y displacement") +
  theme_minimal()

# Plotting using ggplot2
ggplot(main_outcome_data_left, aes(x = Time_secs, y = y_axis_disp_m)) +
  geom_line(color = "blue") + 
  # geom_point(color = "red") +
 facet_wrap(~ Patient.ID) +  # Create a facet for each patient ID
  labs(title = "Y displacement over Time (Left knee)",
       x = "Time (seconds)",
       y = "Y displacement") +
  theme_minimal()

# Plotting using ggplot2
data1<- subset(main_outcome_data_right, Patient.ID=="1 MA R")
data2<- subset(main_outcome_data_left, Patient.ID=="1 MA L")


p1=ggplot(data1, aes(x = Time_secs, y = Force_N)) +
  geom_line(color = "blue") + 
  # geom_point(color = "red") +
 facet_wrap(~ Patient.ID) +  # Create a facet for each patient ID
  labs(title = "(a) Right knee data of a participant",
       x = "Time (seconds)",
       y = "Force (Newtons)") +
  theme_minimal()+ # Avoid text overlapping
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))


p4=ggplot(data2, aes(x = Time_secs, y = Force_N)) +
  geom_line(color = "blue") + 
  # geom_point(color = "red") +
 facet_wrap(~ Patient.ID) +  # Create a facet for each patient ID
  labs(title = "(d) Left knee data of a participant",
       x = "Time (seconds)",
       y = "Force (Newtons)") +
  theme_minimal()+ # Avoid text overlapping
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))




p2=ggplot(data1, aes(x = Time_secs, y = x_axis_disp_m)) +
  geom_line(color = "blue") + 
  # geom_point(color = "red") +
 facet_wrap(~ Patient.ID) +  # Create a facet for each patient ID
  labs(title = "(b) Right knee data of a participant",
       x = "Time (seconds)",
       y = "X-displacement (meters)") +
  theme_minimal()+ # Avoid text overlapping
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))


p5=ggplot(data2, aes(x = Time_secs, y = x_axis_disp_m)) +
  geom_line(color = "blue") + 
  # geom_point(color = "red") +
 facet_wrap(~ Patient.ID) +  # Create a facet for each patient ID
  labs(title = "(e) Left knee data of a participant",
       x = "Time (seconds)",
       y = "X-displacement (meters)") +
  theme_minimal()+ # Avoid text overlapping
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))


p3=ggplot(data1, aes(x = Time_secs, y = y_axis_disp_m)) +
  geom_line(color = "blue") + 
  # geom_point(color = "red") +
 facet_wrap(~ Patient.ID) +  # Create a facet for each patient ID
  labs(title = "(c) Right knee data of a participant",
       x = "Time (seconds)",
       y = "Y-displacement (meters)") +
  theme_minimal()+ # Avoid text overlapping
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))


p6=ggplot(data2, aes(x = Time_secs, y = y_axis_disp_m)) +
  geom_line(color = "blue") + 
  # geom_point(color = "red") +
 facet_wrap(~ Patient.ID) +  # Create a facet for each patient ID
  labs(title = "(f) Left knee data of a participant",
       x = "Time (seconds)",
       y = "Y-displacement (meters)") +
  theme_minimal()+ # Avoid text overlapping
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))




grid.arrange(p1,p4,p2,p5, p3, p6, nrow=3, ncol=2)




library(plotly)

# Create plotly object

p1 <- plot_ly(data1, x = ~Time_secs, y = ~ Force_N, type = 'scatter', mode = 'lines+markers')



# Add title and labels
p1 <- p1 %>% layout(title = "(a)",
                  xaxis = list(title = "Time (seconds)"),
                  yaxis = list(title = "Force (Newtons)"))



p1

# Create plotly object

p <- plot_ly(data1, x = ~Time_secs, y = ~	x_axis_disp_m, type = 'scatter', mode = 'lines+markers')



# Add title and labels
p <- p %>% layout(title = "X displacement over Time",
                  xaxis = list(title = "Time (seconds)"),
                  yaxis = list(title = "Force"))
p

p <- plot_ly(data1, x = ~Time_secs, y = ~	y_axis_disp_m, type = 'scatter', mode = 'lines+markers')



# Add title and labels
p <- p %>% layout(title = "Y displacement over Time",
                  xaxis = list(title = "Time (seconds)"),
                  yaxis = list(title = "Force"))
p

head(data1)
# Converting data to time series
ts_data1 <- ts(data1$Force_N, frequency = dim(data1)[1] / 20)  # Approx 20 cycles per patient

# ts_data1

options(warn=-1)
#Automatic Procedure for Detection of Outliers
outlier.ts1<-tsoutliers::tso(ts_data1 ,types = c("AO","LS","TC"),maxit.iloop=10,tsmethod = "auto.arima")
outlier.ts1

#Corrected data based on outlier detection via residuals of fitted ARIMA(1,1,1)
corrected_outlier.ts1<- outlier.ts1$yadj
# corrected_outlier.ts1

plot.tsoutliers1(outlier.ts1,title_name="",
                xlabel="Time", ylabel="Force data for Patient 1 MA R",xaxt=NULL,lwd=3)


text<-c("No outlier", "Outlier")
legend(x=40,y=3,legend = text,col=c("blue","red"),
      cex=1, horiz = FALSE,pt.cex = 1,fill=c("blue","red"),ncol=1)

head(data1)

 head(main_outcome_data_right)

# Step 2: Clean data and Detrend and deseasonalize data outcome for each patient
detrend_deseasonalize <- function(df, variable) {
  data<-  as.numeric(unlist(as.vector(df[, paste0(variable)])))
  ts_data <- ts(data, frequency = length(data) / 20)  # Approx 20 cycles per patient
    # Stage 1 pre-processing (outlier detection and correction based automatically fitted ARIMA model
  outlier.ts<-tsoutliers::qwer
  corrected_outlier.ts<- outlier.ts$yadj #corrected data
    
    # Stage 2 pre-processing (Detrending and Deseasonalising the data/series)
  decomposed <- stl(corrected_outlier.ts, s.window = "periodic")
    # remove trend and seasonal/cyclical components
  deseasonalized <- decomposed$time.series[, "remainder"]
  df$deseasonalized_var <- deseasonalized
  names(df)[which(names(df)=="deseasonalized_var")] <- paste0("deseasonalized_",variable)
  return(df)
}


# A function for transforming data that did not need automatic outlier detection and correction
detrend_deseasonalize_no_outlier_detect <- function(df, variable) {
  data<-  as.numeric(unlist(as.vector(df[, paste0(variable)])))
  ts_data <- ts(data, frequency = length(data) / 20)  # Approx 20 cycles per patient
  decomposed <- stl(ts_data, s.window = "periodic")
    # remove trend and seasonal/cyclical components
  deseasonalized <- decomposed$time.series[, "remainder"]
  df$deseasonalized_var <- deseasonalized
  names(df)[which(names(df)=="deseasonalized_var")] <- paste0("deseasonalized_",variable)
  return(df)
}
# Fast-fourier transformation

# fft_func<- function(df){
     # ts_data <- ts(df$Force_mm, frequency = length(df$Force_mm) / 20)  # Approx 20 cycles per patient
     # Extract the time series values
     # time_series <- ts_data$value
     # Perform FFT
     # df$force_fft <- fft(ts_data)
     # return(df)

    # }head(fft_func(df=data1))


fft_trans=fft_func(df=data1)
# Calculate the magnitude of the FFT result
magnitude <- Mod(fft_trans$force_fft)

# Create a frequency vector
n <- dim(data1)[1]
frequency <- seq(0, n-1) * (1 / n)

# Plot the magnitude of the FFT result
plot(frequency, magnitude, type = "h", main = "FFT of Time Series Data", xlab = "Frequency", ylab = "Magnitude")
# Visualization
par(mfrow = c(2, 1))
plot(data1$Force_mm, type = "l", main = "Original Signal")
plot(magnitude, type = "l", main = "FFT Magnitudes")
# Identify significant frequencies (peaks in the magnitude plot)
# threshold=median(magnitude)
# significant_frequencies <- frequency[which(magnitude > threshold)]  # Define your own threshold

# Print significant frequencies
# print(significant_frequencies)

main_outcome_data_right$Patient.ID<- as.factor(main_outcome_data_right$Patient.ID)
main_outcome_data_left$Patient.ID<- as.factor(main_outcome_data_left$Patient.ID)

dim(main_outcome_data_right)
dim(main_outcome_data_left)

length(levels(main_outcome_data_right$Patient.ID))

library(parallel)
library(openxlsx)
detectCores()

# DON'T RE-RUN THIS CODE (ALREADY DONE)
# Parallel computing
# Define the function to process each unique ID
process_data <- function(id, main_outcome_data_right) {
  data_i <- subset(main_outcome_data_right, Patient.ID == id)
  
  data_i <- detrend_deseasonalize(df = data_i, variable = "Force_N")
  data_i <- detrend_deseasonalize(df = data_i, variable = "x_axis_disp_m")
  data_i <- detrend_deseasonalize(df = data_i, variable = "y_axis_disp_m")
  
  setwd("/Users/clementtwumasi/Desktop/AIRehab_cluster project/Right_knee transformed data")
  file_name <- paste0("Transformed_data_", id, ".xlsx")
  openxlsx::write.xlsx(data_i, file_name)
  
  return(data_i)
}

# List of unique IDs
unique_ids <- unique(main_outcome_data_right$Patient.ID)

# Use mclapply to apply the function in parallel
detrend_deseasonalised_data_R <- mclapply(unique_ids, process_data, main_outcome_data_right, mc.cores = detectCores())



# Import all transformed data (Right Knee data)
setwd("/Users/clementtwumasi/Desktop/AIRehab_cluster project/Right_knee transformed data")
transformed_data_right<- NULL
N<- length(levels(main_outcome_data_right$"Patient.ID"))
unique_ids<- levels(main_outcome_data_right$"Patient.ID")
for(id in 1:N){
    print(paste("i=", id))
 transformed_data_right[[id]]<- readxl::read_excel(paste0("Transformed_data_",unique_ids[id], ".xlsx"))
    }

# Right knee
combined_main_DTS_data_right<-  do.call("bind_rows", transformed_data_right)
combined_main_DTS_data_right$deseasonalized_Force_N<- as.numeric(combined_main_DTS_data_right$deseasonalized_Force_N)
combined_main_DTS_data_right$deseasonalized_x_axis_disp_m<-as.numeric(combined_main_DTS_data_right$deseasonalized_x_axis_disp_m)
combined_main_DTS_data_right$deseasonalized_y_axis_disp_m<- as.numeric(combined_main_DTS_data_right$deseasonalized_y_axis_disp_m)

head(combined_main_DTS_data_right)
dim(combined_main_DTS_data_right)

# DON'T RE-RUN THIS CODE (ALREADY DONE)
library(parallel)
library(openxlsx)

# Define the function to process each unique ID
process_data <- function(id, main_outcome_data_left) {
  data_i <- subset(main_outcome_data_left, Patient.ID == id)
  
  data_i <- detrend_deseasonalize(df = data_i, variable = "Force_N")
  data_i <- detrend_deseasonalize(df = data_i, variable = "x_axis_disp_m")
  data_i <- detrend_deseasonalize(df = data_i, variable = "y_axis_disp_m")
  
  file_name <- paste0("/Users/clementtwumasi/Desktop/AIRehab_cluster project/Left_knee transformed data/Transformed_data_", id, ".xlsx")
  openxlsx::write.xlsx(data_i, file_name)
  
  return(data_i)
}

# Set the working directory
setwd("/Users/clementtwumasi/Desktop/AIRehab_cluster project/Left_knee transformed data")

# List of unique IDs
unique_ids <- levels(main_outcome_data_left$Patient.ID)

# Use mclapply to apply the function in parallel
detrend_deseasonalised_data_L <- mclapply(unique_ids, process_data, main_outcome_data_left, mc.cores = detectCores())




# Import all transformed data (Right Knee data)
setwd("/Users/clementtwumasi/Desktop/AIRehab_cluster project/Left_knee transformed data")
transformed_data_left<- NULL
N<- length(levels(main_outcome_data_left$"Patient.ID"))
unique_ids<- levels(main_outcome_data_left$"Patient.ID")
for(id in 1:N){
    # print(paste("i=", id))
 transformed_data_left[[id]]<- readxl::read_excel(paste0("Transformed_data_",unique_ids[id], ".xlsx"))
    }

# left knee
combined_main_DTS_data_left<-  do.call("bind_rows", transformed_data_left)
combined_main_DTS_data_left$deseasonalized_Force_N<- as.numeric(combined_main_DTS_data_left$deseasonalized_Force_N)
combined_main_DTS_data_left$deseasonalized_x_axis_disp_m<-as.numeric(combined_main_DTS_data_left$deseasonalized_x_axis_disp_m)
combined_main_DTS_data_left$deseasonalized_y_axis_disp_m<- as.numeric(combined_main_DTS_data_left$deseasonalized_y_axis_disp_m)
head(combined_main_DTS_data_left)
dim(combined_main_DTS_data_left)

# Right knee (Correlation)
options(warn=-1)
X_matrix<- combined_main_DTS_data_right[, c('deseasonalized_Force_N','deseasonalized_x_axis_disp_m',
                                'deseasonalized_y_axis_disp_m')]

names(X_matrix)<- c("Force (N)", "X displacement (m)","Y displacement (m)")

PerformanceAnalytics::chart.Correlation(X_matrix, histogram=TRUE, pch=19, method="spearman")

# Left knee (Correlation)
options(warn=-1)
X_matrix<- combined_main_DTS_data_left[, c('deseasonalized_Force_N','deseasonalized_x_axis_disp_m',
                                'deseasonalized_y_axis_disp_m')]

names(X_matrix)<- c("Force (N)", "X displacement (m)","Y displacement (m)")

PerformanceAnalytics::chart.Correlation(X_matrix, histogram=TRUE, pch=19, method="spearman")

names(combined_main_DTS_data_right)


# Plotting using ggplot2
ggplot(combined_main_DTS_data_right, aes(x = Time_secs, y = deseasonalized_Force_N)) +
  geom_line(color = "blue") + 
  # geom_point(color = "red") +
 facet_wrap(~ Patient.ID) +  # Create a facet for each patient ID
  labs(title = "Detrended and Deseasonalised Force over Time (Right knee)",
       x = "Time (seconds)",
       y = "Force") +
  theme_minimal()


# Plotting using ggplot2
ggplot(combined_main_DTS_data_left, aes(x = Time_secs, y = deseasonalized_Force_N)) +
  geom_line(color = "blue") + 
  # geom_point(color = "red") +
 facet_wrap(~ Patient.ID) +  # Create a facet for each patient ID
  labs(title = "Detrended and Deseasonalised Force over Time (Left knee)",
       x = "Time (seconds)",
       y = "Force") +
  theme_minimal()




p2 <- plot_ly(subset(combined_main_DTS_data_right, Patient.ID=="1 MA R"), 
              x = ~Time_secs, y = ~deseasonalized_Force_N, type = 'scatter', mode = 'lines+markers')



# Add title and labels
p2 <- p2 %>% layout(title = "Deseasonalised Force over Time",
                  xaxis = list(title = "Time (seconds)"),
                  yaxis = list(title = "Force"))
p2

p <- plot_ly(data1, x = ~Time_secs, y = ~ Force_N, type = 'scatter', mode = 'lines+markers')



# Add title and labels
p <- p %>% layout(title = "Force over Time",
                  xaxis = list(title = "Time (seconds)"),
                  yaxis = list(title = "Force"))
p

processed_data_right<- as.data.frame(combined_main_DTS_data_right)
head(processed_data_right)

processed_data_left<- as.data.frame(combined_main_DTS_data_left)
head(processed_data_left)

names(combined_main_DTS_data_right)

library(readxl)
library(dplyr)
library(tidyr)
library(cluster)
library(factoextra)
library(ggplot2)
library(rgl)
library(officer)
library(gridExtra)
library(ggfortify)

head(processed_data_right)

# Computing BMI from Height and Weight
processed_data_right$BMI<- processed_data_right$wt/((processed_data_right$ht*.01)^2)
processed_data_left$BMI<- processed_data_left$wt/((processed_data_left$ht*.01)^2)

head(processed_data_right)

names(processed_data_right)

# Select the relevant variables
selected_columns <- c("deseasonalized_Force_N",'deseasonalized_x_axis_disp_m', 'deseasonalized_y_axis_disp_m')

# Right knee
selected_data_right <- processed_data_right%>% select(all_of(selected_columns))

# Handle missing values by filling them with the mean of the column
 selected_data_right <- selected_data_right %>% mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))



# Left knee
selected_data_left <- processed_data_left%>% select(all_of(selected_columns))

# Handle missing values by filling them with the mean of the column
 selected_data_left <- selected_data_left %>% mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

?NbClust::NbClust

# library("NbClust")

Optimal_clusters_right<- NbClust::NbClust(data =selected_data_right, diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 5, method = "kmeans")

Optimal_clusters_right

setwd("/Users/clementtwumasi/Desktop/AIRehab_cluster project/Optimal clusters")

write.csv(Optimal_clusters_right$Best.nc, "Optimal_clusters_right_Optimal.nc.csv")

Optimal_clusters_right$Best.nc

Optimal_clusters_left<- NbClust::NbClust(data =selected_data_left, diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 5, method = "kmeans")

Optimal_clusters_left

setwd("/Users/clementtwumasi/Desktop/AIRehab_cluster project/Optimal clusters")

write.csv(Optimal_clusters_left$Best.nc, "Optimal_clusters_left_Optimal.nc.csv")



# Generate synthetic time-dependent data (replace this with your actual data)
set.seed(123)
time <- seq(0, 10, by = 0.01)  # Time vector from 0 to 10 seconds with 0.01s interval
force <- abs(sin(2 * pi * 1 * time) + rnorm(length(time), sd = 0.1))   # Force with noise
x_disp <- sin(2 * pi * 0.5 * time) + rnorm(length(time), sd = 0.1) # X displacement with noise
y_disp <- sin(2 * pi * 0.75 * time) + rnorm(length(time), sd = 0.1) # Y displacement with noise

# Combine the data into a data frame
data <- data.frame(time = time, force = force, x_disp = x_disp, y_disp = y_disp)

head(data)

dim(data)

#  Define the FFT Automation Function
# that calculates the FFT for each time-series variable and extracts frequency-domain features.
compute_fft_features <- function(time_series, sampling_rate) {
  # Compute the FFT
  fft_result <- fft(time_series)
  
  # Get the length of the FFT result
  n <- length(fft_result)
  
  # Compute the corresponding frequencies
  freq <- (0:(n-1)) * (sampling_rate / n)
  
  # Compute the power spectrum (magnitude squared of FFT)
  power_spectrum <- Mod(fft_result)^2 / n
  
  # Extract meaningful frequency-domain features
  dominant_frequency <- freq[which.max(power_spectrum[1:(n/2)])] # Dominant frequency
  max_power <- max(power_spectrum[1:(n/2)])                     # Maximum power
  
  # Return a named vector of features
  return(c(dominant_frequency = dominant_frequency, max_power = max_power))
}


# Apply the FFT Function to Each Variable
# Define sampling rate (e.g., 100 Hz for 0.01s intervals)
sampling_rate <- 100  

# Apply FFT to each variable and extract features
force_features <- compute_fft_features(data$force, sampling_rate)
x_disp_features <- compute_fft_features(data$x_disp, sampling_rate)
y_disp_features <- compute_fft_features(data$y_disp, sampling_rate)

# Combine all features into a single data frame for clustering
features <- data.frame(force_features, x_disp_features, y_disp_features)
print(features)
dim(features)

# Automate and Prepare Data for Clustering
# Segment the data (for example, into 100-point windows)
window_size <- 100
feature_list <- list()

for (i in seq(1, nrow(data) - window_size, by = window_size)) {
  segment <- data[i:(i + window_size - 1), ]
  
  # Compute FFT features for each segment
  force_features <- compute_fft_features(segment$force, sampling_rate)
  x_disp_features <- compute_fft_features(segment$x_disp, sampling_rate)
  y_disp_features <- compute_fft_features(segment$y_disp, sampling_rate)
  
  # Combine features into a single row
  segment_features <- data.frame(force_features, x_disp_features, y_disp_features)
  
  # Add to list
  feature_list[[length(feature_list) + 1]] <- segment_features
}

# Combine all segments into a single data frame
feature_matrix <- do.call(rbind, feature_list)
print(feature_matrix)






# normalised data (right)
selected_data_right_norm<- selected_data_right #scale(selected_data_right,center=TRUE, scale=TRUE)

# normalised data (left)
selected_data_left_norm<- selected_data_left #scale(selected_data_left,center=TRUE, scale=TRUE)

# Based on the above methods, let's assume we found 4 to be optimal
optimal_k <- 3

# Right knee
# Perform K-means clustering with the optimal number of clusters
set.seed(100)
kmeans_result_right <- kmeans(selected_data_right_norm, centers = optimal_k, iter.max = 15, nstart = 25)


# Left knee
# Perform K-means clustering with the optimal number of clusters
set.seed(100)
kmeans_result_left <- kmeans(selected_data_left_norm, centers = optimal_k, iter.max = 15, nstart = 25)

# Add the cluster assignments to the original data
processed_data_right$Cluster_kmeans <- as.factor(kmeans_result_right$cluster)

head(processed_data_right)

# Add the cluster assignments to the original data
processed_data_right$Cluster_kmeans <- as.factor(kmeans_result_right$cluster)

head(processed_data_right)

# table(processed_data_right$Cluster_kmeans)

# Add the cluster assignments to the original data
processed_data_left$Cluster_kmeans <- as.factor(kmeans_result_left$cluster)

head(processed_data_left)

table(processed_data_right$Cluster_kmeans)

table(processed_data_left$Cluster_kmeans)

############### Right knee ############### 
# Compute the dissimilarity matrix (Right knee data)
d_right <- dist(selected_data_right_norm, method = "euclidean")

# Perform hierarchical clustering using Ward's method
hc_result_right <- hclust(d_right, method = "ward.D2")

# Plot the dendrogram
# plot(hc_result_right, main = "Hierarchical Clustering Dendrogram (right knee)", xlab = "", sub = "")
# Cut the tree into the optimal number of clusters
hc_clusters_right <- cutree(hc_result_right, k = optimal_k)
hc_clusters_factor_right <- as.factor(hc_clusters_right)

# Add the cluster assignments to the original data
processed_data_right$Cluster_hierarchical <- hc_clusters_factor_right
# head(processed_data_right)


############### Left knee ############### 
# Compute the dissimilarity matrix (Left knee data)
d_left <- dist(selected_data_left_norm, method = "euclidean")

# Perform hierarchical clustering using Ward's method
hc_result_left <- hclust(d_left, method = "ward.D2")
# Cut the tree into the optimal number of clusters
hc_clusters_left <- cutree(hc_result_left, k = optimal_k)
hc_clusters_factor_left <- as.factor(hc_clusters_left)

# Add the cluster assignments to the original data
processed_data_left$Cluster_hierarchical <- hc_clusters_factor_left
# head(processed_data_left)


table(hc_clusters_factor_left)

table(hc_clusters_factor_right)

head(processed_data_right)

# Right knee
PAM_result_right<- pam(selected_data_right_norm, k = optimal_k)
# Add the cluster assignments to the original data
processed_data_right$Cluster_PAM <- as.factor(PAM_result_right$cluster)

# Left knee
PAM_result_left<- pam(selected_data_left_norm, k = optimal_k)
# Add the cluster assignments to the original data
processed_data_left$Cluster_PAM <- as.factor(PAM_result_left$cluster)

head(processed_data_right)

###### Optimally chosen value for sample and sampsize in the clara() function (Right knee)##########
# Set up a grid of potential values for samples and sampsize
sample_values <- seq(5, 10, by = 1)      # Test from 5 to 10 samples
sampsize_values <- seq(10, 500, by = 5) # Test different sample sizes

best_score <- -Inf
best_samples <- NA
best_sampsize <- NA

# Loop through different combinations of samples and sampsize
for (s in sample_values) {
  for (ss in sampsize_values) {
    clara_fit <- clara(x = selected_data_right_norm, k = optimal_k, samples = s, sampsize = ss)
    silhouette_score <- clara_fit$silinfo$avg.width
    
    # Keep track of the best parameters
    if (silhouette_score > best_score) {
      best_score <- silhouette_score
      best_samples <- s
      best_sampsize <- ss
    }
  }
}

cat("Optimal 'samples':", best_samples, "\n")
cat("Optimal 'sampsize':", best_sampsize, "\n")
cat("Best Silhouette Score:", best_score, "\n")


########### Right knee data #####vv###
# Fit CLARA model
set.seed(123)  # Set seed for reproducibility
clara_model_right <- clara(
  x = selected_data_right_norm,             # Your data
  k = optimal_k,                # Number of clusters
  samples = 7,          # Number of random samples
  sampsize = 15,        # Size of each sample (adjust based on dataset size)
  metric = "euclidean"  # Distance metric (other options: "manhattan")
)

# View the clustering result
# print(clara_model_right)
# Add the cluster assignments to the original data
processed_data_right$Cluster_CLARA <- as.factor(clara_model_right$clustering)

###### Optimally chosen value for sample and sampsize in the clara() function (left knee)##########
# Set up a grid of potential values for samples and sampsize
sample_values <- seq(5, 10, by = 1)      # Test from 5 to 10 samples
sampsize_values <- seq(10, 500, by = 5) # Test different sample sizes

best_score <- -Inf
best_samples <- NA
best_sampsize <- NA

# Loop through different combinations of samples and sampsize
for (s in sample_values) {
  for (ss in sampsize_values) {
    clara_fit <- clara(x = selected_data_left_norm, k = optimal_k, samples = s, sampsize = ss)
    silhouette_score <- clara_fit$silinfo$avg.width
    
    # Keep track of the best parameters
    if (silhouette_score > best_score) {
      best_score <- silhouette_score
      best_samples <- s
      best_sampsize <- ss
    }
  }
}

cat("Optimal 'samples':", best_samples, "\n")
cat("Optimal 'sampsize':", best_sampsize, "\n")
cat("Best Silhouette Score:", best_score, "\n")


########### Right knee data #####vv###
# Fit CLARA model
set.seed(123)  # Set seed for reproducibility
clara_model_left <- clara(
  x = selected_data_left_norm,             # Your data
  k = optimal_k,                # Number of clusters
  samples = 6,          # Number of random samples
  sampsize = 10,        # Size of each sample (adjust based on dataset size)
  metric = "euclidean"  # Distance metric (other options: "manhattan")
)

# View the clustering result
# print(clara_model_right)
# Add the cluster assignments to the original data
processed_data_left$Cluster_CLARA <- as.factor(clara_model_left$clustering)

head(processed_data_right)

# Perform PCA
pca_result_right <- prcomp(selected_data_right_norm, scale. = F)

pca_result_left <- prcomp(selected_data_left_norm, scale. = F)

p_right<- list()
p_left<- list()

################ K-means clustering plots ############
# Define custom grey color palette
# grey_palette <-  c("grey0", "grey30", "grey")
# Define color-blind friendly palette
cb_palette <- c("#0072B2", "#E69F00", "#009E73")

# Plot PCA results with clusters using fviz_pca_ind
p_right[[1]]<- fviz_pca_ind(pca_result_right,
             geom.ind = "point", # Show points only
             # col.ind = clusters_factor, # Color by cluster
             palette = cb_palette, # Color palette
             addEllipses = TRUE, # Add confidence ellipses for each cluster
             ellipse.type = "convex", # Convex hull to frame clusters
             repel = TRUE, habillage=as.factor(kmeans_result_right$cluster),label = "none",linetype=0) + # Avoid text overlapping
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
theme(legend.position ="top")+
ggtitle("(a) K-means Clustering (right knee)")+labs(x="PC1 (99.73%)", y="PC2 (0.22%)")


# Hierarchical clustering plots
p_right[[2]]=fviz_pca_ind(pca_result_right,
             geom.ind = "point", # Show points only
             # col.ind = clusters_factor, # Color by cluster
             palette = cb_palette, # Color palette
             addEllipses = TRUE, # Add confidence ellipses for each cluster
             ellipse.type = "convex", # Convex hull to frame clusters
             repel = TRUE, habillage=hc_clusters_factor_right,label = "none",linetype=0) + # Avoid text overlapping
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
theme(legend.position ="top")+
ggtitle("(b) Hierarchical Clustering (right knee)")+labs(x="PC1 (99.73%)", y="PC2 (0.22%)")


# Partition Around Medoids Clustering plots
p_right[[3]]=fviz_pca_ind(pca_result_right,
             geom.ind = "point", # Show points only
             # col.ind = clusters_factor, # Color by cluster
             palette = cb_palette, # Color palette
             addEllipses = TRUE, # Add confidence ellipses for each cluster
             ellipse.type = "convex", # Convex hull to frame clusters
             repel = TRUE, habillage=processed_data_right$Cluster_PAM,label = "none",linetype=0) + # Avoid text overlapping
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
theme(legend.position ="top")+
ggtitle("(c) PAM Clustering (right knee)")+labs(x="PC1 (99.73%)", y="PC2 (0.22%)")



# CLARA (Clustering Large Applications) clustering
p_right[[4]]=fviz_pca_ind(pca_result_right,
             geom.ind = "point", # Show points only
             # col.ind = clusters_factor, # Color by cluster
             palette = cb_palette, # Color palette
             addEllipses = TRUE, # Add confidence ellipses for each cluster
             ellipse.type = "convex", # Convex hull to frame clusters
             repel = TRUE, habillage=processed_data_right$Cluster_CLARA,label = "none",linetype=0) + # Avoid text overlapping
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
theme(legend.position ="top")+
ggtitle("(d) CLARA Clustering (right knee)")+labs(x="PC1 (99.73%)", y="PC2 (0.22%)")




gridExtra::grid.arrange(p_right[[1]],p_right[[2]], p_right[[3]],p_right[[4]], nrow=2, ncol=2)


################ K-means clustering plots ############
# Define custom grey color palette
# grey_palette <-  c("grey0", "grey30", "grey")
# Define color-blind friendly palette
cb_palette <- c("#0072B2", "#E69F00", "#009E73")

# Plot PCA results with clusters using fviz_pca_ind
p_left[[1]]<- fviz_pca_ind(pca_result_left,
             geom.ind = "point", # Show points only
             # col.ind = clusters_factor, # Color by cluster
             palette = cb_palette, # Color palette
             addEllipses = TRUE, # Add confidence ellipses for each cluster
             ellipse.type = "convex", # Convex hull to frame clusters
             repel = TRUE, habillage=as.factor(kmeans_result_left$cluster),label = "none",linetype=0) + # Avoid text overlapping
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
theme(legend.position ="top")+
ggtitle("(a) K-means Clustering (left knee)")+labs(x="PC1 (99.74%)", y="PC2 (0.23%)")


# Hierarchical clustering plots
p_left[[2]]=fviz_pca_ind(pca_result_left,
             geom.ind = "point", # Show points only
             # col.ind = clusters_factor, # Color by cluster
             palette = cb_palette, # Color palette
             addEllipses = TRUE, # Add confidence ellipses for each cluster
             ellipse.type = "convex", # Convex hull to frame clusters
             repel = TRUE, habillage=hc_clusters_factor_left,label = "none",linetype=0) + # Avoid text overlapping
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
theme(legend.position ="top")+
ggtitle("(b) Hierarchical Clustering (left knee)")+labs(x="PC1 (99.74%)", y="PC2 (0.23%)")


# Partition Around Medoids Clustering plots
p_left[[3]]=fviz_pca_ind(pca_result_left,
             geom.ind = "point", # Show points only
             # col.ind = clusters_factor, # Color by cluster
             palette = cb_palette, # Color palette
             addEllipses = TRUE, # Add confidence ellipses for each cluster
             ellipse.type = "convex", # Convex hull to frame clusters
             repel = TRUE, habillage=processed_data_left$Cluster_PAM,label = "none",linetype=0) + # Avoid text overlapping
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
theme(legend.position ="top")+
ggtitle("(c) PAM Clustering (left knee)")+labs(x="PC1 (99.74%)", y="PC2 (0.23%)")



# CLARA (Clustering Large Applications) clustering
p_left[[4]]=fviz_pca_ind(pca_result_left,
             geom.ind = "point", # Show points only
             # col.ind = clusters_factor, # Color by cluster
             palette = cb_palette, # Color palette
             addEllipses = TRUE, # Add confidence ellipses for each cluster
             ellipse.type = "convex", # Convex hull to frame clusters
             repel = TRUE, habillage=processed_data_left$Cluster_CLARA,label = "none",linetype=0) + # Avoid text overlapping
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
theme(legend.position ="top")+
ggtitle("(d) CLARA Clustering (left knee)")+labs(x="PC1 (99.74%)", y="PC2 (0.23%)")




gridExtra::grid.arrange(p_left[[1]],p_left[[2]], p_left[[3]],p_left[[4]], nrow=2, ncol=2)




# A function to summarise and plot Silhouette Score/Width distribution across clustering methods:

Silhouette_summary<- function(kmeans=kmeans_result_right, 
                              pam=PAM_result_right, hierarch=hc_clusters_right, 
                              clara=clara_model_right, distance_matrix=d_right, knee_type="Right knee"){

kmeans_silhouette <- as.data.frame(silhouette(kmeans$cluster, distance_matrix))
kmeans_avg_silhouette <- median(kmeans_silhouette[, 3])  # Average silhouette width

# Hierarchical Silhouette Score
hc_silhouette <- as.data.frame(silhouette(hierarch,  distance_matrix))
hc_avg_silhouette <- median(hc_silhouette[, 3])

# PAM Silhouette Score
pam_silhouette <- as.data.frame(silhouette(pam))
pam_avg_silhouette <- median(pam_silhouette$sil_width)  # Silhouette width directly from PAM object

# CLARA Silhouette Score
clara_silhouette <- as.data.frame(silhouette(clara$clustering,  distance_matrix))
clara_avg_silhouette <- median(clara_silhouette[, 3])


# Print Silhouette Scores
cat(paste("Overall Median Silhouette Scores(", knee_type, "):"),"\n")
cat("CLARA:", clara_avg_silhouette, "\n")
cat("Hierarchical:", hc_avg_silhouette, "\n")
cat("K-means:", kmeans_avg_silhouette, "\n")
cat("PAM:", pam_avg_silhouette, "\n")




# Comparative Boxplot of Silhouette Scores Across Clustering Methods
# Create a data frame with Silhouette Scores for each method
silhouette_data <- data.frame(
  Silhouette_Width = c(kmeans_silhouette[, 3], hc_silhouette[, 3], pam_silhouette[, 3], clara_silhouette[, 3]),
  Cluster = factor(c(kmeans_silhouette[, 1], hc_silhouette[, 1], pam_silhouette[, 1], clara_silhouette[, 1])),
  Method = rep(c("K-means", "Hierarchical", "PAM", "CLARA"), 
               times = c(length(kmeans_silhouette[, 3]), length(hc_silhouette[, 3]), 
                         length(pam_silhouette[, 3]), length(clara_silhouette[, 3])))
)


# Calculate average Silhouette Scores for each method
avg_sil_scores <- silhouette_data %>%
  group_by(Method, Cluster) %>%
  summarize(Average_Silhouette = median(Silhouette_Width)) %>%
  ungroup()




# Create the boxplot using ggplot2
si=ggplot(silhouette_data, aes(x = Method, y = Silhouette_Width, fill = Cluster)) +
  geom_boxplot() +
  scale_fill_manual(values = c('#0072B2', '#E69F00', '#009E73')) +  # Set custom palette colors
  labs(title = paste(knee_type, "data clustering "),
       x = "Clustering Methods",
       y = "Silhouette Score") +
  theme_minimal() +
  theme(legend.position = "top")+
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
 geom_vline(xintercept = c(1.5, 2.5, 3.5), linetype = "dashed")+   # Add vertical dashed lines
geom_text(data = avg_sil_scores, aes(x = Method, y = 1.1, label = round(Average_Silhouette, 3)),
                   position = position_dodge(width = 0.8), size = 3.5, color = "black")+
geom_text(data = avg_sil_scores, aes(x = Method, y = 1.35, label = paste0("Median")),
                   position = position_dodge(width = 0.8), size = 3.5, color = "black")+
geom_text(data = avg_sil_scores, aes(x = Method, y = 1.25, label = paste0("Score:")),
                   position = position_dodge(width = 0.8), size = 3.5, color = "black")


return(list(sil_boxplot=si, avg_sil_scores=avg_sil_scores ))
    
    }



 

# Right knee data
Silhouette_summary_results_right<-  Silhouette_summary(kmeans=kmeans_result_right, 
                              pam=PAM_result_right, hierarch=hc_clusters_right, 
                              clara=clara_model_right, distance_matrix=d_right, knee_type="(a) Right knee")


# grid.arrange(si_right, nrow=2)

# Left knee data
Silhouette_summary_results_left<-  Silhouette_summary(kmeans=kmeans_result_left, 
                              pam=PAM_result_left, hierarch=hc_clusters_left, 
                              clara=clara_model_left, distance_matrix=d_left, knee_type="(b) Left knee")



# Based on the 2-stage data pre-processing
grid.arrange(Silhouette_summary_results_right$sil_boxplot+labs(x="")+ # First plot
  annotate("text", x=1, y=1.5, label= "Overall median= 0.602",fontface = "bold")+
  annotate("text", x=2, y=1.5, label= "Overall median= 0.637",fontface = "bold")+
  annotate("text", x=3, y=1.5, label= "Overall median= 0.612",fontface = "bold")+
 annotate("text", x=4, y=1.5, label= "Overall median= 0.591",fontface = "bold"),
Silhouette_summary_results_left$sil_boxplot+theme(legend.position = "none")+ # Second plot
  annotate("text", x=1, y=1.5, label= "Overall median= 0.598",fontface = "bold")+
  annotate("text", x=2, y=1.5, label= "Overall median= 0.517",fontface = "bold")+
  annotate("text", x=3, y=1.5, label= "Overall median= 0.588",fontface = "bold")+
 annotate("text", x=4, y=1.5, label= "Overall median= 0.569",fontface = "bold")
            )

# autoplot(kmeans_result_right, data=processed_data_right, frame = TRUE)+
 # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
# panel.background = element_blank(), axis.line = element_line(colour = "black"))+ theme(legend.position ="top")+
# labs(title = "K-means Clustering (right knee)")

# Visualize the clusters using fviz_cluster from factoextra
p1=fviz_cluster(kmeans_result_left, data = selected_data_left, ellipse.type = "norm", geom = "point") +
  labs(title = "K-means Clustering (left knee)", x = "Principal Component 1", y = "Principal Component 2")+
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+ theme(legend.position ="top")

# p1

# kmeans_result
# autoplot(kmeans_result_left, data=processed_data_left, frame = TRUE)+
 # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
# panel.background = element_blank(), axis.line = element_line(colour = "black"))+ theme(legend.position ="top")+
# labs(title = "K-means Clustering (left knee)")

# kmeans_result
# options(repr.plot.width=8, repr.plot.height=8,repr.plot.res=300) #Setting plot size

options(repr.plot.width=8, repr.plot.height=8,repr.plot.res=300) #Setting plot size
p1=autoplot(kmeans_result_right, data=processed_data_right, frame = TRUE)+
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+ theme(legend.position ="top")+
labs(title = "(a) K-means Clustering (right knee)")

p2= autoplot(kmeans_result_left, data=processed_data_left, frame = TRUE)+
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+ theme(legend.position ="top")+
labs(title = "(b) K-means Clustering (left knee)")


 # grid.arrange(p1,p2, nrow=1)

# Define color-blind friendly palette
cb_palette <- c("#0072B2", "#E69F00", "#009E73")


Visual_3D_right<- plot_ly(x=processed_data_right$deseasonalized_x_axis_disp_m, y=processed_data_right$deseasonalized_y_axis_disp_m, 
        z=processed_data_right$deseasonalized_Force_N,
        type="scatter3d", mode="markers", color=processed_data_right$Cluster_hierarchical,
                         colors = cb_palette)%>% 
layout(scene = list(xaxis = list(title = 'X'),
                                   yaxis = list(title = 'Y'),
                                   zaxis = list(title = 'Force')))

Visual_3D_right

# We can indicate the axis and the rotation velocity
# play3d( spin3d( axis = c(0, 0, 1), rpm = 20), duration = 10 )

# 3D scatter plot with density
# Define colors for each cluster
colors <- cb_palette # Colors for the three clusters
# Map cluster assignments to colors
point_colors <- colors[processed_data_right$Cluster_hierarchical]

# 3D scatter plot with density
scatterplot3d::scatterplot3d(x=processed_data_right$deseasonalized_x_axis_disp_m, 
                             y=processed_data_right$deseasonalized_y_axis_disp_m, 
        z=processed_data_right$deseasonalized_Force_N, color =point_colors, pch = 19,
     xlab="Adjusted X-displacement (meters)", ylab="Adjusted Y-displacement (meters)", zlab="Adjusted Force (Newtons) at right knee",
                            type="p")

legend(x=-.8, y=6, legend = paste("Cluster", 1:3), col = colors, pch = 19, bt="n",ncol = 3)


head(processed_data_right)

library(magick)
library(reticulate)

# Use the specific Conda environment
use_condaenv("r-reticulate", required = TRUE)

# Verify that the plotly module is installed in Python
py_run_string("import plotly")

setwd("/Users/clementtwumasi/Desktop/AIRehab_cluster project/3D Scatter Plot_clusters")
# Export as HTML:

htmlwidgets::saveWidget(as_widget(Visual_3D_right), "3d_scatter_plot_RightKnee_Kmeans.html")


# Use kaleido to save the plot as a high-resolution png
plotly::save_image(Visual_3D_right, "3d_scatter_plot_RightKnee_Kmeans.png", format = "png", width = 1200, height = 800, scale = 2)

setwd("/Users/clementtwumasi/Desktop/AIRehab_cluster project/3D Scatter Plot_clusters/GIF Right Knee_Kmeans")
# Create a function to generate plotly frames
generate_frame <- function(frame_number) {
  angle <- frame_number * 4
  plot <- plot_ly(x=processed_data_right$deseasonalized_x_axis_disp_m, y=processed_data_right$deseasonalized_y_axis_disp_m, 
        z=processed_data_right$deseasonalized_Force_N,
        type="scatter3d", mode="markers", color=processed_data_right$Cluster_kmeans,
                         colors = c( "red","green","blue"))%>% 
                layout(scene = list(camera = list(eye = list(x = cos(angle*pi/180)*2, y = sin(angle*pi/180)*2, z = 0.5)),
                                    xaxis = list(title = 'X'),
                                   yaxis = list(title = 'Y'),
                                   zaxis = list(title = 'Force')))
  return(plot)
}

# Save frames as PNG files
frames <- lapply(1:90, function(i) {
  plot <- generate_frame(i)
  filename <- sprintf("frame_%03d.png", i)
  plotly::save_image(plot, filename, format = "png", width = 1200, height = 800, scale = 2)
  filename
} )


# Generate frames
frames <- lapply(1:90, function(i) {
  generate_frame(i)
})

# Combine PNG files into an animated GIF
frames <- image_read(paste0("frame_", sprintf("%03d", 1:90), ".png"))
animated_gif_right  <- image_animate(frames, fps = 10)

# Save the GIF
image_write(animated_gif_right, "3d_scatter_plot_RightKnee_Kmeans.gif")


animated_gif_right 

# library(av)
setwd("/Users/clementtwumasi/Desktop/AIRehab_cluster project/3D Scatter Plot_clusters/GIF Right Knee_Kmeans")
# Convert the animated GIF to a movie file
av::av_encode_video("3d_scatter_plot_RightKnee_Kmeans.gif", output = "3d_scatter_plot_RightKnee_Kmeans_MP4.mp4", framerate = 8)


names(processed_data_left)

Visual_3D_left<- plot_ly(x=processed_data_left$deseasonalized_x_axis_disp_m, y=processed_data_left$deseasonalized_y_axis_disp_m, 
        z=processed_data_left$deseasonalized_Force_N,
        type="scatter3d", mode="markers", color=processed_data_left$Cluster_CLARA,
                        colors = cb_palette)%>% 
layout(scene = list(xaxis = list(title = 'X'),
                                   yaxis = list(title = 'Y'),
                                   zaxis = list(title = 'Force')))


Visual_3D_left

# 3D scatter plot with density
# Define colors for each cluster
colors <- cb_palette # Colors for the three clusters
# Map cluster assignments to colors
point_colors <- colors[processed_data_left$Cluster_CLARA]

scatterplot3d::scatterplot3d(x=processed_data_left$deseasonalized_x_axis_disp_m, y=processed_data_left$deseasonalized_y_axis_disp_m, 
        z=processed_data_left$deseasonalized_Force_N, color = point_colors, pch = 19,
                           xlab="Adjusted X-displacement (meters)", ylab="Adjusted Y-displacement (meters)",
                             zlab="Adjusted Force (Newtons) at left knee",
                            type="p")

legend(x=-.8, y=6, legend = paste("Cluster", 1:3), col = colors, pch = 19, bt="n",ncol = 3)


names(processed_data_right)

# 3D scatter plot with density
options(repr.plot.width=8, repr.plot.height=8,repr.plot.res=300) #Setting plot size
par(mfrow=c(2,1), mar=c(4,4,1,1))
# Define colors for each cluster
colors <- cb_palette # Colors for the three clusters
# Map cluster assignments to colors
point_colors <- colors[processed_data_right$Cluster_hierarchical]

# 3D scatter plot with density
p1=scatterplot3d::scatterplot3d(x=processed_data_right$deseasonalized_x_axis_disp_m, y=processed_data_right$deseasonalized_y_axis_disp_m, 
        z=processed_data_right$deseasonalized_Force_N, color =point_colors, pch = 19,
            xlab="Adjusted X-displacement (meters)", ylab="Adjusted Y-displacement (meters)", zlab="Adjusted Force (Newtons)",
                            type="p",main="(a) Right knee")

legend(x=-.8, y=7, legend = paste("Cluster", 1:3), col = colors, pch = 19, bt="n",ncol = 3)


# 3D scatter plot with density
# Define colors for each cluster
colors <- cb_palette   # Colors for the three clusters
# Map cluster assignments to colors
point_colors <- colors[processed_data_left$Cluster_CLARA]

scatterplot3d::scatterplot3d(x=processed_data_left$deseasonalized_x_axis_disp_m, y=processed_data_left$deseasonalized_y_axis_disp_m, 
        z=processed_data_left$deseasonalized_Force_N, color = point_colors, pch = 19,
    xlab="Adjusted X-displacement (meters)", ylab="Adjusted Y-displacement (meters)", zlab="Adjusted Force (Newtons)",
                            type="p",main="(b) Left knee")

legend(x=-.8, y=7, legend = paste("Cluster", 1:3), col = colors, pch = 19, bt="n",ncol = 3)







setwd("/Users/clementtwumasi/Desktop/AIRehab_cluster project/3D Scatter Plot_clusters")
# Export as HTML:

htmlwidgets::saveWidget(as_widget(Visual_3D_left), "3d_scatter_plot_LeftKnee_Kmeans.html")


# Use kaleido to save the plot as a high-resolution png
plotly::save_image(Visual_3D_left, "3d_scatter_plot_LeftKnee_Kmeans.png", format = "png", width = 1200, height = 800, scale = 2)

setwd("/Users/clementtwumasi/Desktop/AIRehab_cluster project/3D Scatter Plot_clusters/GIF Left Knee_Kmeans")
# Create a function to generate plotly frames
generate_frame <- function(frame_number) {
  angle <- frame_number * 4
  plot <- plot_ly(x=processed_data_left$deseasonalized_x_axis_disp_m, y=processed_data_left$deseasonalized_y_axis_disp_m, 
        z=processed_data_left$deseasonalized_Force_N,
        type="scatter3d", mode="markers", color=processed_data_left$Cluster_kmeans,
                         colors = c( "red","green","blue"))%>% 
                layout(scene = list(camera = list(eye = list(x = cos(angle*pi/180)*2, y = sin(angle*pi/180)*2, z = 0.5)),
                                    xaxis = list(title = 'X'),
                                   yaxis = list(title = 'Y'),
                                   zaxis = list(title = 'Force')))
  return(plot)
}

# Save frames as PNG files
frames <- lapply(1:90, function(i) {
  plot <- generate_frame(i)
  filename <- sprintf("frame_%03d.png", i)
  plotly::save_image(plot, filename, format = "png", width = 1200, height = 800, scale = 2)
  filename
} )

# Generate frames
frames <- lapply(1:90, function(i) {
  generate_frame(i)
})

# Combine PNG files into an animated GIF
frames <- image_read(paste0("frame_", sprintf("%03d", 1:90), ".png"))
animated_gif_left <- image_animate(frames, fps = 10)

# Save the GIF
image_write(animated_gif_left, "3d_scatter_plot_LeftKnee_Kmeans.gif")


animated_gif_left

# library(av)

# Convert the animated GIF to a movie file
av::av_encode_video("3d_scatter_plot_LeftKnee_Kmeans.gif", output = "3d_scatter_plot_LeftKnee_Kmeans_MP4.mp4", framerate = 8)


head(processed_data_right)

# Non-parametric Multivariate normality 
library(energy)

# Perform the Henze-Zirkler test for multivariate normality (Right knee data)
hz_test_right <- mvnorm.etest(processed_data_right[, c(12,13,14)], R = 100)
print(hz_test_right)

# Perform the Henze-Zirkler test for multivariate normality
hz_test_left <- mvnorm.etest(processed_data_left[, c(12,13,14)], R = 100)
print(hz_test_left)

#Install package called Matrix using install.packages("Matrix")
library("Matrix") #load package Matrix to Construct a Block Diagonal Matrix

# A function to perform Multivariate Kruskal Wallis Test (dubbed multkw)
multkw<- function(group,y,simplify=FALSE){
  ### sort data by group ###
  o<-order(group)
  group<-group[o]
  y<-as.matrix(y[o,])
  n<-length(group)
  k<-dim(y)[2]   #k=p
  
  if (dim(y)[1] != n)
    return("number of observations not equal to length of group")
  groupls<-unique(group)
  g<-length(groupls) #number of groups #
  groupind<-sapply(groupls,"==",group) #group indicator#
  ni<-colSums(groupind) #num of subj of each group #
  r<-apply(y,2,rank) #corresponding rank variable#
  
  ### calculation of statistic ###
  r.ik<-t(groupind)%*%r*(1/ni)  #gxp, mean rank of kth variate in ith group#
  m<- (n+1)/2 #expected value of rik#
  u.ik<-t(r.ik-m)
  U<-as.vector(u.ik)
  V<-1/(n-1)*t(r-m)%*%(r-m) #pooled within-group cov matrix
  Vstar<-Matrix::bdiag(lapply(1/ni,"*",V))
  W2<-as.numeric(t(U)%*%solve(Vstar)%*%U)
  
  ### return stat and p-value ###
  returnlist<-data.frame(statistic=W2,d.f.=k*(g-1),
                         p.value=pchisq(W2,k*(g-1),lower.tail=F))
  
  if (simplify==TRUE) return (W2)
  else return (returnlist)
  
}

# Right knee datasets
Multivariate_KW_result_right<- multkw(group=processed_data_right$Cluster_hierarchical,
                    y=processed_data_right[, c(12,13,14)], simplify=FALSE)


Multivariate_KW_result_right

# Univariate Kruskal Wallis (Right data)
cat("1. Force (N) data:", "\n")
kruskal.test(deseasonalized_Force_N~ Cluster_hierarchical, data = processed_data_right)

cat("2. X displacement data:", "\n")
kruskal.test(deseasonalized_x_axis_disp_m~ Cluster_hierarchical, data = processed_data_right)


cat("3. Y displacement data:", "\n")
kruskal.test(deseasonalized_y_axis_disp_m~ Cluster_hierarchical, data = processed_data_right)



library("FSA")

head(processed_data_right)

# Function to perform Dunn's test and return adjusted p-values (based on Hierarchical clustering)

cat("1. Force (N) data:", "\n")
#perform Dunn's Test with Bonferroni correction for p-values
dunn_test_Force_right<- dunnTest(deseasonalized_Force_N ~ Cluster_hierarchical,
         data=processed_data_right,
         method="bonferroni")

dunn_test_Force_right


cat("2. X displacement data:", "\n")
#perform Dunn's Test with Bonferroni correction for p-values
dunn_test_X_right<- dunnTest(deseasonalized_x_axis_disp_m ~ Cluster_hierarchical,
         data=processed_data_right,
         method="bonferroni")

dunn_test_X_right


cat("3. Y displacement data:", "\n")
#perform Dunn's Test with Bonferroni correction for p-values
dunn_test_Y_right<- dunnTest(deseasonalized_y_axis_disp_m ~ Cluster_hierarchical,
         data=processed_data_right,
         method="bonferroni")

dunn_test_Y_right

# Right knee datasets
Multivariate_KW_result_left<- multkw(group=processed_data_left$Cluster_CLARA,
                    y=processed_data_left[, c(12,13,14)], simplify=FALSE)


Multivariate_KW_result_left

# Univariate Kruskal Wallis (Left data)
cat("1. Force (N) data:", "\n")
kruskal.test(deseasonalized_Force_N~ Cluster_CLARA, data = processed_data_left)

cat("2. X displacement data:", "\n")
kruskal.test(deseasonalized_x_axis_disp_m~ Cluster_CLARA, data = processed_data_left)


cat("3. Y displacement data:", "\n")
kruskal.test(deseasonalized_y_axis_disp_m~ Cluster_CLARA, data = processed_data_left)



# Function to perform Dunn's test and return adjusted p-values

cat("1. Force (N) data:", "\n")
#perform Dunn's Test with Bonferroni correction for p-values
dunn_test_Force_left<- dunnTest(deseasonalized_Force_N ~ Cluster_CLARA,
         data=processed_data_left,
         method="bonferroni")

dunn_test_Force_left


cat("2. X displacement data:", "\n")
#perform Dunn's Test with Bonferroni correction for p-values
dunn_test_X_left<- dunnTest(deseasonalized_x_axis_disp_m ~ Cluster_CLARA,
         data=processed_data_left,
         method="bonferroni")

dunn_test_X_left


cat("3. Y displacement data:", "\n")
#perform Dunn's Test with Bonferroni correction for p-values
dunn_test_Y_left<- dunnTest(deseasonalized_y_axis_disp_m ~ Cluster_CLARA,
         data=processed_data_left,
         method="bonferroni")

dunn_test_Y_left

names(processed_data_right)

# Visualize: Specify the comparisons you want
library("ggpubr")
my_comparisons <- list( c("1", "2"),
                       c("1", "3"),c("2", "3"))

cb_palette <- c("#0072B2", "#E69F00", "#009E73")
my_cols<-  cb_palette


#####  Right knee ####       
bxp1<- ggboxplot(processed_data_right, x = "Cluster_hierarchical", y = "deseasonalized_Force_N",
          fill = "Cluster_hierarchical")+scale_fill_manual(values=my_cols) #you can change "fill" to "color"

bxp1<- bxp1+stat_compare_means(comparisons = my_comparisons, hide.ns = F,
    label = c("p.signif"),palette = c('red'),label.x.npc ='center',#ref.group =".all."
  label.y.npc ='top')+ # Add pairwise comparisons p-value
  stat_compare_means(method = "kruskal.test", label.y = 30,label.x=1,size = 3)+     # Add global p-value
 theme(text = element_text(size = 10))+  
theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5))+
  theme(axis.title.x=element_blank())+ggtitle("(a) Right knee")

bxp1<- bxp1+labs(x="Identified clusters",y="Adjusted Force (Newtons)",fill = "Clusters:")+
geom_dotplot(binaxis = "y", stackdir = "center", binwidth = .01)


bxp2<- ggboxplot(processed_data_right, x = "Cluster_hierarchical", y = "deseasonalized_x_axis_disp_m",
          fill = "Cluster_hierarchical")+scale_fill_manual(values=my_cols) #you can change "fill" to "color"

bxp2<- bxp2+stat_compare_means(comparisons = my_comparisons, hide.ns = F,
    label = c("p.signif"),palette = c('red'),label.x.npc ='center',#ref.group =".all."
  label.y.npc ='top')+ # Add pairwise comparisons p-value
  stat_compare_means(method = "kruskal.test", label.y = 1.5,label.x=1,size = 3)+     # Add global p-value
 theme(text = element_text(size = 10))+  
theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5))+ggtitle("(b) Right knee")

bxp2<- bxp2+labs(x="Identified clusters",y="Adjusted X-displacement (meters)",fill = "Clusters:")+
geom_dotplot(binaxis = "y", stackdir = "center", binwidth = .001)+
  theme(axis.title.x=element_blank())


bxp3<- ggboxplot(processed_data_right, x = "Cluster_hierarchical", y = "deseasonalized_y_axis_disp_m",
          fill = "Cluster_hierarchical")+scale_fill_manual(values=my_cols) #you can change "fill" to "color"

bxp3<- bxp3+stat_compare_means(comparisons = my_comparisons, hide.ns = F,
    label = c("p.signif"),palette = c('red'),label.x.npc ='center',#ref.group =".all."
  label.y.npc ='top')+ # Add pairwise comparisons p-value
  stat_compare_means(method = "kruskal.test", label.y = 1,label.x=1,size = 3)+     # Add global p-value
 theme(text = element_text(size = 10))+  
  theme(axis.title.x=element_blank())+
theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5))

bxp3<- bxp3+labs(x="Identified clusters",y="Adjusted Y-displacement (meters)",fill = "Clusters:")+
geom_dotplot(binaxis = "y", stackdir = "center", binwidth = .000000001)+ggtitle("(c) Right knee")


#####  Left knee ####      
bxp4<- ggboxplot(processed_data_left, x = "Cluster_CLARA", y = "deseasonalized_Force_N",
          fill = "Cluster_CLARA")+scale_fill_manual(values=my_cols) #you can change "fill" to "color"

bxp4<- bxp4+stat_compare_means(comparisons = my_comparisons, hide.ns = F,
    label = c("p.signif"),palette = c('red'),label.x.npc ='center',#ref.group =".all."
  label.y.npc ='top')+ # Add pairwise comparisons p-value
  stat_compare_means(method = "kruskal.test", label.y = 30,label.x=1,size = 3)+     # Add global p-value
 theme(text = element_text(size = 10)) + 
theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5))

bxp4<- bxp4+labs(x="Identified clusters",y="Adjusted Force (Newtons)",fill = "Clusters:")+
geom_dotplot(binaxis = "y", stackdir = "center", binwidth = .01)+
  theme(legend.position = "none")+ggtitle("(d) Left knee")


bxp5<- ggboxplot(processed_data_left, x = "Cluster_CLARA", y = "deseasonalized_x_axis_disp_m",
          fill = "Cluster_CLARA")+scale_fill_manual(values=my_cols)+ggtitle("(e) Left knee")#you can change "fill" to "color"

bxp5<- bxp5+stat_compare_means(comparisons = my_comparisons, hide.ns = F,
    label = c("p.signif"),palette = c('red'),label.x.npc ='center',#ref.group =".all."
  label.y.npc ='top')+ # Add pairwise comparisons p-value
  stat_compare_means(method = "kruskal.test", label.y = 1.5,label.x=1,size = 3)+     # Add global p-value
 theme(text = element_text(size = 10))+  
theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5))

bxp5<- bxp5+labs(x="Identified clusters",y="Adjusted X-displacement (meters)",fill = "Clusters:")+
geom_dotplot(binaxis = "y", stackdir = "center", binwidth = .001)+
  theme(legend.position = "none")


bxp6<- ggboxplot(processed_data_left, x = "Cluster_CLARA", y = "deseasonalized_y_axis_disp_m",
          fill = "Cluster_CLARA")+scale_fill_manual(values=my_cols) #you can change "fill" to "color"

bxp6<- bxp6+stat_compare_means(comparisons = my_comparisons, hide.ns = F,
    label = c("p.signif"),palette = c('red'),label.x.npc ='center',#ref.group =".all."
  label.y.npc ='top')+ # Add pairwise comparisons p-value
  stat_compare_means(method = "kruskal.test", label.y = 1,label.x=1,size = 3)+     # Add global p-value
 theme(text = element_text(size = 10))+  
theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5))

bxp6<- bxp6+labs(x="Identified clusters",y="Adjusted Y-displacement (meters)",fill = "Clusters:")+
geom_dotplot(binaxis = "y", stackdir = "center", binwidth = .000000001)+
  theme(legend.position = "none")+ggtitle("(f) Left knee")



gridExtra::grid.arrange(bxp1,bxp2, bxp3, 
                       bxp4,bxp5,bxp6,
                        ncol=3, nrow=2)





# Visualize: Specify the comparisons you want
library("ggpubr")
my_comparisons <- list( c("1", "2"),
                       c("1", "3"),c("2", "3"))

my_cols<-  c( "red","green","blue")


#####  Right knee ####       
bxp1<- ggboxplot(processed_data_right, x = "Cluster_kmeans", y = "deseasonalized_Force_N",
          fill = "Cluster_kmeans")+scale_fill_manual(values=my_cols) #you can change "fill" to "color"

bxp1<- bxp1+stat_compare_means(comparisons = my_comparisons, hide.ns = F,
    label = c("p.signif"),palette = c('red'),label.x.npc ='center',#ref.group =".all."
  label.y.npc ='top')+ # Add pairwise comparisons p-value
  stat_compare_means(method = "kruskal.test", label.y = 30,label.x=1,size = 3)+     # Add global p-value
 theme(text = element_text(size = 10))+  
theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5))+
  theme(axis.title.x=element_blank())

bxp1<- bxp1+labs(x="K-means identified clusters",y="Force (N) at right knee",fill = "Clusters:")+
geom_dotplot(binaxis = "y", stackdir = "center", binwidth = .01)


bxp2<- ggboxplot(processed_data_right, x = "Cluster_kmeans", y = "deseasonalized_x_axis_disp_m",
          fill = "Cluster_kmeans")+scale_fill_manual(values=my_cols) #you can change "fill" to "color"

bxp2<- bxp2+stat_compare_means(comparisons = my_comparisons, hide.ns = F,
    label = c("p.signif"),palette = c('red'),label.x.npc ='center',#ref.group =".all."
  label.y.npc ='top')+ # Add pairwise comparisons p-value
  stat_compare_means(method = "kruskal.test", label.y = 1.5,label.x=1,size = 3)+     # Add global p-value
 theme(text = element_text(size = 10))+  
theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5))

bxp2<- bxp2+labs(x="K-means identified clusters",y="X-displacement (m) at right knee",fill = "Clusters:")+
geom_dotplot(binaxis = "y", stackdir = "center", binwidth = .001)+
  theme(axis.title.x=element_blank())


bxp3<- ggboxplot(processed_data_right, x = "Cluster_kmeans", y = "deseasonalized_y_axis_disp_m",
          fill = "Cluster_kmeans")+scale_fill_manual(values=my_cols) #you can change "fill" to "color"

bxp3<- bxp3+stat_compare_means(comparisons = my_comparisons, hide.ns = F,
    label = c("p.signif"),palette = c('red'),label.x.npc ='center',#ref.group =".all."
  label.y.npc ='top')+ # Add pairwise comparisons p-value
  stat_compare_means(method = "kruskal.test", label.y = 1,label.x=1,size = 3)+     # Add global p-value
 theme(text = element_text(size = 10))+  
  theme(axis.title.x=element_blank())+
theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5))

bxp3<- bxp3+labs(x="K-means identified clusters",y="Y-displacement (m) at right knee",fill = "Clusters:")+
geom_dotplot(binaxis = "y", stackdir = "center", binwidth = .000000001)


#####  Left knee ####      
bxp4<- ggboxplot(processed_data_left, x = "Cluster_kmeans", y = "deseasonalized_Force_N",
          fill = "Cluster_kmeans")+scale_fill_manual(values=my_cols) #you can change "fill" to "color"

bxp4<- bxp4+stat_compare_means(comparisons = my_comparisons, hide.ns = F,
    label = c("p.signif"),palette = c('red'),label.x.npc ='center',#ref.group =".all."
  label.y.npc ='top')+ # Add pairwise comparisons p-value
  stat_compare_means(method = "kruskal.test", label.y = 30,label.x=1,size = 3)+     # Add global p-value
 theme(text = element_text(size = 10)) + 
theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5))

bxp4<- bxp4+labs(x="K-means identified clusters",y="Force (N) at left knee",fill = "Clusters:")+
geom_dotplot(binaxis = "y", stackdir = "center", binwidth = .01)+
  theme(legend.position = "none")


bxp5<- ggboxplot(processed_data_left, x = "Cluster_kmeans", y = "deseasonalized_x_axis_disp_m",
          fill = "Cluster_kmeans")+scale_fill_manual(values=my_cols) #you can change "fill" to "color"

bxp5<- bxp5+stat_compare_means(comparisons = my_comparisons, hide.ns = F,
    label = c("p.signif"),palette = c('red'),label.x.npc ='center',#ref.group =".all."
  label.y.npc ='top')+ # Add pairwise comparisons p-value
  stat_compare_means(method = "kruskal.test", label.y = 1.5,label.x=1,size = 3)+     # Add global p-value
 theme(text = element_text(size = 10))+  
theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5))

bxp5<- bxp5+labs(x="K-means identified clusters",y="X-displacement (m) at left knee",fill = "Clusters:")+
geom_dotplot(binaxis = "y", stackdir = "center", binwidth = .001)+
  theme(legend.position = "none")


bxp6<- ggboxplot(processed_data_left, x = "Cluster_kmeans", y = "deseasonalized_y_axis_disp_m",
          fill = "Cluster_kmeans")+scale_fill_manual(values=my_cols) #you can change "fill" to "color"

bxp6<- bxp6+stat_compare_means(comparisons = my_comparisons, hide.ns = F,
    label = c("p.signif"),palette = c('red'),label.x.npc ='center',#ref.group =".all."
  label.y.npc ='top')+ # Add pairwise comparisons p-value
  stat_compare_means(method = "kruskal.test", label.y = 1,label.x=1,size = 3)+     # Add global p-value
 theme(text = element_text(size = 10))+  
theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5))

bxp6<- bxp6+labs(x="K-means identified clusters",y="Y-displacement (m) at left knee",fill = "Clusters:")+
geom_dotplot(binaxis = "y", stackdir = "center", binwidth = .000000001)+
  theme(legend.position = "none")



 # gridExtra::grid.arrange(bxp1,bxp2, bxp3, 
                        # bxp4,bxp5,bxp6,
                         # ncol=3, nrow=2)





setwd("/Users/clementtwumasi/Desktop/AIRehab_cluster project")

# saving/exporting the combined transformed data for left and right knees

write.csv(processed_data_left, "processed_data_left_32patients.csv")

write.csv(processed_data_right, "processed_data_right_32patients.csv")

setwd("/Users/clementtwumasi/Desktop/AIRehab_cluster project")
# left knee
processed_data_left_knee<- read.csv("processed_data_left_32patients.csv")
processed_data_left_knee<- processed_data_left_knee[, -1]
processed_data_left_knee$Cluster_CLARA<- as.factor(processed_data_left_knee$Cluster_CLARA)
levels(processed_data_left_knee$Cluster_CLARA)<- c("Cluster 1","Cluster 2","Cluster 3")
processed_data_left_knee$gender<- factor(processed_data_left_knee$gender,levels=c(0,1), labels=c("Female", "Male"))

# right knee
processed_data_right_knee<- read.csv("processed_data_right_32patients.csv")
processed_data_right_knee<- processed_data_right_knee[, -1]
processed_data_right_knee$Cluster_hierarchical<- as.factor(processed_data_right_knee$Cluster_hierarchical)
levels(processed_data_right_knee$Cluster_hierarchical)<- c("Cluster 1","Cluster 2","Cluster 3")
processed_data_right_knee$gender<- factor(processed_data_right_knee$gender,levels=c(0,1), labels=c("Female", "Male"))

head(processed_data_left_knee)

head(processed_data_right_knee)

library(caret)
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
library(broom)
library(caTools)
library(pROC)

# library(glmmTMB)
# library(brms)
# library(rstan)

head(processed_data_right_knee)

# Setting the reference level
processed_data_right_knee$Cluster_hierarchical<- relevel(processed_data_right_knee$Cluster_hierarchical, ref="Cluster 1")


#Splitting the data using a function  into 90% training and 10% testing datasets

# Right knee
set.seed(123)
index_right <- createDataPartition(processed_data_right_knee$Cluster_hierarchical, p = .8, list = FALSE)
train_right <- processed_data_right_knee[index_right, ]
test_right <- processed_data_right_knee[-index_right, ]

dim(train_right)
dim(test_right)

table(processed_data_right_knee$Cluster_hierarchical)/dim(processed_data_right_knee)[1]

table(processed_data_left_knee$Cluster_CLARA)/dim(processed_data_left_knee)[1]

freq_right=table(processed_data_right_knee$Patient.ID, processed_data_right_knee$Cluster_hierarchical)
freq_left=table(processed_data_left_knee$Patient.ID, processed_data_left_knee$Cluster_CLARA)

dominating_cluster_func<- function(x, N=32){
     cluster_dominant<- rep(NA, N)
     clusters<- c("Cluster1", "Cluster2", "Cluster3")
    
     for(i in 1:N){  
      cluster_dominant[i]<- clusters[as.numeric(which.max(x[i, ]))]
     }

     return(cluster_dominant)   
}



cat("Dominating clusters of the study participants (Right knee):", "\n")
table(dominating_cluster_func(x=freq_right,N=32))


cat("Dominating clusters of the study participants (Left knee):", "\n")
table(dominating_cluster_func(x=freq_left,N=32))

#Loading some relevant packages
library(CGPfunctions)
library(ggplot2)
library("car")
library(ggpubr)
library(tidyr)
library(dplyr)
library(rstatix)
library(gridExtra)

freq_right_df<- as.data.frame(freq_right)
names(freq_right_df)<- c("patient_id", "Cluster", "Freq_assign")
freq_right_df$patient_id<- as.character(freq_right_df$patient_id)
freq_right_df$Cluster<- as.character(freq_right_df$Cluster)
freq_right_df$Knee_type<- rep("Right knee", dim(freq_right_df)[1])

freq_left_df<- as.data.frame(freq_left)
names(freq_left_df)<- c("patient_id", "Cluster", "Freq_assign")
freq_left_df$patient_id<- as.character(freq_left_df$patient_id)
freq_left_df$Cluster<- as.character(freq_left_df$Cluster)
freq_left_df$Knee_type<- rep("Left knee", dim(freq_left_df)[1])

head(freq_right_df)
head(freq_left_df)

combined_df<- rbind(freq_right_df,freq_left_df)

options(warn=-1)
theme_set(
 theme_bw()+theme(legend.position="none")
)

combined_df %>%
  ggplot(aes(Cluster, Freq_assign, color = Cluster), linetype = "dashed") +
  geom_point(aes(fill = Cluster), size = 2) +
  geom_line(aes(group = patient_id),
            color = "grey",
            arrow = arrow(type = "closed", length = unit(0.05, "inches"))) +
  facet_grid(rows = vars(Knee_type)) +
  labs(x = "Time-varying cluster membership across the 32 participants", y = "Frequency of cluster assignment", title = " ") +
  scale_color_manual(values = c("#0072B2", "#E69F00", "#009E73")) + # Set custom colors for lines and points
  scale_fill_manual(values = c("#0072B2", "#E69F00", "#009E73")) +  # Set custom fill colors for points
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
 ylim(0,800)


table(processed_data_right_knee$Patient.ID, processed_data_right_knee$Cluster_hierarchical)

as.data.frame(table(processed_data_right_knee$Patient.ID, processed_data_right_knee$Cluster_hierarchical))

table(processed_data_left_knee$Patient.ID, processed_data_left_knee$Cluster_CLARA)

# Setting the reference level
processed_data_left_knee$Cluster_CLARA<- relevel(processed_data_left_knee$Cluster_CLARA, ref="Cluster 1")


# Left knee
set.seed(123)
index_left <- createDataPartition(processed_data_left_knee$Cluster_CLARA, p = .8, list = FALSE)
train_left <- processed_data_left_knee[index_left, ]
test_left <- processed_data_left_knee[-index_left, ]


dim(train_left)
dim(test_left)

# formula <- bf(Cluster_kmeans ~ gender + age + Pain + BMI + (1 | Patient.ID))
# multinom_model_right <- brm(formula, data = train_right, family = categorical())
# summary(multinom_model_right )

# plot(multinom_model_right )



# Right knee
# Training the multinomial model
multinom_model_right <- multinom(Cluster_hierarchical ~ gender+age+Pain+BMI+Time_secs+
                                 Force_N+x_axis_disp_m+y_axis_disp_m
                                 , data = train_right)

# Checking the model
summary(multinom_model_right)

# Estimated odds ratio
cat("Estimated Odds-ratios for variables:", "\n")
round(exp(coef(multinom_model_right)),2)

# Optional: Get the p-values (requires `z` and `Pr(>|z|)` manually)
# Estimating the p-values
z_right <- summary(multinom_model_right)$coefficients/summary(multinom_model_right)$standard.errors

# 2-tailed z test
cat("Estimated p-values for variables:", "\n")
p_values_right <- (1 - pnorm(abs(z_right), 0, 1)) * 2
round(p_values_right,5)

 # step(multinom_model_right)

# Option 2 for display
tidy_right<- tidy(multinom_model_right, conf.int = TRUE,exponentiate = TRUE)%>% 
filter(term %in% c( "age", "genderMale", "BMI","Pain"))

cbind(tidy_right[, 1:2], round(tidy_right[, 3:8], 4))

# Left knee
# Training the multinomial model
multinom_model_left <- multinom(Cluster_CLARA ~ gender+age+Pain+BMI+Time_secs+
                                 Force_N+x_axis_disp_m+y_axis_disp_m
                                 , data = train_left)

# Checking the model
summary(multinom_model_left)


# Estimated odds ratio
cat("Estimated Odds-ratio for variables:", "\n")
round(exp(coef(multinom_model_left)), 2)

# Optional: Get the p-values (requires `z` and `Pr(>|z|)` manually)
# Estimating the p-values
z_left <- summary(multinom_model_left)$coefficients/summary(multinom_model_left)$standard.errors

# 2-tailed z test
cat("Estimated p-values for variables:", "\n")
p_values_left<- (1 - pnorm(abs(z_left), 0, 1)) * 2
round(p_values_left, 5)

tidy_left<- tidy(multinom_model_left, conf.int = TRUE,exponentiate = TRUE)%>% 
filter(term %in% c( "age", "genderMale", "BMI","Pain"))

cbind(tidy_left[, 1:2], round(tidy_left[, 3:8], 4))

# Predicting the values for test dataset
test_right$ClassPredicted <- predict(multinom_model_right, newdata = test_right, "class")

# Building classification table
tab_right <- table(test_right$Cluster_hierarchical, test_right$ClassPredicted)

# Calculating accuracy (%) - sum of diagonal elements divided by total obs
round((sum(diag(tab_right ))/sum(tab_right))*100,2)

options(warn=-1)
results_right<- pROC::multiclass.roc(as.numeric(test_right$Cluster_hierarchical), as.numeric(test_right$ClassPredicted))
results_right

# Get predicted probabilities
pred_probs_right <- predict(multinom_model_right, newdata = test_right, "probs")


# Get predicted classes
pred_classes_right<- predict(multinom_model_right, newdata = test_right,type = "class")


# Calculate accuracy
conf_matrix_right <- confusionMatrix(pred_classes_right, test_right$Cluster_hierarchical)
conf_matrix_right
accuracy_right <- conf_matrix_right$overall['Accuracy']
print(accuracy_right)


# Convert the actual class to a factor with the same levels as predicted classes
actual_classes_right <- factor(test_right $Cluster_hierarchical, levels = levels(pred_classes_right  ))

# Create a list to store ROC curve objects
roc_list_right  <- list();

# Plot ROC curve for each class

for (class in levels(actual_classes_right)) {
  # One-vs-all approach
   
  roc_list_right[[class]] <- roc(actual_classes_right  == class, pred_probs_right[, class])
  plot(roc_list_right[[class]], main = paste("ROC Curve (Right knee):", class), col = "blue", lwd = 2)
  print(paste("AUC for class", class, ":", auc(roc_list_right[[class]])))
}


# Predicting the values for test dataset
test_left$ClassPredicted <- predict(multinom_model_left, newdata = test_left, "class")

# Building classification table
tab_left <- table(test_left$Cluster_CLARA, test_left$ClassPredicted)

# Calculating accuracy (%) - sum of diagonal elements divided by total obs
round((sum(diag(tab_left ))/sum(tab_left))*100,2)

options(warn=-1)
results_left<- pROC::multiclass.roc(as.numeric(test_left$Cluster_CLARA), as.numeric(test_left$ClassPredicted))
results_left

# Get predicted probabilities
pred_probs_left <- predict(multinom_model_left, newdata = test_left, "prob")

# Get predicted classes
pred_classes_left <- predict(multinom_model_left, newdata = test_left,type = "class")


# Calculate accuracy
conf_matrix_left <- confusionMatrix(pred_classes_left, test_left$Cluster_CLARA)
conf_matrix_left
accuracy_left <- conf_matrix_left$overall['Accuracy']
print(accuracy_left)


# Convert the actual class to a factor with the same levels as predicted classes
actual_classes_left <- factor(test_left$Cluster_CLARA, levels = levels(pred_classes_left ))

# Create a list to store ROC curve objects
roc_list_left <- list();

# Plot ROC curve for each class

for (class in levels(actual_classes_left)) {
  # One-vs-all approach
   
  roc_list_left[[class]] <- roc(actual_classes_left == class, pred_probs_left[, class])
  plot(roc_list_left[[class]], main = paste("ROC Curve (Left knee):", class), col = "blue", lwd = 2)
  print(paste("AUC for class", class, ":", auc(roc_list_left[[class]])))
}


head(test_right)

coef(multinom_model_right)

prediction_func<- function(fitted_model, gender, age, Pain, BMI, Time_in_secs,
                          Force_in_N, x_disp_m, y_disp_m){
       estimated_coefs<- coef(fitted_model)
       cluster2_ref_1<- estimated_coefs[1, ]
       cluster3_ref_1<- estimated_coefs[2, ]  
       linear_comb_cluster2_ref_1<- (cluster2_ref_1[1]+(cluster2_ref_1[2]*gender)+ (cluster2_ref_1[3]*age)+
                                     (cluster2_ref_1[4]*BMI)+ (cluster2_ref_1[5]*Time_in_secs)+
                                     (cluster2_ref_1[6]*Force_in_N)+(cluster2_ref_1[7]*x_disp_m)
                                     +(cluster2_ref_1[8]*y_disp_m)
                                     )

     linear_comb_cluster3_ref_1<- (cluster3_ref_1[1]+(cluster3_ref_1[2]*gender)+ (cluster3_ref_1[3]*age)+
                                     (cluster3_ref_1[4]*BMI)+ (cluster3_ref_1[5]*Time_in_secs)+
                                     (cluster3_ref_1[6]*Force_in_N)+(cluster3_ref_1[7]*x_disp_m)
                                     +(cluster3_ref_1[8]*y_disp_m)
                                     )

    
       prob_cluster2_ref_1<- exp(linear_comb_cluster2_ref_1)/(1+exp(linear_comb_cluster2_ref_1))
       prob_cluster3_ref_1<- exp(linear_comb_cluster3_ref_1)/(1+exp(linear_comb_cluster3_ref_1))

       prob_combined<- data.frame(Prediction_cluster2_ref_1=prob_cluster2_ref_1,
                               Prediction_cluster3_ref_1= prob_cluster3_ref_1) 
    rownames(prob_combined)<- NULL
     return(prob_combined)

    }

prediction_func(fitted_model=multinom_model_right, gender=0, age=12, Pain=0, BMI=20, Time_in_secs=0,
                          Force_in_N=0, x_disp_m=0, y_disp_m=0)


prediction_func(fitted_model=multinom_model_right, gender=1, age=12, Pain=0, BMI=20, Time_in_secs=5,
                          Force_in_N=15, x_disp_m=2, y_disp_m=.5)


prediction_func(fitted_model=multinom_model_right, gender=1, age=12, Pain=0, BMI=20, Time_in_secs=30,
                          Force_in_N=15, x_disp_m=2, y_disp_m=.5)


