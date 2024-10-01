#-----Section-01-Adding Libraries-----
# Loading libraries for Plots, Calculations, Mapping and much more.
library(ggplot2)
library(ppcor)
library(car)
library(RcmdrMisc)
library(corrplot)
library(corrgram)
library(Amelia)
library(rcompanion)
library(psych)

#-----Section-02-Set & Get Work Directory-----
# Setting working directory before processing any work or test in using R Script,
# useful to save the Graphs, Plots, Images, and Data in set directory.
setwd(dirname(file.choose()))
# Getting the path directory to confirm the working directory.
getwd()

#-----Section-03-Data Frame & Data Set-----
# Reading the data from csv file and putting it into new data-frame.
DS7006.Data <- read.csv("u2550212_DS7006_CW2_data.csv", stringsAsFactors = FALSE)
# To inspect top 6 rows of the data and variable head for general view of data-set.
head(DS7006.Data)
# The counterpart of head() is tail(), which prints the last six rows.
tail(DS7006.Data)
# A data-frame having column(character, integer or logical) variables data type, which stands for “structure”.
str(DS7006.Data)
# Attaching the CSV data 'DS7006.Data' for working in R faster
attach(DS7006.Data)

#-----Section-04-Missing Data?-----
# The data set might have some missing values,
# checking for missing data in the entire data-set.
missing_data <- DS7006.Data
# Displaying the missing data summary so it can give an proper results.
apply(missing_data, MARGIN = 2, FUN = function(x) sum(is.na(x)))
# Mapping up missing values by column using miss-map.
png(file="png/fig.1.1.DS7006_Data_MissMap.png",width = 640, height = 480)
missmap(missing_data, margins = c(10, 5), col = c("black", "grey"), legend = TRUE,main = "Fig.: 1.1 Missingness Map of DS7006 QDA")
dev.off()
# However, to remove variables or data from work space or environment in R.
rm(missing_data)
# Summary for getting Mean, Std. deviation, Maximum and Minimum to check the central tendency.
# These statistics can be useful for understanding the central tendency and variability of the data distribution in each category within the data-set.
# The summary from the data-set can be parametric or non parametric, checking for few columns or variables.
summary(DS7006.Data)


#-----Section-05-Boxplot, Histogram, & Q-Q Plot-----
# Step 1: Boxplot, Histogram, and Q-Q plot of Dependent Variable (TotalDeath)

# Boxplot: A boxplot will give a visual representation of the distribution of dependent variable, TotalDeath.
# It shows the median, quartiles, and potential outliers.
png(file="png/fig.2.1.Total_death_Boxplot.png", width=640, height=480)
boxplot(Total_death, main="Fig.: 2.1 Boxplot of Total_death",
        ylab="Per Thousand Total_death", xlab="Total_death")
dev.off()
# Inspect outliers with boxplot status, getting some variables & objects to understand. 
Out <- boxplot.stats(Total_death)$out
Outa <- which(Total_death %in% c(Out))
print(DS7006.Data[Outa,1:3])
# Remove unnecessary objects from environment.
rm(Out,Outa)

# Histogram: A histogram will provide a visual representation of the distribution of TotalDeath,
# allowing to see the frequency distribution of different ranges.
# Histogram 1.1 With frequency.
png(file="png/fig.3.1.Total_death_Histogram_Frequency.png", width=640, height=480)
hist(Total_death, col = "light blue", border = "dark blue", freq = T, ylim = c(0,100),
     xlab = "Per Thousand Total_death", main = "Fig.: 3.1 Histogram of Total_death")
# Add a rug plot
rug (Total_death)
dev.off()

# Histogram 1.2 With density.
# Probability density histogram a continuous version of the histogram with densities,
# specifies how the probability density is distributed over the range of values.
png(file="png/fig.3.2.Total_death_Histogram_Density.png", width=640, height=480)
hist(Total_death, col = "light blue", border = "dark blue", freq = F, ylim = c(0,1),
     xlab = "Per Thousand Total_death", main = "Fig.: 3.2 Histogram of Total_death")
# Add a rug plot
rug (Total_death)
# Add a density curve
lines (density(sort(Total_death)))
# Add a Normal curve
xfit <- seq(from = min(Total_death), to = max(Total_death), by = 0.1)
yfit = dnorm(xfit, mean(Total_death), sd(Total_death))
lines(xfit, yfit, lty = "dotted")
# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 2)
dev.off()
# Remove unnecessary objects from environment.
rm(xfit, yfit)

# Histogram 1.3 Histogram with approximation normal.
png(file="png/fig.3.3.Total_death_Histogram_Approx_Normal.png", width=640, height=480)
plotNormalHistogram(Total_death, xlab = "Per Thousand Total_death", main = "Fig.: 3.3 Histogram of Total_death")
dev.off()

# Q-Q plot
# Q-Q plot: A Q-Q plot (Quantile-Quantile plot) compares the distribution of the variable to a normal distribution.
# This can help to assess whether the data is approximately normally distributed.
png(file="png/fig.4.1.Total_death_Q_Q_Plot.png", width=640, height=480)
qqnorm(Total_death, main = "Fig.: 4.1 Q-Q plot of Total_death", xlab = "Theoretical Quantiles Total_death",
       ylab = "Per Thousand Total_death")
qqline(Total_death,col = c("red"))
dev.off()


#-----Section-06-K-S Test Dependent Variable!-----
# Step 2: K-S Test of Dependent Variable TotalDeath
# The Kolmogorov-Smirnov (K-S) test can be used to assess whether the dependent variable
# follows a specific distribution (in this case, whether it follows a normal distribution).
ks.test(Total_death, "pnorm")


#-----Section-07-Boxplot & Histogram of Independent Variables-----
# Step 3: Boxplot & Histogram of Independent Variables
# These visualizations will help you understand the distribution of the independent variables.
# Boxplot for Age Variable
png(file="png/fig.5.1.BoxplotOfAgeVariable.png", width=640, height=480)
boxplot(Children,Early_working_age,Prime_working_age,Mature_working_age,Elderly, main="Fig.: 5.1 Boxplot of Age Variable",
        ylab="Per Thousand Population",names=c("Children","Early_working_age","Prime_working_age","Mature_working_age","Elderly"))
dev.off()

# Boxplot for General Health Variable
png(file="png/fig.5.2.BoxplotOfGeneralHealthVariable.png", width=640, height=480)
boxplot(Good_health, Fair_health, Bad_health, main="Fig.: 5.2 Boxplot of General Health Variable",
        ylab="Per Thousand Population", names=c("Good_health", "Fair_health", "Bad_health"))
dev.off()

#-----Section-08-Parametric Statistical Tests-----
# Step 4: Pearson Correlation Coefficients between Dependent and Independent Variables
# Calculate and list all correlation coefficients between the dependent variable TotalDeath
# and the independent variables (AGE, Religion, EmploymentStatus, Industry, PopulationWorking).
# The resulting correlation value will range between -1 and 1.
# A value of 1 indicates a perfect positive correlation.
# A value of -1 indicates a perfect negative correlation.
# A value of 0 indicates no correlation.
# Pearson correlation is appropriate for linear relationships between variables.
# It measures the strength and direction of a linear relationship.
correlation_matrix <- cor(DS7006.Data[,3:21],method = "pearson")
# print(correlation_matrix)
png(file="png/fig.6.1.CorrelationMatrix.png", width=1080, height=640)
corrgram(correlation_matrix, order = FALSE, cor.method = "pearson", lower.panel = panel.cor,
         upper.panel = panel.pie, text.panel = panel.txt, main = "Fig.: 6.1 Correlation Matrix",
         cex.labels = 1, mar = c(1,1,1,1))
dev.off()
rm(correlation_matrix)


# Selecting Strong Correlation Variables
correlation_matrix <- cor(DS7006.Data[,c(3,4,8,9,10,11,12,13,14,15,16)],method = "pearson")
# print(correlation_matrix)
png(file="png/fig.6.2.CorrelationMatrix.png", width=1080, height=640)
corrgram(correlation_matrix, order = FALSE, cor.method = "pearson", lower.panel = panel.cor,
         upper.panel = panel.pie, text.panel = panel.txt, main = "Fig.: 6.2 Correlation Matrix",
         cex.labels = 1, mar = c(1,1,1,1))
dev.off()
rm(correlation_matrix)

# Selecting Strong Correlation Variables - Final
correlation_matrix_strong <- DS7006.Data[,c(3,4,8,11,12,13,15,16)]
png(file="png/fig.6.3.StrongCorrelationVariablesMatrix.png", width=640, height=480)
cor_m_d <- cor(correlation_matrix_strong, method = "pearson")
corrplot(cor_m_d, type = "upper", tl.col = "black", tl.srt = 45,
         main = "Fig.: 6.3 Strong Correlation Variables Matrix", mar = c(1,1,1,1))
# print(cor_m_d)
dev.off()
rm(cor_m_d)


# Step 6: Kaiser-Meyer-Olkin (KMO) test with Strong Correlation Variables
# KMO Test
kmo_result <- KMO(cor(correlation_matrix_strong, method = "pearson"))
print(kmo_result)
rm(kmo_result)

rm(correlation_matrix_strong)

#-----Section-9-Regression Modeling-----
# Step 8: Regression Modelling
# We use the lm() function to fit a linear regression model.
# The formula specifies the relationship between the dependent variable (TotalDeath)
# and independent variables (AGE, Religion, Industry).
reg_model <- lm(Total_death ~ Children + Early_working_age + Limited_a_lot + Economically_active + Unemployed + Economically_inactive + Fair_health + Bad_health,
                data = DS7006.Data)

# Display the summary of the regression model
summary(reg_model)
rm(reg_model)


# detach the data frame from environment
detach(DS7006.Data)
# remove all variables from the environment
rm(list=ls())
# remove all plots or graphs
dev.off()
