

############## OKTAY CAN SEVİMGİN / oktaycansevimgin2022@gmail.com ########

# Number of observations

dataset = read.table(file = "C:/Users/Oktay Can/Desktop/Dataset/DatasetNA.txt", header = TRUE)

#Var can be changed .This is for Var 1
var <- dataset$Var1[!is.na(dataset$Var1)]
var <- as.numeric(gsub(",", ".", var))

total_values <- 0

for(i in seq_along(var)) {
  
  total_values <- total_values + 1
}

print(total_values)

# Minimum  #Var can be changed .This is for Var 1

var_min <- var[1]
for (i in var) {
  if (i < var_min) {
    var_min <- i
  }
}
print(var_min)

# Maximum  #Var can be changed .This is for Var 1

var_max <- var[1]
for (i in var) {
  if (i > var_max) {
    var_max <- i
  }
}
print(var_max)

# Range  #Var can be changed .This is for Var 1

var_min <- var[1]
for (i in var) {
  if (i < var_min) {
    var_min <- i
  }
}

var_max <- var[1]
for (i in var) {
  if (i > var_max) {
    var_max <- i
  }
}

var_range <- c(var_min,var_max)
print(var_range)

# Sum  #Var can be changed .This is for Var 1

total <- 0

for (i in seq_along(var)){
  total <- total + var[i]
}

print(total)


# Mean  #Var can be changed .This is for Var 1

total <- 0

calculate_mean <- function(var) {
  total <- 0
  n <- 0
  
  for (i in seq_along(var)) {
    total <- total + var[i]
    n <- n + 1
  }
  
  mean_value <- total / n
  return(mean_value)
}

mean_result <- calculate_mean(var)
print(mean_result)


# Median  #Var can be changed .This is for Var 1

calculate_median <- function(var) {
  
  for (i in 1:length(var)) {
    for (j in i:length(var)) {
      if (var[j] < var[i]) {
        temp <- var[i]
        var[i] <- var[j]
        var[j] <- temp
      }
    }
  }
  
  n <- length(var)
  
  if (n %% 2 == 0) {
    
    median_value <- (var[n/2] + var[n/2 + 1]) / 2
  } else {
    
    median_value <- var[(n + 1) / 2]
  }
  
  return(median_value)
}

median_result <- calculate_median(var)
print(median_result)

# Sum of squares 

total <- 0

for (i in seq_along(var)){
  total <- total + var[i]
}


total_values <- 0

for(i in seq_along(var)) {
  
  total_values <- total_values + 1
}

n<- total_values

mean_var <- total/n
            
Sumofsquares <- 0

for (i in seq_along(var)){
  Sumof <- (var[i]- mean_var)**2
  Sumofsquares <- Sumofsquares + Sumof[]
}
print(Sumofsquares)

# Variance

total <- 0

for (i in seq_along(var)){
  total <- total + var[i]
}


total_values <- 0

for(i in seq_along(var)) {
  
  total_values <- total_values + 1
}

n<- total_values


mean_var <- total/n


Sumofsquares <- 0

for (i in seq_along(var)){
  Sumof <- (var[i]- mean_var)**2
  Sumofsquares <- Sumofsquares + Sumof[]
}

variance <-(Sumofsquares/(n-1))
print(variance)

# Standard deviation

total <- 0

for (i in seq_along(var)){
  total <- total + var[i]
}


total_values <- 0

for(i in seq_along(var)) {
  
  total_values <- total_values + 1
}


n<- total_values


mean_var <- total/n


Sumofsquares <- 0

for (i in seq_along(var)){
  Sumof <- (var[i]- mean_var)**2
  Sumofsquares <- Sumofsquares + Sumof[]
}

variance <-(Sumofsquares/(n-1))


a <- c(variance)
tol <- 1e-8 
b <- a / 2  

while(abs(b^2 - a) > tol) {
  b <- (b + a/b) / 2
}

standard_deviation <- b

print(standard_deviation)
