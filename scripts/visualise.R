# Import Porstmouth data
porstmouth <- read.csv("data/Portsmouth.csv")

print the first 7 days of the data
print(porstmouth[1:7,])

# Plot the first 24 hours of the data
plot(porstmouth$time[1:24], porstmouth$elevation[1:24], type = "l")