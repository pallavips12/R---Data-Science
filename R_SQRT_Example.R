##install.packages('neuralnet')
library("neuralnet")

#Going to create a neural network to perform sqare rooting
#Type ?neuralnet for more information on the neuralnet library

#Generate 50 random numbers uniformly distributed between 0 and 100
#And store them as a dataframe
traininginput <-  as.data.frame(runif(50, min=0, max=400))
trainingoutput <- sqrt(traininginput)

#Column bind the data into one variable
trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")
View(trainingdata)


#Train the neural network
#formula - a symbolic description of the model to be fitted.
#data = a data frame containing the variables specified in the formula.
#hidden - the number of hidden neurons (vertices) in each layer.
#threshold - 	is a numeric value specifying the threshold for the partial(error tolerate)
## derivatives of the error function as stopping criteria.
#lifesign & lifesignstep means show only the 10th step in iteration


net.sqrt <- neuralnet( formula = Output~Input, 
                       data = trainingdata, 
                       hidden=10, 
                       threshold=0.05,
                       lifesign = "full",
                       lifesign.step = 10 )
print(net.sqrt)

#Plot the neural network
plot(net.sqrt)


#To test the neural network on some training data let us generate some squared numbers
testdata <- as.data.frame((1:25)^2) 

#Run them through the neural network
##compute function is used to compute the output for a given input using the neuralnet model object
?compute
net.results <- compute(net.sqrt, testdata) 

#Lets see what properties net.sqrt has and print the results
ls(net.results)
print(net.results$net.result)

#Lets display a better version of the results
cleanoutput <- cbind ( testdata,  sqrt(testdata),  as.data.frame(net.results$net.result) )
colnames(cleanoutput) <- c("Input", "Expected Output", "Neural Net Output" )
print(cleanoutput)


  