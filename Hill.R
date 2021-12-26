#Hill Plot for Normal and Student's T distribution

#######################################################################################################

#Executed once

#Hill estimator function

hill_estimator <- function(k, input){
  term1 <- input[1:k] %>% log() %>% mean()
  term2 <- log(input[k+1])
  hill_estimate <- term1 - term2
  return(hill_estimate)
}


#Modifying the Hill estimator as per Dowd

modified_hill_estimator <- function(k, input){
  term1 <- input[1:k] %>% log() %>% mean()
  term2 <- log(input[k+1])
  hill_estimate <- term1 - term2 - 0.0015*k
  return(hill_estimate)
}


#Setting up the title

chr1 <- "Hill Plot"
distribution_normal <- "Normal Distribution"
chr2 <- "Sample Size ="
chr3 <- "Degrees of Freedom ="
distribution_t <- "Student's T Distribution"



#######################################################################################################


#Inputs to be varied

#1.Sample size and degrees of freedom

sample_size <- 1000
df_t <- 5
set.seed(2048)


#2.Generating the sample data

normal_data <- rnorm(n=sample_size, mean=0, sd=1)
t_data <- rt(n=sample_size, df=df_t)



#3.Sorting the data in descending order

ordered_normal_data <- sort(x=normal_data, decreasing = TRUE)
ordered_t_data <- sort(x=t_data, decreasing=TRUE)



#4.Calculating the number of positive observations

positive_normal <- sum(normal_data>0)-1
positive_t <- sum(t_data>0)-1



#5.Setting the order parameter

k_normal <- 1:positive_normal
k_t <- 1:positive_t



#6.Generating the Hill output data for the normal distribution

output_hill_normal <- vapply(X=k_normal, FUN=hill_estimator,
                             FUN.VALUE = numeric(1), input=ordered_normal_data)
output_hill_normal_modified <- vapply(X=k_normal, FUN=modified_hill_estimator,
                                      FUN.VALUE = numeric(1), input=ordered_normal_data)
output_normal_data <- as_tibble(cbind(k_normal, output_hill_normal, output_hill_normal_modified))



#7.Generating the Hill output data for the t distribution

output_hill_t <- vapply(X=k_t, FUN=hill_estimator,
                        FUN.VALUE = numeric(1), input=ordered_t_data)
output_t_data <- as_tibble(cbind(k_t, output_hill_t))



#8.Plotting Hill estimate vs k

#8.a.Creating the title phrases

title_normal <- paste(chr1, distribution_normal,
                      chr2, as.character(sample_size))

title_t <- paste(chr1, distribution_t, chr2, 
                 as.character(sample_size), chr3, as.character(df_t))

#8.b.Creating the plots upto the last positive value

plot_hill_normal <- ggplot(data=output_normal_data, aes(x=k_normal, y=output_hill_normal))+ geom_line() + ggtitle(label=title_normal)
plot_hill_modified_normal <- ggplot(data=output_normal_data, aes(x=k_normal, y=output_hill_normal_modified))+ geom_line() + ggtitle(label=title_normal)
plot_hill_normal
plot_hill_modified_normal

plot_hill_t <- ggplot(data=output_t_data, aes(x=k_t, y=output_hill_t))+ geom_line() + ggtitle(label=title_t)
plot_hill_t


#Following section not complete. Not to be executed.
#9.Optimal value of k

#As implemented by Danielsson (2001)
#From the "tea" package

library(tea)
optimal_k_normal <- danielsson(data=normal_data, B=500)
optimal_k_t <- danielsson(data=t_data, B=500)
#"tea" package has a function named ggplot, but that is different.
#To use the regular ggplot function you have to remove "tea" from the session
detach(tea, unload=TRUE)


#10.Plotting Hill estimator slightly beyond the optimal value

plot_hill_normal_100 <- ggplot(data=output_normal_data[1:100,], aes(x=k_normal, y=output_hill_normal))+ geom_line() + ggtitle(label=title_normal)
plot_hill_t_100 <- ggplot(data=output_t_data[1:100,], aes(x=k_t, y=output_hill_t))+ geom_line() + ggtitle(label=title_t)
