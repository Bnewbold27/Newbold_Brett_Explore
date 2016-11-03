# HW7

#1

freqTable <- function(df){
  #  This function freqTable() creates a frequency table for data frames
  
  #  Parameters: 
  #  df - dataFrame
  
  #  Returns:
  #  a frequency table for every categorical and logical variable
  
  #  bn-2016
  
  if(is.data.frame(df)) # Lets make sure it is actually a data frame
  {
    fact_var <- df[sapply(df,is.factor)] # Collecting the factor columns
    log_var <- df[sapply(df,is.logical)] # Collecting the logical columns
    new_df <- data.frame(fact_var,log_var) # Combining the two into a refined dataFrame
    
    if(ncol(new_df)>0) # Making sure there are logical or factor variables
    {    
      freq <-list() # Empty list to store all the freq tables
      for (i in 1:ncol(new_df)) # For each column in the new dataFrame
      {
        freq[[i]] <- as.data.frame(table((new_df)[,i])) # Since table is evaulating one column of the time it creates
        # a simple contigency table for each variable. as.data.frame then converts it to a data frame to store in 
        # table() to give the counts of ith columnb in fn, convert it to a data frame and put into freq[[i]]
        names(freq[[i]])[1]=colnames(new_df[i])
        # freq[[i]] defaults the first column of each dataFrame to be "Var1". Therefore for our frequency we need to 
        # display what the exact variable (column name) is from the new_df 
      } 
      return(freq)
    }
    else
      print("There are no categorical or logical variables in the data frame.")  
  }
  else
    print("The input must be a data frame!")
}


# 2

#a
summStat <- function(df){
  #  This function summStat() accepts any dataframe as a parameter
  
  #  Parameters: 
  #  df - dataFrame
  
  #  Returns:
  #  a summary statistics table for each numeric variable
  
  #  bn-2016
  
  if(is.data.frame(df))
  {
    
    num_var <- df[sapply(df, is.numeric)]   #  sapply() will reveal which columns in x are numeric columns. 
    #  x[] outputs those column that are TRUE. 
    if(ncol(num_var) != 0) # Making sure there are numeric columns in the dataframe
    {
      s <- list() # Creating a list to store each summary
      for(i in 1:ncol(num_var)) 
      {
        s[[i]] <- summary(num_var[,i]) # Taking the summary statistics for each numeric column and storing into on list
      }
      return(s) 
    }
    else
      print("This data frame has no numeric variables.")
  }
  else
    print("The input must be a data frame!")
}



# b

rSquared <- function(df){
  #  This function rSquared() accepts any dataframe as a parameter and returns a
  #  dataframe that contains each pair of column names in the first column as a 
  #  single string separated by a "-" and their corresponding r^2 values in the
  #  second column.
  
  #  Parameters: 
  #  df - dataFrame
  
  #  Returns:
  #  dataFrame that contains each pair of column names in the first column and 
  #  the Pearson correlation coefficient for all coefficients whose absolute value
  #  is greater than the threshold
  
  #  bn-2016
  
  if(is.data.frame(df))
  {
    
    y <- df[sapply(df,is.numeric)]   # Making sure only to use the numeric columns
    
    if(ncol(y)>=2)   # We need at least two numeric columns to run the function
    {
      c <- combn(colnames(y), 2)   # Possible combinations for each pair of column names (lists them in columns)
      Var_Pairs <- paste(c[1,], c[2,], sep = "-")   # Creating the pairs by pasting the rows of c
      m <- cor(y, method = "pearson")   # Creates the correlation coefficients between the columns of y as a matrix
      r <- m[which(lower.tri(m))]  # We only need the entries from the lower or upper triangular, choose lower 
      # because their order corresponds to the strings created earlier. 
      # which() tells us the index values of the lower triangular and correspond 
      # to the order of the strings. m[] then outputs the values in that order
      values <- r^2 # Assigning the values from r^2
      new_df <- data.frame(Var_Pairs, values)   # Putting together the pieces for the new data frame
      names(new_df) <- sub("^Var_Pairs$", "Variable Pairs", names(new_df)) # Creating new column names
      names(new_df) <- sub("^values$", "R-Square", names(new_df))
      return(new_df)
    }
    else
      print(paste("This Data Frame does not have two or more numerical columns", 
                  "to compute the Pearson correlation coefficient(s)."))
  }
  else
    print("The input must be a data frame!")
}



#c

threshold <- function(df,t=0.7){
  #  This function threshold() accepts any dataframe as a parameter and returns a
  #  correlation coefficients whose absolute value is greater than the threshold
  
  #  Parameters: 
  #  df - dataFrame
  #  t - numeric threshold
  
  #  Returns:
  #  dataFrame that contains each pair of column names in the first column and 
  #  the Pearson correlation coefficient for all coefficients whose absolute value
  #  is greater than the threshold
  
  #  bn-2016
  
  if(is.data.frame(df))
  {
    if(t>0 && t<1 && is.numeric(t)){
      
      y <- df[sapply(df,is.numeric)]   # Making sure only to use the numeric columns
      
      if(ncol(y)>=2) # We need at least two numeric columns to run the function
      {
        c <- combn(colnames(y), 2) # Possible combinations for each pair of column names (lists them in columns)
        
        pairs <- paste(c[1,], c[2,], sep = "-") # Creating the pairs by pasting the rows of c
        
        m <- cor(y, method = "pearson") # Creates the correlation coefficients between the columns of y as a matrix
        
        r <- m[which(lower.tri(m))]  # We only need the entries from the lower or upper triangular, choose lower 
        # because their order corresponds to the strings created earlier. 
        # which() tells us the index values of the lower triangular and correspond 
        # to the order of the strings. m[] then outputs the values in that order
        
        Pearson_Exceeds_Threshold <- r[which(abs(r) > t)] # All coefficients whose absolute value is greater than the threshold
        
        Variable_Pairs <- pairs[which(abs(r) > t)] # The pairs that correspond to the filtered coefficients
        
        new_df <- data.frame(Variable_Pairs, Pearson_Exceeds_Threshold) # Putting together the pieces for the new data frame
        names(new_df) <- gsub("_", " ", names(new_df)) # I wonder if Prof G created gsub..
        if(nrow(new_df)!=0)
        {
          return(new_df)
        }
        else
          print(paste("There are no r values greater than the absolute value of", t))
      }
      else 
        print(paste("This Data Frame does not have two or more numerical columns", 
                    "to compute the Pearson correlation coefficient(s)."))
    }
    else
      print("Invalid threshold: the correlation threshold must be between 0 and 1.")
  }  
  else
    print("The input must include a data frame!")
}


#3

library(ggplot2)
library(grid)
library(gridExtra)

hist <- function(df,ps = 'off',bsizes=c(30)){
  #  This function hist() accepts any dataframe as a parameter
  
  #  Parameters: 
  #  df - dataFrame
  #  ps - plotswitch with three values ('off', 'on', or 'grid')
  #  bsizes - vector containing numeric values representing number of bins for histograms
  
  #  Returns:
  #  pairs of histograms if ps = 'on' or 'grid' and individual grids if ps = 'grid'
  
  #  bn-2016
  
  if(is.data.frame(df))
  {
    if(ps == 'grid' | ps == 'on' | ps == 'off'){ # Making sure a valid plotswitch is called
      
      if(all(bsizes == floor(bsizes) & all(bsizes == abs(bsizes)))){ # Making sure bin sizes are positive integers
        
        if(is.numeric(bsizes)){ 
          
          num_var <- df[sapply(df,is.numeric)]   # Making sure only to use the numeric columns
          
          if(ps=='on')  # If plotswitch is 'on'
            for(i in 1:length(bsizes)){ # For each value provided in vector
              for(j in 1:(ncol(num_var))){ # For each numeric variable
                bin_width <- (max(num_var[,j]) - min(num_var[,j]))/bsizes[i] # Calculating the bin width
                var_mean <- mean(num_var[[j]]) # Calculates the mean of each column
                mean = sprintf("%8.3f ", var_mean) # Helps round the decimal to 3 places
                counts <- ggplot(num_var, aes(x=num_var[,j])) + # Creating histogram with x axis corresponding to variable name
                  geom_histogram(colour = "blue", fill = "blue", binwidth = bin_width) + # Detailing histogram using calculated bin width
                  geom_vline(xintercept = mean(num_var[,j]),colour="red") + # Plotting vertical line where the mean is 
                  annotate("text",x=var_mean,y=0, colour="black",label=mean,hjust=0) + # Labeling the mean line
                  labs(x=colnames(num_var)[j]) # Labeling the x axis
                density <- counts + aes(y = ..density..) + labs(y = "density") # Density histograms
                print(counts)
                print(density)
              }
            }
          else if(ps == 'grid')
            for(i in 1:length(bsizes)){
              count_list <- list() # Creating lists with the intention of storing mulitple ggplots
              density_list <- list()
              for(j in 1:(ncol(num_var))){ # Nested for loop to consider all variables with all bin sizes
                bin_width <- (max(num_var[,j]) - min(num_var[,j]))/bsizes[i] # Calculating the binwidth for each number in vector
                var_mean <- mean(num_var[[j]]) # Calculates the mean of each column
                mean = sprintf("%8.3f ", var_mean) # Helps round the decimal to 3 places
                count_list[[j]] <- ggplot(num_var, aes(x=num_var[,j])) + # Creating histogram with x axis corresponding to variable of df
                  geom_histogram(colour = "blue", fill = "blue", binwidth = bin_width) + # Detailing histogram using calculated bin width
                  geom_vline(xintercept = mean(num_var[,j]),colour="red") + # Plotting vertical line where the mean is 
                  annotate("text",x=var_mean,y=0, colour="black",label=mean,hjust=0) + # Labeling the mean line
                  labs(x=colnames(num_var)[j]) # Labeling the x axis
                density_list[[j]] <- count_list[[j]] + aes(y = ..density..) + labs(y = "density") # Creating density plots
              }
              grid.arrange(grobs=count_list,ncol=2) # Putting all the plots on one main grid
              grid.arrange(grobs=density_list,ncol=2)
            }
        }else
          print("Please input positive integers for the bin sizes!") # If the bin sizes were not numeric
      }
      else      
        print("Please input positive integer bin sizes.") # If the bin sizes were not postive integers
    }
    else
      print("Please input a valid plot switch : 'off', 'on', or leave blank for no plot.")
  }
  else
    print("The input must include a data frame!")
}


#4

barGraph <- function(df, ps='off'){
  #  This function barGraph() accepts any dataframe as a parameter
  
  #  Parameters: 
  #  df - dataFrame
  #  ps - plotswitch with three values ('off', 'on', or 'grid')
  
  #  Returns:
  #  a gray bar graph for every categorical and binary variable
  
  #  bn-2016
  if(is.data.frame(df))
  {
    if(ps == 'grid' | ps == 'on' | ps == 'off'){ # Making sure a valid plotswitch is called
      fact_var <- df[sapply(df,is.factor)] # Collecting the factor columns
      log_var <- df[sapply(df,is.logical)] # Collecting the logical columns
      new_df <- data.frame(fact_var,log_var) # Combining the two into a refined dataFrame
      
      if(ps == 'grid' | ps == 'on') # Making sure 'on' or 'grid'
        
        if(length(new_df)!=0)
        { # We need at least one factor or logical column in the dataframe
          var_names <- colnames(new_df) # Column names of the factor and logical variables 
          for (i in 1:length(new_df)) # Loop for each column
          {
            plot <- ggplot(new_df, aes_string(x=var_names[i])) + # Plotting each variable from new_df while labeling the name on x-axis 
              geom_bar(fill="gray", colour="black") # Gray bar graph 
            print(plot) # print each plot in the loop
          }
        } 
      else
        print("There are no factor or logical variables in this dataframe!")
    }          
    else
      print("Please input a valid plot switch : 'off', 'on', or leave blank for no plot.")
  }
  else
    print("The input must include a data frame!")
}


explore <- function(df, ps = 'off', t=0.7, bsizes=c(30)){
  #  This function explore() accepts any dataframe as a parameter
  
  #  Parameters: 
  #  df - dataFrame
  #  ps - plotswitch with three values ('off', 'on', or 'grid')
  #  t - numeric threshold
  #  bsizes - vector containing numeric binsizes to use for plotting
  
  #  Returns:
  #  outputs of the "smaller" functions 
  
  #  bn-2016
  
  if(is.data.frame(df)){ # If the input is indeed a data frame
    
    output <- list() # Creating list to store the output of each function
    output[[length(output)+1]] <- freqTable(df)      
    output[[length(output)+1]] <- summStat(df)
    output[[length(output)+1]] <- rSquared(df) 
    output[[length(output)+1]] <- threshold(df,t)
    print(output) # Returns an R list 
    hist(df,ps,bsizes) 
    barGraph(df,ps)
  }
  else 
    print("The input must be a data frame!") # Wanted to include so explore can immediately recognize and stop without having to output each function
}
