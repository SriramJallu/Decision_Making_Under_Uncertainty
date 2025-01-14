rm(list=ls())
library(terra)
library(sf)
library(sp)
library(ggplot2)
set.seed(12345)


your_df <- read.csv("read_your_input_df_for_crop/any_model.csv")

# https://smebluepages.com/fertilizer-industry-statistics-in-africa/
# https://africafertilizer.org/#/en/fertilizer-cost-build-up/

price_N <- 1.5
price_P <- 14
price_K <- 2.3
yld_mkt_price <- 1

#https://www.selinawamucii.com/insights/prices/cameroon/maize
#/#:~:text=What%20is%20the%20price%20of,kilogram%2Fpound%20in
#%20Cameroon%20today%3F&text=The%20retail%20price%20range%20in,
#lb)%20in%20Yaound%C3%A9%20and%20Douala.

yld_mkt_price <- 1

# Risk Attitudes of Farmers

# Light Risk Avesrion
ura1_func <- function(profit) {
  if (profit >= 0) {
    return(profit^0.8)
  } else {
    return(-2.5 * (abs(profit)^0.8))
  }
}

# High Risk Avesrion
ura2_func <- function(profit) {
  if (profit >= 0) {
    return(profit^0.2)
  } else {
    return(-3.5 * (abs(profit)^0.2))
  }
}


objective_func <- function (NPK, row_index) {
	
  # Define the variables required for your yield calculations
  profit <- numeric(ncol(your_df))
  
  for (k in 1:ncol(your_df)) {
    yield_value <- #yield calculations
    cost <- tN * price_N + tP * price_P + tK * price_K
    profit[k] <- yield_value * yld_mkt_price - cost
    
  }
  
  # ura1_expected_ut_avg <- mean(sapply(profit, ura1_func), na.rm = TRUE)
  # ura2_expected_ut_avg <- mean(sapply(profit, ura2_func), na.rm = TRUE)
  urn_expected_ut_avg <- mean(profit, na.rm = TRUE)
  return(-urn_expected_ut_avg)
  
}

init_temp_find <- function(row_index, max_iters, N_range, P_range, K_range,
                      init_step = 10, sample_size = 5) {
  
  
  T_init_values <- numeric(sample_size)
  
  for (sample in 1:sample_size) {
    current_NPK <- c(runif(1, N_range[1], N_range[2]),
                     runif(1, P_range[1], P_range[2]),
                     runif(1, K_range[1], K_range[2]))
    
    current_EU <- objective_func(current_NPK, row_index)
    del_EU_values <- numeric(max_iters)
    current_step <- init_step
    
    for (iter in 1:max_iters) {
      
      new_NPK <- current_NPK + runif(3, -current_step, current_step)
      
      new_step <- 2*init_step / (1 + exp(10*iter/max_iters))
      
      current_step <- new_step
      
      new_NPK[1] <- max(min(new_NPK[1], N_range[2]), N_range[1])
      new_NPK[2] <- max(min(new_NPK[2], P_range[2]), P_range[1])
      new_NPK[3] <- max(min(new_NPK[3], K_range[2]), K_range[1])
      
      new_EU <- objective_func(new_NPK, row_index)
      del_EU <- new_EU - current_EU
      del_EU_values[iter] <- del_EU
      
      
      if(del_EU > 0) {
        current_EU <- new_EU
        current_NPK <- new_NPK
      }
      
    }
    
    average_del_EU <- mean(del_EU_values[del_EU_values < 0])
    T_init_values[sample] <- average_del_EU/log(0.8)
    
  }
  
  return(mean(T_init_values, na.rm = TRUE))
  
}

##REF
## https://machinelearningmastery.com/simulated-annealing-from-scratch-in-python/

custom_SA <- function(init_NPK, row_index, alpha, max_temp, max_iters, temp_iters, stopping_threshold, N_range, P_range, K_range,
                      max_change_count, init_step_N, init_step_P, init_step_K) {
  
  current_NPK <- init_NPK
  current_EU <- objective_func(current_NPK, row_index)
  best_NPK <- current_NPK
  best_EU <- current_EU
  
  temp <- max_temp
  no_imp <- 0
  temp_count <- 0
  no_change_count <- 0
  stop_iter <- max_iters
  
  accepted_EU_values <- numeric(max_iters)
  best_EU_values <- numeric(max_iters)
  del_EU_values <- numeric(max_iters)
  probs_list <- numeric(max_iters)
  temp_list <- numeric(max_iters)
  step_list_N <- numeric(max_iters)
  step_list_P <- numeric(max_iters)
  step_list_K <- numeric(max_iters)
  
  N_vals <- numeric(max_iters)
  P_vals <- numeric(max_iters)
  K_vals <- numeric(max_iters)
  
  current_step_N <- init_step_N
  current_step_P <- init_step_P
  current_step_K <- init_step_K
  
  for (iter in 1:max_iters) {
    
    step_list_N[iter] <- current_step_N
    step_list_P[iter] <- current_step_P
    step_list_K[iter] <- current_step_K
    
    new_NPK <- current_NPK + c(runif(1, -current_step_N, current_step_N),
                               runif(1, -current_step_P, current_step_P),
                               runif(1, -current_step_K, current_step_K))
    
    new_step_N <- 2*init_step_N / (1 + exp(10*iter/max_iters))
    new_step_P <- 2*init_step_P / (1 + exp(10*iter/max_iters))
    new_step_K <- 2*init_step_K / (1 + exp(10*iter/max_iters))
    
    
    
    current_step_N <- new_step_N
    current_step_P <- new_step_P
    current_step_K <- new_step_K
    
    
    new_NPK[1] <- max(min(new_NPK[1], N_range[2]), N_range[1])
    new_NPK[2] <- max(min(new_NPK[2], P_range[2]), P_range[1])
    new_NPK[3] <- max(min(new_NPK[3], K_range[2]), K_range[1])
    
    N_vals[iter] <- new_NPK[1]
    P_vals[iter] <- new_NPK[2]
    K_vals[iter] <- new_NPK[3]
    
    new_EU <- objective_func(new_NPK, row_index)
    
    
    del_EU <- new_EU - current_EU
    del_EU_values[iter] <- del_EU
    
    if(del_EU <= 0) {
      
      current_EU <- new_EU
      current_NPK <- new_NPK
      no_imp <- 0
      probs_list[iter] <- 1
      
    } else {
      acceptance_prob <- exp(-(del_EU/temp))
      probs_list[iter] <- acceptance_prob
      if (runif(1) < acceptance_prob) {
        current_EU <- new_EU
        current_NPK <- new_NPK
        no_imp <- 0
      } else {
        no_imp <- no_imp + 1
      }
    }
    
    accepted_EU_values[iter] <- current_EU
    
    if (current_EU < best_EU) {
      best_EU <- current_EU
      best_NPK <- current_NPK
    }
    
    
    if (iter > 1 && accepted_EU_values[iter] == accepted_EU_values[iter - 1]) {
      no_change_count <- no_change_count + 1
    } else {
      no_change_count <- 0
    }
    
    
    best_EU_values[iter] <- best_EU
    
    #temp <- temp / (1 + 0.5 * temp)
    temp_count <- temp_count + 1
    if (temp_count == temp_iters) {
      temp <- temp * alpha
      temp_count <- 0
    }
    temp_list[iter] <- temp
    
    #if (no_imp >= temp_iters || abs(del_EU) < stopping_threshold) {
    #  break
    #}
    
    if (abs(del_EU) < stopping_threshold) {
      cat("Stopping Threshold reached", abs(del_EU))
      stop_iter <- iter
      break
    }
    
    if (no_change_count > max_change_count) {
      cat("No improvement over", max_change_count, " iterations")
      stop_iter <- iter
      break
    }
    
    
    
  }
  # plot(1:stop_iter, -accepted_EU_values[1:stop_iter], type = "l", col = "blue",
  #      xlab = "Iteration", ylab = "Best Expected Utility",
  #      main = paste("Best Expected Utility Over Iterations for Row", row_index))

  # plot(1:stop_iter, step_list_N[1:stop_iter], type = "l", col = "blue",
  #      xlab = "Iteration", ylab = "Step Size",
  #      main = paste("Steps"))

  # plot(1:stop_iter, temp_list[1:stop_iter], type = "l", col = "blue",
  #      xlab = "Iteration", ylab = "Temperature",
  #      main = paste("Temeprature"))
  # # 
  # plot(1:stop_iter, probs_list[1:stop_iter], type = "l", col = "blue",
  #      xlab = "Iteration", ylab = "Probability",
  #      main = paste("Probability"))
  # 
  # plot(1:stop_iter, del_EU_values[1:stop_iter], type = "l", col = "blue",
  #      xlab = "Iteration", ylab = "DEL EU",
  #      main = paste("del EU"))
  # 
  # plot(1:stop_iter, N_vals[1:stop_iter], type = "l", col = "blue",
  #      xlab = "Iteration", ylab = "N values",
  #      main = paste("N values"))
  # 
  # plot(1:stop_iter, P_vals[1:stop_iter], type = "l", col = "blue",
  #      xlab = "Iteration", ylab = "P values",
  #      main = paste("P values"))
  # 
  # plot(1:stop_iter, K_vals[1:stop_iter], type = "l", col = "blue",
  #      xlab = "Iteration", ylab = "K values",
  #      main = paste("K values"))
  
  # 
  # iterations <- 1:stop_iter

  N_vals_plot <- N_vals[1:stop_iter]
  P_vals_plot <- P_vals[1:stop_iter]
  K_vals_plot <- K_vals[1:stop_iter]

  # df <- data.frame(
  #   Iteration = rep(iterations, 3),
  #   Value = c(N_vals_plot, P_vals_plot, K_vals_plot),
  #   Type = rep(c("N", "P", "K"), each = length(iterations))
  # )
  # 
  # plot <- ggplot(df, aes(x = Iteration, y = Value, color = Type)) +
  #               geom_line(linewidth = 1) +
  #               labs(title = "NPK Trajectory",
  #                    x = "Iteration",
  #                    y = "Value",
  #                    color = "Nutrient") +
  #               theme_minimal()
  # 
  # print(plot)
  list(N = best_NPK[1], P = best_NPK[2], K = best_NPK[3], MaxEU = -best_EU, Stop_Iteration = stop_iter,
       Prob = probs_list, Temp = temp_list, DelEU = del_EU, 
       EU_iter_vals = -accepted_EU_values, Del_EU_vals = del_EU_values, df = df,
       N_vals_plot = N_vals_plot, P_vals_plot = P_vals_plot, K_vals_plot = K_vals_plot)
}

set.seed(12345)

init_temp_test <- init_temp_find(row_index = 6, max_iters = 250,
                                 N_range = c(0, 200), P_range = c(0, 200), K_range = c(50, 250), init_step= 20)
print(init_temp_test)


set.seed(12345)
test_1 <- custom_SA(init_NPK = c(100, 100, 150), row_index = 6, max_temp = init_temp_test, alpha = 0.85, max_iters = 1500,
                    temp_iters = 10, stopping_threshold = 1e-03, N_range = c(0, 200), P_range = c(0, 200), K_range = c(50, 250),
                    max_change_count = 150, init_step_N = 40, init_step_P = 40, init_step_K = 40)

cat("Optimal NPK:", test_1$N, test_1$P, test_1$K, "Max Expected Utility:", test_1$MaxEU, "\n", test_1$Stop_Iteration)

