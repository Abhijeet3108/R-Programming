#######################################################################################################
PROBLEM STATEMENT:

Choose the two models from Exercise 1 that gave the lowest CV error. For each model, use the
bootstrap method to estimate the 95% error-confidence interval. Use b = 100; i.e. construct
100 different bootstrap samples. For each model, list all 100 model errors. Based on the two
confidence intervals, which model would you select for deployment?
#####################################################################################################

ques2=function(dataset,svm)
{
  result=c()
  for(i in 1:100)
  {
    sampledata=dataset[sample(nrow(dataset),replace=T),]
    error=table(predict(svm,subset(sampledata,select=-Severity)),sampledata$Severity)
    error2=(error[1,2]+error[2,1])/length(dataset$Severity)*100
    cat("ERROR ",i,":",error2,"\n")
    #print(percent)
    result=c(result,error2)
  }
  result=sort(result)
  #print(result)
  lower=(result[2]+result[3])/2
  upper=(result[97]+result[98])/2
  print("Lower bound")
  print(lower)
  print("Upper bound")
  print(upper)
}


##################OUTPUT################################


ERROR  1 : 8.949011 
ERROR  2 : 11.13424 
ERROR  3 : 8.116545 
ERROR  4 : 10.30177 
ERROR  5 : 11.44641 
ERROR  6 : 9.677419 
ERROR  7 : 9.05307 
ERROR  8 : 10.50989 
ERROR  9 : 10.82206 
ERROR  10 : 9.885536 
ERROR  11 : 11.55047 
ERROR  12 : 9.573361 
ERROR  13 : 11.13424 
ERROR  14 : 9.573361 
ERROR  15 : 11.03018 
ERROR  16 : 12.17482 
ERROR  17 : 11.03018 
ERROR  18 : 9.469303 
ERROR  19 : 10.50989 
ERROR  20 : 10.50989 
ERROR  21 : 12.38293 
ERROR  22 : 9.365245 
ERROR  23 : 11.75858 
ERROR  24 : 9.573361 
ERROR  25 : 8.844953 
ERROR  26 : 10.92612 
ERROR  27 : 10.09365 
ERROR  28 : 8.636837 
ERROR  29 : 11.13424 
ERROR  30 : 9.885536 
ERROR  31 : 10.09365 
ERROR  32 : 12.27888 
ERROR  33 : 10.718 
ERROR  34 : 9.573361 
ERROR  35 : 10.19771 
ERROR  36 : 10.61394 
ERROR  37 : 9.781478 
ERROR  38 : 10.19771 
ERROR  39 : 9.469303 
ERROR  40 : 10.92612 
ERROR  41 : 9.573361 
ERROR  42 : 8.949011 
ERROR  43 : 9.677419 
ERROR  44 : 9.781478 
ERROR  45 : 11.23829 
ERROR  46 : 9.781478 
ERROR  47 : 9.677419 
ERROR  48 : 9.573361 
ERROR  49 : 10.09365 
ERROR  50 : 12.17482 
ERROR  51 : 9.261186 
ERROR  52 : 10.50989 
ERROR  53 : 8.844953 
ERROR  54 : 10.09365 
ERROR  55 : 12.38293 
ERROR  56 : 12.90323 
ERROR  57 : 10.09365 
ERROR  58 : 9.885536 
ERROR  59 : 12.07076 
ERROR  60 : 10.40583 
ERROR  61 : 11.34235 
ERROR  62 : 9.677419 
ERROR  63 : 12.59105 
ERROR  64 : 10.09365 
ERROR  65 : 8.532778 
ERROR  66 : 9.365245 
ERROR  67 : 8.740895 
ERROR  68 : 11.13424 
ERROR  69 : 10.40583 
ERROR  70 : 8.949011 
ERROR  71 : 9.677419 
ERROR  72 : 10.30177 
ERROR  73 : 10.61394 
ERROR  74 : 11.03018 
ERROR  75 : 10.61394 
ERROR  76 : 9.05307 
ERROR  77 : 9.989594 
ERROR  78 : 10.82206 
ERROR  79 : 11.03018 
ERROR  80 : 10.82206 
ERROR  81 : 10.61394 
ERROR  82 : 11.65453 
ERROR  83 : 10.19771 
ERROR  84 : 11.34235 
ERROR  85 : 9.989594 
ERROR  86 : 11.34235 
ERROR  87 : 11.23829 
ERROR  88 : 10.61394 
ERROR  89 : 10.82206 
ERROR  90 : 10.61394 
ERROR  91 : 10.09365 
ERROR  92 : 11.44641 
ERROR  93 : 10.40583 
ERROR  94 : 12.17482 
ERROR  95 : 11.03018 
ERROR  96 : 11.03018 
ERROR  97 : 9.989594 
ERROR  98 : 7.908429 
ERROR  99 : 10.82206 
ERROR  100 : 10.82206 
[1] "Lower bound"
[1] 8.324662
[1] "Upper bound"
[1] 12.38293

