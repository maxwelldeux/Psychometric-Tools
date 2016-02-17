#Direct RR

#Thorndike Case 2

#As cited in Sackett & Yang (2000) 

#Three inputs:
#cor = correlation between x and y variable
#sdr = restricted standard deviation
#sdu = unrestricted standard deviation
directRR <- function(cor, sdr, sdu) {
  corrected <- ((sdu/sdr)*cor)/sqrt(1 + (cor^2)*((sdu^2/sdr^2) - 1))
  return (corrected)
}

#Example

directRR(.30, 6.0, 8.2)  #0.3948702
