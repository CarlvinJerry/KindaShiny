
options(scipen=999)

x = c(3,8,10,17,24,27)
x = c(1,2,3,4,5,6,7,8,9,3)
means = mean(x)

m2 = sum((x - means)^2)/length(x)

m4 = sum((x - means)^4)/length(x)

#m3/(m2 ^ 1.5)

(m4/(m2^2))*((length(x) * (length(x)+1))/ ((length(x)-2)*(length(x)-3)* (length(x)-1)))-((3*((length(x)-1)^2))/(((length(x)-3)* (length(x)-2))))



(sum(x-means))* 100000000000000

y = c(5,5,4,3,1)
mean(y)
fourthdev = sum((y-mean(y))^4)
seconddev = sum((y-mean(y))^2)

(fourthdev/(seconddev^2)) * (((5*6)/(4*3*2)) -((3*16)/6))
