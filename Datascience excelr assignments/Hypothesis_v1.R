#n = 50 , u =4 , S =3 , xbar = 4.2

# 1st step  define hpyothesis
#Ho = u <= 4.2
#Ha = u >=4.2

#2nd test testing T 
#(xbar-u) / (S/Sqrt(n))
#calculate T value
T<-(4.2-4)/(3/(sqrt(50)))           

#find P value 
#pt(tvalue,n-1)
1-pt(0.47,39)
0.05

pt(T,39)

pt(0.4714045,39)  
   