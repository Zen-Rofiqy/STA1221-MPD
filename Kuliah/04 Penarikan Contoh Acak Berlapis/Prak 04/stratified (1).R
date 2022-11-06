A <- c(35,43,36,39,28,28,29,25,38,27,26,32,29,40,35,41,37,31,45,34)
summary(A)
B <- c(27,15,4,41,49,25,10,30)
summary(B)
rural <- c(8,14,12,15,30,32,21,20,34,7,11,24)
summary(rural)

Ni=c(155, 62, 93)
ni=c(20, 8, 12)
N=sum(Ni)
n=sum(ni)
ybari=c(33.9, 25.12, 19)
Si=c(5.95, 15.25, 9.36)



#============dugaan rataan populasi==========
Ni=c(155, 62, 93)
ni=c(20, 8, 12)
N=sum(Ni)
n=sum(ni)
ybari=c(33.9, 25.12, 19)
Si=c(5.95, 15.25, 9.36)

ybar <- 1/N*sum(Ni*ybari) #27.67
Vduga=1/N^2*sum((Ni^2)*(1-ni/Ni)*(Si^2/ni))
boe <- 2*sqrt(Vduga) #2.81

#===========dugaan total populasi========
Ni=c(155, 62, 93)
ni=c(20, 8, 12)
N=sum(Ni)
n=sum(ni)
ybari=c(33.9, 25.12, 19)
Si=c(5.95, 15.25, 9.36)

tau_duga = sum(Ni*ybari) #8578.94
Vduga = sum((Ni^2)*(1-ni/Ni)*(Si^2/ni))
boe = 2*sqrt(Vduga) #870.32


#=============dugaan proporsi=================\
Ni=c(155, 62, 93)
ni=c(20, 8, 12)
N=sum(Ni)
n=sum(ni)
pi <- c(0.8,0.25,0.5)
pduga <- 1/N*sum(Ni*pi)
Vduga <- 1/N^2*sum(Ni^2*(Ni-ni)/Ni*pi*(1-pi)/(ni-1))
boe <- 2*sqrt(Vduga)

#===============ukuran contoh ===============


#1. BOE
#rataan
Ni=c(155, 62, 93)
N=sum(Ni)
ragam = c(25,225,100)
ai = c(1/3,1/3,1/3)
B = 2
D = B^2/4

nrataan = sum(Ni^2*ragam/ai)/(N^2*D+sum(Ni*ragam))
ni = ai*nrataan

#total
B= 400
D = B^2/(4*N^2)
ntotal = sum(Ni^2*ragam/ai)/(N^2*D+sum(Ni*ragam))
ni <- ai*ntotal


#2. alokasi contoh di setiap lapisan
Ni=c(155, 62, 93)
N=sum(N)
B = 2
ci = c(9,9,16)
sd = c(5,15,10)

#menentukan ukuran contoh keseluruhan
D = B^2/4 #karena rataan
n = sum((Ni*sd)/sqrt(ci))*sum((Ni*sd)*sqrt(ci))/(N^2*D+sum((Ni*sd^2)))

#alokasi sampel perstrata
ni = n*(Ni*sd/sqrt(ci))/sum((Ni*sd)/sqrt(ci))
  
                                               
#3. Neyman Allocation
#untuk cost sama
Ni=c(155, 62, 93)
N=sum(Ni)
sd = c(5,15,10)
B =2

#menentukan ukuran contoh keseluruhan
D = B^2/4
n = (sum(Ni*sd))^2/(N^2*D+ sum(Ni*sd^2))

#alokasi sampel perstrata
ni = n*Ni*sd/sum(Ni*sd)

#4. Proportional allocation : ragam dianggap sama
Ni=c(155, 62, 93)
N=sum(Ni)
sd = c(5,15,10)
sd = 10
B =2

#menentukan ukuran contoh keseluruhan
D = B^2/4 
n = sum(Ni*sd^2)/(N*D+1/N*sum(Ni*sd^2))
#alokasi sampel perstrata
ni = n*Ni/N
