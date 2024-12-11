!problem : Write a FORTRAN program to calculate the sum of series such as tan(x)

!problem 24


program tanx
IMPLICIT NONE

real :: tan_x,sin_x,cox_x, x
integer :: n,serial_num
real,parameter :: pi = 3.1416
   
   do 
   print*, "Enter serial number(0 to EXIT) :"
   read*, serial_num

   if(serial_num == 0) EXIT 

   print*, "Enter the value of x:(in degree)"
   read*, x
   print*, "Enter the value of N: "
   read*, n

   x = pi*(x/180.0)

   n = n-1

   tan_x = sin_x(x,n)/cox_x(x,n)
   print*, "sinx", sin_x(x,n)
   print*, "coxx", cox_x(x,n)

   print*, "Without formating the sum of series tan x is : ", tan_x
   print 72, tan_x
   72 format(2X,"The sum of the tan series is: ",f18.3)
    
   end do

end program 


real function sin_x(x,n)
IMPLICIT NONE 
real, intent(in)::x
integer,intent(in)::n

real:: fact
integer:: i

fact = 1
sin_x = x

   do i=1,n
    fact = fact*(2*i+1)*(2*i)
	sin_x = sin_x+((-1)**i*x**(2*i+1))/fact
   end do

end function 

 real function cox_x(x,n)
 IMPLICIT NONE 
 real, intent(in)::x
 integer, intent(in)::n

 real :: fact

 integer :: i
 fact = 1.0

 cox_x = 1 

     do i=1,n
	   fact = fact*(2*i)*(2*i-1)
	   cox_x = cox_x*((-1)**i*x**(2*i))/fact
	 end do

end function 