!Problem : Write a Fortran program to calculate the value of y from the following using function 
	   ! y = 2*x^2+3*x+4   for   x<2
	   ! y = 0             for 	 x=2
	   ! y = 2*x^2+3*x-4   for   x>2

!problem 42


program value_y

IMPLICIT NONE 

real :: y, x

   print*, "Enter the value of x :" 
   read*, x

	print*, y(x)
    print 12, y(x)
    12 format(2X,"result by using format :" f7.2)

    end program 

      real function y(x)

      IMPLICIT NONE 
	  
	  !real :: y 
	  real :: x
	   
	   if (x < 2.0) then 
	     y= 2*x**2+3*x+4

	   else if (x==2.0) then 
		 y = 0.0
	   else
	     y= 2*x**2+3*x-4

	   end if 

  end function 
	     