!Problem : Write a Fortran program to calculate the sum of series such as (1+bx)^-1 when |x|<=1/|b|
!problem 29
 



program sum_of_series

IMPLICIT NONE
real:: x, sum ,b 
integer :: n, i, serial_num 

     do 
	    print*, "Enter the serial Number : "
		read*, serial_num
		if (serial_num == 0) EXIT

		print*, "Enter the value of X where |x|<=1/|b| & value of B :"

		read*, x,b 

		!print*, "Enter the value of which terms you want to calculate :"          ! read di n er value ta if condition er agee nite parben 
		!read*, n


		if (abs(x)<= (1/abs(b))) THEN 

		print*, "Enter the value of which terms you want to calculate :"
		read*, n

		sum = 0
		   do i = 1, n
		      sum = sum + ((-1)**(i+1)*(b*x)**(i-1))
		   end do 

		print 72, sum
		
		72 format(2X "Sum of series is : ", f6.3 )

		else 
		    print*, "Check the value of X. Other wish the series will be Undefined"

		end if 

	  end do 

end program 			  