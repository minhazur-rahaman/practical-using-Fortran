!Problem : Write a Fortran program to calculate the sum of series such as (1+x)^-1 when |x|<=1
!problem 27
!next problem ta funtion di solve kora ase deke nien 



program sum_of_series

IMPLICIT NONE
real:: x, sum
integer :: n, i, serial_num 

     do 
	    print*, "Enter the serial Number : "
		read*, serial_num
		if (serial_num == 0) EXIT

		print*, "Enter the value of X where |x|<=1 :"

		read*, x

		!print*, "Enter the value of which terms you want to calculate :"          ! read di n er value ta if condition er agee nite parben 
		!read*, n


		if (abs(x)<= 1) THEN 

		print*, "Enter the value of which terms you want to calculate :"
		read*, n

		sum = 0
		   do i = 1, n
		      sum = sum + ((-1)**(i+1)*x**(i-1))
		   end do 

		print 72, sum
		
		72 format(2X "Sum of series is : ", f6.3 )

		else 
		    print*, "Check the value of X. Other wish the series will be Undefined"

		end if 

	  end do 

end program 			  