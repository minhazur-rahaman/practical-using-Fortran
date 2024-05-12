!Problem : Write a Fortran program to calculate the sum of series such as (1-x)^-1 when |x|<=1
!problem 28
!I solve this problem by using Function (maximam time function use korte bole exam e)



program sum_of_series

IMPLICIT NONE
real:: x, sum_ser
integer :: n, serial_num 

     do 
	    print*, "Enter the serial Number : "
		read*, serial_num
		if (serial_num == 0) EXIT

		print*, "Enter the value of X where |x|<=1 :"

		read*, x

		print*, "Enter the value of which terms you want to calculate :" 
		read*, n


		if (abs(x)<= 1) THEN 

		
		
		!print*, sum_ser(x,n)			 !result without using format
		print 72, sum_ser(x,n)			 !function ta call kora hoice
		
		72 format(2X "Sum of series is : ", f6.3 )

		else 
		    print*, "Check the value of X. Other wish the series will be Undefined"

		end if 

	  end do 

end program 


     Real Function sum_ser(x,n)			   !function er je nam ta diben oii ta ar expretion name ta same hoite hobe
	 IMPLICIT NONE

	 real, intent(in) :: x
	 integer, intent(in) :: n 
	 integer :: i
	 !real :: sum_ser

  	       sum_ser = 0
		   do i = 1, n
		      sum_ser = sum_ser + x**(i-1)
		   end do 

	  end function 