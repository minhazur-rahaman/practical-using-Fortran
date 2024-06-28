!problem : Write a FORTRAN program to calculate the sum of series such as e^x

!problem 25



program sum_of_series_ex

IMPLICIT NONE
real :: x, sum, factorial

integer :: serial_num, n ,i
!real, parameter :: pi= 3.1416

       do 
          print*, "Enter the serial number : "
          read*, serial_num

          if (serial_num == 0) EXIT

             print*, "Enter the value of X:" 	
             read*, x


             print*, "Enter the N therms of number:"
             read*, n

             sum = 1      
             factorial = 1

            do i = 1, n
                  factorial = factorial*(i)
                  sum = sum +(x**(i-1))/factorial
            end do

		 print*, factorial
         print*, "Sum will be = ", sum

       end do 
 
 end program 

