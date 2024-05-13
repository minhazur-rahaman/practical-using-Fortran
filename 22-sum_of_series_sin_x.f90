!problem : Write a FORTRAN program to calculate the sum of series such as sin x

!problem 22



program sum_of_series_sin

IMPLICIT NONE
real :: x, sum, factorial

integer :: serial_num, n ,i
real, parameter :: pi= 3.1416

       do 
          print*, "Enter the serial number : "
          read*, serial_num

          if (serial_num == 0) EXIT

             print*, "Enter the value of X:(in degree)" 			! sin series e x er value kinto degree te ni amra. 
             read*, x

			 !radiant e ni e gelam 
             x = pi*(x/180)

             print*, "Enter the N therms of number:"
             read*, n

			 n = n-1 								   ! AKTO kotin ase na bujle bolben

             sum = x       
             factorial = 1

            do i = 1, n
                  factorial = factorial*(2*i+1)*(2*i)
                  sum = sum +((-1)**i*x**(2*i+1))/factorial
            end do


         print*, "Sum will be = ", sum

       end do 
 
 end program 
 
