!Write a Fortran program to calculate the sum of series such as Log(1+x) 
!program 26
 

program sum_of_series_log
IMPLICIT NONE

integer :: i, serial_num, n
real :: sum, x 					!jodi sum = 0 first e initialize koren taile next bar jakon loop ta ghure asbe ager value ta save kore rakbe.so result ulta-palta asbe

      do 
         print*, "Enter the serial number : "
         read*, serial_num

            if(serial_num == 0) EXIT 


            print*, "Enter the value of equation variable x : "

            read*, x

            print*, "Enter the value which terms you want to calculate (n) : "

            read*, n


			sum = 0
                do i = 1, n
	
	                 sum = sum + (((-1)**(i+1)*x**i)/i)

                end do


      print*,"Result without using format: ", sum
      !by using format
 
   print 66, sum

   66 format(2X "sum will be: ",f10.2)
 
end do 


end program sum_of_series_log
