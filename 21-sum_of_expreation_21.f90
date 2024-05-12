!Write a Fortran program to print the sum of an expression such as (1-2x)^-1; when |x|<= 1/2
   ! expression : 1+2*x+(2*x)^2+(2*x)^3+.... 
!program 20 

program sum_of_expreation_21
IMPLICIT NONE

integer :: i, serial_num, n
real :: sum, x 

      do 
        print*, "Enter the serial number : "
        read*, serial_num

        if(serial_num == 0) EXIT 
             

        print*, "Enter the value of equation variable x : "

        read*, x

		     print*, "Enter the value which terms you want to calculate (n) : "

             read*, n

            if (x<= 0.5) THEN
			    sum = 0

                do i = 1, n

	               sum = sum+ ((2*x)**(i-1))

                end do

  print*, sum
  !by using format
 
    print 72, sum

    72 format("sum will be: ",f5.2)

 else 

    print*, "The equation is undefine "

end if 
end do 


end program sum_of_expreation_21
