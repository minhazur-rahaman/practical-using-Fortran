!Write a Fortran program to print the sum of an expression such as b/r+ 2b/(r+a)+3b/(r+2a)+.........upto n terms 
!program 20 

program sum_of_expreation_20
IMPLICIT NONE

integer :: i, serial_num, n
real :: sum, b, r, a 			   ! jodi apni sum = 0 variable declare korar time e den taile do loop ta akbar ghurar por garbage value dekabe 

       do 
          print*, "Enter the serial number : "
          read*, serial_num

          if(serial_num == 0) EXIT 

          print*, "Enter the value of equation variable a, b, r : "

          read*, a, b, r

		  print*, "Enter the value which terms you want to calculate (n) : "

          read*, n

		      sum = 0
              do i = 1, n
			 
                  sum = sum+ ((i*b)/(r+(i-1)*a)**i )
			  
              end do
 
  print*, "Sum of this given expression is = ", sum

end do 


end program sum_of_expreation_20
