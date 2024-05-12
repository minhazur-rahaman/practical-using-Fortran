!Write a Fortran program to print the sum of an expression such as x+x^2/2!+ x^3/3!+ x^4/4!........upto n terms 
!program 19 

program sum_of_expreation_19
IMPLICIT NONE

integer :: i, serial_num, n
real :: sum, fact_val , x

       do 
       print*, "Enter the serial number : "
       read*, serial_num

       if(serial_num == 0) EXIT 
         print*, "Enter the value which terms you want to calculate (n) : "

		 read*, n

		 print*, "Enter the value of X : "

         read*, x

		    fact_val = 1
			sum = 0

            do i = 1, n
			   
               fact_val = fact_val*i
	           sum = sum+ ((x**i)/fact_val)

            end do

   print*, "the value of factorial is :", fact_val   ! you donot have to print factorial value but you can see 

print*, "sum of given expression is = ", sum

end do 


end program sum_of_expreation_19
