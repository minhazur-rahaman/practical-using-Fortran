!write a FORTRAN program to print the sum of an expression such as 1+1/2^2+1/3^2+.....upto n terms
!program 17


program sum_of_expresstion
IMPLICIT NONE 

integer :: n, i, value, serial_num 
real :: sum

do 
   print*, "Enter the serial number : "
   read*, serial_num
   
   if (serial_num == 0) EXIT

     print*, "print the value of N : "
     read*, n

   ! skip it 

   !do i=1, n
    !  value = i
     ! print*, "1","/",i+1,"^2"
	 
   ! end do


   sum = 0 
   do i=1, n
      sum = sum + (1/real(i**2))
	  !print*, sum  
	  !loop er modde akta print statement chalai dekte paren je man ghula ki babe sum hocce

   end do

   !print*, sum             ! without format use 
   ! using format how its look like
   print 66, sum
   66 format ("The sum of the expression is : ", f6.2)
     
  end do

end program sum_of_expresstion
