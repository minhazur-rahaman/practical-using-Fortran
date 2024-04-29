!write a FORTRAN program to print the sum of an expression such as 1/a+2/(a+b)+3/(a+2b)+4/(a+3b).....upto n terms
!program 18

program sum_of_expresstion
IMPLICIT NONE 

integer :: a, b, n, i, value, serial_num
real :: sum


do 
  print*, "Enter the serial number : "
  read*, serial_num

  if (serial_num == 0) EXIT

   print*, "print the value of N : "
   read*, n

   ! skip it skip comment 

   !do i=1, n
    !  value = i
     ! print*, "1","/",i+1,"^2"
	 
   ! end do

print*, "print the value of a, b : "
read*, a, b 


   if ((a==0 .and. b==0) .or. (a==(1-n)*b)) THEN 	  ! a==-(i-1)*b (bujccen tw? ekane kinto i na hoi n use korsi !
       print*, "The expression will be undefined! "
   else 

   sum = 0 
   do i=1, n
      sum = sum + (i/real(a+(i-1)*b))
	  !print*, sum  
	  !loop er modde akta print statement chalai dekte paren je man ghula ki babe sum hocce

   end do
   ! use format 
   print 72, sum
   72 format ("The sum of given expresion : " , f6.3  )

   end if 

   end do 

     

end program sum_of_expresstion
