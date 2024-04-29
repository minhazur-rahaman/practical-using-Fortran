!write a FORTRAN program to print the product of an expression such as (1/(a+b))(2/(a+2b))(3/(a+3b)).....upto n terms
!program 15


program product_of_expresstion
IMPLICIT NONE 

integer :: n, a, b, i, value, serial_num 
real :: product

do 
  print*, "Enter the serial number : "
  read*, serial_num

  if (serial_num == 0) EXIT



print*, "Enter the value of until which term you want to calculate the product(n)  : "
read*, n
   ! expression kinto chai nai jodi apnr valo lage tw apni ay do loop ta chalai expression ta deki dite paren kinto ayta ak line print hoi na(kharap)
   print*, "The given expression is : "
   do i=1, n
      value = i
      print*, "(",i, "/(a+", i ,"b)"

   end do


print*, "Type the value of a, b: "
read*, a, b

   if ((a==0 .and. b==0) .or. (a==((1-n)*b))) THEN
   print*, "The exp. undefined!" 

   else 

   !product = 1, ayta dore nite hobe ta na hole compiler icca maton value assign kore nibe.
   product = 1.0 
   do i=1, n
      product = (product * i)/(a+(i*b))
	  !print*, "Minhaz"
   end do
   ! result 
   print*, product

   !use format
   !print 10, product
   !10 format ("the result :", f6.2)
   end if 
     
   end do
end program product_of_expresstion