program the_product_of_expression
IMPLICIT NONE
integer :: n, i, serial_num, k,upper_v, lower_v
real    :: product

do 
  print*, "Enter the serial number : "
  read*,serial_num
  if (serial_num == 0) EXIT
  print*, "Enter the value of which terms you want to find product : "
  read*, n
  product = 1
do i = 1, n
  product = (product*((2*i)-1)/(2*i))

end do
  ! this do loop to print the expression but ayta ak line print hocce na (!)
  do k= 1, n-1, 2
     upper_v = k
	 lower_v = k+1
	   print*, "(",upper_v,"/",lower_v,")"

  end do
 ! use format 
print 72, product  

72 format ("The product of the given expression is : ", f6.2)

end do

end program the_product_of_expression