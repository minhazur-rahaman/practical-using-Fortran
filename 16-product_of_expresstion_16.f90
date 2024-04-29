!write a FORTRAN program to print the product of an expression such as (b/r)(2b/(r+a)^2)(3b/(r+2a)^3).....upto n terms
!program 16


program product_of_expresstion_R_16
IMPLICIT NONE 

integer :: n, a, b, r, i, value 
real :: product

print*, "Type the value of until which term you want to calculate the product(n) : "
read*, n

   do i=1, n
      value = i
      print*, i,"b", "/(r+", i-1 ,"a)^", i

   end do


print*, "Type the value of a, b, r: "
read*, a, b, r
  if((r==0 .and. a==0) .or. r==((1-n)*a)) THEN
    print*, "The equation is undefined!"
  else 
    product = 1.0 
    do i=1, n
       product = product *i*b/(r+(i-1)*a)**i 
    end do
	! the wanted resuslt 
    print*, product
   
   end if   

end program product_of_expresstion_R_16