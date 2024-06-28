!Write a FORTRAN program to solve a quadratic equation ax^2+bx^2+cx+d=0 and print 
 !the roots(real or complex) by normally




program quadraric_equation_normally
IMPLICIT NONE
real :: a,b,c,d,discri,f_root,s_root, real_p, imag
integer :: serial_num

do 
   print*, "Enter the serial Number: "
   read*, serial_num
   if(serial_num == 0) EXIT
 
   print*, "Enter the value of quadratic coefficient: a, b: linear coefficient: c constant coefficient: d : "
   read*, a,b,c,d 
    if ((a+b)==0)then 
    print*, "This is not a quadratic eqn: "
     else
    discri = c**2-4*(a+b)*d
    if (discri > 0) THEN
    f_root = (-c + sqrt(discri))/(2*(a+b))
    s_root = (-c - sqrt(discri))/(2*(a+b))
    print*, "the root are real and unequal"

   print 72, f_root,s_root
   72 format(2X, "first root : ",f9.4 , "    second root: ",f9.4)
    
   else if (discri == 0) THEN
   f_root = (-c + sqrt(discri))/(2*(a+b))
   print*, "The roots are real and equal: " , f_root

   else 
   print*, "the roots are complex: "
   real_p = -c/(2*(a+b))
   imag = sqrt(abs(discri))/(2*(a+b))
   print 66, real_p,imag,real_p,imag
   66 format(2X, "first root: ", f9.4, "+i" , f9.4, "second root: ", f9.4, "+i" , f9.4)
  end if 
  end if 
end do

end program   