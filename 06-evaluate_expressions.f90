program evaluate_expressions
IMPLICIT NONE
real ::a,b,c,d,r,x,u,v
integer :: serial_num

do
print*, "Enter the serial number: "
read*, serial_num
if (serial_num == 0) EXIT

print*, "Type the value of a, b, r, d :" 
read*, a, b, d, r
if (r==d .and. b==0) THEN
print*, "the equation undefine"

else 
u =	abs(a+(1/b))/sin(r-d)
print*, "value of U is : ",u 
print*, "Type the value of c, x :	"
read*, c, x
v = (c*(1/u)-u*cos(x))/b
print*, "value of V is : ",v

end if
end do 

end program evaluate_expressions
