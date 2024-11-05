program area_perimeter
IMPLICIT NONE
real ::x1,y1,x2,y2,x3,y3,area,a,b,c,s,perimeter
integer :: serial_num

Do
   print*, "Enter the serial number: "
   read*, serial_num
    if (serial_num == 0) EXIT
   print*, "Type the value of x1, y1 : "
   read*, x1,y1
   print*, "Type the value of x2, y2 : "
   read*, x2,y2
   print*, "Type the value of x3, y3 : "
   read*, x3,y3
a = sqrt((x1-x2)**2+(y1-y2)**2)
b = sqrt((x1-x3)**2+(y1-y3)**2)
c = sqrt((x2-x3)**2+(y2-y3)**2)

!now we have to check the necessary condition to form a triangle
   if ((a+b)>c .and. (b+c)>a .and. (a+c)>b ) THEN
   s = (a+b+c)/2.0
   area = sqrt(s*(s-a)*(s-b)*(s-c))
   perimeter = a+b+c
   print 10, area, perimeter
   10 format("The area of the triangle is: ",f6.2,"perimeter : ",f5.3)
   !print*, "The perimeter of the triangle is: ",perimeter
   else 
print*, "Condition is not satisfy so triangle will not form"
   end if 
end do

end program area_perimeter
 

