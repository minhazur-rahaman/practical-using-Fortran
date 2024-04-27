program area_perimeter_of_rectangle
IMPLICIT NONE
real ::x1,y1,x2,y2,x3,y3,x4,y4,area,a,b,c,d,e,f,perimeter
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
    print*, "Type the value of x4, y4 : "
    read*, x4,y4
a = sqrt((x1-x2)**2+(y1-y2)**2)
b = sqrt((x2-x3)**2+(y2-y3)**2)
c = sqrt((x3-x4)**2+(y3-y4)**2)
d = sqrt((x1-x4)**2+(y1-y4)**2)

e = sqrt((x1-x3)**2+(y1-y3)**2)
f = sqrt((x2-x4)**2+(y2-y4)**2)


!now we have to check the necessary condition to form a triangle
 if (a==c .and. b==d .and. e==f) THEN
 area = a*b
 perimeter = 2.0*(a+b)
!here we use formate sothat how many digit will print after decimal point 
 print 10, area
 10 format("The area of the triangle is: ",f6.2)
 print 12, perimeter
 12 format("The perimeter of the triangle is: ",f6.3)
 else 
 print*, "Condition is not satisfy so rectangle will not form"
 end if 
end do

end program area_perimeter_of_rectangle
