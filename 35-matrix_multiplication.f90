!problem : Write a FORTRAN program, which reads two matrix and print their Multiplication in a matrix form
!problem 35



program matrix_multiplication
IMPLICIT NONE

integer, ALLOCATABLE , DIMENSION(:,:) :: a, b, c
INTEGER :: a_rows, a_cols, b_rows, b_cols, c_rows, c_cols, i, j ,k , serial_num 

   do 
  	  print*, "Enter the serial Number: "
	  read*, serial_num 
	  if(serial_num == 0) EXIT
	     print*, "Enter the value of how many Rows & Colum in A matrix : "
	     read*, a_rows, a_cols 

		 print*, "Enter the value of how many Rows & Colum in B matrix : "
	     read*, b_rows, b_cols

		 c_rows = a_rows
		 c_cols	= b_cols
		 
		 Allocate(a(a_rows, a_cols),b(b_rows, b_cols),c(c_rows, c_cols)) 

		 print*, "Enter the elements of A matrix in Rowwise :"
		 read*, ((a(i,j), j = 1, a_cols), i = 1, a_rows)

		 print*, "Enter the elements of B matrix in Rowwise :"
		 read*, ((b(i,j), j = 1, b_cols), i = 1, b_rows)

		 

		 print*, "The matrix from of A is :"
		   do i = 1, a_rows
		      print*, (a(i,j), j = 1, a_cols)
		   end do
		 !print*, ((a(i,j), j = 1, a_cols), i = 1, a_rows)	       !ay babe ken je matrix ta print hocce na bujteci na. 

		 print*, "The matrix from of B is :"
		    do i = 1, b_rows
		       print*, (b(i,j), j = 1, b_cols)
		    end do

			 !print*, ((b(i,j), j = 1, b_cols), i = 1, b_rows)


		    do i = 1, c_rows
		    	do j = 1, c_cols
				   c(i, j) = 0
				end do
			end do


			  



	     !two matrix will be multiflicable when 1st matrix colums and 2nd matrix rows are equal. so we will give a condition that

			if (a_cols == b_rows) THEN 

			 c = 0	     ! deken C matrix jeta ase oii ta 2 babe 0 kora jabe. do loop chalai korlam ak babe or  c= 0 assin kore dilam
		    	
				!matrix multiplication er janno 3 bar loop cholbe
		      	do k = 1, c_rows
			    	 do i = 1, b_cols
					     do j = 1, a_cols
						    c(k,i) = c(k,i) + a(k,j)*b(j,i)
						 end do
					end do
				end do


			  print*, "The Multiplication result of A & B: "

			  do i = 1, c_rows
			     print*, (c(i,j), j = 1, c_cols)
			  end do

			  else 
			   	 print*, "Sorry its not posiable to multipli between A & B matrix! "

			 end if 

		deallocate(a)
		deallocate(b)
		deallocate(c)
			     

   end do 



end program 