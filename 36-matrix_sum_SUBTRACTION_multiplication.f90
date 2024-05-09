!problem : Write a FORTRAN program, which reads two matrix and print their Sum, Subtraction and Multiplication in a matrix form
!problem 36



program matrix_sum_SUBTRACTION_multiplication
IMPLICIT NONE

integer, ALLOCATABLE , DIMENSION(:,:) :: a, b, c	 ! :,: aytar mane hoilo a,b,c 2 ta kore value nibe.
INTEGER :: a_rows, a_cols, b_rows, b_cols, c_rows, c_cols, i, j ,k , serial_num 

   do 
  	  print*, "Enter the serial Number: "
	  read*, serial_num 
	  if(serial_num == 0) EXIT
	     print*, "Enter the value of how many Rows & Colum in A matrix : "
	     read*, a_rows, a_cols 

		 print*, "Enter the value of how many Rows & Colum in B matrix : "
	     read*, b_rows, b_cols

		 c_rows = a_rows	 ! value assign
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

			 ! C matrix ortat jeta resultant matrix oii tar value ghula 0 kore nilam jeno compilar andaje value dore nite na pare 
		    do i = 1, c_rows
		    	do j = 1, c_cols
				   c(i, j) = 0
				end do
			end do

			! code for SUM


			   if((a_rows==b_rows) .AND. (a_cols==b_cols)) THEN
                    do i = 1, a_rows
		               do j = 1, a_cols
		                  c(i,j) = a(i,j)+b(i,j)
		               end do
	             	end do
	            print*, "sum of the two matrix is : " 

                 do i = 1, a_rows
                    print*, (c(i,j), j= 1, a_cols)

                 end do
              else 
                print*, "aha ! its not posible to sum in between this two matrix. sorry yeear . try again plzzz"
              end if 


			  ! code for SUBTRACTION 

			  			   if((a_rows==b_rows) .AND. (a_cols==b_cols)) THEN
                             do i = 1, a_rows
		                       do j = 1, a_cols
		                         c(i,j) = a(i,j)-b(i,j)
		                       end do
	                       	 end do
	                       print*, "subtraction of the two matrix is : " 

                        do i = 1, a_rows
                           print*, (c(i,j), j= 1, a_cols)

                        end do
                           else 
                              print*, "aha ! its not posible to subtraction in between this two matrix. sorry yeear . try again plzzz"
                        end if 



	     !two matrix will be multiflicable when 1st matrix colums and 2nd matrix rows are equal. so we will give a condition that

			if (a_cols == b_rows) THEN 

			 !c = 0	     ! deken C matrix jeta ase oii ta 2 babe 0 kora jabe. do loop chalai korlam ak babe or  c= 0 assin kore dilam
		    	
				!matrix multiplication er janno 3 bar loop cholbe	
				!loop er modde loop cholle ta ke bole (nested loop)
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

	  deallocate(a)		  ! deken, jodi deallocate na kori taile agee je a er array value ghula dicilam oii ghula problem kore ay janno kisu allocate korle ta de deallocate kore dite hobe. 
	  deallocate(b)		  ! sundor na deallocate er bisoita?!
	  deallocate(c)
			     

   end do 



end program