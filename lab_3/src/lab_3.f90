program lab_3
   use Environment
   use Source_IO
   use Source_Process

   implicit none

   character(:), allocatable :: F1, F2, F3
   type(TextLine), pointer   :: List => Null()

   ! Входной текстовый файл
   F1 = "../data/F1.txt"
   ! Текстовый файл в естественном порядке строк
   F2 = "F2.txt"
   ! Текстовый файл в отсортированном порядке строк
   F3 = "F3.txt"

   List => Read_Text(F1)

   write(*,*) Form_Lengths_Of(List)

   if (Associated(List)) then
      call Output_Source_Code(F2, List)
      List => Add_Sorting_To(List)
      call Output_Source_Code(F3, List)
   endif

end program lab_3
