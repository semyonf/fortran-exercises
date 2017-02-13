program lab_3
   use Environment
   use m_io
   use m_sorting
   use m_common

   implicit none

   character(:), allocatable :: F1, F2, F3
   type(LineStruct), pointer :: List => Null()

   ! Входной текстовый файл
   F1 = "../data/F1.txt"
   ! Текстовый файл в естественном порядке строк
   F2 = "F2.txt"
   ! Текстовый файл в отсортированном порядке строк
   F3 = "F3.txt"

   List => Read_LineStruct(F1)

   if (Associated(List)) then
      call Process(List)
      call Output_To_File(F2, List, .false.)
      call Output_To_File(F3, List, .true.)
      call DeleteFromMemory(List)
   endif

end program lab_3
