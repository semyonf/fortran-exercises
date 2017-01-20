program lab_3
   use Environment
   use Source_IO
   use Source_Process

   implicit none

   character(:), allocatable :: F1, F2, F3
   type(Text), pointer   :: Original => Null()

   ! Входной текстовый файл
   F1 = "../data/F1.txt"
   ! Текстовый файл в естественном порядке строк
   F2 = "F2.txt"
   ! Текстовый файл в отсортированном порядке строк
   F3 = "F3.txt"

   Original => Read_Text(F1)

   if (Associated(Original)) then
      call Output_To_File(F2, Original, .false.)
      ! call Add_Sorting_To(Original, Sorted)
      ! call Testing(Original)
      write(*,*) Form_Lengths_Of(Original)
      ! call xxx(Original)
      ! call Output_To_File(F2, Original, .false.)
      ! call Output_To_File(F3, Original, .true.)
   endif

end program lab_3
