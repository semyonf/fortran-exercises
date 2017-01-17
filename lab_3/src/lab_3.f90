program lab_3
   use Environment
   use Source_IO
   use Source_Process

   implicit none

   character(:), allocatable :: F1, F2, F3

   type(TextLine), pointer :: Original => Null(), & ! Оригинальный текст
                              Normal   => Null(), & ! Естественный порядок строк
                              Sorted   => Null()    ! Сортированные строки

   ! Входной текстовый файл
   F1 = "../data/F1.txt"
   ! Текстовый файл в естественном порядке строк
   F2 = "F2.txt"
   ! Текстовый файл в отсортированном порядке строк
   F3 = "F3.txt"

   Original => Read_Text(F1)

   ! Временно
   Normal => Original
   Sorted => Original

   if (Associated(Original)) then
      call Output_Source_Code(F2, Normal)
      call Output_Source_Code(F3, Sorted)
   endif

end program lab_3
