module m_io
   use Environment
   use m_common

   implicit none

contains

   ! Чтение из файла
   function Read_LineStruct(InputFile) result (List)
      intent(in) InputFile

      integer             :: In
      character(*)        :: InputFile
      type(LineStruct), pointer :: List

      open (file=InputFile, encoding=E_, newunit=In)
         List => Read_LineStruct_Line(In)
      close (In)
   end function Read_LineStruct

   ! Чтение строки
   recursive function Read_LineStruct_Line(In) result(Line)
      intent(in) In

      integer, parameter      :: max_len = 1024
      integer                 :: IO, In
      type(LineStruct), pointer :: Line
      character(max_len, CH_) :: Characters

      ! Чтение символов во временный массив символов бОльшей длины
      read (In, "(a)", iostat=IO) Characters
      call Handle_IO_Status(IO, "reading line from file")
      if (IO == 0) then
         allocate(Line)
         ! Хранение в размещаемом поле массива символов без лишних пустых символов
         Line%Characters = Trim(Characters)
         Line%Next(1)%p => Read_LineStruct_Line(In)
      else
         Line => Null()
      end if
   end function Read_LineStruct_Line

   ! Вывод в файл
   subroutine Output_To_File(OutputFile, List, Sorting)
      intent(in) :: OutputFile, Sorting
      intent(out) :: List

      character(*)              :: OutputFile
      type(LineStruct), pointer :: List
      integer                   :: Out
      logical                   :: Sorting

      if (Sorting) then
         ! Перемотать к минимальной строчке
         call Rewind_To_Shortest(List)
      endif

      open (file=OutputFile, encoding=E_, newunit=Out)
         write(Out,*) '-------START-------'
         call Output_Line(Out, List, Sorting)
         write(Out,*) '--------END--------'
      close (Out)
   end subroutine Output_To_File

   ! Вывод строки исходного текста
   recursive subroutine Output_Line(Out, List, Sorting)
      intent(in) Out, List, Sorting

      integer    :: Out, IO
      type(LineStruct) :: List
      logical    :: Sorting

      write (Out, "(a)", iostat=IO) List%Characters

      call Handle_IO_Status(IO, "writing line to file")
      if (Sorting) then
         if (Associated(List%Next(2)%p)) &
            call Output_Line(Out, List%Next(2)%p, Sorting)
      else
         if (Associated(List%Next(1)%p)) &
            call Output_Line(Out, List%Next(1)%p, Sorting)
      endif
   end subroutine Output_Line
end module m_io
