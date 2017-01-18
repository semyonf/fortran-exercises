module Source_IO
   use Environment

   implicit none

   ! Структура данных для хранения строки
   type TextLine
      character(:, CH_), allocatable :: Characters
      type(TextLine), pointer        :: NormalNext => Null(), SortedNext => Null()
   end type TextLine

contains

   ! Чтение из файла
   function Read_Text(InputFile) result (List)

      intent(in) InputFile

      integer                 :: In
      character(*)            :: InputFile
      type(TextLine), pointer :: List

      open (file=InputFile, encoding=E_, newunit=In)
         List => Read_Text_Line(In)
      close (In)
   end function Read_Text

   ! Чтение строки
   recursive function Read_Text_Line(In) result(Line)

      intent(in) In

      integer, parameter      :: max_len = 1024
      integer                 :: IO, In
      type(TextLine), pointer :: Line
      character(max_len, CH_) :: Characters

      ! Чтение символов во временный массив символов бОльшей длины
      read (In, "(a)", iostat=IO) Characters
      call Handle_IO_Status(IO, "reading line from file")
      if (IO == 0) then
         allocate(Line)
         ! Хранение в размещаемом поле массива символов без лишних пустых символов
         Line%Characters = Trim(Characters)
         Line%NormalNext => Read_Text_Line(In)
      else
         Line => Null()
      end if
   end function Read_Text_Line

   ! Вывод в файл
   subroutine Output_Source_Code(OutputFile, List)

      intent(in) OutputFile, List

      character(*)   :: OutputFile
      type(TextLine) :: List
      integer        :: Out

      open (file=OutputFile, encoding=E_, newunit=Out)
         call Output_Source_Line(Out, List)
      close (Out)
   end subroutine Output_Source_Code

   ! Вывод строки исходного текста
   recursive subroutine Output_Source_Line(Out, List)

      intent(in) Out, List

      integer        :: Out, IO
      type(TextLine) :: List

      write (Out, "(a)", iostat=IO) List%Characters
      call Handle_IO_Status(IO, "writing line to file")
      if (Associated(List%NormalNext)) &
         call Output_Source_Line(Out, List%NormalNext)
   end subroutine Output_Source_Line
end module Source_IO
