module Source_IO
   use Environment

   implicit none

   ! Структура данных для хранения строки текста
   type TextLine
      character(:, CH_), allocatable :: Characters
      type(TextLine), pointer        :: NormalNext => Null(), SortedNext => Null()
   end type TextLine

contains

   ! Чтение текста из файла
   function Read_Text(InputFile) result (Text)

      intent(in) InputFile

      integer                 :: In
      character(*)            :: InputFile
      type(TextLine), pointer :: Text

      open (file=InputFile, encoding=E_, newunit=In)
         Text => Read_Text_Line(In)
      close (In)
   end function Read_Text

   ! Чтение строки текста
   recursive function Read_Text_Line(In) result(Line)

      intent(in) In

      integer, parameter      :: max_len = 1024
      integer                 :: IO, In
      type(TextLine), pointer :: Line
      character(max_len, CH_) :: Characters

      ! Чтение строки во временную строку бОльшей длины
      read (In, "(a)", iostat=IO) Characters
      call Handle_IO_Status(IO, "reading line from file")
      if (IO == 0) then
         allocate (Line)
         ! Хранение в размещаемом поле символов без завершающих пробелов
         Line%Characters = Trim(Characters)
         Line%NormalNext => Read_Text_Line(In)
      else
         Line => Null()
      end if
   end function Read_Text_Line

   ! Вывод исходного текста
   subroutine Output_Source_Code(OutputFile, Text)

      intent(in) OutputFile, Text

      character(*)   :: OutputFile
      type(TextLine) :: Text
      integer        :: Out

      open (file=OutputFile, encoding=E_, newunit=Out)
         call Output_Source_Line(Out, Text)
      close (Out)
   end subroutine Output_Source_Code

   ! Вывод строки исходного текста
   recursive subroutine Output_Source_Line(Out, Text)

      intent(in) Out, Text

      integer        :: Out, IO
      type(TextLine) :: Text

      write (Out, "(a)", iostat=IO) Text%Characters
      call Handle_IO_Status(IO, "writing line to file")
      if (Associated(Text%NormalNext)) &
         call Output_Source_Line(Out, Text%NormalNext)
   end subroutine Output_Source_Line
end module Source_IO
