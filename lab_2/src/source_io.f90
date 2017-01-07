module Source_IO
   use Environment

   implicit none

   ! Структура данных для хранения строки исходного текста
   type TextLine
      character(:, CH_), allocatable :: String
      type(TextLine), pointer        :: Next  => Null()
   end type TextLine

contains

   ! Получение значений из файла конфигурации
   subroutine Read_Config_File(ConfigFile, First, Last, K)
      character(*), intent(in)  :: ConfigFile
      integer,      intent(out) :: First, Last, K

      integer                   :: In

      open (file=ConfigFile, newunit=In)
         read(In, *) First, Last, K
      close (In)

      if (.not. (First <= Last .and. (K < First .or. K > Last))) then
         write(*,*) 'Incorrect settings'
         STOP
      endif
   end subroutine Read_Config_File

   ! Чтение исходного кода
   function Read_Source(InputFile) result (Source)
      character(*), intent(in) :: InputFile

      type(TextLine), pointer :: Source
      integer                 :: In

      open (file=InputFile, encoding=E_, newunit=In)
         Source => Read_Source_Line(In)
      close (In)
   end function Read_Source

   ! Чтение строки исходного кода
   recursive function Read_Source_Line(in) result(Code)
      type(TextLine), pointer :: Code
      integer, intent(in)       :: In
      integer, parameter        :: max_len = 1024
      character(max_len, CH_)   :: string
      integer                   :: IO

      ! Чтение строки во временную строку бОльшей длины
      read (In, "(a)", iostat=IO) string
      call Handle_IO_Status(IO, "reading line from source code")
      if (IO == 0) then
         allocate (Code)
         ! Хранение в размещаемом поле символов без завершающих пробелов
         Code%String = Trim(string)
         Code%Next => Read_Source_Line(In)
      else
         Code => Null()
      end if
   end function Read_Source_Line

   ! Вывод исходного кода
   subroutine Output_Source_Code(OutputFile, Code)
      character(*), intent(in)      :: OutputFile
      type(TextLine), intent(in)  :: Code
      integer  :: Out

      open (file=OutputFile, encoding=E_, newunit=Out)
         call Output_Source_Line(Out, Code)
      close (Out)
   end subroutine Output_Source_Code

   ! Вывод строки исходного кода
   recursive subroutine Output_Source_Line(Out, Code)
      integer, intent(in)           :: Out
      type(TextLine), intent(in)  :: Code
      integer  :: IO

      write (Out, "(a)", iostat=IO) Code%String
      call Handle_IO_Status(IO, "writing line to file")
      if (Associated(Code%next)) &
         call Output_Source_Line(Out, Code%next)
   end subroutine Output_Source_Line
end module Source_IO
