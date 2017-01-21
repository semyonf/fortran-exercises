module Source_IO
   use Environment

   implicit none

   ! Костыль для создания массива указателей
   type NextPtr
      type(LineStruct), pointer :: p => Null()
   end type NextPtr

   ! Структура данных для хранения текста
   type LineStruct
      character(:, CH_), allocatable :: Characters
      type(NextPtr)                  :: Next(2)
   end type LineStruct

contains


   ! Установить указатель к Н-ному элементу по порядку
   recursive subroutine Set_Ptr_To_Nth_Of(Ptr, Counter, List)
      intent(in)    List
      intent(out)   Ptr
      intent(inout) Counter

      type(LineStruct), pointer :: List
      type(LineStruct), pointer :: Ptr
      integer                   :: Counter

      if (Counter /= 1) then
         Counter = Counter - 1
         call Set_Ptr_To_Nth_Of(Ptr, Counter, List%Next(1)%p)
      else
         Ptr => List
      endif
   end subroutine Set_Ptr_To_Nth_Of

! ---------Формирование массива длин---------

   ! Подсчет строк в тексте List
   pure recursive subroutine Count_Elements_In(List, Size)
      intent(in) List
      intent(inout) Size

      type(LineStruct) :: List
      integer    :: Size

      Size = Size + 1

      if (Associated(List%Next(1)%p)) &
         call Count_Elements_In(List%Next(1)%p, Size)
   end subroutine Count_Elements_In

   ! Составление массива длин строк текста List
   pure function Form_Lengths_Of(List) result(Lengths)
      intent(in) List

      type(LineStruct)     :: List
      integer              :: Size
      integer, allocatable :: Lengths(:)

      Size = 0
      call Count_Elements_In(List, Size)
      allocate(Lengths(Size))

      call Count_Lengths(List, Lengths, Size)
   end function Form_Lengths_Of

   ! Подсчет длин строк в тексте List
   pure recursive subroutine Count_Lengths(List, Lengths, IterationsLeft)
      intent(in) List
      intent(inout) IterationsLeft, Lengths

      type(LineStruct)           :: List
      integer, allocatable :: Lengths(:)
      integer              :: IterationsLeft

      Lengths(1 + Size(Lengths) - IterationsLeft) = len(List%Characters)

      IterationsLeft = IterationsLeft - 1
      if (IterationsLeft /= 0) &
         call Count_Lengths(List%Next(1)%p, Lengths, IterationsLeft)
   end subroutine Count_Lengths

   ! Перемотка списка к минимальной строчке
   function Rewind_To_Shortest(List) result (ShortestPtr)
      intent(in) List

      type(LineStruct), pointer :: List, ShortestPtr
      integer                   :: Shortest

      Shortest = minloc(Form_Lengths_Of(List), dim=1)

      call Set_Ptr_To_Nth_Of(ShortestPtr, Shortest, List)

   end function Rewind_To_Shortest

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
         List => Rewind_To_Shortest(List)
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
end module Source_IO
