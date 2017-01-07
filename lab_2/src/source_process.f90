module Source_Process
   use Environment
   use Source_IO

   implicit none

contains

   pure function Move_Lines(Original, First, Last, K) result(Result)

      intent(in) &
         First, Last, K, Original

      integer                 :: First, Last, K
      type(TextLine)          :: Original
      type(TextLine), pointer :: Block, Prepared, Result

      Block    => Get_Cut_Lines(.false., Original, First, Last, 1)    ! Получили переносимые строки
      Prepared => Get_Cut_Lines(.true.,Original, First, Last, 1)      ! Удалили переносимые строки с их мест
      Result   => Paste_Lines(Prepared, Block, K - (Last - First), 1) ! Вставили строки на требуемое место
   end function Move_Lines

   ! Функция для получения вырезаемых строк (или наоборот, НЕвырезаемых)
   pure recursive function Get_Cut_Lines(Inverted, Source, Start, End, Current) result(Lines)

      intent(in) &
         Source, Inverted, Start, End, Current

      type(TextLine) :: Source
      logical        :: Inverted
      integer        :: Start, End, Current

      type(TextLine), pointer :: Lines

      allocate (Lines)
      if (Inverted) then
         if (Start <= Current .and. Current <= End) then
            Lines => Get_Cut_Lines(Inverted, Source%Next, Start, End, Current + 1)
         else
            Lines%String = Source%String
            if (Associated(Source%Next)) then
               Lines%Next => Get_Cut_Lines(Inverted, Source%Next, Start, End, Current + 1)
            endif
         endif
      else
         if (Start <= Current) then
            Lines%String = Source%String
            if (Current <= End .and. Associated(Source%Next)) then
               Lines%Next => Get_Cut_Lines(Inverted, Source%Next, Start, End, Current + 1)
            endif
         else
            if (Associated(Source%Next)) then
               Lines => Get_Cut_Lines(Inverted, Source%Next, Start, End, Current + 1)
            endif
         endif
      endif
   end function Get_Cut_Lines

   ! Вставка строк в указанное место
   pure recursive function Paste_Lines(Prepared, Block, Position, Current) result(Final)

      intent(in) &
         Prepared, Block, Position, Current

      type(TextLine) :: Prepared, Block
      integer        :: Position, Current

      type(TextLine), pointer :: Final

      allocate (Final)
      if (Current < Position .or. .not. Associated(Block%Next)) then
         Final%String = Prepared%String
         if (Associated(Prepared%Next)) &
            Final%Next => Paste_Lines(Prepared%Next, Block, Position, Current + 1)
      else
         Final%String = Block%String
         Final%Next => Paste_Lines(Prepared, Block%Next, Position, Current + 1)
      endif
   end function Paste_Lines

end module Source_process
