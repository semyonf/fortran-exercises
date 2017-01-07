module Source_Process
   use Environment
   use Source_IO

   implicit none

contains

   pure function Move_Lines(Original, First, Last, K) result(Ressie)
      integer, intent(in)        :: First, Last, K
      type(TextLine), intent(in) :: Original
      type(TextLine), pointer    :: Block, Prepared, Ressie

      Block => Form_Block(Original, First, Last, 1)   ! Получили переносимые строки
      Prepared => Cut_Lines(Original, First, Last, 1) ! Удалили переносимые строки с их мест
      Ressie => Paste_Lines(Prepared, Block, K - (Last - First), 1)    ! Вставили строки на требуемое место


   end function Move_Lines

   ! Получение строк кроме тех, которые в диапазоне
   pure recursive function Cut_Lines(Original, Start, End, Current) result(Prepared)
      type(TextLine), intent(in)  :: Original
      integer, intent(in)         :: Start, End, Current

      type(TextLine), pointer :: Prepared

      allocate (Prepared)

      if (Start <= Current .and. Current <= End) then
         Prepared => Cut_Lines(Original%Next, Start, End, Current + 1)
      else
         Prepared%String = Original%String
         if (Associated(Original%Next)) then
            Prepared%Next => Cut_Lines(Original%Next, Start, End, Current + 1)
         endif
      endif
   end function Cut_Lines

   ! Получение строк из диапазона
   pure recursive function Form_Block(Original, Start, End, Current) result(Block)
      type(TextLine), intent(in)  :: Original
      integer, intent(in)         :: Start, End, Current

      type(TextLine), pointer :: Block

      allocate (Block)

      if (Start <= Current) then
         Block%String = Original%String
         if (Current < End .and. Associated(Original%Next)) then
            Block%Next => Form_Block(Original%Next, Start, End, Current + 1)
         endif
      else
         if (Associated(Original%Next)) then
            Block => Form_Block(Original%Next, Start, End, Current + 1)
         endif
      endif
   end function Form_Block

   ! Вставка строк в указанное место
   pure recursive function Paste_Lines(Prepared, Block, Position, Current) result(Final)
      type(TextLine), intent(in) :: Prepared, Block
      integer, intent(in)        :: Position, Current

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
