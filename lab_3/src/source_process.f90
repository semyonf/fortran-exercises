module Source_Process
   use Environment
   use Source_IO

   implicit none

contains

   ! test
   pure subroutine Process(Original)
      intent(inout) :: Original

      type(Text)           :: Original
      integer, allocatable :: Lengths(:)
      logical, allocatable :: NotChecked(:)

      Lengths = Form_Lengths_Of(Original)
      allocate(NotChecked(Size(Lengths)))
      NotChecked = .true.

   end subroutine Process

   pure recursive subroutine Set_Pointer(Original, A, B)
      intent(inout) Original
      intent(in) A, B

      type(Text) :: Original
      integer    :: A, B



   end subroutine Set_Pointer

   pure recursive subroutine Sort_Text(Original, Lengths, NotChecked)
      intent(inout) Original, Lengths, NotChecked

      type(Text)           :: Original
      integer, allocatable :: Lengths(:)
      logical, allocatable :: NotChecked(:)
      integer              :: MinA, MinB

      MinA = minloc(Lengths, dim=1, mask=NotChecked)
      NotChecked(MinA) = .false.
      MinB = minloc(Lengths, dim=1, mask=NotChecked)

      call Set_Pointer(Original, MinA, MinB)

      if (any(NotChecked)) then
         call Sort_Text(Original, Lengths, NotChecked)
      endif
   end subroutine Sort_Text

   ! Подсчет строк в тексте Original
   ! РАБОТАЕТ
   pure recursive subroutine Count_Elements_In(Original, Size)
      intent(in) Original
      intent(inout) Size

      type(Text) :: Original
      integer    :: Size

      Size = Size + 1

      if (Associated(Original%Next(1)%p)) &
         call Count_Elements_In(Original%Next(1)%p, Size)
   end subroutine Count_Elements_In

   ! Составление массива длин строк текста Original
   ! РАБОТАЕТ
   pure function Form_Lengths_Of(Original) result(Lengths)
      intent(in) Original

      type(Text)           :: Original
      integer              :: Size
      integer, allocatable :: Lengths(:)

      Size = 0
      call Count_Elements_In(Original, Size)
      allocate(Lengths(Size))

      call Count_Lengths(Original, Lengths, Size)
   end function Form_Lengths_Of

   ! Подсчет длин строк в тексте Original
   ! РАБОТАЕТ
   pure recursive subroutine Count_Lengths(Original, Lengths, IterationsLeft)
      intent(in) Original
      intent(inout) IterationsLeft, Lengths

      type(Text)           :: Original
      integer, allocatable :: Lengths(:)
      integer              :: IterationsLeft

      Lengths(1 + Size(Lengths) - IterationsLeft) = len(Original%Characters)

      IterationsLeft = IterationsLeft - 1
      if (IterationsLeft /= 0) &
         call Count_Lengths(Original%Next(1)%p, Lengths, IterationsLeft)
   end subroutine Count_Lengths

   ! ! test
   ! pure recursive subroutine Testing(Original)
   !    intent(inout) :: Original

   !    type(Text) :: Original

   !    Original%Next(2)%p => Original%Next(1)%p

   !    if (Associated(Original%Next(1)%p%Next(1)%p)) &
   !       call Testing(Original%Next(1)%p)
   ! end subroutine Testing


   ! subroutine xxx(Original)
   !    intent(inout) :: Original

   !    type(Text) :: Original

   !    Original%Characters = '123'
   !    Original%Next(2)%p => Original%Next(1)%p%Next(1)%p

   ! end subroutine xxx


end module Source_process
