program lab_1_2
   use Environment

   implicit none
   integer, parameter      :: N_RECORDS = 15, L_NAME = 15, L_POSITION = 15
   character(kind=CH_)     :: names(N_RECORDS, L_NAME), positions(N_RECORDS, L_POSITION), types(N_RECORDS, L_POSITION)
   integer                 :: In, Out, occurrences(N_RECORDS, 1)
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"

   ! Чтение списка сотрудников
   call ReadEmployeeList(input_file, names, positions)

   ! Вывод списка сотрудников и количества профессий
   call WriteEmployeeList(output_file, names, positions)

   ! Поиск повторившихся профессий
   call ProcessPositions(positions, types, occurrences, N_RECORDS)

   ! Вывод профессий и их вхождений
   call WritePositionsOccured(output_file, types, occurrences)

contains

   ! Подпроцесс для поиска и подсчета повторений профессий
   pure subroutine ProcessPositions(positions, types, occurrences, size)
      implicit none

      character(kind=CH_)  :: positions(:,:), types(:,:)
      integer              :: occurrences(:,:), size
      intent(in)  positions, size
      intent(out) types, occurrences

      integer                          :: N_unique, i, duplicates
      logical, allocatable             :: repeated(:)
      character(kind=CH_), allocatable :: newPositions(:,:)

      newPositions = positions
      allocate(repeated(size))
      repeated = .false.

      do while (.not. all(repeated))
         N_unique = N_unique + 1
         types(N_unique, :) = newPositions(1, :)
         occurrences(N_unique,1) = count([(all(types(N_unique,:) == positions(i,:)), i = 1, N_RECORDS)])
         repeated = repeated .or. [(all(types(N_unique,:) == positions(i,:)), i = 1, N_RECORDS)]
         do i = 1, N_RECORDS
            if (repeated(i) .eqv. .true.) then
               duplicates = duplicates + 1
            else
               newPositions(i - duplicates,:) = positions(i,:)
            endif
         enddo
         duplicates = 0
      enddo

   end subroutine ProcessPositions

   ! Подпроцесс для записи списка профессий
   subroutine WritePositionsOccured(output_file, types, occurrences)
      implicit none

      character(*)        output_file
      character(kind=CH_) types(:,:)
      integer             occurrences(:, :)
      intent(in)  output_file, types, occurrences

      integer :: Out, IO, i
      character(:), allocatable :: format

      format = '(' // L_POSITION // 'a1, I3)'

      open (file=output_file, encoding=E_, newunit=Out)
         write(Out, format, iostat=IO) (types(i,:), occurrences(i,1), i = 1, count(occurrences /= 0))
         call Handle_IO_status(IO, 'WritePositionsOccured')
      close (Out)

   end subroutine WritePositionsOccured

   ! Подпроцесс для чтения списка сотрудников
   subroutine ReadEmployeeList(input_file, names, positions)
      implicit none

      character(*) input_file
      character(kind=CH_) names(:,:), positions(:,:)
      intent(in)  input_file
      intent(out) names, positions

      integer :: In, IO, i
      character(:), allocatable :: format

      format = '(' // L_NAME // 'a1, ' // L_POSITION // 'a1)'

      open (file=input_file, encoding=E_, newunit=In)
         read(In, format, iostat=IO) (names(i,:), positions(i,:), i = 1, N_RECORDS)
         call Handle_IO_status(IO, 'ReadEmployeeList')
      close (In)

   end subroutine ReadEmployeeList

   ! Подпроцесс для записи списка сотрудников
   subroutine WriteEmployeeList(output_file, names, positions)
      implicit none

      character(*) output_file
      character(kind=CH_) names(:,:), positions(:,:)
      intent(in)  output_file, names, positions

      integer :: Out, IO, i
      character(:), allocatable ::format

      format = '(' // L_NAME // 'a1, ' // L_POSITION // 'a1)'

      open (file=output_file, encoding=E_, newunit=Out)
         write(Out, format, iostat=IO) (names(i,:), positions(i,:), i = 1, N_RECORDS)
         call Handle_IO_status(IO, 'WriteEmployeeList')
      close (Out)
   end subroutine WriteEmployeeList

end program lab_1_2
