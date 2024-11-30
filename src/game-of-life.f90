module game_of_life
  use raylib
  implicit none
  private

  public :: init_area, render_grid, key_events_handler
  integer, allocatable :: cells(:,:)
  integer :: cell_size = 5
  integer :: tick_rate = 1
  logical :: is_tick_rate_changed = .false.
  real :: tick_rate_display_accumulator = 0.0
  real :: accumulator = 0.0

contains
  subroutine init_area(width, height)
    integer :: width, height, cx, cy, i, j, rand_id, count_alive = 0
    real :: rand_v

    cx = width / cell_size
    cy = height / cell_size


    allocate(cells(cx, cy))

    ! print *, cx, cy, size(cells, 1), size(cells, 2)
    do i = 1, size(cells, 1)
      do j = 1, size(cells, 2)
        call random_number(rand_v)
        rand_id = rand_v * 101
        ! if (rand_id < 10 .and. count_alive < 50) then
        if (rand_id < 50) then
          cells(i, j) = 1
          count_alive = count_alive + 1
        else 
          cells(i, j) = 0
        endif
      end do
    end do
    call evaluate
  end subroutine init_area

  subroutine render_grid()
    integer :: i, j, n_cell_s, rand_id, rposx = 0, rposy = 0
    real :: delta, update_at, rand_v
    character(len=255) :: str
    delta = get_frame_time()
    rposx = 0
    rposy = 0
    do i = 1, size(cells, 2)
      do j = 1, size(cells, 1)
        if (cells(j, i) == 1) then
          call draw_rectangle(rposx + 1, rposy, cell_size - 2, cell_size - 2, BLACK)
        else
          call draw_rectangle(rposx + 1, rposy, cell_size - 2, cell_size - 2, LIGHTGRAY)
        end if
        rposx = rposx + cell_size
      end do
      rposy = rposy + cell_size
      rposx = 0
    end do


    if (tick_rate /= 0) then
      accumulator = accumulator + delta
      update_at = 1 / tick_rate
      if (accumulator > update_at) then
        accumulator = accumulator - update_at
        call evaluate
      end if
    else
      accumulator = 0.0
    end if
  
    if (is_tick_rate_changed .eqv. .true.) then
      tick_rate_display_accumulator = tick_rate_display_accumulator + delta
      if (tick_rate_display_accumulator > 3) then
        is_tick_rate_changed = .false.
        tick_rate_display_accumulator = 0.0
      end if
      write(str, '(I10)') tick_rate
      call draw_text("Tick Rate: " // str, 10, 10, 24, BLACK)
    end if
  end subroutine render_grid


  subroutine evaluate()
    integer, dimension(size(cells, 1), size(cells, 2)) :: new_cells
    integer :: neighbor_count = 0, i, j

    do i = 1, size(cells, 1)
      do j = 1, size(cells, 2)
        ! top left
        if ((i-1 > 0) .and. (j-1 > 0)) then
          if (cells(i-1, j-1) == 1) then
            neighbor_count = neighbor_count + 1
          endif
        endif

        ! top
        if (i-1 > 0) then
          if (cells(i-1, j) == 1) then
            neighbor_count = neighbor_count + 1
          endif
        endif

        ! top right
        if ((i-1 > 0) .and. (j+1 <= size(cells, 2))) then
          if (cells(i-1, j+1) == 1) then
            neighbor_count = neighbor_count + 1
          endif
        endif

        ! right
        if (j+1 <= size(cells, 2)) then
          if (cells(i, j+1) == 1) then
            neighbor_count = neighbor_count + 1
          endif
        endif

        ! bottom right
        if ((i+1 <= size(cells, 1)) .and. (j+1 <= size(cells, 2))) then
          if (cells(i+1, j+1) == 1) then
            neighbor_count = neighbor_count + 1
          endif
        endif

        ! bottom
        if (i+1 <= size(cells, 1)) then
          if (cells(i+1, j) == 1) then
            neighbor_count = neighbor_count + 1
          endif
        endif

        ! bottom left
        if ((i+1 <= size(cells, 1)) .and. (j-1 > 0)) then
          if (cells(i+1, j-1) == 1) then
            neighbor_count = neighbor_count + 1
          endif
        endif

        ! left
        if (j-1 > 0) then
          if (cells(i, j-1) == 1) then
            neighbor_count = neighbor_count + 1
          endif
        endif

        if (cells(i, j) == 1) then
          if (neighbor_count < 2) then
            new_cells(i, j) = 0
          else if (neighbor_count == 2 .or. neighbor_count == 3) then
            new_cells(i, j) = 1
          else if (neighbor_count > 3) then
            new_cells(i, j) = 0
          end if
        else
          if (neighbor_count == 3) then
            new_cells(i, j) = 1
          end if
        end if
        neighbor_count = 0
      end do
    end do
    cells = new_cells
  end subroutine evaluate

  subroutine key_events_handler()
    if (is_key_pressed(KEY_RIGHT)) then
      if (tick_rate < 24) then
        tick_rate = tick_rate + 1
      end if
      is_tick_rate_changed = .true.
    end if

    if (is_key_pressed(KEY_LEFT)) then
      if (tick_rate >= 1) then
        tick_rate = tick_rate - 1
      end if
      is_tick_rate_changed = .true.
    end if
  end subroutine key_events_handler

end module game_of_life