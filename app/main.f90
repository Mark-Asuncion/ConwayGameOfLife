program main
    use, intrinsic :: iso_c_binding, only: c_null_char
    use :: raylib
    use :: game_of_life
    implicit none (type, external)

    integer, parameter :: SCREEN_WIDTH  = 1280
    integer, parameter :: SCREEN_HEIGHT = 720

    ! setup
    call random_seed()
    call init_window(SCREEN_WIDTH, SCREEN_HEIGHT, 'Game of Life' // c_null_char)
    call set_target_fps(30)
    call init_area(SCREEN_WIDTH, SCREEN_HEIGHT)

    do while (.not. window_should_close())
        call begin_drawing()
            call clear_background(RAYWHITE)
            call key_events_handler()
            call render_grid()
        call end_drawing()
    end do

    call close_window()
end program main
