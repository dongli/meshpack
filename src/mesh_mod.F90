! ------------------------------------------------------------------------------
! Express arbitrary types of mesh by a uniform data structure, but we should
! also consider not to degenerate the efficiency if the mesh is structured.
! ------------------------------------------------------------------------------

module mesh_mod

  implicit none

  private

  public mesh_type

  type mesh_type
    integer :: num_cell = 0
    integer :: cell_max_edges = 6
    real(8), allocatable, dimension(  :) :: cell_x
    real(8), allocatable, dimension(  :) :: cell_y
    real(8), allocatable, dimension(  :) :: cell_z
    real(8), allocatable, dimension(  :) :: cell_lon
    real(8), allocatable, dimension(  :) :: cell_lat
    real(8), allocatable, dimension(  :) :: cell_area
    real(8), allocatable, dimension(:,:) :: cell_distance
    integer, allocatable, dimension(  :) :: cell_edge_size
    integer, allocatable, dimension(:,:) :: cell_ngb_idx
    integer, allocatable, dimension(:,:) :: cell_node_idx
    integer, allocatable, dimension(:,:) :: cell_edge_idx

    integer :: num_node = 0
    integer :: node_max_cells = 3
    real(8), allocatable, dimension(  :) :: node_x
    real(8), allocatable, dimension(  :) :: node_y
    real(8), allocatable, dimension(  :) :: node_z
    real(8), allocatable, dimension(  :) :: node_lon
    real(8), allocatable, dimension(  :) :: node_lat
    real(8), allocatable, dimension(  :) :: node_area
    real(8), allocatable, dimension(  :) :: node_edge_size
    integer, allocatable, dimension(:,:) :: node_ngb_idx
    integer, allocatable, dimension(:,:) :: node_cell_idx
    integer, allocatable, dimension(:,:) :: node_edge_idx

    integer :: num_edge = 0
    real(8), allocatable, dimension(  :) :: edge_x
    real(8), allocatable, dimension(  :) :: edge_y
    real(8), allocatable, dimension(  :) :: edge_z
    real(8), allocatable, dimension(  :) :: edge_lon
    real(8), allocatable, dimension(  :) :: edge_lat
    real(8), allocatable, dimension(  :) :: edge_area
    real(8), allocatable, dimension(  :) :: edge_length
    integer, allocatable, dimension(:,:) :: edge_ngb_idx
    integer, allocatable, dimension(:,:) :: edge_cell_idx
    integer, allocatable, dimension(:,:) :: edge_node_idx
  contains
    procedure :: init => mesh_init
    final :: mesh_final
  end type mesh_type

contains

  subroutine mesh_init(this, num_cell, num_node, num_edge, cell_max_edges, node_max_cells)

    class(mesh_type), intent(inout) :: this
    integer         , intent(in   ) :: num_cell
    integer         , intent(in   ) :: num_node
    integer         , intent(in   ) :: num_edge
    integer         , intent(in   ) :: cell_max_edges
    integer         , intent(in   ) :: node_max_cells

    this%num_cell  = num_cell
    this%num_node  = num_node
    this%num_edge  = num_edge
    this%cell_max_edges = cell_max_edges

    allocate(this%cell_x        (               num_cell))
    allocate(this%cell_y        (               num_cell))
    allocate(this%cell_z        (               num_cell))
    allocate(this%cell_lon      (               num_cell))
    allocate(this%cell_lat      (               num_cell))
    allocate(this%cell_area     (               num_cell))
    allocate(this%cell_distance (cell_max_edges,num_cell))
    allocate(this%cell_edge_size(               num_cell))
    allocate(this%cell_ngb_idx  (cell_max_edges,num_cell))
    allocate(this%cell_node_idx (cell_max_edges,num_cell))
    allocate(this%cell_edge_idx (cell_max_edges,num_cell))

    allocate(this%node_x        (               num_node))
    allocate(this%node_y        (               num_node))
    allocate(this%node_z        (               num_node))
    allocate(this%node_lon      (               num_node))
    allocate(this%node_lat      (               num_node))
    allocate(this%node_area     (               num_node))
    allocate(this%node_edge_size(               num_node))
    allocate(this%node_ngb_idx  (node_max_cells,num_node))
    allocate(this%node_cell_idx (node_max_cells,num_node))
    allocate(this%node_edge_idx (node_max_cells,num_node))

    allocate(this%edge_x        (                 num_edge))
    allocate(this%edge_y        (                 num_edge))
    allocate(this%edge_z        (                 num_edge))
    allocate(this%edge_lon      (                 num_edge))
    allocate(this%edge_lat      (                 num_edge))
    allocate(this%edge_area     (                 num_edge))
    allocate(this%edge_length   (                 num_edge))
    allocate(this%edge_ngb_idx  (cell_max_edges*2,num_edge))
    allocate(this%edge_cell_idx (               2,num_edge))
    allocate(this%edge_node_idx (               2,num_edge))

  end subroutine mesh_init

  subroutine mesh_final(this)

    type(mesh_type), intent(inout) :: this

    if (allocated(this%cell_x        )) deallocate(this%cell_x        )
    if (allocated(this%cell_y        )) deallocate(this%cell_y        )
    if (allocated(this%cell_z        )) deallocate(this%cell_z        )
    if (allocated(this%cell_lon      )) deallocate(this%cell_lon      )
    if (allocated(this%cell_lat      )) deallocate(this%cell_lat      )
    if (allocated(this%cell_area     )) deallocate(this%cell_area     )
    if (allocated(this%cell_distance )) deallocate(this%cell_distance )
    if (allocated(this%cell_edge_size)) deallocate(this%cell_edge_size)
    if (allocated(this%cell_ngb_idx  )) deallocate(this%cell_ngb_idx  )
    if (allocated(this%cell_node_idx )) deallocate(this%cell_node_idx )
    if (allocated(this%cell_edge_idx )) deallocate(this%cell_edge_idx )

    if (allocated(this%node_x        )) deallocate(this%node_x        )
    if (allocated(this%node_y        )) deallocate(this%node_y        )
    if (allocated(this%node_z        )) deallocate(this%node_z        )
    if (allocated(this%node_lon      )) deallocate(this%node_lon      )
    if (allocated(this%node_lat      )) deallocate(this%node_lat      )
    if (allocated(this%node_area     )) deallocate(this%node_area     )
    if (allocated(this%node_edge_size)) deallocate(this%node_edge_size)
    if (allocated(this%node_ngb_idx  )) deallocate(this%node_ngb_idx  )
    if (allocated(this%node_cell_idx )) deallocate(this%node_cell_idx )
    if (allocated(this%node_edge_idx )) deallocate(this%node_edge_idx )
    
    if (allocated(this%edge_x        )) deallocate(this%edge_x        )
    if (allocated(this%edge_y        )) deallocate(this%edge_y        )
    if (allocated(this%edge_z        )) deallocate(this%edge_z        )
    if (allocated(this%edge_lon      )) deallocate(this%edge_lon      )
    if (allocated(this%edge_lat      )) deallocate(this%edge_lat      )
    if (allocated(this%edge_area     )) deallocate(this%edge_area     )
    if (allocated(this%edge_length   )) deallocate(this%edge_length   )
    if (allocated(this%edge_cell_idx )) deallocate(this%edge_cell_idx )
    if (allocated(this%edge_node_idx )) deallocate(this%edge_node_idx )

  end subroutine mesh_final

end module mesh_mod
