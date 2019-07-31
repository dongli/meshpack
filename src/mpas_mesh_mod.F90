module mpas_mesh_mod

  use fiona
  use mesh_mod

  implicit none

  private

  public mpas_mesh_input

contains

  subroutine mpas_mesh_input(file_path, mesh)

    character(*)   , intent(in   ) :: file_path
    type(mesh_type), intent(inout) :: mesh

    integer nCells, nVertices, nEdges, maxEdges, vertexDegree

    call io_init()

    call io_create_dataset('mpas_mesh', file_path=file_path, mode='input')
    call io_get_dim('mpas_mesh', 'nCells'      , nCells)
    call io_get_dim('mpas_mesh', 'nVertices'   , nVertices)
    call io_get_dim('mpas_mesh', 'nEdges'      , nEdges)
    call io_get_dim('mpas_mesh', 'maxEdges'    , maxEdges)
    call io_get_dim('mpas_mesh', 'vertexDegree', vertexDegree)

    call mesh%init(nCells, nVertices, nEdges, maxEdges, vertexDegree)

    call io_start_input('mpas_mesh')
    call io_input('mpas_mesh', 'xCell'          , mesh%cell_x         )
    call io_input('mpas_mesh', 'yCell'          , mesh%cell_y         )
    call io_input('mpas_mesh', 'zCell'          , mesh%cell_z         )
    call io_input('mpas_mesh', 'lonCell'        , mesh%cell_lon       )
    call io_input('mpas_mesh', 'latCell'        , mesh%cell_lat       )
    call io_input('mpas_mesh', 'areaCell'       , mesh%cell_area      )
    call io_input('mpas_mesh', 'dcEdge'         , mesh%cell_distance  )
    call io_input('mpas_mesh', 'nEdgesOnCell'   , mesh%cell_edge_size )
    call io_input('mpas_mesh', 'cellsOnCell'    , mesh%cell_ngb_idx   )
    call io_input('mpas_mesh', 'verticesOnCell' , mesh%cell_node_idx  )
    call io_input('mpas_mesh', 'edgesOnCell'    , mesh%cell_edge_idx  )

    call io_input('mpas_mesh', 'xVertex'        , mesh%node_x         )
    call io_input('mpas_mesh', 'yVertex'        , mesh%node_y         )
    call io_input('mpas_mesh', 'zVertex'        , mesh%node_z         )
    call io_input('mpas_mesh', 'lonVertex'      , mesh%node_lon       )
    call io_input('mpas_mesh', 'latVertex'      , mesh%node_lat       )
    call io_input('mpas_mesh', 'areaTriangle'   , mesh%node_area      )
    call io_input('mpas_mesh', 'cellsOnVertex'  , mesh%node_cell_idx  )
    call io_input('mpas_mesh', 'edgesOnVertex'  , mesh%node_edge_idx  )

    call io_input('mpas_mesh', 'xEdge'          , mesh%edge_x         )
    call io_input('mpas_mesh', 'yEdge'          , mesh%edge_y         )
    call io_input('mpas_mesh', 'zEdge'          , mesh%edge_z         )
    call io_input('mpas_mesh', 'lonEdge'        , mesh%edge_lon       )
    call io_input('mpas_mesh', 'latEdge'        , mesh%edge_lat       )
    call io_input('mpas_mesh', 'dvEdge'         , mesh%edge_length    )
    call io_input('mpas_mesh', 'edgesOnEdge'    , mesh%edge_ngb_idx   )
    call io_input('mpas_mesh', 'cellsOnEdge'    , mesh%edge_cell_idx  )
    call io_input('mpas_mesh', 'verticesOnEdge' , mesh%edge_node_idx  )

    call io_end_input('mpas_mesh')

  end subroutine mpas_mesh_input

end module mpas_mesh_mod
