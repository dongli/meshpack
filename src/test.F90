program test

  use mesh_mod,      only: mesh_type
  use mpas_mesh_mod, only: mpas_mesh_input

  implicit none

  type(mesh_type) mesh
  character(256) mesh_file_path

  call get_command_argument(1, mesh_file_path)

  call mpas_mesh_input(mesh_file_path, mesh)

end program test
