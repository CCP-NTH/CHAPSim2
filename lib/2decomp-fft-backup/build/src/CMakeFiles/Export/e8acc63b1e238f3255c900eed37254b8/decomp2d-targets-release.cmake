#----------------------------------------------------------------
# Generated CMake target import file for configuration "RELEASE".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "decomp2d" for configuration "RELEASE"
set_property(TARGET decomp2d APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(decomp2d PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELEASE "Fortran"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/lib/libdecomp2d.a"
  )

list(APPEND _cmake_import_check_targets decomp2d )
list(APPEND _cmake_import_check_files_for_decomp2d "${_IMPORT_PREFIX}/lib/libdecomp2d.a" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
