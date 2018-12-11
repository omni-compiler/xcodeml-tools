module atlas_FieldSet_module

implicit none

private

contains

function field_by_idx_size_t() result(field)
  use atlas_Field_module, only: atlas_Field
  type(atlas_Field) :: field
end function

function field_by_idx_int() result(field)
  use atlas_Field_module, only: atlas_Field
  type(atlas_Field) :: field
end function

end module atlas_FieldSet_module
